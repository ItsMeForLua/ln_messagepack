/-
Copyright (C) <2025>  Andrew D. France

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
-/

namespace Float

def inf : Float :=
  Float.ofBits 0x7ff0000000000000

def negInf : Float :=
  Float.ofBits 0xfff0000000000000

def nan : Float :=
  -- This is a quiet NaN pattern, but it may be worth noting that many others exist
  Float.ofBits 0x7ff8000000000000

end Float

deriving instance BEq for ByteArray
deriving instance Repr for ByteArray

-- Maximum limits to prevent DoS attacks
private def MAX_PAYLOAD_SIZE : Nat := 64 * 1024 * 1024  -- 64MB
private def MAX_RECURSION_DEPTH : Nat := 1000
private def MAX_COLLECTION_SIZE : Nat := 1024 * 1024    -- 1M elements

namespace ln_messagepack

/-! # MessagePack Core Implementation -/

/-! ## MessagePack Value Type -/
inductive MsgPackValue where
  | nil : MsgPackValue
  | bool : Bool → MsgPackValue
  | int : Int → MsgPackValue
  | uint : Nat → MsgPackValue
  | float : Float → MsgPackValue
  | str : String → MsgPackValue
  | bin : ByteArray → MsgPackValue
  | arr : Array MsgPackValue → MsgPackValue
  | map : Array (MsgPackValue × MsgPackValue) → MsgPackValue
  | ext : UInt8 → ByteArray → MsgPackValue
  deriving BEq, Inhabited

/-! ## Pretty-printing -/
partial def reprMsgPackValue : MsgPackValue → String
  | .nil => "nil"
  | .bool b => toString b
  | .int i => toString i
  | .uint n => toString n
  | .float f => toString f
  | .str s => s!"\"{s}\""
  | .bin b => s!"bin[{b.size}]"
  | .arr a => "arr[" ++ ", ".intercalate (a.map reprMsgPackValue |>.toList) ++ "]"
  | .map m => "map{" ++ ", ".intercalate (m.map (fun (k,v) => s!"{reprMsgPackValue k}: {reprMsgPackValue v}") |>.toList) ++ "}"
  | .ext t d => s!"ext({t})[{d.size}]"

instance : Repr MsgPackValue where
  reprPrec v _ := reprMsgPackValue v

/-! ## ENCODING LOGIC -/

private def concatByteArrays (arr : Array ByteArray) : ByteArray :=
  arr.foldl (init := ByteArray.empty) (· ++ ·)

private def encodeUInt16 (u : UInt16) : ByteArray :=
  ByteArray.mk #[UInt8.ofNat (u.toNat >>> 8), UInt8.ofNat (u.toNat &&& 0xFF)]

private def encodeUInt32 (u : UInt32) : ByteArray :=
  ByteArray.mk #[
    UInt8.ofNat (u.toNat >>> 24), UInt8.ofNat ((u.toNat >>> 16) &&& 0xFF),
    UInt8.ofNat ((u.toNat >>> 8) &&& 0xFF), UInt8.ofNat (u.toNat &&& 0xFF)
  ]

private def encodeUInt64 (u : UInt64) : ByteArray :=
  ByteArray.mk #[
    UInt8.ofNat (u.toNat >>> 56), UInt8.ofNat ((u.toNat >>> 48) &&& 0xFF),
    UInt8.ofNat ((u.toNat >>> 40) &&& 0xFF), UInt8.ofNat ((u.toNat >>> 32) &&& 0xFF),
    UInt8.ofNat ((u.toNat >>> 24) &&& 0xFF), UInt8.ofNat ((u.toNat >>> 16) &&& 0xFF),
    UInt8.ofNat ((u.toNat >>> 8) &&& 0xFF), UInt8.ofNat (u.toNat &&& 0xFF)
  ]

private def encodeInt (i : Int) : ByteArray :=
  if i >= 0 && i < 128 then ByteArray.mk #[UInt8.ofNat i.toNat]
  else if i >= -32 && i < 0 then ByteArray.mk #[UInt8.ofInt i]
  else if i >= 0 && i < 256 then ByteArray.mk #[0xcc, UInt8.ofNat i.toNat]
  else if i >= 0 && i < 65536 then ByteArray.mk #[0xcd] ++ encodeUInt16 (UInt16.ofNat i.toNat)
  else if i >= 0 && i < 4294967296 then ByteArray.mk #[0xce] ++ encodeUInt32 (UInt32.ofNat i.toNat)
  else if i >= 0 then ByteArray.mk #[0xcf] ++ encodeUInt64 (UInt64.ofNat i.toNat)
  else if i >= -128 then ByteArray.mk #[0xd0, UInt8.ofInt i]
  else if i >= -32768 then ByteArray.mk #[0xd1] ++ encodeUInt16 (UInt16.ofInt i)
  else if i >= -2147483648 then ByteArray.mk #[0xd2] ++ encodeUInt32 (UInt32.ofInt i)
  else ByteArray.mk #[0xd3] ++ encodeUInt64 (UInt64.ofInt i)

private def encodeString (s : String) : ByteArray :=
  let bytes := s.toUTF8
  let len := bytes.size
  if len < 32 then ByteArray.mk #[UInt8.ofNat (0xa0 + len)] ++ bytes
  else if len < 256 then ByteArray.mk #[0xd9, UInt8.ofNat len] ++ bytes
  else if len < 65536 then ByteArray.mk #[0xda] ++ encodeUInt16 (UInt16.ofNat len) ++ bytes
  else ByteArray.mk #[0xdb] ++ encodeUInt32 (UInt32.ofNat len) ++ bytes

/-
By using a `partial def`, we allow Lean's termination checking to
succeed on the recursion that is happening in the `.arr` and `.map` cases.
-/
partial def encodeToBytes (v : MsgPackValue) : ByteArray :=
  match v with
  | .nil      => ByteArray.mk #[0xc0]
  | .bool b   => ByteArray.mk #[if b then 0xc3 else 0xc2]
  | .int i    => encodeInt i
  | .uint n   => encodeInt (n : Int)
  | .str s    => encodeString s
  | .bin b    =>
      let len := b.size
      if len < 256 then ByteArray.mk #[0xc4, UInt8.ofNat len] ++ b
      else if len < 65536 then ByteArray.mk #[0xc5] ++ encodeUInt16 (UInt16.ofNat len) ++ b
      else ByteArray.mk #[0xc6] ++ encodeUInt32 (UInt32.ofNat len) ++ b
  | .arr a    =>
      let len := a.size
      let encodedElems := concatByteArrays (a.map encodeToBytes)
      if len < 16 then ByteArray.mk #[UInt8.ofNat (0x90 + len)] ++ encodedElems
      else if len < 65536 then ByteArray.mk #[0xdc] ++ encodeUInt16 (UInt16.ofNat len) ++ encodedElems
      else ByteArray.mk #[0xdd] ++ encodeUInt32 (UInt32.ofNat len) ++ encodedElems
  | .map m    =>
      let len := m.size
      let encodedPairs := concatByteArrays (m.map (fun (k, v) => encodeToBytes k ++ encodeToBytes v))
      if len < 16 then ByteArray.mk #[UInt8.ofNat (0x80 + len)] ++ encodedPairs
      else if len < 65536 then ByteArray.mk #[0xde] ++ encodeUInt16 (UInt16.ofNat len) ++ encodedPairs
      else ByteArray.mk #[0xdf] ++ encodeUInt32 (UInt32.ofNat len) ++ encodedPairs
  | .float f  =>
      let bits := f.toBits  -- Use the built-in method
      ByteArray.mk #[0xcb] ++ encodeUInt64 bits
  | .ext t d  =>
    let len := d.size
    let typeByte := ByteArray.mk #[t]
    if len == 1 then ByteArray.mk #[0xd4] ++ typeByte ++ d
    else if len == 2 then ByteArray.mk #[0xd5] ++ typeByte ++ d
    else if len == 4 then ByteArray.mk #[0xd6] ++ typeByte ++ d
    else if len == 8 then ByteArray.mk #[0xd7] ++ typeByte ++ d
    else if len == 16 then ByteArray.mk #[0xd8] ++ typeByte ++ d
    else if len < 256 then ByteArray.mk #[0xc7, UInt8.ofNat len] ++ typeByte ++ d
    else if len < 65536 then ByteArray.mk #[0xc8] ++ encodeUInt16 (UInt16.ofNat len) ++ typeByte ++ d
    else ByteArray.mk #[0xc9] ++ encodeUInt32 (UInt32.ofNat len) ++ typeByte ++ d

/-! ## DECODING LOGIC -/

open Except

-- Validation helper to prevent DoS attacks
private def validateLength (len : Nat) (offset : Nat) (totalSize : Nat) : Except String Unit :=
  if len > MAX_PAYLOAD_SIZE then
    throw s!"payload too large: {len} bytes (max {MAX_PAYLOAD_SIZE})"
  else if len > MAX_COLLECTION_SIZE then
    throw s!"collection too large: {len} elements (max {MAX_COLLECTION_SIZE})"
  else if offset + len > totalSize then
    throw s!"length {len} at offset {offset} exceeds total size {totalSize}"
  else
    pure ()

-- Safe byte access with bounds checking
private def getByte (bytes : ByteArray) (i : Nat) : Except String UInt8 :=
  if h : i < bytes.size then
    pure (bytes[i]'h)  -- Needs to have a proof here, always.
  else
    throw s!"unexpected end of input at offset {i}"

-- Safe slice extraction with validation
private def getSlice (bytes : ByteArray) (start len : Nat) : Except String ByteArray :=
  if len > MAX_PAYLOAD_SIZE then
    throw s!"slice too large: {len} bytes"
  else if start >= bytes.size then
    throw s!"start offset {start} beyond array size {bytes.size}"
  else if start + len > bytes.size then
    throw s!"slice [{start}:{start + len}) exceeds array size {bytes.size}"
  else
    pure (bytes.extract start (start + len))

private def readUInt16BE (bytes : ByteArray) (offset : Nat) : Except String UInt16 := do
  let hi ← getByte bytes offset
  let lo ← getByte bytes (offset + 1)
  pure ((hi.toUInt16 <<< 8) ||| lo.toUInt16)

private def readUInt32BE (bytes : ByteArray) (offset : Nat) : Except String UInt32 := do
  let b0 ← getByte bytes offset
  let b1 ← getByte bytes (offset + 1)
  let b2 ← getByte bytes (offset + 2)
  let b3 ← getByte bytes (offset + 3)
  pure ((b0.toUInt32 <<< 24) ||| (b1.toUInt32 <<< 16) ||| (b2.toUInt32 <<< 8) ||| b3.toUInt32)

private def readUInt64BE (bytes : ByteArray) (offset : Nat) : Except String UInt64 := do
  let b0 ← getByte bytes offset
  let b1 ← getByte bytes (offset + 1)
  let b2 ← getByte bytes (offset + 2)
  let b3 ← getByte bytes (offset + 3)
  let b4 ← getByte bytes (offset + 4)
  let b5 ← getByte bytes (offset + 5)
  let b6 ← getByte bytes (offset + 6)
  let b7 ← getByte bytes (offset + 7)
  pure ((b0.toUInt64 <<< 56) ||| (b1.toUInt64 <<< 48) ||| (b2.toUInt64 <<< 40) ||| (b3.toUInt64 <<< 32) |||
        (b4.toUInt64 <<< 24) ||| (b5.toUInt64 <<< 16) ||| (b6.toUInt64 <<< 8) ||| b7.toUInt64)

private def toSInt8 (b : UInt8) : Int :=
  let n := b.toNat
  if n < 128 then n else n - 256

private def toSInt16 (w : UInt16) : Int :=
  let n := w.toNat
  if n < 0x8000 then n else n - 0x10000

private def toSInt32 (w : UInt32) : Int :=
  let n := w.toNat
  if n < 0x80000000 then n else n - 0x100000000

private def toSInt64 (w : UInt64) : Int :=
  let n := w.toNat
  if n < 0x8000000000000000 then n else n - 0x10000000000000000

/-
The main MessagePack parser with recursion depth tracking:
Returns the value, and the next offset.
-/
partial def parse (bytes : ByteArray) (offset : Nat) (depth : Nat := 0) : Except String (MsgPackValue × Nat) := do
  if depth > MAX_RECURSION_DEPTH then
    throw s!"maximum recursion depth exceeded: {depth}"

  let b ← getByte bytes offset
  let offset' := offset + 1

  if b <= 0x7f then
    pure (MsgPackValue.int (Int.ofNat b.toNat), offset')
  else if b >= 0xe0 then
    pure (MsgPackValue.int (Int.ofNat b.toNat - 256), offset')
  else if b == 0xc0 then
    pure (MsgPackValue.nil, offset')
  else if b == 0xc2 then
    pure (MsgPackValue.bool false, offset')
  else if b == 0xc3 then
    pure (MsgPackValue.bool true, offset')
  else if b == 0xcc then
    let n ← getByte bytes offset'
    pure (MsgPackValue.uint n.toNat, offset' + 1)
  else if b == 0xcd then
    let u ← readUInt16BE bytes offset'
    pure (MsgPackValue.uint u.toNat, offset' + 2)
  else if b == 0xce then
    let u ← readUInt32BE bytes offset'
    pure (MsgPackValue.uint u.toNat, offset' + 4)
  else if b == 0xcf then
    let u ← readUInt64BE bytes offset'
    pure (MsgPackValue.uint u.toNat, offset' + 8)
  else if b == 0xd0 then
    let n ← getByte bytes offset'
    pure (MsgPackValue.int (toSInt8 n), offset' + 1)
  else if b == 0xd1 then
    let u ← readUInt16BE bytes offset'
    pure (MsgPackValue.int (toSInt16 u), offset' + 2)
  else if b == 0xd2 then
    let u ← readUInt32BE bytes offset'
    pure (MsgPackValue.int (toSInt32 u), offset' + 4)
  else if b == 0xd3 then
    let u ← readUInt64BE bytes offset'
    pure (MsgPackValue.int (toSInt64 u), offset' + 8)
  else if b >= 0xa0 && b <= 0xbf then
    let len := (b - 0xa0).toNat
    if len == 0 then
      pure (MsgPackValue.str "", offset')
    else
      validateLength len offset' bytes.size
      let sbytes ← getSlice bytes offset' len
      pure (MsgPackValue.str (String.fromUTF8! sbytes), offset' + len)
  else if b == 0xd9 then
    let lenByte ← getByte bytes offset'
    let len := lenByte.toNat
    if len == 0 then
      pure (MsgPackValue.str "", offset' + 1)
    else
      validateLength len (offset' + 1) bytes.size
      let sbytes ← getSlice bytes (offset' + 1) len
      pure (MsgPackValue.str (String.fromUTF8! sbytes), offset' + 1 + len)
  else if b == 0xda then
    let u ← readUInt16BE bytes offset'
    let len := u.toNat
    if len == 0 then
      pure (MsgPackValue.str "", offset' + 2)
    else
      validateLength len (offset' + 2) bytes.size
      let sbytes ← getSlice bytes (offset' + 2) len
      pure (MsgPackValue.str (String.fromUTF8! sbytes), offset' + 2 + len)
  else if b == 0xdb then
    let u ← readUInt32BE bytes offset'
    let len := u.toNat
    if len == 0 then
      pure (MsgPackValue.str "", offset' + 4)
    else
      validateLength len (offset' + 4) bytes.size
      let sbytes ← getSlice bytes (offset' + 4) len
      pure (MsgPackValue.str (String.fromUTF8! sbytes), offset' + 4 + len)
  else if b == 0xc4 then
    let lenByte ← getByte bytes offset'
    let len := lenByte.toNat
    validateLength len (offset' + 1) bytes.size
    let bin ← getSlice bytes (offset' + 1) len
    pure (MsgPackValue.bin bin, offset' + 1 + len)
  else if b == 0xc5 then
    let u ← readUInt16BE bytes offset'
    let len := u.toNat
    validateLength len (offset' + 2) bytes.size
    let bin ← getSlice bytes (offset' + 2) len
    pure (MsgPackValue.bin bin, offset' + 2 + len)
  else if b == 0xc6 then
    let u ← readUInt32BE bytes offset'
    let len := u.toNat
    validateLength len (offset' + 4) bytes.size
    let bin ← getSlice bytes (offset' + 4) len
    pure (MsgPackValue.bin bin, offset' + 4 + len)
  else if b >= 0x90 && b <= 0x9f then
    let len := (b - 0x90).toNat
    if len > MAX_COLLECTION_SIZE then
      throw s!"array too large: {len} elements (max {MAX_COLLECTION_SIZE})"
    let mut currentOffset := offset'
    let mut elems := #[]
    for _ in [0:len] do
      let (v, next) ← parse bytes currentOffset (depth + 1)
      elems := elems.push v
      currentOffset := next
    pure (MsgPackValue.arr elems, currentOffset)
  else if b == 0xdc then
    let u ← readUInt16BE bytes offset'
    let len := u.toNat
    if len > MAX_COLLECTION_SIZE then
      throw s!"array too large: {len} elements (max {MAX_COLLECTION_SIZE})"
    let mut currentOffset := offset' + 2
    let mut elems := #[]
    for _ in [0:len] do
      let (v, next) ← parse bytes currentOffset (depth + 1)
      elems := elems.push v
      currentOffset := next
    pure (MsgPackValue.arr elems, currentOffset)
  else if b == 0xdd then
    let u ← readUInt32BE bytes offset'
    let len := u.toNat
    if len > MAX_COLLECTION_SIZE then
      throw s!"array too large: {len} elements (max {MAX_COLLECTION_SIZE})"
    let mut currentOffset := offset' + 4
    let mut elems := #[]
    for _ in [0:len] do
      let (v, next) ← parse bytes currentOffset (depth + 1)
      elems := elems.push v
      currentOffset := next
    pure (MsgPackValue.arr elems, currentOffset)
  else if b >= 0x80 && b <= 0x8f then
    let len := (b - 0x80).toNat
    if len > MAX_COLLECTION_SIZE then
      throw s!"map too large: {len} pairs (max {MAX_COLLECTION_SIZE})"
    let mut currentOffset := offset'
    let mut pairs := #[]
    for _ in [0:len] do
      let (k, next1) ← parse bytes currentOffset (depth + 1)
      let (v, next2) ← parse bytes next1 (depth + 1)
      pairs := pairs.push (k, v)
      currentOffset := next2
    pure (MsgPackValue.map pairs, currentOffset)
  else if b == 0xde then
    let u ← readUInt16BE bytes offset'
    let len := u.toNat
    if len > MAX_COLLECTION_SIZE then
      throw s!"map too large: {len} pairs (max {MAX_COLLECTION_SIZE})"
    let mut currentOffset := offset' + 2
    let mut pairs := #[]
    for _ in [0:len] do
      let (k, next1) ← parse bytes currentOffset (depth + 1)
      let (v, next2) ← parse bytes next1 (depth + 1)
      pairs := pairs.push (k, v)
      currentOffset := next2
    pure (MsgPackValue.map pairs, currentOffset)
  else if b == 0xdf then
    let u ← readUInt32BE bytes offset'
    let len := u.toNat
    if len > MAX_COLLECTION_SIZE then
      throw s!"map too large: {len} pairs (max {MAX_COLLECTION_SIZE})"
    let mut currentOffset := offset' + 4
    let mut pairs := #[]
    for _ in [0:len] do
      let (k, next1) ← parse bytes currentOffset (depth + 1)
      let (v, next2) ← parse bytes next1 (depth + 1)
      pairs := pairs.push (k, v)
      currentOffset := next2
    pure (MsgPackValue.map pairs, currentOffset)
  else if b == 0xd4 then -- fixext 1
    let type ← getByte bytes offset'
    let data ← getSlice bytes (offset' + 1) 1
    pure (.ext type data, offset' + 2)
  else if b == 0xd5 then -- fixext 2
    let type ← getByte bytes offset'
    let data ← getSlice bytes (offset' + 1) 2
    pure (.ext type data, offset' + 3)
  else if b == 0xd6 then -- fixext 4
    let type ← getByte bytes offset'
    let data ← getSlice bytes (offset' + 1) 4
    pure (.ext type data, offset' + 5)
  else if b == 0xd7 then -- fixext 8
    let type ← getByte bytes offset'
    let data ← getSlice bytes (offset' + 1) 8
    pure (.ext type data, offset' + 9)
  else if b == 0xd8 then -- fixext 16
    let type ← getByte bytes offset'
    let data ← getSlice bytes (offset' + 1) 16
    pure (.ext type data, offset' + 17)
  else if b == 0xc7 then -- ext 8
    let len ← getByte bytes offset'
    validateLength len.toNat (offset' + 2) bytes.size
    let type ← getByte bytes (offset' + 1)
    let data ← getSlice bytes (offset' + 2) len.toNat
    pure (.ext type data, offset' + 2 + len.toNat)
  else if b == 0xc8 then -- ext 16
    let len ← readUInt16BE bytes offset'
    validateLength len.toNat (offset' + 3) bytes.size
    let type ← getByte bytes (offset' + 2)
    let data ← getSlice bytes (offset' + 3) len.toNat
    pure (.ext type data, offset' + 3 + len.toNat)
  else if b == 0xc9 then -- ext 32
    let len ← readUInt32BE bytes offset'
    validateLength len.toNat (offset' + 5) bytes.size
    let type ← getByte bytes (offset' + 4)
    let data ← getSlice bytes (offset' + 5) len.toNat
    pure (.ext type data, offset' + 5 + len.toNat)
  else if b == 0xca then -- float 32
    let u ← readUInt32BE bytes offset'
    let f32 := Float32.ofBits u
    let f := f32.toFloat
    pure (MsgPackValue.float f, offset' + 4)
  else if b == 0xcb then -- float 64
    let u ← readUInt64BE bytes offset'
    let f := Float.ofBits u
    pure (MsgPackValue.float f, offset' + 8)
  else
    throw s!"unsupported msgpack format code: {b}"

/-- Top level decoder for a full byte array. -/
def decodeFromBytes (bytes : ByteArray) : Except String MsgPackValue := do
  if bytes.isEmpty then
    throw "cannot decode empty byte array"
  let (v, consumed) ← parse bytes 0
  if consumed != bytes.size then
    throw s!"did not consume all bytes. Consumed {consumed} of {bytes.size}"
  pure v

end ln_messagepack

import LnMessagepack.Core

/-
Copyright [2025] [Andrew D. France]

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-/

namespace LnMessagepack

/-- A typeclass for types that can be encoded into a `MsgPackValue`. -/
class MsgPackEncode (α : Type) where
  encode : α → MsgPackValue

/-- A typeclass for types that can be decoded from a `MsgPackValue`. -/
class MsgPackDecode (α : Type) where
  /-- Decoding can fail, so it returns an `Option`. -/
  decode : MsgPackValue → Option α

export MsgPackEncode (encode)
export MsgPackDecode (decode)

/-- Encodes a value of type `α` directly to `ByteArray`. -/
def encodeToMsgPackBytes [MsgPackEncode α] (a : α) : ByteArray :=
  encodeToBytes (encode a)

/-- Decodes a `ByteArray` directly into a value of type `α`. -/
def decodeFromMsgPackBytes [MsgPackDecode α] (bs : ByteArray) : Option α :=
  match decodeFromBytes bs with
  | .ok v => decode v
  | .error _ => none

/-! ## Standard Instances -/

instance : MsgPackEncode Bool where
  encode b := .bool b
instance : MsgPackDecode Bool where
  decode | .bool b => some b | _ => none

instance : MsgPackEncode String where
  encode s := .str s
instance : MsgPackDecode String where
  decode | .str s => some s | _ => none

instance : MsgPackEncode Nat where
  encode n := .uint n
instance : MsgPackDecode Nat where
  decode
    | .uint n => some n
    | .int i => if i >= 0 then some i.toNat else none
    | _ => none

instance : MsgPackEncode Int where
  encode i := .int i
instance : MsgPackDecode Int where
  decode
    | .int i => some i
    | .uint n => some (n : Int)
    | _ => none

/-! ## Generic Collection Instances -/

instance [MsgPackEncode α] : MsgPackEncode (Array α) where
  encode arr := .arr (arr.map encode)

instance [MsgPackDecode α] : MsgPackDecode (Array α) where
  -- `Array.mapM` applies `decode` to each element and returns `none` if any fail.
  decode | .arr a => a.mapM decode | _ => none

instance [MsgPackEncode α] : MsgPackEncode (List α) where

  encode xs := encode xs.toArray
  -- Reusing the Array Instance for simplicity.
instance [MsgPackDecode α] : MsgPackDecode (List α) where
  -- Reusing the Array Instance for simplicity.
  decode v := (decode (α := Array α) v).map Array.toList

/-! ## Extension Type Example: Timestamp -/

/-- Represents a UTC timestamp, which is compatible with MessagePack's timestamp extension type. -/
structure Timestamp where
  /-- Seconds since the Unix epoch (1970-01-01 00:00:00 UTC). -/
  sec : Int
  /-- Nanoseconds for precision. -/
  nsec : UInt32
  deriving BEq, Repr

-- This instance teaches Lean how to print a Timestamp.
instance : ToString Timestamp where
  toString ts := s!"Timestamp(sec: {ts.sec}, nsec: {ts.nsec})"

-- Helper to convert UInt64 to big-endian bytes.
private def uint64ToBytesBE (u : UInt64) : ByteArray :=
  ByteArray.mk #[
    UInt8.ofNat (u.toNat >>> 56), UInt8.ofNat ((u.toNat >>> 48) &&& 0xFF),
    UInt8.ofNat ((u.toNat >>> 40) &&& 0xFF), UInt8.ofNat ((u.toNat >>> 32) &&& 0xFF),
    UInt8.ofNat ((u.toNat >>> 24) &&& 0xFF), UInt8.ofNat ((u.toNat >>> 16) &&& 0xFF),
    UInt8.ofNat ((u.toNat >>> 8) &&& 0xFF), UInt8.ofNat (u.toNat &&& 0xFF)
  ]

-- Helper to convert bigger endian bytes to UInt64.
private def bytesToUInt64BE (b : ByteArray) : UInt64 :=
  (b.get! 0).toUInt64 <<< 56 ||| (b.get! 1).toUInt64 <<< 48 |||
  (b.get! 2).toUInt64 <<< 40 ||| (b.get! 3).toUInt64 <<< 32 |||
  (b.get! 4).toUInt64 <<< 24 ||| (b.get! 5).toUInt64 <<< 16 |||
  (b.get! 6).toUInt64 <<< 8  ||| (b.get! 7).toUInt64

instance : MsgPackEncode Timestamp where
  encode ts :=
    -- NOTE: MessagePack spec reserves extension type `-1` for timestamps.
    let type : UInt8 := UInt8.ofInt (-1)
    -- We will use the 96-bit format: 32-bit nanoseconds, and 64-bit seconds.
    -- This REQUIRES a 12-byte data payload.
    let nsecBytes := ByteArray.mk #[
      (ts.nsec >>> 24).toUInt8, ((ts.nsec >>> 16) &&& 0xff).toUInt8,
      ((ts.nsec >>> 8) &&& 0xff).toUInt8, (ts.nsec &&& 0xff).toUInt8
    ]
    let secBytes := uint64ToBytesBE (UInt64.ofInt ts.sec)
    .ext type (nsecBytes ++ secBytes)

instance : MsgPackDecode Timestamp where
  decode
    | .ext type payload =>
      -- Neccesary check for timestamp type (-1) and 12-byte payload for 96-bit format.
      if type.toInt8 == -1 && payload.size == 12 then
        let nsec :=
          (payload.get! 0).toUInt32 <<< 24 ||| (payload.get! 1).toUInt32 <<< 16 |||
          (payload.get! 2).toUInt32 <<< 8 ||| (payload.get! 3).toUInt32
        let sec := (bytesToUInt64BE (payload.extract 4 12)).toNat
        some { sec, nsec }
      else
        none
    | _ => none

end LnMessagepack

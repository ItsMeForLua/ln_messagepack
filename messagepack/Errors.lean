/-! ## MessagePack Error Types -/

inductive MsgPackError where
  /-- Input validation errors -/
  | emptyInput : MsgPackError
  | unexpectedEOF : Nat → MsgPackError  -- offset where EOF occurred
  | incompleteData : Nat → Nat → MsgPackError  -- expected length, actual length
  
  /-- Format and parsing errors -/
  | unsupportedFormat : UInt8 → MsgPackError  -- format code
  | invalidUTF8 : ByteArray → MsgPackError
  | malformedFloat : UInt64 → MsgPackError  -- bits that couldn't be converted
  
  /-- Security and resource limits -/
  | payloadTooLarge : Nat → Nat → MsgPackError  -- actual size, max size
  | collectionTooLarge : Nat → Nat → MsgPackError  -- actual count, max count
  | recursionDepthExceeded : Nat → MsgPackError  -- depth reached
  
  /-- Structural errors -/
  | extraBytes : Nat → Nat → MsgPackError  -- consumed, total
  | invalidMapKey : MsgPackValue → MsgPackError  -- key that caused issues
  | duplicateMapKey : MsgPackValue → MsgPackError
  
  /-- Extension type errors -/
  | unknownExtensionType : UInt8 → ByteArray → MsgPackError
  | invalidExtensionLength : UInt8 → Nat → Nat → MsgPackError  -- type, actual, expected
  
  /-- Type conversion errors -/
  | integerOverflow : String → MsgPackError  -- description of overflow
  | typeMismatch : String → String → MsgPackValue → MsgPackError  -- expected, actual, value
  deriving BEq, Inhabited

/-- Convert error to human-readable string -/
def MsgPackError.toString : MsgPackError → String
  | .emptyInput => "cannot decode empty input"
  | .unexpectedEOF offset => s!"unexpected end of input at offset {offset}"
  | .incompleteData expected actual => s!"incomplete data: expected {expected} bytes, got {actual}"
  | .unsupportedFormat code => s!"unsupported format code: 0x{code.toNat.toDigits 16 |>.asString}"
  | .invalidUTF8 bytes => s!"invalid UTF-8 sequence in {bytes.size} bytes"
  | .malformedFloat bits => s!"malformed float with bits: 0x{bits.toNat.toDigits 16 |>.asString}"
  | .payloadTooLarge actual max => s!"payload too large: {actual} bytes (max {max})"
  | .collectionTooLarge actual max => s!"collection too large: {actual} elements (max {max})"
  | .recursionDepthExceeded depth => s!"maximum recursion depth exceeded: {depth}"
  | .extraBytes consumed total => s!"extra bytes: consumed {consumed} of {total}"
  | .invalidMapKey key => s!"invalid map key: {reprMsgPackValue key}"
  | .duplicateMapKey key => s!"duplicate map key: {reprMsgPackValue key}"
  | .unknownExtensionType typ data => s!"unknown extension type {typ} with {data.size} bytes"
  | .invalidExtensionLength typ actual expected => s!"extension type {typ}: expected {expected} bytes, got {actual}"
  | .integerOverflow desc => s!"integer overflow: {desc}"
  | .typeMismatch expected actual value => s!"type mismatch: expected {expected}, got {actual} ({reprMsgPackValue value})"

instance : ToString MsgPackError where
  toString := MsgPackError.toString

/-- Result type for MessagePack operations -/
abbrev MsgPackResult (α : Type) := Except MsgPackError α

/-! ## Updated helper functions using proper error types -/

private def validateLength (len : Nat) (offset : Nat) (totalSize : Nat) : MsgPackResult Unit :=
  if len > MAX_PAYLOAD_SIZE then
    throw (.payloadTooLarge len MAX_PAYLOAD_SIZE)
  else if offset + len > totalSize then
    throw (.incompleteData (offset + len) totalSize)
  else
    pure ()

private def validateCollectionSize (size : Nat) : MsgPackResult Unit :=
  if size > MAX_COLLECTION_SIZE then
    throw (.collectionTooLarge size MAX_COLLECTION_SIZE)
  else
    pure ()

private def getByte (bytes : ByteArray) (i : Nat) : MsgPackResult UInt8 :=
  if h : i < bytes.size then
    pure (bytes[i]'h)
  else
    throw (.unexpectedEOF i)

private def getSlice (bytes : ByteArray) (start len : Nat) : MsgPackResult ByteArray :=
  if len > MAX_PAYLOAD_SIZE then
    throw (.payloadTooLarge len MAX_PAYLOAD_SIZE)
  else if start >= bytes.size then
    throw (.unexpectedEOF start)
  else if start + len > bytes.size then
    throw (.incompleteData (start + len) bytes.size)
  else
    pure (bytes.extract start (start + len))

/-- Enhanced UTF-8 validation -/
private def parseStringFromBytes (bytes : ByteArray) : MsgPackResult String :=
  match String.fromUTF8? bytes with
  | some s => pure s
  | none => throw (.invalidUTF8 bytes)

/-- Example of enhanced decoding function signature -/
partial def parseWithTypedErrors (bytes : ByteArray) (offset : Nat) (depth : Nat := 0) : MsgPackResult (MsgPackValue × Nat) := do
  if depth > MAX_RECURSION_DEPTH then
    throw (.recursionDepthExceeded depth)
  
  let b ← getByte bytes offset
  -- ... rest of parsing logic with typed errors
  
  -- Example for unsupported format:
  throw (.unsupportedFormat b)

def decodeFromBytesTyped (bytes : ByteArray) : MsgPackResult MsgPackValue := do
  if bytes.isEmpty then
    throw .emptyInput
  let (v, consumed) ← parseWithTypedErrors bytes 0
  if consumed != bytes.size then
    throw (.extraBytes consumed bytes.size)
  pure v

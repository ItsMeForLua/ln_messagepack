## ln_messagepack

A [MessagePack](https://msgpack.org/index.html) serialization library for Lean 4.

`ln_messagepack` provides a simple API for encoding and decoding Lean data structures into the MessagePack binary format, making it ideal for data exchange, network protocols, and compact storage.

## Features

-  **Type-safe API:** Uses typeclasses (`MsgPackEncode`/`MsgPackDecode`) for safe serialization.
-  **Primitives:** Full support for nil, bool, int, uint, and string types.
-  **Collections:** Out-of-the-box support for `Array` and `List`.
-  **Extension Types:** Support for the ext format family, with Timestamp provided as a built-in example.
-  **Custom Structs:** Easily extend to your own data types by implementing the `MsgPackEncode`/`MsgPackDecode` instances.
-  **Zero Dependencies:** ln_messagepack requires no third party libraries.
- **Tested & Benchmarked:** Includes a test suite and performance benchmarks.

# Why Not JSON?

Both MessagePack and JSON are data serialization formats. MessagePack uses binary encoding, which makes it both smaller and faster to serialize and deserialize than JSON is. MessagePack supports a more extensive range of data types and produces much smaller message sizes. MessagePack is best when speed and minimal data size are your priorities, whereas JSON is better when human readability is.

## Setup

Add `ln_messagepack` to your project's `lakefile.lean` dependencies:

```lean
import Lake
open Lake DSL

package "my-project" where
  -- ...

require «ln_messagepack» from git
  "https://github.com/itsmeforlua/ln_messagepack" @ "main"

lean_lib «MyProject» where
  -- ...
```

Then, in any file where you need to use the library, simply import the MessagePack module:

```lean
import ln_messagepack.Messagepack

open ln_messagepack
```
**Note:** The `open ln_messagepack` line is important because it brings the library functions (encode, decode, encodeToBytes, decodeFromBytes) into scope so you can use them directly without prefixing them with the module name.

## API at a Glance

The core of the library revolves around two typeclasses and several functions:

- `class MsgPackEncode (α : Type)`: A typeclass for types that can be encoded.
- `class MsgPackDecode (α : Type)`: A typeclass for types that can be decoded.
- `encode (a : α) : MsgPackValue`: Encodes a value to MsgPackValue.
- `decode (v : MsgPackValue) : Option α`: Decodes a MsgPackValue to Option α.
- `encodeToBytes (v : MsgPackValue) : ByteArray`: Encodes MsgPackValue to bytes.
- `decodeFromBytes (bs : ByteArray) : Except String MsgPackValue`: Decodes bytes to MsgPackValue.

## Usage

### 1. Basic Encoding & Decoding

You can serialize any type that has a `MsgPackEncode` instance directly to bytes.

```lean
import ln_messagepack.Messagepack

#eval do
  -- A simple string
  let originalStr := "hello messagepack"
  
  -- Encode the string to MsgPackValue then to bytes
  let msgPackValue := encode originalStr
  let encodedBytes := ln_messagepack.encodeToBytes msgPackValue
  IO.println s!"Encoded: {encodedBytes}"
  
  -- Decode the bytes back into a String
  match ln_messagepack.decodeFromBytes encodedBytes with
  | .ok value => 
    match decode value with
    | some str => IO.println s!"Decoded: {str}"
    | none => IO.println "Failed to decode to String"
  | .error msg => IO.println s!"Decoding error: {msg}"
```

### 2. Custom Data Types

Making your own data types serializable is as simple as defining instances for `MsgPackEncode` and `MsgPackDecode`.

```lean
import ln_messagepack.Messagepack

-- 1. Define your custom structure
structure User where
  id : Nat
  name : String
  deriving BEq, Repr

-- 2. Provide an instance for MsgPackEncode
instance : MsgPackEncode User where
  encode u := .arr #[
    encode u.id,
    encode u.name
  ]

-- 3. Provide an instance for MsgPackDecode
instance : MsgPackDecode User where
  decode
    | .arr #[idVal, nameVal] => do
      let id ← decode idVal
      let name ← decode nameVal
      pure { id, name }
    | _ => none

-- Example usage
#eval do
  let user : User := { id := 123, name := "Alice" }
  let msgPackValue := encode user
  let bytes := ln_messagepack.encodeToBytes msgPackValue
  IO.println s!"Encoded: {bytes}"

  match ln_messagepack.decodeFromBytes bytes with
  | .ok value =>
    match decode value with
    | some (decodedUser : User) => IO.println s!"User: {decodedUser.name} (ID: {decodedUser.id})"
    | none => IO.println "Failed to decode to User"
  | .error msg => IO.println s!"Decoding error: {msg}"
```

### 3. Extension Types: Timestamp

The library includes built-in support for the standard Timestamp extension type.

```lean
import ln_messagepack.Messagepack

def main : IO Unit := do
  let ts : Timestamp := { sec := 1672531200, nsec := 500000000 }
  let msgPackValue := encode ts
  let bytes := ln_messagepack.encodeToBytes msgPackValue
  IO.println s!"Encoded timestamp: {bytes}"

  match ln_messagepack.decodeFromBytes bytes with
  | .ok value =>
    match decode value with
    | some (decodedTs : Timestamp) => IO.println s!"Decoded timestamp (sec): {decodedTs.sec}"
    | none => IO.println "Failed to decode to Timestamp"
  | .error msg => IO.println s!"Decoding error: {msg}"
```

## 4. Working with Arrays and Lists

```lean
import ln_messagepack.Messagepack

#eval do
  -- Arrays
  let numbers : Array Int := #[1, 2, 3, 4, 5]
  let msgPackValue := encode numbers
  let encodedArray := ln_messagepack.encodeToBytes msgPackValue
  
  match ln_messagepack.decodeFromBytes encodedArray with
  | .ok value =>
    match decode value with
    | some (decodedArray : Array Int) => IO.println s!"Array roundtrip successful: {decodedArray}"
    | none => IO.println "Failed to decode Array"
  | .error msg => IO.println s!"Array decoding error: {msg}"

  -- Lists
  let strings : List String := ["hello", "world", "from", "lean"]
  let listMsgPack := encode strings
  let encodedList := ln_messagepack.encodeToBytes listMsgPack
  
  match ln_messagepack.decodeFromBytes encodedList with
  | .ok value =>
    match decode value with
    | some (decodedList : List String) => IO.println s!"List roundtrip successful: {decodedList}"
    | none => IO.println "Failed to decode List"
  | .error msg => IO.println s!"List decoding error: {msg}"
```

Getting Started Tips

### For Building Executables

- Use `def main : IO Unit := do` as your entry point
- Run with `lake exec <your-executable-name>`
- The `main` function is required for the linker to create a working executable

### For Testing/Experimentation

- Use `#eval do` for quick testing
- Run with `lake build` to see the output during compilation
- Great for trying out the library without creating executables

### Common Gotchas

1. **Always include `open ln_messagepack`** after your import to access functions directly
2. **Use `def main` for executables**, `#eval` for testing only
3. **Handle the `Except String MsgPackValue`** return type from `decodeFromBytes`
4. **Provide explicit type annotations** when decoding (e.g., `some (str : String)`)

## Current Status

The API covers the vast majority of the MessagePack specification.

- [x] Nil, Boolean, Integer, String
- [x] Binary data (`ByteArray`)
- [x] Arrays and Lists
- [x] Extension types (ext family)
- [x] Floats (float / double)

# Performance Benchmarks
A simple benchmark was ran in order to test the performance of encoding and decoding an array of 10,000 User objects. The results are a rough guide and may vary based on your machine.

```console
--- Running Benchmarks for ln_messagepack ---

[1] Preparing data (10000 users)...

[2] Benchmarking Encoding
  Encode 10k users: 4 ms
    Total encoded size: 148509 bytes

[3] Benchmarking Decoding
  Decode 10k users: 2 ms
     Verification successful.

--- Benchmarks Complete ---
```
These benchmarks were ran on the author's laptop. The results are provided as a way to give a general idea of the library's performance, but your mileage may vary depending on your hardware.

## Contributing

Contributions are welcome! Please feel free to open an issue to discuss a bug or feature, or submit a pull request.

## License

This library is available under the LGPL v2.1 [License](LICENSE).

---

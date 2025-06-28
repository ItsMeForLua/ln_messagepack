## ln-messagepack

An efficient, fast, and pure [MessagePack](https://msgpack.org/index.html) serialization library for Lean 4.

`ln-messagepack` provides a simple and type-safe API for encoding and decoding Lean data structures into the MessagePack binary format, making it ideal for data exchange, network protocols, and compact storage.

## Features

- ✅ **Type-safe API:** Uses typeclasses (`MsgPackEncode`/`MsgPackDecode`) for safe serialization.
- ✅ **Primitives:** Full support for nil, bool, int, uint, and string types.
- ✅ **Collections:** Out-of-the-box support for `Array` and `List`.
- ✅ **Extension Types:** Support for the ext format family, with Timestamp provided as a built-in example.
- ✅ **Custom Structs:** Easily extend to your own data types by implementing the `MsgPackEncode`/`MsgPackDecode` instances.
- ✅ **Zero Dependencies:** Relies only on the standard batteries library.
- ✅ **Pure Lean:** No FFI or external dependencies.
- ✅ **Tested & Benchmarked:** Includes a test suite and performance benchmarks.

# Why Not JSON?

Both MessagePack and JSON are data serialization formats. MessagePack uses binary encoding, which makes it both smaller and faster to serialize and deserialize than JSON is. MessagePack supports a more extensive range of data types and produces much smaller message sizes. MessagePack is best when speed and minimal data size are your priorities, whereas JSON is better when human readability is.

## Setup

Add `ln-messagepack` to your project's `lakefile.lean` dependencies:

```lean
import Lake
open Lake DSL

package "my-project" where
  -- ...

require «ln-messagepack» from git
  "https://github.com/itsmeforlua/ln-messagepack" @ "main"

lean_lib «MyProject» where
  -- ...
```

Then, in any file where you need to use the library, simply import the MessagePack module:

```lean
import LnMessagepack.MessagePack
```

## API at a Glance

The core of the library revolves around two typeclasses and two functions:

- `class MsgPackEncode (α : Type)`: A typeclass for types that can be encoded.
- `class MsgPackDecode (α : Type)`: A typeclass for types that can be decoded.
- `encodeToMsgPackBytes (a : α) : ByteArray`: Encodes an `α` directly to bytes.
- `decodeFromMsgPackBytes (bs : ByteArray) : Option α`: Decodes bytes into an `Option α`.

## Usage

### 1. Basic Encoding & Decoding

You can serialize any type that has a `MsgPackEncode` instance directly to bytes.

```lean
import LnMessagepack.MessagePack

-- A simple string
let originalStr := "hello messagepack"

-- Encode the string to a ByteArray
let encodedBytes := encodeToMsgPackBytes originalStr
--=> #[177, 104, 101, 108, 108, 111, 32, 109, 101, 115, 115, 97, 103, 101, 112, 97, 99, 107]

-- Decode the bytes back into a String
let decoded? : Option String := decodeFromMsgPackBytes encodedBytes
--=> some "hello messagepack"
```

### 2. Custom Data Types

Making your own data types serializable is as simple as defining instances for `MsgPackEncode` and `MsgPackDecode`.

```lean
import LnMessagepack.MessagePack

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
```

### 3. Extension Types: Timestamp

The library includes built-in support for the standard Timestamp extension type.

```lean
import LnMessagepack.MessagePack

def main : IO Unit := do
  let ts : Timestamp := { sec := 1672531200, nsec := 500000000 }
  let bytes := encodeToMsgPackBytes ts
  IO.println s!"Encoded timestamp: {bytes}"

  match decodeFromMsgPackBytes (α := Timestamp) bytes with
  | some decodedTs => IO.println s!"Decoded timestamp (sec): {decodedTs.sec}"
  | none => IO.println "Decoding failed!"
```

## Current Status

This library is stable and ready for use. The API covers the vast majority of the MessagePack specification.

- [x] Nil, Boolean, Integer, String
- [x] Binary data (`ByteArray`)
- [x] Arrays and Lists
- [x] Extension types (ext family)
- [ ] Floats (float / double) — Not yet implemented, but will be **SOON**.

# Performance Benchmarks
A simple benchmark was ran in order to test the performance of encoding and decoding an array of 10,000 User objects. The results are a rough guide and may vary based on your machine.

```console
--- Running Benchmarks for LnMessagepack ---

[1] Preparing data (10000 users)...

[2] Benchmarking Encoding
  Encode 10k users: 4 ms
    Total encoded size: 148509 bytes

[3] Benchmarking Decoding
  Decode 10k users: 2 ms
    ✅ Verification successful.

--- Benchmarks Complete ---
```
These benchmarks were ran on the author's laptop. The results are provided as a way to give a general idea of the library's performance, but your mileage may vary depending on your hardware.

## Contributing

Contributions are welcome! Please feel free to open an issue to discuss a bug or feature, or submit a pull request.

## License

This library is available under the Apache 2.0 License.

---

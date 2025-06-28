# **ln-messagepack**

A fast, efficient, and pure [MessagePack](https://msgpack.org/) serialization library for Lean 4.

ln-messagepack provides a simple, type-safe API for encoding and decoding Lean data structures into the MessagePack binary format, making it ideal for data exchange, network protocols, and compact storage.

## **Features**

* ✅ **Type-safe API**: Uses typeclasses (`MsgPackEncode`/`MsgPackDecode`) for safe serialization.
* ✅ **Comprehensive Primitives**: Full support for nil, bool, int, uint, and string types.
* ✅ **Collections**: Out-of-the-box support for Array and List.
* ✅ **Custom Structs**: Easily extend to your own data types by implementing the `MsgPackEncode`/`Decode` instances.
* ✅ **Zero Dependencies**: Relies only on the standard batteries library.
* ✅ **Pure Lean**: No FFI or external dependencies.

## **Setup**

Add ln-messagepack to your project's `lakefile.lean` dependencies:

```lean
import Lake
open Lake DSL

package "my-project" where
--  ...

require "ln-messagepack" from git
  "https://github.com/itsmeforlua/ln-messagepack" @ "main"

lean_lib «MyProject» where
--  ...
-- Then, in any file where you need to use the library, simply import LnMessagepack.MessagePack.
-- 1. Basic Encoding & Decoding: You can serialize any type that has a MsgPackEncode instance directly to bytes.import LnMessagepack

-- A simple string
let originalStr := "hello messagepack"

-- Encode the string to a ByteArray
let encodedBytes : ByteArray := encodeToMsgPackBytes originalStr
-- => [177, 104, 101, 108, 108, 111, 32, ... ]

-- Decode the bytes back into a String
let decodedStr? : Option String := decodeFromMsgPackBytes encodedBytes
-- => some "hello messagepack"
-- 2. Custom Data Types: Making your own data types serializable is as simple as defining instances for MsgPackEncode and MsgPackDecode.import LnMessagepack

-- 1. Define your custom structure
structure User where
  id : Nat
  name : String
  deriving BEq, Repr

-- 2. Provide an instance for MsgPackEncode
-- Here, we serialize a User as a 2-element array: [id, name]
instance : MsgPackEncode User where
  encode u := .arr #[
    encodeToBytes (.uint u.id),
    encodeToBytes (.str u.name)
  ]

-- 3. Provide an instance for MsgPackDecode
-- The decoder expects the same array structure.
instance : MsgPackDecode User where
  decode
    | .arr #[idVal,

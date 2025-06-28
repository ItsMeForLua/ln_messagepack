import LnMessagepack.MessagePack

/-
Copyright 2024 Andrew D. France

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

/-!
# `ln-messagepack` Demonstration

This file serves as a demo for the `ln-messagepack` library.
It demonstrates how to:
1. Define a custom data structure (`User`).
2. Implement the `MsgPackEncode` and `MsgPackDecode` typeclasses for it.
3. Perform round-trip serialization and deserialization for various types.

This file is not part of the core of library itself, but is an example of how a
user might interact with the public API.

To run the demo executable, you must CD into the library root, and then run `lake exe messagepack`.
-/

-- By convention, users should only need to import the library's main module, like so:

open LnMessagepack

/-- A basic structure to demonstrate custom type serialization. -/
structure User where
  /-- A unique identifier for the user. -/
  id : Nat
  /-- The user's name. -/
  name : String
  deriving BEq, Repr

-- Add a ToString instance so IO.println knows how to print a User.
instance : ToString User where
  toString u := toString (repr u)

/--
Teaches `ln-messagepack` how to encode a `User`.
We serialize a `User` as a 2-element MessagePack array: `[id, name]`.
-/
instance : MsgPackEncode User where
  encode u := .arr #[
    encode u.id,
    encode u.name
  ]

/--
Teaches `ln-messagepack` how to decode a `User`.
The decoder expects the same 2-element array structure used for encoding.
If the `MsgPackValue` is not an array of the correct shape, decoding will fail.
-/
instance : MsgPackDecode User where
  decode
    | .arr #[idVal, nameVal] => do
      let id ← decode idVal
      let name ← decode nameVal
      pure { id, name }
    | _ => none

/--
The main entry point for the demo executable.

This function runs a series of round trip tests, and prints the results
to the console. It provides a quick visual confirmation that the core
features of the library are working as expected.
-/
def main : IO Unit := do
  IO.println "--- ln-messagepack Demo ---"

  --Example Usage:
  -- (1) String round trip
  IO.println "\n[1] String Round Trip"
  let originalStr := "hello messagepack"
  IO.println s!"Original: \"{originalStr}\""
  let encodedStrBytes := encodeToMsgPackBytes originalStr
  IO.println s!"Encoded (bytes): {encodedStrBytes}"
  match decodeFromMsgPackBytes (α := String) encodedStrBytes with
  | some decodedStr =>
    IO.println s!"Decoded: \"{decodedStr}\""
    if decodedStr == originalStr then
      IO.println "✅ Success: Decoded string matches original."
    else
      IO.println "❌ Failure: Mismatch!"
  | none =>
    IO.println "❌ Failure: Decoding failed."


  -- (2) Custom type round trip
  IO.println "\n[2] Custom Type (User) Round Trip"
  let user : User := { id := 123, name := "Alice" }
  IO.println s!"Original: {user}"
  let encodedUserBytes := encodeToMsgPackBytes user
  IO.println s!"Encoded (bytes): {encodedUserBytes}"

  match decodeFromMsgPackBytes (α := User) encodedUserBytes with
  | some decodedUser =>
    IO.println s!"Decoded: {decodedUser}"
    if decodedUser == user then
      IO.println "✅ Success: Decoded user matches original."
    else
      IO.println "❌ Failure: Mismatch!"
  | none =>
    IO.println "❌ Failure: Decoding failed."

  -- (3) List round trip
  IO.println "\n[3] List Round Trip"
  let xs : List Nat := [1, 2, 3, 4, 5]
  IO.println s!"Original: {xs}"
  let encodedListBytes := encodeToMsgPackBytes xs
  IO.println s!"Encoded (bytes): {encodedListBytes}"
  match decodeFromMsgPackBytes (α := List Nat) encodedListBytes with
  | some ys =>
    IO.println s!"Decoded: {ys}"
    if ys == xs then
      IO.println "✅ Success: Decoded list matches original."
    else
      IO.println "❌ Failure: Mismatch!"
  | none =>
    IO.println "❌ Failure: Decoding failed."

  -- (4) Array round trip
  IO.println "\n[4] Array Round Trip"
  let arr : Array Bool := #[true, false, true]
  IO.println s!"Original: {arr}"
  let encodedArrBytes := encodeToMsgPackBytes arr
  IO.println s!"Encoded (bytes): {encodedArrBytes}"
  match decodeFromMsgPackBytes (α := Array Bool) encodedArrBytes with
  | some arr2 =>
    IO.println s!"Decoded: {arr2}"
    if arr2 == arr then
      IO.println "✅ Success: Decoded array matches original."
    else
      IO.println "❌ Failure: Mismatch!"
  | none =>
    IO.println "❌ Failure: Decoding failed."

  -- (5) Timestamp (Extension Type) round trip
  IO.println "\n[5] Timestamp Round Trip"
  let originalTimestamp : Timestamp := { sec := 1672531200, nsec := 500000000 }
  IO.println s!"Original: {originalTimestamp}"
  let encodedTimestampBytes := encodeToMsgPackBytes originalTimestamp
  IO.println s!"Encoded (bytes): {encodedTimestampBytes}"
  match decodeFromMsgPackBytes (α := Timestamp) encodedTimestampBytes with
  | some decodedTimestamp =>
    IO.println s!"Decoded: {decodedTimestamp}"
    if decodedTimestamp == originalTimestamp then
      IO.println "✅ Success: Decoded timestamp matches original."
    else
      IO.println "❌ Failure: Mismatch!"
  | none =>
    IO.println "❌ Failure: Decoding failed."

  IO.println "\n--- End Demo ---"

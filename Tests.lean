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

open LnMessagepack

/-!
  A very simple testing framework.
  We define a `TestState` to keep track of passes and failures,
  and helper functions to run test cases.
-/
structure TestState where
  passed : Nat := 0
  failed : Nat := 0

def test_case (name : String) (cond : Bool) : StateT TestState IO Unit := do
  if cond then
    IO.println s!"  ✅ {name}"
    modify fun st => { st with passed := st.passed + 1 }
  else
    IO.println s!"  ❌ {name}"
    modify fun st => { st with failed := st.failed + 1 }

def test_round_trip [BEq α] [Repr α] [MsgPackEncode α] [MsgPackDecode α]
  (name : String) (value : α) : StateT TestState IO Unit := do
  let encoded := encodeToMsgPackBytes value
  let decoded? := decodeFromMsgPackBytes (α := α) encoded
  let cond := decoded? == some value
  let testName := s!"{name} round-trip"

  if cond then
    test_case testName cond
  else
    -- Print extra debug info on failure
    IO.println s!"  ❌ {testName}"
    IO.println s!"    Original: {repr value}"
    IO.println s!"    Encoded:  {encoded}"
    IO.println s!"    Decoded:  {repr decoded?}"
    modify fun st => { st with failed := st.failed + 1 }

/-!
  For testing purposes, we define a sample `User` struct locally.
  The `Timestamp` struct is imported directly from the library API.
-/
structure User where
  id : Nat
  name : String
  deriving BEq, Repr

instance : ToString User where
  toString u := toString (repr u)

instance : MsgPackEncode User where
  encode u := .arr #[
    encode u.id,
    encode u.name
  ]

instance : MsgPackDecode User where
  decode
    | .arr #[idVal, nameVal] => do
      let id ← decode idVal
      let name ← decode nameVal
      pure { id, name }
    | _ => none

-- The main function for our test executable.
def main : IO UInt32 := do
  IO.println "--- Running Test Suite for LnMessagepack ---"

  -- Define the entire sequence of tests as a single monadic action.
  let testSuite : StateT TestState IO Unit := do
    -- SECTION: Primitive Types
    IO.println "\n[1] Primitives"
    test_round_trip "True" true
    test_round_trip "False" false
    test_round_trip "Positive Int" (12345 : Int)
    test_round_trip "Negative Int" (-12345 : Int)
    test_round_trip "Zero Int" (0 : Int)
    test_round_trip "Positive Nat" (54321 : Nat)
    test_round_trip "String" "hello, world!"

    -- SECTION: Collections
    IO.println "\n[2] Collections"
    test_round_trip "Array of Ints" #[1, -2, 3, -4] (α := Array Int)
    test_round_trip "List of Strings" ["a", "b", "c"] (α := List String)
    test_round_trip "Nested Array" #[#[1, 2], #[3]] (α := Array (Array Nat))

    -- SECTION: Custom Types
    IO.println "\n[3] Custom Types"
    let user : User := { id := 123, name := "Alice" }
    test_round_trip "User Struct" user
    -- This uses the *real* Timestamp type from the library API.
    let ts : Timestamp := { sec := 1672531200, nsec := 500000000 }
    test_round_trip "Timestamp Ext" ts

    -- SECTION: Edge Cases
    IO.println "\n[4] Edge Cases"
    test_round_trip "Empty String" ""
    test_round_trip "Empty Array" #[] (α := Array Nat)
    test_round_trip "Empty List" [] (α := List Nat)

    let badBytes := ByteArray.mk #[0xff, 0xff, 0xff]
    let decodedBad? : Option String := decodeFromMsgPackBytes badBytes
    test_case "Decoding invalid bytes returns none" (decodedBad? == none)

  -- Now, run the test suite action with an initial empty state.
  let (_, finalState) ← StateT.run testSuite { passed := 0, failed := 0 }

  -- Print final results
  IO.println "\n--- Test Summary ---"
  IO.println s!"{finalState.passed} passed, {finalState.failed} failed."

  if finalState.failed > 0 then
    return 1 -- Return a non-zero exit code to indicate failure
  else
    return 0

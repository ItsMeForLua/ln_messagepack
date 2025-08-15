import LnMessagepack.Core
import LnMessagepack.Typeclasses

/-
Copyright (C) <2025>  <Andrew D. France>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
-/
open LnMessagepack

/--
Keeps track of the number of passed/failed tests.
-/
structure TestState where
  passed : Nat := 0
  failed : Nat := 0
deriving Repr

def test_case (name : String) (cond : Bool) : StateT TestState IO Unit := do
  if cond then
    IO.println s!"  ✓ {name}"
    modify fun st => { st with passed := st.passed + 1 }
  else
    IO.println s!"  × {name}"
    modify fun st => { st with failed := st.failed + 1 }

/-!
Provide BEq instances for Float and Float32 so we can use `==`
-/
instance : BEq Float where
  beq x y := Float.toBits x == Float.toBits y   -- compares bit patterns

instance : BEq Float32 where
  beq x y := Float32.toBits x == Float32.toBits y

-- Add Repr instance for ByteArray
instance : Repr ByteArray where
  reprPrec ba _ := s!"#[{", ".intercalate (ba.toList.map toString)}]"

-- Helper function to check if two floats are both NaN
def bothNaN (x y : Float) : Bool :=
  x.isNaN && y.isNaN

def bothNaN32 (x y : Float32) : Bool :=
  x.isNaN && y.isNaN

-- Simplified test_round_trip without unsafe operations
def test_round_trip [BEq α] [Repr α] [MsgPackEncode α] [MsgPackDecode α]
  (name : String) (value : α) : StateT TestState IO Unit := do
  let encoded := encodeToMsgPackBytes value
  let decoded? := decodeFromMsgPackBytes (α := α) encoded
  let testName := s!"{name} round-trip"

  let same : Bool :=
    match decoded? with
    | some d => d == value
    | none => false

  if same then
    test_case testName true
  else
    IO.println s!"  × {testName}"
    IO.println s!"    Original: {repr value}"
    IO.println s!"    Encoded:  {repr encoded}"
    IO.println s!"    Decoded:  {repr decoded?}"
    modify fun st => { st with failed := st.failed + 1 }

-- Special test for Float NaN (since NaN != NaN)
def test_float_nan (name : String) (value : Float) : StateT TestState IO Unit := do
  let encoded := encodeToMsgPackBytes value
  let decoded? := decodeFromMsgPackBytes (α := Float) encoded
  let testName := s!"{name} round-trip"

  let same : Bool :=
    match decoded? with
    | some d => bothNaN value d || d == value
    | none => false

  if same then
    test_case testName true
  else
    IO.println s!"  × {testName}"
    IO.println s!"    Original: {repr value}"
    IO.println s!"    Encoded:  {repr encoded}"
    IO.println s!"    Decoded:  {repr decoded?}"
    modify fun st => { st with failed := st.failed + 1 }

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
      let id   ← decode idVal
      let name ← decode nameVal
      pure { id, name }
    | _ => none

def main : IO UInt32 := do
  IO.println "--- Running Test Suite for LnMessagepack ---"

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
    test_round_trip "Float" (3.14159 : Float)
    test_round_trip "Float32" (2.71828 : Float32)
    test_round_trip "Zero Float" (0.0 : Float)
    test_round_trip "Negative Float" (-123.456 : Float)
    test_round_trip "Float Infinity" Float.inf
    test_float_nan "Float NaN" Float.nan  -- Use special NaN test
    test_round_trip "Float -Infinity" Float.negInf

    -- SECTION: Collections
    IO.println "\n[2] Collections"
    test_round_trip "Array of Ints" #[1, -2, 3, -4] (α := Array Int)
    test_round_trip "List of Strings" ["a", "b", "c"] (α := List String)
    test_round_trip "Nested Array" #[#[1, 2], #[3]] (α := Array (Array Nat))

    -- SECTION: Custom Types
    IO.println "\n[3] Custom Types"
    let user : User := { id := 123, name := "Alice" }
    test_round_trip "User Struct" user
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

  let (_, finalState) ← StateT.run testSuite { passed := 0, failed := 0 }

  IO.println "\n--- Test Summary ---"
  IO.println s!"{finalState.passed} passed, {finalState.failed} failed."

  if finalState.failed > 0 then
    return 1
  else
    return 0

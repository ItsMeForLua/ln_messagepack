import ln_messagepack.Core
import ln_messagepack.Typeclasses
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

open ln_messagepack

/-!
  This file contains performance benchmarks for the Ln-Messagepack library.
  We measure the time it takes to encode and decode a large data structure.
-/

-- For benchmarking, we will use the same User struct that is in our tests.
structure User where
  id : Nat
  name : String
  deriving BEq, Repr

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

-- Helper function to time an IO action and print the result.
def time_it (name : String) (action : IO α) : IO α := do
  let start ← IO.monoMsNow
  let result ← action
  let stop ← IO.monoMsNow
  let elapsed := stop - start
  IO.println s!"  {name}: {elapsed} ms"
  return result


def main : IO Unit := do
  IO.println "--- Running Benchmarks for LnMessagepack ---"

  -- (1) Create a large, realistic data set.
  let userCount := 10000
  IO.println s!"\n[1] Preparing data ({userCount} users)..."
  let users := (List.range userCount).map (fun i => ({ id := i, name := s!"User #{i}" } : User))
  let usersArray := users.toArray

  -- (2) Benchmark the encoding process.
  IO.println "\n[2] Benchmarking Encoding"
  let encodedBytes ← time_it "Encode 10k users" (IO.lazyPure (fun _ => encodeToMsgPackBytes usersArray))
  IO.println s!"    Total encoded size: {encodedBytes.size} bytes"

  -- (3) Benchmark the decoding process.
  IO.println "\n[3] Benchmarking Decoding"
  let decodedUsers? ← time_it "Decode 10k users" (IO.lazyPure (fun _ => decodeFromMsgPackBytes (α := Array User) encodedBytes))

  -- (4) Verify the result to make sure the benchmark was valid.
  if let some decodedUsers := decodedUsers? then
    if decodedUsers.size == userCount then
      IO.println "    ✅ Verification successful."
    else
      IO.println "    ❌ Verification failed: Decoded array has wrong size."
  else
    IO.println "    ❌ Verification failed: Decoding returned none."

  IO.println "\n--- Benchmarks Complete ---"

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

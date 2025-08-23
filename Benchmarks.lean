import ln_messagepack.Core
import ln_messagepack.Typeclasses
import Lean.Data.Json
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
open Lean Json
set_option diagnostics true
set_option pp.explicit true
set_option pp.universes true

-- Test data structures
structure User where
  id : Nat
  name : String
  active : Bool
  score : Float
  deriving BEq, Repr

structure ComplexData where
  users : Array User
  metadata : Array (String × Nat)
  timestamps : Array Int
  flags : Array Bool
  deriving BEq, Repr

-- MessagePack instances
instance : MsgPackEncode User where
  encode u := .arr #[encode u.id, encode u.name, encode u.active, encode u.score]

instance : MsgPackDecode User where
  decode
    | .arr #[idVal, nameVal, activeVal, scoreVal] => do
      let id ← decode idVal
      let name ← decode nameVal
      let active ← decode activeVal
      let score ← decode scoreVal
      pure { id, name, active, score }
    | _ => none

instance : MsgPackEncode ComplexData where
  encode cd := .map #[
    (.str "users", encode cd.users),
    (.str "metadata", encode cd.metadata),
    (.str "timestamps", encode cd.timestamps),
    (.str "flags", encode cd.flags)
  ]

instance : MsgPackDecode ComplexData where
  decode
    | .map pairs => do
      let mut users? : Option (Array User) := none
      let mut metadata? : Option (Array (String × Nat)) := none
      let mut timestamps? : Option (Array Int) := none
      let mut flags? : Option (Array Bool) := none

      for (key, value) in pairs do
        match key with
        | .str "users" => users? := decode value
        | .str "metadata" => metadata? := decode value
        | .str "timestamps" => timestamps? := decode value
        | .str "flags" => flags? := decode value
        | _ => pure ()

      match users?, metadata?, timestamps?, flags? with
      | some users, some metadata, some timestamps, some flags =>
        pure { users, metadata, timestamps, flags }
      | _, _, _, _ => none
    | _ => none

-- JSON instances
instance : Lean.ToJson User where
  toJson u := Json.mkObj [
    ("id", Json.num u.id),
    ("name", Json.str u.name),
    ("active", Json.bool u.active),
    ("score", match JsonNumber.fromFloat? u.score with
              | .inl _ => Json.str "NaN or Infinity" -- error case
              | .inr jn => Json.num jn) -- success case
  ]
instance : Lean.FromJson User where
  fromJson? j := do
    let id ← j.getObjValAs? Nat "id"
    let name ← j.getObjValAs? String "name"
    let active ← j.getObjValAs? Bool "active"
    let score ← j.getObjValAs? Float "score"
    pure { id, name, active, score }

instance : Lean.ToJson ComplexData where
  toJson cd := Json.mkObj [
    ("users", Json.arr (cd.users.map Lean.ToJson.toJson)),
    ("metadata", Json.arr (cd.metadata.map (fun (k,v) => Json.mkObj [("key", Json.str k), ("value", Json.num v)]))),
    ("timestamps", Json.arr (cd.timestamps.map (fun i => Json.num (Int.natAbs i)))),
    ("flags", Json.arr (cd.flags.map Json.bool))
  ]

instance : Lean.FromJson ComplexData where
  fromJson? j := do
    let users ← j.getObjValAs? (Array User) "users"
    let metadataJson ← j.getObjValAs? (Array Json) "metadata"
    let metadata ← metadataJson.mapM (fun mj => do
      let key ← mj.getObjValAs? String "key"
      let value ← mj.getObjValAs? Nat "value"
      pure (key, value))
    let timestampsJson ← j.getObjValAs? (Array Nat) "timestamps"
    let timestamps := timestampsJson.map (fun n => Int.ofNat n)
    let flags ← j.getObjValAs? (Array Bool) "flags"
    pure { users, metadata, timestamps, flags }

-- Helper function to time an action and return timing
def timeAction (name : String) (action : IO α) : IO (α × UInt64) := do
  let start ← IO.monoMsNow
  let result ← action
  let stop ← IO.monoMsNow
  let elapsed := stop - start
  IO.println s!"  {name}: {elapsed} ms"
  return (result, elapsed)

-- CSV logging helper
def logBenchmarkResult (csvFile : String) (testName : String) (dataSize : Nat)
                      (msgpackEncodeTime : UInt64) (msgpackDecodeTime : UInt64) (msgpackSize : Nat)
                      (jsonEncodeTime : UInt64) (jsonDecodeTime : UInt64) (jsonSize : Nat) : IO Unit := do
  let csvLine := s!"{testName},{dataSize},{msgpackEncodeTime},{msgpackDecodeTime},{msgpackSize},{jsonEncodeTime},{jsonDecodeTime},{jsonSize}\n"
  IO.FS.withFile csvFile IO.FS.Mode.append fun handle =>
    handle.putStr csvLine

-- Initialize CSV file with headers
def initializeCsvFile (csvFile : String) : IO Unit := do
  let headers := "test_name,data_size,msgpack_encode_ms,msgpack_decode_ms,msgpack_size_bytes,json_encode_ms,json_decode_ms,json_size_bytes\n"
  IO.FS.writeFile csvFile headers

-- Generate test data
def generateUsers (count : Nat) : Array User :=
  (List.range count).toArray.map fun i => {
    id := i,
    name := s!"User #{i}",
    active := i % 2 == 0,
    score := Float.ofNat (i % 1000) / 10.0
  }

def generateComplexData (userCount : Nat) : ComplexData := {
  users := generateUsers userCount,
  metadata := (List.range (userCount / 100)).toArray.map fun i => (s!"meta_{i}", i * 42),
  timestamps := (List.range (userCount / 10)).toArray.map fun i => Int.ofNat (i * 1000000),
  flags := (List.range userCount).toArray.map fun i => i % 3 != 0
}

-- Edge case generators
def generateLargeStrings : Array String :=
  #["", "x", "hello world", String.mk (List.replicate 1000 'a'), String.mk (List.replicate 10000 'b')]

def generateExtremeNumbers : Array Int :=
  #[Int.ofNat 0, Int.ofNat 1, -1, Int.ofNat 127, -128, Int.ofNat 255, -256, Int.ofNat 32767, -32768, Int.ofNat 65535, -65536, Int.ofNat 2147483647, -2147483648]

def generateFloatEdgeCases : Array Float :=
  #[0.0, 1.0, -1.0, 3.14159, -3.14159, 1.23456789, -1.23456789]

-- Benchmark functions
def benchmarkSerialization (csvFile : String) (name : String) (count : Nat) : IO Unit := do
  IO.println s!"\n=== {name} Benchmark ({count} items) ==="

  let users := generateUsers count

  -- MessagePack benchmarks
  IO.println "\n[MessagePack]"
  let (msgpackEncoded, msgpackEncodeTime) ← timeAction "Encode" (IO.lazyPure fun _ => encodeToMsgPackBytes users)
  IO.println s!"  Size: {msgpackEncoded.size} bytes"

  let (msgpackDecoded?, msgpackDecodeTime) ← timeAction "Decode" (IO.lazyPure fun _ => decodeFromMsgPackBytes (α := Array User) msgpackEncoded)
  match msgpackDecoded? with
  | some decoded =>
    if decoded.size == count then
      IO.println "  ✓ Verification successful"
    else
      IO.println s!"  × Verification failed: expected {count}, got {decoded.size}"
  | none => IO.println "  × Decoding failed"

  -- JSON benchmarks
  IO.println "\n[JSON]"
  let (jsonEncoded, jsonEncodeTime) ← timeAction "Encode" (IO.lazyPure fun _ => (Lean.ToJson.toJson users).compress.toUTF8)
  IO.println s!"  Size: {jsonEncoded.size} bytes"

  let (jsonDecoded?, jsonDecodeTime) ← timeAction "Decode" (IO.lazyPure fun _ =>
    match String.fromUTF8? jsonEncoded with
    | some jsonStr =>
      match Json.parse jsonStr with
      | .ok json => Lean.FromJson.fromJson? (α := Array User) json |>.toOption
      | .error _ => none
    | none => none)

  match jsonDecoded? with
  | some decoded =>
    if decoded.size == count then
      IO.println "  ✓ Verification successful"
    else
      IO.println s!"  × Verification failed: expected {count}, got {decoded.size}"
  | none => IO.println "  × Decoding failed"

  -- Size comparison
  let ratio := (msgpackEncoded.size.toFloat / jsonEncoded.size.toFloat * 100).round.toUInt32
  IO.println s!"\nSize Comparison: MessagePack is {ratio}% the size of JSON"

  -- Log to CSV
  logBenchmarkResult csvFile name count msgpackEncodeTime msgpackDecodeTime msgpackEncoded.size jsonEncodeTime jsonDecodeTime jsonEncoded.size

def benchmarkEdgeCases : IO Unit := do
  IO.println "\n=== Edge Cases Benchmark ==="

  -- Test empty collections
  IO.println "\n[Empty Collections]"
  let emptyArray : Array String := #[]
  let emptyMsgPack := encodeToMsgPackBytes emptyArray
  let emptyJson := (Lean.ToJson.toJson emptyArray).compress.toUTF8
  IO.println s!"Empty array - MessagePack: {emptyMsgPack.size} bytes, JSON: {emptyJson.size} bytes"

  -- Test large strings
  IO.println "\n[String Edge Cases]"
  for str in generateLargeStrings do
    let msgpackSize := (encodeToMsgPackBytes str).size
    let jsonSize := (Lean.ToJson.toJson str).compress.toUTF8.size
    IO.println s!"String len {str.length} - MessagePack: {msgpackSize} bytes, JSON: {jsonSize} bytes"

  -- Test extreme numbers
  IO.println "\n[Number Edge Cases]"
  for num in generateExtremeNumbers do
    let msgpackSize := (encodeToMsgPackBytes num).size
    let jsonSize := (Lean.ToJson.toJson num).compress.toUTF8.size
    IO.println s!"Number {num} - MessagePack: {msgpackSize} bytes, JSON: {jsonSize} bytes"

  -- Test float edge cases
  IO.println "\n[Float Edge Cases]"
  for f in generateFloatEdgeCases do
    let msgpackSize := (encodeToMsgPackBytes f).size
    let jsonSize := (Lean.ToJson.toJson f).compress.toUTF8.size
    let fname := toString f
    IO.println s!"Float {fname} - MessagePack: {msgpackSize} bytes, JSON: {jsonSize} bytes"

def benchmarkComplexData (csvFile : String) (userCount : Nat) : IO Unit := do
  IO.println s!"\n=== Complex Data Benchmark ({userCount} users) ==="

  let complexData := generateComplexData userCount

  -- MessagePack
  IO.println "\n[MessagePack Complex]"
  let (msgpackEncoded, msgpackEncodeTime) ← timeAction "Encode Complex" (IO.lazyPure fun _ => encodeToMsgPackBytes complexData)
  IO.println s!"  Size: {msgpackEncoded.size} bytes"

  let (msgpackDecoded?, msgpackDecodeTime) ← timeAction "Decode Complex" (IO.lazyPure fun _ => decodeFromMsgPackBytes (α := ComplexData) msgpackEncoded)
  match msgpackDecoded? with
  | some decoded =>
    if decoded.users.size == userCount then
      IO.println "  ✓ Complex verification successful"
    else
      IO.println s!"  × Complex verification failed"
  | none => IO.println "  × Complex decoding failed"

  -- JSON
  IO.println "\n[JSON Complex]"
  let (jsonEncoded, jsonEncodeTime) ← timeAction "Encode Complex" (IO.lazyPure fun _ => (Lean.ToJson.toJson complexData).compress.toUTF8)
  IO.println s!"  Size: {jsonEncoded.size} bytes"

  let (jsonDecoded?, jsonDecodeTime) ← timeAction "Decode Complex" (IO.lazyPure fun _ =>
    match String.fromUTF8? jsonEncoded with
    | some jsonStr =>
      match Json.parse jsonStr with
      | .ok json => Lean.FromJson.fromJson? (α := ComplexData) json |>.toOption
      | .error _ => none
    | none => none)

  match jsonDecoded? with
  | some decoded =>
    if decoded.users.size == userCount then
      IO.println "  ✓ Complex verification successful"
    else
      IO.println s!"  × Complex verification failed"
  | none => IO.println "  × Complex decoding failed"

  -- Size comparison
  let ratio := (msgpackEncoded.size.toFloat / jsonEncoded.size.toFloat * 100).round.toUInt32
  IO.println s!"\nComplex Data Size Comparison: MessagePack is {ratio}% the size of JSON"

  -- Log to CSV
  logBenchmarkResult csvFile "Complex" userCount msgpackEncodeTime msgpackDecodeTime msgpackEncoded.size jsonEncodeTime jsonDecodeTime jsonEncoded.size

def benchmarkMemoryLimits : IO Unit := do
  IO.println "\n=== Memory Limits Test ==="

  -- Test approaching the 1M element limit
  let sizes := #[100000, 500000, 900000, 1000000]

  for size in sizes do
    IO.println s!"\n[Testing {size} elements]"
    let largeArray := (List.range size).toArray

    try
      let (encoded, encodeTime) ← timeAction s!"Encode {size} ints" (IO.lazyPure fun _ => encodeToMsgPackBytes largeArray)
      let (decoded?, decodeTime) ← timeAction s!"Decode {size} ints" (IO.lazyPure fun _ => decodeFromMsgPackBytes (α := Array Nat) encoded)
      match decoded? with
      | some decoded =>
        if decoded.size == size then
          IO.println s!"  ✓ Successfully handled {size} elements"
        else
          IO.println s!"  × Size mismatch: expected {size}, got {decoded.size}"
      | none => IO.println s!"  × Failed to decode {size} elements"
    catch e =>
      IO.println s!"  × Exception with {size} elements: {e}"

def main : IO Unit := do
  let csvFile := "benchmark_results.csv"

  IO.println "MessagePack vs JSON Benchmark Suite"
  IO.println "======================================================="

  initializeCsvFile csvFile

  -- Basic performance tests
  benchmarkSerialization csvFile "Small" 1000
  benchmarkSerialization csvFile "Medium" 10000
  benchmarkSerialization csvFile "Large" 100000
  benchmarkSerialization csvFile "VeryLarge" 1000000

  benchmarkEdgeCases

  benchmarkComplexData csvFile 10000

  benchmarkMemoryLimits

  IO.println "\nBenchmark Suite Complete!"
  IO.println s!"Results logged to: {csvFile}"
  IO.println "============================================"

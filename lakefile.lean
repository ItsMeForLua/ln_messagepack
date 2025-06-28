import Lake
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
open Lake DSL

package "ln-messagepack" where

lean_lib LnMessagepack

-- The main executable is for demo.
lean_exe messagepack where
  root := `Main

@[default_target]
lean_exe tests where
  root := `Tests

lean_exe bench where
  root := `Benchmarks

require batteries from git "https://github.com/leanprover-community/batteries" @ "main"

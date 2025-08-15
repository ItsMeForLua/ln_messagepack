import Lake
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
open Lake DSL

package "ln_messagepack" where

lean_lib ln_messagepack

-- The main executable (for now) is for demo.
lean_exe messagepack where
  root := `Messagepack

@[default_target]
lean_exe tests where
  root := `Tests

lean_exe bench where
  root := `Benchmarks

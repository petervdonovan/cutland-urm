import Lake
open Lake DSL

package «urm» {
  -- add package configuration options here
}

lean_lib «Urm» {
  -- add library configuration options here
}

@[default_target]
lean_exe «urm» {
  root := `Main
}

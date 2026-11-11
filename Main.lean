import Lake.CLI.Main
open Lake Lean

unsafe def declsFoundIn (lib : LeanLib) (decls : Array Name) : IO (Array Name) := do
  let imports := lib.config.roots.map fun module => { module }
  let env ← Lean.importModules imports {}
  let found ← decls.filterM fun decl => pure (env.contains decl)
  env.freeRegions
  return found

unsafe def main (args : List String) : IO UInt32 := do
  unless args.length == 1 do
    println! "This command takes exactly one argument: the path to a file containing a list of declarations to check."
    return 1
  let filename : System.FilePath := args[0]!
  unless ← filename.pathExists do
    println! "Could not find declaration list {filename}."
    return 1
  let (elanInstall?, leanInstall?, lakeInstall?) ← findInstall?
  let config ← MonadError.runEIO <| mkLoadConfig { elanInstall?, leanInstall?, lakeInstall? }
  let (ws?, log) ← (loadWorkspace config).run?
  log.replay (logger := .stderr)
  let some ws := ws? | return 1
  let lines ← IO.FS.lines filename
  let decls := lines.map (·.toName)
  -- see comments in https://github.com/leanprover/lean4/pull/6325
  enableInitializersExecution
  let mut found : NameSet := {}
  for lib in ws.root.leanLibs do
    for decl in ← declsFoundIn lib decls do
      found := found.insert decl
  let mut ok := true
  for line in lines do
    unless found.contains line.toName do
      println! "{line} is missing."
      ok := false
  return if ok then 0 else 1

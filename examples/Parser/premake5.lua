include "../../build/Tests.lua"

project "Parser"
  kind "ConsoleApp"
  language "C#"
  debugdir "."
  
  files { "**.cs", "./*.lua" }
  excludes { "obj/**/*.*" }

  links
  {
    "CppSharp",
    "CppSharp.AST",
    "CppSharp.Parser"
  }

  SetupManagedProject()
  SetupParser()

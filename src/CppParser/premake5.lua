clang_msvc_flags =
{
  "/wd4146", "/wd4244", "/wd4800", "/wd4345",
  "/wd4355", "/wd4996", "/wd4624", "/wd4291",
  "/wd4251",
  "/wd4141", -- 'inline' : used more than once
}

if EnableNativeProjects() then

local libDir = path.join(bindir, "Release_" .. target_architecture())

local pattern = path.join(libDir, "*CppSharp.CppParser*")

if os.getenv("CI") == "true" and #os.matchfiles(pattern) > 0 then
 
print("Using CppParser library from cache found at " .. libDir)

for k, v in pairs(os.matchfiles(pattern)) do
  print("Found file: " .. v)
end

SearchLLVM()
CopyClangIncludes()

else

project "CppSharp.CppParser"

  kind "SharedLib"
  language "C++"
  SetupNativeProject()
  rtti "Off"
  defines { "DLL_EXPORT" }

  if os.istarget("linux") then
    linkgroups "On"
  end

  filter "toolset:msc*"
    buildoptions { clang_msvc_flags }

  linkoptions { "/ignore:4099" } -- LNK4099: linking object as if no debug info

  filter {}

  files
  {
    "*.h",
    "*.cpp",
    "*.lua"
  }

  SearchLLVM()
  SetupLLVMIncludes()
  SetupLLVMLibs()
  CopyClangIncludes()

  filter {}

end

project "Std-symbols"

  kind "SharedLib"
  language "C++"
  SetupNativeProject()
  rtti "Off"
  defines { "DLL_EXPORT" }

  filter { "toolset:msc*" }
    buildoptions { clang_msvc_flags }

  filter {}

  AddPlatformSpecificFiles("Bindings/CSharp", "Std-symbols.cpp")
end

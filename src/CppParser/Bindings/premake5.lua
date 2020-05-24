include ("CSharp")
-- TODO: fix the C# parser bindings on win
if EnableNativeProjects() and os.ishost("windows") then

include ("CLI")

end
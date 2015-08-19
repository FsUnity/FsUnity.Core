namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsUnity.Core")>]
[<assembly: AssemblyProductAttribute("FsUnity.Core")>]
[<assembly: AssemblyDescriptionAttribute("Backport of FSharp.Core for compatibility with Unity3d Game Engine")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"

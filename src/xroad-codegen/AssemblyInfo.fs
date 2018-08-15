namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("xroad-codegen")>]
[<assembly: AssemblyProductAttribute("xroad-codegen")>]
[<assembly: AssemblyDescriptionAttribute("Tool for generating service interfaces from X-Road WSDL descriptions.")>]
[<assembly: AssemblyVersionAttribute("1.0.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0.0")>]
[<assembly: AssemblyInformationalVersionAttribute("1.0.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] AssemblyTitle = "xroad-codegen"
    let [<Literal>] AssemblyProduct = "xroad-codegen"
    let [<Literal>] AssemblyDescription = "Tool for generating service interfaces from X-Road WSDL descriptions."
    let [<Literal>] AssemblyVersion = "1.0.0"
    let [<Literal>] AssemblyFileVersion = "1.0.0"
    let [<Literal>] AssemblyInformationalVersion = "1.0.0"

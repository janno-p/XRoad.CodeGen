[<RequireQualifiedAccess>]
module XRoad.CodeGen.Cli

let [<Literal>] Usage =
    """
Usage:
  xroad-codegen [options] <assembly-name> <wsdl-location>
  xroad-codegen --version
  xroad-codegen --help | -h

Options [options]:
  -l, --language <code>     Language code that is used to extract documentation
                            service description. Defaults to estonian (et).
  -s, --service <name> [*]  Service filter (can be used multiple times)
                            Can be used to specify services which are extracted from
                            service description.
  -n, --namespace <ns>      Root namespace for generated code.
                            By default assembly name is used.
  -d, --dll <file>          Output assembly file location.
                            By default assembly is generated in temporary folder.
  -f, --source-file <file>  Output source file location.
                            When not specified, no source file will be generated.
"""

[<RequireQualifiedAccess>]
module XRoad.CodeGen.Cli

let [<Literal>] Usage =
    """
Usage:
  xroad-codegen [options] <wsdl-location>
  xroad-codegen --version
  xroad-codegen --help | -h

Options [options]:
  -l, --language <code>     Language code that is used to extract documentation
                            service description. Defaults to estonian (et).
  -s, --service <name> [*]  Service filter (can be used multiple times)
                            Can be used to specify services which are extracted from
                            service description.
"""

[<RequireQualifiedAccess>]
module XRoad.CodeGen.Cli

open System
open Fake.Core.CommandLineParsing

let [<Literal>] Usage =
    """
Usage:
    xroad-codegen --version
    xroad-codegen --help | -h
"""

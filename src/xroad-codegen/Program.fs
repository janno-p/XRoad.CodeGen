module XRoad.CodeGen.Program

open System
open System.Reflection
open Fake.Core

type Verbosity =
    | Silent
    | Normal
    | Verbose

type CliAction =
    | ShowVersion
    | ShowHelp
    | Main of CodeGenOptions
    | InvalidUsage of string

let printVersion () =
    printfn "%s" AssemblyVersionInformation.AssemblyInformationalVersion

let printPath () =
    printfn "Tool path: %s" (typeof<CliAction>.GetTypeInfo().Assembly.Location)

let handleAction (verbosity: Verbosity) (action: CliAction) =
    match action with
    | ShowVersion ->
        printVersion()
        printPath()
        0
    | ShowHelp ->
        printf "%s" Cli.Usage
        0
    | Main options ->
        ProducerDefinition.makeProducerType options
        0
    | InvalidUsage str ->
        eprintfn "%s" str
        printfn "%s" Cli.Usage
        1

let parseAction (options: DocoptMap) =
    let verbosity =
        options |> DocoptResult.getFlagCount "--verbose"
    (Verbosity.Normal,
        if options |> DocoptResult.hasFlag "--version" then
            ShowVersion
        elif options |> DocoptResult.hasFlag "--help" then
            ShowHelp
        else
            match options |> DocoptResult.tryGetArgument("<assembly-name>"), options |> DocoptResult.tryGetArgument "<wsdl-location>" with
            | (Some(assemblyName), Some(location)) ->
                {
                    AssemblyName = assemblyName
                    Location = location
                    LanguageCode =
                        options
                        |> DocoptResult.tryGetArgument("--language")
                    Services =
                        options
                        |> DocoptResult.tryGetArguments("--service")
                        |> Option.defaultValue []
                    RootNamespace =
                        options
                        |> DocoptResult.tryGetArgument("--namespace")
                    DllFileName =
                        options
                        |> DocoptResult.tryGetArgument("--dll")
                    SourceFileName =
                        options
                        |> DocoptResult.tryGetArgument("--source-file")
                } |> Main
            | _ ->
                InvalidUsage "Please specify what you want to do!"
    )

[<EntryPoint>]
let main argv =
    try
        let verbosity, action =
            let parser = Docopt(Cli.Usage)
            parser.Parse(argv) |> parseAction
        handleAction verbosity action
    with
    | exn ->
        printfn "Error while parsing command line, usage is:"
        printfn "%s" Cli.Usage
        eprintfn "Exception occured: %s" exn.Message
        1

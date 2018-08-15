namespace XRoad.CodeGen

open System.Xml.Linq
open XRoad.Runtime
open XRoad.Runtime.Attributes

type XRoadMessageProtocolVersion =
    | Version20 of string
    | Version30 of string
    | Version31Ee of string
    | Version31Eu of string
    | Version40
    with
        member this.ProducerName =
            match this with
            | Version20(s) | Version30(s) | Version31Ee(s) | Version31Eu(s) -> Some(s)
            | Version40 -> None
        member this.EnumValue =
            match this with
            | Version20(_) -> XRoadProtocol.Version20
            | Version30(_) -> XRoadProtocol.Version30
            | Version31Ee(_) -> XRoadProtocol.Version31Ee
            | Version31Eu(_) -> XRoadProtocol.Version31Eu
            | Version40(_) -> XRoadProtocol.Version40
        member this.HeaderNamespace =
            match this with
            | Version20(_) -> XmlNamespace.XRoad20
            | Version30(_) -> XmlNamespace.XRoad30
            | Version31Ee(_) -> XmlNamespace.XRoad31Ee
            | Version31Eu(_) -> XmlNamespace.XRoad31Eu
            | Version40(_) -> XmlNamespace.XRoad40

[<AutoOpen>]
module private XRoadProtocolExtensions =
    let private messageProtocolNamespace = function
        | Version20(_) -> XmlNamespace.XRoad20
        | Version30(_) -> XmlNamespace.XRoad30
        | Version31Ee(_) -> XmlNamespace.XRoad31Ee
        | Version31Eu(_) -> XmlNamespace.XRoad31Eu
        | Version40 -> XmlNamespace.XRoad40

    let private messageProtocolElementName name mpv = XName.Get(name, messageProtocolNamespace mpv)

    let titleElementName = messageProtocolElementName "title"
    let versionElementName = messageProtocolElementName "version"

    let private rpcHeaders = ["asutus"; "andmekogu"; "isikukood"; "ametnik"; "id"; "nimi"; "toimik"; "allasutus"; "amet"; "ametniknimi"; "asynkroonne"; "autentija"; "makstud"; "salastada"; "salastada_sertifikaadiga"; "salastatud"; "salastatud_sertifikaadiga"]
    let private docLegacyHeaders = ["consumer"; "producer"; "userId"; "id"; "service"; "issue"; "unit"; "position"; "userName"; "async"; "authenticator"; "paid"; "encrypt"; "encryptCert"; "encrypted"; "encryptedCert"]
    let private docHeaders = ["client"; "service"; "centralService"; "id"; "userId"; "requestHash"; "issue"; "protocolVersion"]

    let private isHeaderOf ns hdrs (xn: XName) = if xn.NamespaceName = ns then hdrs |> List.exists ((=) xn.LocalName) else false

    let isMessageProtocolHeaderFunc = function
        | Version20(_) -> isHeaderOf XmlNamespace.XRoad20 rpcHeaders
        | Version30(_) -> isHeaderOf XmlNamespace.XRoad30 docLegacyHeaders
        | Version31Ee(_) -> isHeaderOf XmlNamespace.XRoad31Ee docLegacyHeaders
        | Version31Eu(_) -> isHeaderOf XmlNamespace.XRoad31Eu docLegacyHeaders
        | Version40(_) -> isHeaderOf XmlNamespace.XRoad40 docHeaders

type CodeGenOptions =
    {
        AssemblyName: string
        Location: string
        LanguageCode: string option
        Services: string list
        RootNamespace: string option
        DllFileName: string option
        SourceFileName: string option
    }
    with
        member this.LanguageCodeOrDefault =
            this.LanguageCode |> Option.defaultValue "et"

        member this.RootNamespaceOrDefault =
            match this.RootNamespace with
            | None -> this.AssemblyName
            | Some(x) when x |> System.String.IsNullOrWhiteSpace -> this.AssemblyName
            | Some(x) -> x

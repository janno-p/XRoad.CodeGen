namespace XRoad.Runtime

open System.Xml

[<RequireQualifiedAccessAttribute>]
module XmlNamespace =
    let [<Literal>] Http = "http://schemas.xmlsoap.org/soap/http"
    let [<Literal>] Mime = "http://schemas.xmlsoap.org/wsdl/mime/"
    let [<Literal>] Soap = "http://schemas.xmlsoap.org/wsdl/soap/"
    let [<Literal>] SoapEnc = "http://schemas.xmlsoap.org/soap/encoding/"
    let [<Literal>] SoapEnv = "http://schemas.xmlsoap.org/soap/envelope/"
    let [<Literal>] Wsdl = "http://schemas.xmlsoap.org/wsdl/"
    let [<Literal>] Xmime = "http://www.w3.org/2005/05/xmlmime"
    let [<Literal>] Xml = "http://www.w3.org/XML/1998/namespace"
    let [<Literal>] Xmlns = "http://www.w3.org/2000/xmlns/";
    let [<Literal>] Xop = "http://www.w3.org/2004/08/xop/include"
    let [<Literal>] XRoad20 = "http://x-tee.riik.ee/xsd/xtee.xsd"
    let [<Literal>] XRoad30 = "http://x-rd.net/xsd/xroad.xsd"
    let [<Literal>] XRoad31Ee = "http://x-road.ee/xsd/x-road.xsd"
    let [<Literal>] XRoad31Eu = "http://x-road.eu/xsd/x-road.xsd"
    let [<Literal>] XRoad40 = "http://x-road.eu/xsd/xroad.xsd"
    let [<Literal>] XRoad40Id = "http://x-road.eu/xsd/identifiers"
    let [<Literal>] XRoad40Repr = "http://x-road.eu/xsd/representation.xsd"
    let [<Literal>] Xsd = "http://www.w3.org/2001/XMLSchema"
    let [<Literal>] Xsi = "http://www.w3.org/2001/XMLSchema-instance"

    /// Defines namespaces which are handled separately (not generated).
    let predefined =
        [ Http; Mime; Soap; SoapEnc; SoapEnv; Wsdl; Xmime; Xml; Xmlns; Xop; Xsd; Xsi; XRoad40; XRoad40Id; XRoad40Repr ]

[<AutoOpen>]
module internal XmlExtensions =
    type XmlReader with
        member this.ReadXsiNullAttribute() =
            let nilValue = (this.GetAttribute("nil", XmlNamespace.Xsi) |> Option.ofObj |> Option.defaultValue "").ToLower()
            nilValue.Equals("1") || nilValue.Equals("true")

        member this.ReadXsiTypeAttribute() =
            match this.GetAttribute("type", XmlNamespace.Xsi) with
            | null -> null
            | typeValue ->
                let qualifiedName = typeValue.Split([| ':' |], 2)
                let nsprefix, nm =
                    match qualifiedName with
                    | [| nm |] -> "", nm
                    | [| nsprefix; nm |] -> nsprefix, nm
                    | _ -> failwith "never"
                let ns = this.LookupNamespace(nsprefix)
                XmlQualifiedName(nm, ns)

        member __.IsQualifiedTypeName(qualifiedName: XmlQualifiedName, nm: string, ns: string, isAnonymous, isDefault) =
            if qualifiedName |> isNull then isAnonymous || isDefault else qualifiedName.Name.Equals(nm) && qualifiedName.Namespace.Equals(ns)

        member this.ReadToNextElement(name, ns, depth, allowContent) =
            while this.Depth > depth do
                if this.NodeType = XmlNodeType.Element && this.Depth = depth + 1 && not allowContent then
                    failwithf "Expected end element of type `%s%s`, but element `%s` was found instead." (match ns with "" -> "" | n -> sprintf "%s:" n) name this.LocalName
                this.Read() |> ignore
            if this.Depth = depth && (this.IsEmptyElement || this.NodeType = XmlNodeType.Element) then
                this.Read() |> ignore

        member this.FindNextStartElement(depth) =
            let rec findNextStartElement () =
                if this.Depth < depth then false
                elif this.Depth = depth && this.NodeType = XmlNodeType.Element then true
                else this.Read() |> ignore; findNextStartElement()
            findNextStartElement()

        member this.IsMatchingElement(name: string, ns: string) =
            name.Equals(this.LocalName) && ns.Equals(this.NamespaceURI)

        member this.MoveToElement(depth, name, ns) =
            while this.Depth < depth do this.Read() |> ignore
            let isElement () = this.Depth = depth && this.NodeType = XmlNodeType.Element && (name |> isNull || (this.LocalName = name && this.NamespaceURI = ns))
            let rec findElement () =
                if isElement() then true
                elif this.Read() then
                    if this.Depth < depth then false
                    else findElement()
                else false
            isElement() || findElement()

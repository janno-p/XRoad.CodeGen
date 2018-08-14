namespace XRoad.Runtime

open System
open System.Collections.Generic
open System.IO
open System.Security.Cryptography.X509Certificates
open System.Xml
open XRoad.Runtime.Attributes

[<AllowNullLiteral>]
type internal SerializerContext() =
    let attachments = Dictionary<string, BinaryContent>()
    member val IsMtomMessage = false with get, set
    member val IsMultipart = false with get, set
    member val Attachments = attachments with get
    member this.AddAttachment(contentID, content, useXop) =
        if useXop then this.IsMtomMessage <- true
        attachments.Add(contentID, content)
    member __.GetAttachment(href: string) =
        if href.StartsWith("cid:") then
            let contentID = href.Substring(4)
            match attachments.TryGetValue(contentID) with
            | true, value -> value
            | _ -> null;
        else failwithf "Invalid multipart content reference: `%s`." href

type internal MethodPartMap =
    { IsEncoded: bool
      IsMultipart: bool
      Accessor: XmlQualifiedName option }

type internal DeserializerDelegate = delegate of XmlReader * SerializerContext -> obj
type internal SerializerDelegate = delegate of XmlWriter * obj * SerializerContext -> unit
type internal OperationSerializerDelegate = delegate of XmlWriter * obj[] * SerializerContext -> unit

type internal MethodMap =
    { Deserializer: DeserializerDelegate
      Serializer: OperationSerializerDelegate
      Protocol: XRoadProtocol
      Request: MethodPartMap
      Response: MethodPartMap
      ServiceCode: string
      ServiceVersion: string option
      Namespaces: string list
      RequiredHeaders: IDictionary<string, string[]> }

[<Interface>]
type IXRoadRequest =
    abstract Save: Stream -> unit

[<Interface>]
type IXRoadResponse =
    abstract Save: Stream -> unit

type RequestReadyEventArgs(request: IXRoadRequest, header: AbstractXRoadHeader, requestId: string, serviceCode: string, serviceVersion: string) =
    inherit EventArgs()
    member val Request = request with get
    member val RequestId = requestId with get
    member val ServiceCode = serviceCode with get 
    member val ServiceVersion = serviceVersion with get
    member val Header = header with get

type ResponseReadyEventArgs(response: IXRoadResponse, header: AbstractXRoadHeader, requestId: string, serviceCode: string, serviceVersion: string) =
    inherit EventArgs()
    member val Response = response with get
    member val RequestId = requestId with get
    member val ServiceCode = serviceCode with get 
    member val ServiceVersion = serviceVersion with get
    member val Header = header with get

type RequestReadyEventHandler = delegate of obj * RequestReadyEventArgs -> unit
type ResponseReadyEventHandler = delegate of obj * ResponseReadyEventArgs -> unit

[<AbstractClass>]
type AbstractEndpointDeclaration (uri: Uri) =
    let requestEvent = Event<RequestReadyEventHandler, RequestReadyEventArgs>()
    let responseEvent = Event<ResponseReadyEventHandler, ResponseReadyEventArgs>()

    member val AcceptedServerCertificate = Unchecked.defaultof<X509Certificate> with get, set
    member val AuthenticationCertificates = new ResizeArray<X509Certificate>() with get
    member val Uri = uri with get

    [<CLIEvent>]
    member __.RequestReady = requestEvent.Publish

    [<CLIEvent>]
    member __.ResponseReady = responseEvent.Publish

    member internal this.TriggerRequestReady args = requestEvent.Trigger(this, args)
    member internal this.TriggerResponseReady args = responseEvent.Trigger(this, args)

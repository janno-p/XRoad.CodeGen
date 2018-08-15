namespace XRoad.CodeGen

open CodeDom
open System
open System.CodeDom
open System.Collections.Generic
open System.Reflection
open System.Text.RegularExpressions
open System.Xml.Linq
open TypeSchema
open Wsdl
open XRoad.Runtime
open XRoad.Runtime.Attributes
open XRoad.Runtime.Choices

[<AutoOpen>]
module internal Pattern =
    /// Helper function to create full name for given type name.
    let providedTypeFullName nsname name = sprintf "%s.%s" nsname name

    /// Active pattern which checks type definition against collection characteristics.
    /// Returns match if given type should be treated as CollectionType.
    let (|ArrayContent|_|) (schemaType: SchemaTypeDefinition) =
        // SOAP-encoded array-s use special attribute for array type definition.
        let (|ArrayType|_|) (attributes: AttributeSpec list) =
            attributes |> List.tryFind (fun a -> a.Name = Some("arrayType") || a.RefOrType = Reference(XName.Get("arrayType", XmlNamespace.SoapEnc)))
        // Extracts information about array item type.
        let getArrayItemElement contentParticle =
            match contentParticle with
            | Some(All(all)) ->
                if all.MaxOccurs > 1u then failwith "Not implemented: array of anonymous all types."
                elif all.MaxOccurs < 1u then None
                else match all.Elements with
                     | [ single ] when single.MaxOccurs > 1u -> Some(single)
                     | _ -> None
            | Some(ComplexTypeParticle.Choice(choice)) ->
                if choice.MaxOccurs > 1u then failwith "Not implemented: array of anonymous choice types."
                elif choice.MaxOccurs < 1u then None
                elif (choice.Content
                      |> List.fold (fun state ch ->
                            match state, ch with
                            | true, Element(e) when e.MaxOccurs < 2u -> true
                            | true, Sequence(s) when s.MaxOccurs < 2u -> true
                            | _ -> false) true)
                     then None
                else failwith "Not implemented: array of varying choice types."
            | Some(ComplexTypeParticle.Sequence(sequence)) ->
                if sequence.MaxOccurs > 1u then
                    match sequence.Content with
                    | [] -> None
                    | [ Element(single) ] -> Some(single)
                    | _ -> failwith "Not implemented: array of anonymous sequence types."
                elif sequence.MaxOccurs < 1u then None
                else match sequence.Content with
                     | [ Element(single) ] when single.MaxOccurs > 1u -> Some(single)
                     | _ -> None
            | Some(ComplexTypeParticle.Group) -> failwith "group not implemented."
            | None -> None
        // Test type definitions for collection characteristics.
        match schemaType with
        | ComplexDefinition(spec) ->
            match spec.Content with
            // SOAP-encoded array-s inherit soapenc:Array type.
            | ComplexContent(Restriction(rstr)) when rstr.Base.LocalName = "Array" && rstr.Base.NamespaceName = XmlNamespace.SoapEnc ->
                match rstr.Content.Attributes with
                | ArrayType(attrSpec) ->
                    match attrSpec.ArrayType with
                    | Some(_, rank) when rank <> 1 -> failwith "Multidimensional SOAP encoding arrays are not supported."
                    | Some(typeName, _) ->
                        match getArrayItemElement(rstr.Content.Content) with
                        | Some(element) -> Some({ element with Definition = Explicit(Name(typeName)) })
                        | None -> Some({ Name = Some("item"); MinOccurs = 0u; MaxOccurs = UInt32.MaxValue; IsNillable = true; Definition = Explicit(Name(typeName)); Annotation = None; ExpectedContentTypes = None })
                    | None -> failwith "Array underlying type specification is missing."
                | _ ->
                    match getArrayItemElement(rstr.Content.Content) with
                    | Some(_) as element -> element
                    | None -> failwith "Unsupported SOAP encoding array definition."
            // Multiplicity my constrain to using collection type.
            | Particle(content) -> getArrayItemElement(content.Content)
            | _ -> None
        | EmptyDefinition
        | SimpleDefinition(_) -> None

/// Combines operations and types documented in producer definitions.
type internal ProducerDescription =
    { TypeSchemas: Map<string,SchemaNode>
      Services: Service list }
    /// Load producer definition from given uri location.
    static member Load(uri: Uri, languageCode, operationFilter) =
        let document = Http.getXDocument uri
        match document.Element(xnsname "definitions" XmlNamespace.Wsdl) with
        | null -> failwithf "Uri `%A` refers to invalid WSDL document (`definitions` element not found)." uri
        | definitions ->
            { Services = definitions |> ServiceDescription.parseServices languageCode operationFilter
              TypeSchemas = definitions |> Parser.parseSchema (uri.ToString()) }

/// Context keeps track of already generated types for provided types and namespaces
/// to simplify reuse and resolve mutual dependencies between types.
type internal TypeBuilderContext =
    { /// Provided types generated from type schema definitions.
      CachedTypes: Dictionary<SchemaName,RuntimeType>
      /// Provided types generated to group types from same namespace.
      CachedNamespaces: Dictionary<XNamespace,CodeNamespace>
      /// Schema level attribute definition lookup.
      Attributes: Map<string,AttributeSpec>
      /// Schema level element definition lookup.
      Elements: Map<string,ElementSpec>
      /// Schema level type definition lookup.
      Types: Map<string,SchemaTypeDefinition>
      /// X-Road protocol used by this producer.
      MessageProtocol: XRoadMessageProtocolVersion
      /// Provided configuration values for generator output.
      Options: CodeGenOptions }
    with
        /// Find generated type that corresponds to given namespace name.
        /// If type exists, the existing instance is used; otherwise new type is generated.
        member this.GetOrCreateNamespace(nsname: XNamespace) =
            /// Extract producer name from namespace for simpler class name.
            let (|Producer|_|) ns =
                match Regex.Match(ns, @"^http://(((?<producer>\w+)\.x-road\.ee/producer(/(?<path>.+)?)?)|(producers\.\w+\.xtee\.riik\.ee/producer/(?<producer>\w+)(/(?<path>.+))?))$") with
                | m when m.Success ->
                    let suffix =
                        if m.Groups.["path"].Success
                        then sprintf "_%s" <| m.Groups.["path"].Value.ToClassName()
                        else ""
                    Some(sprintf "%s%s" m.Groups.["producer"].Value suffix)
                | _ -> None
            match this.CachedNamespaces.TryGetValue(nsname) with
            | false, _ ->
                let producerName =
                    match nsname.NamespaceName with
                    | Producer(producerName) -> producerName
                    | XmlNamespace.XRoad20 -> "xtee"
                    | XmlNamespace.XRoad31Ee -> "xroad"
                    | ns -> ns.ToClassName()
                let ns = CodeNamespace(String.Join('.', [this.Options.RootNamespaceOrDefault; producerName]))
                let typ = Cls.create(producerName) |> Cls.addAttr TypeAttributes.Public
                Fld.create<string> "__TargetNamespace__"
                |> Fld.init (!^ nsname.NamespaceName)
                |> Fld.setAttr (MemberAttributes.Public ||| MemberAttributes.Const)
                |> Fld.addTo typ
                |> ignore
                ns.Types.Add(typ) |> ignore
                this.CachedNamespaces.Add(nsname, ns)
                ns
            | true, ns -> ns

        /// Get runtime type from cached types if exists; otherwise create the type.
        member this.GetOrCreateType(name: SchemaName) =
            match this.CachedTypes.TryGetValue(name) with
            | true, info -> info
            | _ -> let info = this.CreateType(name)
                   this.CachedTypes.Add(name, info)
                   info

        /// Get runtime type from cached types if exists.
        member this.GetRuntimeType(name: SchemaName) =
            let resolvedName =
                match name with
                | SchemaElement(xname) ->
                    match this.GetElementSpec(xname) with
                    | ({ Definition = Explicit(Name(typeName)) } : ElementSpec) -> SchemaType(typeName)
                    | _ -> name
                | _ -> name
            match this.CachedTypes.TryGetValue(resolvedName) with
            | true, typeInfo -> typeInfo
            | _ -> match resolvedName.XName with
                   | BinaryType(_) -> ContentType
                   | SystemType(typ) -> PrimitiveType(typ)
                   | _ -> failwithf "Invalid type name `%A`: type not found in cache." resolvedName

        /// Generates new RuntimeType instance depending on given type:
        /// xsd:base64Binary and xsd:hexBinary types represent ContentType.
        /// Types that are mapped to system types represent PrimitiveType value.
        /// Types that have multiplicity larger than 1 are defined as CollectionTypes.
        /// Other types will define separate ProvidedType in generated assembly.
        member private this.CreateType(name: SchemaName) =
            match name.XName with
            | BinaryType(_) -> ContentType
            | SystemType(typ) -> PrimitiveType(typ)
            | _ ->
                let codens = this.GetOrCreateNamespace(name.XName.Namespace)
                let schemaType =
                    match name with
                    | SchemaElement(xn) ->
                        this.GetElementSpec(xn)
                        |> this.DereferenceElementSpec
                        |> snd
                        |> this.GetSchemaTypeDefinition
                    | SchemaType(xn) -> this.GetSchemaType(xn)
                match schemaType with
                | ArrayContent element ->
                    match this.DereferenceElementSpec(element) with
                    | dspec, Name(xn) ->
                        let itemName = dspec.Name |> Option.get
                        CollectionType(this.GetRuntimeType(SchemaType(xn)), itemName, None)
                    | dspec, Definition(def) ->
                        let itemName = dspec.Name |> Option.get
                        let suffix = itemName.ToClassName()
                        let typ = Cls.create(name.XName.LocalName + suffix) |> Cls.addAttr TypeAttributes.Public |> Cls.describe (Attributes.xrdAnonymousType LayoutKind.Sequence)
                        codens.Types.Add(typ) |> ignore
                        CollectionType(ProvidedType(typ, providedTypeFullName codens.Name typ.Name), itemName, Some(def))
                | _ ->
                    let attr =
                        match name with
                        | SchemaElement(_) -> Attributes.xrdAnonymousType LayoutKind.Sequence
                        | SchemaType(_) -> Attributes.xrdType name.XName LayoutKind.Sequence
                    let typ = Cls.create(name.XName.LocalName) |> Cls.addAttr TypeAttributes.Public |> Cls.describe attr
                    codens.Types.Add(typ) |> ignore
                    ProvidedType(typ, providedTypeFullName codens.Name typ.Name)

        /// Finds element specification from schema-level element lookup.
        member this.GetElementSpec(name: XName) =
            match this.Elements.TryFind(name.ToString()) with
            | Some(elementSpec) -> elementSpec
            | None -> failwithf "Invalid reference: global element %A was not found in current context." name

        /// Finds element specification from schema-level type lookup.
        member this.GetSchemaType(name: XName) =
            match this.Types.TryFind(name.ToString()) with
            | Some(schemaType) -> schemaType
            | None -> failwithf "Invalid reference: global type `%A` was not found in current context." name

        /// Resolves real type definition from lookup by following the XML schema references if present.
        /// Returns value of type definitions which actually contains definition, not references other definition.
        member this.GetSchemaTypeDefinition typeDefinition =
            let rec findSchemaTypeDefinition typeDefinition =
                match typeDefinition with
                | Definition(spec) -> spec
                | Name(xn) -> match this.Types.TryFind(xn.ToString()) with
                              | Some(schemaType) -> schemaType
                              | None -> failwithf "Missing referenced schema type `%A`." xn
            findSchemaTypeDefinition typeDefinition

        /// Resolves real atrribute definition from lookup by following the XML schema references if present.
        /// Returns value of attribute definitions which actually contains definition, not references other definition.
        member this.GetAttributeDefinition(spec) =
            let rec findAttributeDefinition (spec: AttributeSpec) =
                match spec.RefOrType with
                | Explicit(typeDefinition) ->
                    match spec.Name with
                    | Some(name) -> name, typeDefinition
                    | None -> failwithf "Attribute has no name."
                | Reference(ref) ->
                    match this.Attributes.TryFind(ref.ToString()) with
                    | Some(spec) -> findAttributeDefinition(spec)
                    | None ->
                        match ref with
                        | XmlName "lang" -> "lang", Name(XName.Get("string", XmlNamespace.Xsd))
                        | _ -> failwithf "Missing referenced attribute %A." ref
            findAttributeDefinition(spec)

        /// Resolves real element definition from lookup by following the XML schema references if present.
        /// Returns value of element definitions which actually contains definition, not references other definition.
        member this.DereferenceElementSpec(spec): ElementSpec * TypeDefinition<SchemaTypeDefinition> =
            let rec findElementDefinition (spec: ElementSpec) =
                match spec.Definition with
                | Explicit(typeDefinition) ->
                    match spec.Name with
                    | Some(_) -> spec, typeDefinition
                    | None -> failwithf "Attribute has no name."
                | Reference(ref) ->
                    match this.Elements.TryFind(ref.ToString()) with
                    | Some(spec) -> findElementDefinition(spec)
                    | None -> failwithf "Missing referenced attribute %A." ref
            findElementDefinition(spec)

        /// Initializes new context object from given schema definition.
        static member FromSchema(schema, options: CodeGenOptions) =
            // Validates that schema contains single operation style, as required by X-Road specification.
            let messageProtocol =
                let reduceStyle s1 s2 =
                    if s1 <> s2 then failwith "Mixing services implementing different X-Road message protocol versions is not accepted!"
                    s1
                schema.Services
                |> List.map (fun svc -> svc.Ports |> List.map (fun p -> p.MessageProtocol) |> List.reduce reduceStyle)
                |> List.reduce reduceStyle
            // Initialize type builder context.
            { CachedNamespaces = Dictionary<_,_>()
              CachedTypes = Dictionary<_,_>()
              Attributes =
                  schema.TypeSchemas
                  |> Map.toSeq
                  |> Seq.collect (fun (_,typ) -> typ.Attributes |> Seq.map (fun x -> x.Key.ToString(), x.Value))
                  |> Map.ofSeq
              Elements =
                  schema.TypeSchemas
                  |> Map.toSeq
                  |> Seq.collect (fun (_,typ) -> typ.Elements |> Seq.map (fun x -> x.Key.ToString(), x.Value))
                  |> Map.ofSeq
              Types =
                  schema.TypeSchemas
                  |> Map.toSeq
                  |> Seq.collect (fun (_,typ) -> typ.Types |> Seq.map (fun x -> x.Key.ToString(), x.Value))
                  |> Map.ofSeq
              MessageProtocol = messageProtocol
              Options = options }

/// Functions and types to handle type building process.
[<RequireQualifiedAccess>]
module internal TypeBuilder =
    /// Describes single property for type declaration.
    type PropertyDefinition =
        { /// Name of the property.
          Name: string
          /// Runtime type to use on property.
          Type: RuntimeType
          /// Does property accept nil values?
          IsNillable: bool
          /// Can array items be nil values?
          IsItemNillable: bool option
          /// Can property value be unspecified in resulting SOAP message.
          IsOptional: bool
          /// Does array type property specify wrapper element around items?
          IsWrappedArray: bool option
          // Attribute type:
          IsAttribute: bool
          IsAny: bool
          IsIgnored: bool
          // Documentation tooltips
          Documentation: string option
          UseXop: bool }
        /// Initializes default property with name and optional value.
        static member Create(name, isOptional, doc, useXop) =
            { Type = PrimitiveType(typeof<Void>)
              IsNillable = false
              IsItemNillable = None
              IsOptional = isOptional
              IsWrappedArray = None
              Name = name
              IsAttribute = false
              IsAny = false
              IsIgnored = false
              Documentation = doc
              UseXop = useXop }

    let private getAttributesForProperty idx elementName (prop: PropertyDefinition) =
        match prop.IsWrappedArray, prop.Type with
        | Some(hasWrapper), CollectionType(_,itemName,_) ->
            let isItemNillable = prop.IsItemNillable |> Option.defaultValue false
            [ Attributes.xrdElement idx elementName None prop.IsNillable (not hasWrapper) prop.UseXop
              Attributes.xrdCollection idx (Some(itemName)) None isItemNillable false ]
        | Some(_), _ ->
            failwith "Array should match to CollectionType."
        | None, _ ->
            [ Attributes.xrdElement idx elementName None prop.IsNillable false prop.UseXop ]

    /// Build property declarations from property definitions and add them to owner type.
    let private addTypeProperties (definitions, subTypes) ownerTy =
        let addTypePropertiesFromDefinition definition =
            // Most of the conditions handle XmlSerializer specific attributes.
            let prop = ownerTy |> addProperty(definition.Name, definition.Type, definition.IsOptional)
                               |> Code.comment (definition.Documentation)
            let elementName = if prop.Name <> definition.Name then Some(definition.Name) else None
            if definition.IsIgnored then
                prop |> Prop.describe Attributes.XmlIgnore |> ignore
            elif definition.IsAny then
                prop |> Prop.describe Attributes.XmlAnyElement |> ignore
            elif definition.IsAttribute then
                prop |> Prop.describe Attributes.XmlAttribute |> ignore
            else
                definition |> getAttributesForProperty None elementName |> List.iter (fun attr -> prop |> Prop.describe attr |> ignore) 
        definitions |> List.iter (addTypePropertiesFromDefinition)
        // Add extra types to owner type declaration.
        ownerTy.Members.AddRange(subTypes |> Seq.cast<_> |> Seq.toArray)

    /// Create definition of property that accepts any element not defined in schema.
    let private buildAnyProperty () =
        let prop = PropertyDefinition.Create("AnyElements", false, None, false)
        { prop with Type = PrimitiveType(typeof<XElement[]>); IsAny = true }

    let private annotationToText (context: TypeBuilderContext) (annotation: Annotation option) =
        annotation
        |> Option.bind (fun annotation ->
            annotation.AppInfo
            |> List.collect (fun e -> e.Elements(titleElementName context.MessageProtocol) |> List.ofSeq)
            |> List.fold (fun doc el ->
                let lang = el |> attrOrDefault (xnsname "lang" XmlNamespace.Xml) "et"
                (lang, el.Value)::doc) []
            |> List.tryFind (fst >> ((=) context.Options.LanguageCodeOrDefault))
            |> Option.map snd)

    let nameGenerator name =
        let num = ref 0
        (fun () ->
            num := !num + 1
            sprintf "%s%d" name !num)

    let private buildEnumerationConstants (runtimeType: RuntimeType) (itemType: RuntimeType) (content: RestrictionContent list) =
        let valueExpr (value: string) =
            match itemType with
            | PrimitiveType(t) when t = typeof<int32> -> !^ (Convert.ToInt32(value))
            | _ -> !^ value
        content
        |> List.choose (fun x ->
            match x with
            | Enumeration(value) ->
                Fld.createRef (runtimeType.AsCodeTypeReference(true)) (value.GetValidPropertyName())
                |> Fld.setAttr (MemberAttributes.Public ||| MemberAttributes.Static)
                |> Fld.init (Expr.instOf (runtimeType.AsCodeTypeReference()) [valueExpr value])
                |> Some
            | _ -> None)

    let getChoiceInterface len =
        match len with
        | 1 -> Some(typedefof<IChoiceOf1<_>>)
        | 2 -> Some(typedefof<IChoiceOf2<_,_>>)
        | 3 -> Some(typedefof<IChoiceOf3<_,_,_>>)
        | 4 -> Some(typedefof<IChoiceOf4<_,_,_,_>>)
        | 5 -> Some(typedefof<IChoiceOf5<_,_,_,_,_>>)
        | 6 -> Some(typedefof<IChoiceOf6<_,_,_,_,_,_>>)
        | 7 -> Some(typedefof<IChoiceOf7<_,_,_,_,_,_,_>>)
        | 8 -> Some(typedefof<IChoiceOf8<_,_,_,_,_,_,_,_>>)
        | _ -> None

    /// Collects property definitions from every content element of complexType.
    let rec private collectComplexTypeContentProperties choiceNameGen seqNameGen context (spec: ComplexTypeContentSpec) =
        // Attribute definitions
        let attributeProperties, attrTypes = spec.Attributes |> List.fold (fun (xs, ys) n -> let x, y = n |> buildAttributeProperty context in x::xs, y |> List.append ys) ([], [])
        // Element definitions
        let elementProperties, elemTypes =
            match spec.Content with
            | Some(All(spec)) ->
                if spec.MinOccurs <> 1u || spec.MaxOccurs <> 1u then failwith "not implemented"
                spec.Elements
                |> List.map (buildElementProperty context)
                |> List.unzip
                |> (fun (a, b) -> a, b |> List.collect id)
            | Some(ComplexTypeParticle.Sequence(spec)) ->
                if spec.MinOccurs > 1u || spec.MaxOccurs <> 1u then failwith "not implemented"
                let collectSequenceProperties content =
                    match content with
                    | Choice(cspec) -> let x, ts = collectChoiceProperties choiceNameGen context cspec in [x], ts
                    | Element(spec) -> let x, ts = buildElementProperty context spec in [x], ts
                    | Sequence(sspec) -> (collectSequenceProperties seqNameGen context sspec), []
                    | Any -> [ buildAnyProperty() ], []
                    | Group -> failwith "Not implemented: group in complexType sequence."
                spec.Content |> List.fold (fun (xs, ys) n -> let x, y = n |> collectSequenceProperties in x |> List.append xs, y |> List.append ys) ([], [])
            | Some(ComplexTypeParticle.Choice(cspec)) ->
                let prop, types = collectChoiceProperties choiceNameGen context cspec
                [prop], types
            | Some(ComplexTypeParticle.Group) ->
                failwith "Not implemented: group in complexType."
            | None -> [], []
        (List.concat [attributeProperties; elementProperties], List.concat [attrTypes; elemTypes])

    /// Create single property definition for given element-s schema specification.
    and private buildElementProperty (context: TypeBuilderContext) (spec: ElementSpec) : PropertyDefinition * CodeTypeDeclaration list =
        let dspec, schemaType = context.DereferenceElementSpec(spec)
        let name = dspec.Name |> Option.get
        buildPropertyDef schemaType spec.MaxOccurs name spec.IsNillable (spec.MinOccurs = 0u) context (annotationToText context spec.Annotation) spec.ExpectedContentTypes.IsSome

    /// Create single property definition for given attribute-s schema specification.
    and private buildAttributeProperty (context: TypeBuilderContext) (spec: AttributeSpec) : PropertyDefinition * CodeTypeDeclaration list =
        let name, typeDefinition = context.GetAttributeDefinition(spec)
        // Resolve schema type for attribute:
        let schemaType =
            match typeDefinition with
            | Definition(simpleTypeSpec) -> Definition(SimpleDefinition(simpleTypeSpec))
            | Name(name) -> Name(name)
        let isOptional = match spec.Use with Required -> true | _ -> false
        let prop, types = buildPropertyDef schemaType 1u name false isOptional context (annotationToText context spec.Annotation) false
        { prop with IsAttribute = true }, types

    /// Build default property definition from provided schema information.
    and private buildPropertyDef schemaType maxOccurs name isNillable isOptional context doc useXop : PropertyDefinition * CodeTypeDeclaration list =
        match schemaType with
        | Definition(ArrayContent itemSpec) ->
            match context.DereferenceElementSpec(itemSpec) with
            | dspec, Name(n) ->
                let itemName = dspec.Name |> Option.get
                ({ PropertyDefinition.Create(name, isOptional, doc, useXop) with
                    Type = CollectionType(context.GetRuntimeType(SchemaType(n)), itemName, None)
                    IsNillable = isNillable
                    IsItemNillable = Some(itemSpec.IsNillable)
                    IsWrappedArray = Some(true) }, [])
            | dspec, Definition(def) ->
                let itemName = dspec.Name |> Option.get
                let suffix = itemName.ToClassName()
                let typ = Cls.create(name + suffix) |> Cls.addAttr TypeAttributes.Public |> Cls.describe (Attributes.xrdAnonymousType LayoutKind.Sequence)
                let runtimeType = ProvidedType(typ, typ.Name)
                build context runtimeType def
                ({ PropertyDefinition.Create(name, isOptional, doc, useXop) with
                    Type = CollectionType(runtimeType, itemName, None)
                    IsNillable = isNillable
                    IsItemNillable = Some(itemSpec.IsNillable)
                    IsWrappedArray = Some(true) }, [typ])
        | Definition(def) ->
            let subTy = Cls.create (name + "Type") |> Cls.addAttr TypeAttributes.Public |> Cls.describe (Attributes.xrdAnonymousType LayoutKind.Sequence)
            let runtimeType = ProvidedType(subTy, subTy.Name)
            build context runtimeType def
            if maxOccurs > 1u then
                ({ PropertyDefinition.Create(name, false, doc, useXop) with
                    Type = CollectionType(runtimeType, name, None)
                    IsNillable = isNillable
                    IsWrappedArray = Some(false) }, [subTy])
            else
                ({ PropertyDefinition.Create(name, isOptional, doc, useXop) with
                    Type = runtimeType
                    IsNillable = isNillable }, [subTy])
        | Name(n) ->
            match context.GetRuntimeType(SchemaType(n)) with
            | x when maxOccurs > 1u ->
                ({ PropertyDefinition.Create(name, false, doc, useXop) with
                    Type = CollectionType(x, name, None)
                    IsNillable = isNillable
                    IsWrappedArray = Some(false) }, [])
            | PrimitiveType(x) when x.IsValueType ->
                ({ PropertyDefinition.Create(name, isOptional, doc, useXop) with
                    Type = PrimitiveType(if isNillable then typedefof<Nullable<_>>.MakeGenericType(x) else x)
                    IsNillable = isNillable }, [])
            | x ->
                ({ PropertyDefinition.Create(name, isOptional, doc, useXop) with
                    Type = x
                    IsNillable = isNillable }, [])

    /// Create property definitions for sequence element specification.
    and private collectSequenceProperties _ _ _ : PropertyDefinition list =
        []

    /// Create property definitions for choice element specification.
    and collectChoiceProperties choiceNameGenerator context spec : PropertyDefinition * CodeTypeDeclaration list =
        let idField = Fld.create<int> "__id"
        let valueField = Fld.create<obj> "__value"

        let ctor =
            Ctor.create()
            |> Ctor.setAttr MemberAttributes.Private
            |> Ctor.addParam<int> "id"
            |> Ctor.addParam<obj> "value"
            |> Ctor.addStmt (Stmt.assign (Expr.this @=> "__id") (!+ "id"))
            |> Ctor.addStmt (Stmt.assign (Expr.this @=> "__value") (!+ "value"))

        let choiceInterface =
            getChoiceInterface spec.Content.Length
            |> Option.map CodeTypeReference

        let choiceName = choiceNameGenerator()
        let choiceType =
            Cls.create (choiceName + "Type")
            |> iif choiceInterface.IsSome (Cls.implements choiceInterface.Value)
            |> Cls.setAttr (TypeAttributes.Public ||| TypeAttributes.Sealed)
            |> Cls.describe (Attributes.xrdAnonymousType LayoutKind.Choice)
            |> Cls.addMembers [idField; valueField; ctor]

        let choiceRuntimeType = ProvidedType(choiceType, choiceType.Name)

        let createOptionType name (propList: PropertyDefinition list) =
            let optionType =
                Cls.create (name + "Type")
                |> Cls.describe (Attributes.xrdAnonymousType LayoutKind.Sequence)
            optionType |> addTypeProperties (propList, [])
            optionType

        let addTryMethod (id: int) (methName: string) (runtimeType: RuntimeType) =
            let tryMethod =
                Meth.create methName
                |> Meth.setAttr (MemberAttributes.Public ||| MemberAttributes.Final)
                |> Meth.returns<bool>
                |> Meth.addOutParamRef (runtimeType.AsCodeTypeReference()) "value"
                |> Meth.addStmt (Stmt.assign (!+ "value") (Expr.defaultValue (runtimeType.AsCodeTypeReference())))
                |> Meth.addStmt (Stmt.condIf (Op.equals (Expr.this @=> "__id") (!^ id))
                                             [Stmt.assign (!+ "value") (Expr.cast (runtimeType.AsCodeTypeReference()) (Expr.this @=> "__value"))])
                |> Meth.addStmt (Stmt.ret (Op.equals (Expr.this @=> "__id") (!^ id)))
            choiceType |> Cls.addMember(tryMethod) |> ignore

        let addNewMethod id (name: string) (runtimeType: RuntimeType) =
            let newMethod =
                Meth.create (sprintf "New%s%s" (if Char.IsLower(name.[0]) then "_" else "") name)
                |> Meth.setAttr (MemberAttributes.Static ||| MemberAttributes.Public)
                |> Meth.returnsOf (choiceRuntimeType.AsCodeTypeReference())
                |> Meth.addParamRef (runtimeType.AsCodeTypeReference()) "value"
                |> Meth.addStmt (Stmt.ret (Expr.instOf (choiceRuntimeType.AsCodeTypeReference()) [!^ id; !+ "value"]))
            choiceType |> Cls.addMember(newMethod) |> ignore

        let optionNameGenerator = nameGenerator (sprintf "%sOption" choiceName)

        let addChoiceMethod i mname (t: CodeTypeReference) =
            choiceInterface
            |> Option.iter
                (fun x ->
                    x.TypeArguments.Add(t) |> ignore
                    let m =
                        Meth.create (sprintf "TryGetOption%d" i)
                        |> Meth.returns<bool>
                        |> Meth.addOutParamRef t "value"
                        |> Meth.addStmt (Stmt.ret ((Expr.this @-> mname) @% [!+ "out value"]))
                    m.PrivateImplementationType <- x
                    choiceType |> Cls.addMember m |> ignore)

        let addedTypes =
            spec.Content
            |> List.mapi (fun i choiceContent ->
                let methName (name: string) =
                    sprintf "TryGet%s%s" (if Char.IsLower(name.[0]) then "_" else "") name
                match choiceContent with
                | Element(spec) ->
                    let prop, types = buildElementProperty context spec
                    prop |> getAttributesForProperty (Some(i + 1)) (Some(prop.Name)) |> List.iter (fun attr -> choiceType |> Cls.describe attr |> ignore)
                    addNewMethod (i + 1) prop.Name prop.Type
                    let name = methName prop.Name
                    addTryMethod (i + 1) name prop.Type
                    addChoiceMethod (i + 1) name (prop.Type.AsCodeTypeReference())
                    types
                | Sequence(spec) ->
                    let props, types = buildSequenceMembers context spec
                    let optionName = optionNameGenerator()
                    choiceType |> Cls.describe (Attributes.xrdElement (Some(i + 1)) (Some(optionName)) None false true false) |> ignore
                    let optionType = createOptionType optionName props
                    let optionRuntimeType = ProvidedType(optionType, optionType.Name)
                    addNewMethod (i + 1) optionName optionRuntimeType
                    let name = methName optionName
                    addTryMethod (i + 1) name optionRuntimeType
                    addChoiceMethod (i + 1) name (optionRuntimeType.AsCodeTypeReference())
                    optionType::types
                | Any -> failwith "Not implemented: any in choice."
                | Choice(_) -> failwith "Not implemented: choice in choice."
                | Group -> failwith "Not implemented: group in choice.")
            |> List.collect id

        { PropertyDefinition.Create(choiceName, false, None, false) with Type = choiceRuntimeType }, choiceType::addedTypes

    /// Extract property definitions for all the elements defined in sequence element.
    and private buildSequenceMembers context (spec: ParticleSpec) : PropertyDefinition list * CodeTypeDeclaration list =
        spec.Content
        |> List.map (function
            | Any -> failwith "Not implemented: any in sequence."
            | Choice(_) -> failwith "Not implemented: choice in sequence."
            | Element(espec) -> buildElementProperty context espec
            | Group -> failwith "Not implemented: group in sequence."
            | Sequence(_) -> failwith "Not implemented: sequence in sequence.")
        |> List.unzip
        |> (fun (a, b) -> a, b |> List.collect id)

    /// Populate generated type declaration with properties specified in type schema definition.
    and build (context: TypeBuilderContext) runtimeType schemaType =
        // Extract type declaration from runtime type definition.
        let providedTy =
            match runtimeType with
            | ProvidedType(decl,_) -> decl
            | _ -> failwith "Only generated types are accepted as arguments!"
        // Generates unique type name for every choice element.
        let choiceNameGen = nameGenerator "Choice"
        let seqNameGen = nameGenerator "Seq"
        // Parse schema definition and add all properties that are defined.
        match schemaType with
        | SimpleDefinition(SimpleTypeSpec.Restriction(spec, annotation)) ->
            providedTy |> Code.comment (annotationToText context annotation) |> ignore
            match context.GetRuntimeType(SchemaType(spec.Base)) with
            | ContentType
            | PrimitiveType(_) as rtyp ->
                let values = spec.Content |> buildEnumerationConstants runtimeType rtyp
                values |> List.iter (providedTy.Members.Add >> ignore)
                providedTy
                |> addContentProperty("BaseValue", rtyp, false)
                |> iif (values |> List.isEmpty) (fun x -> x |> Ctor.setAttr (MemberAttributes.Public))
                |> ignore
                Ctor.create() |> providedTy.Members.Add |> ignore
            | _ -> failwith "Simple types should not restrict complex types."
        | SimpleDefinition(ListDef) ->
            failwith "Not implemented: list in simpleType."
        | SimpleDefinition(Union(_)) ->
            failwith "Not implemented: union in simpleType."
        | ComplexDefinition(spec) ->
            // Abstract types will have only protected constructor.
            if spec.IsAbstract then
                providedTy |> Cls.addAttr TypeAttributes.Abstract
                           |> Cls.addMember (Ctor.create() |> Ctor.setAttr MemberAttributes.Family)
                           |> Code.comment (annotationToText context spec.Annotation)
                           |> ignore
            // Handle complex type content and add properties for attributes and elements.
            let specContent =
                match spec.Content with
                | SimpleContent(SimpleContentSpec.Extension(spec)) ->
                    match context.GetRuntimeType(SchemaType(spec.Base)) with
                    | PrimitiveType(_)
                    | ContentType as rtyp ->
                        providedTy |> addProperty("BaseValue", rtyp, false) |> Prop.describe (Attributes.xrdElement None None None false true false) |> ignore
                        Some(spec.Content)
                    | _ ->
                        failwith "ComplexType-s simpleContent should not extend complex types."
                | SimpleContent(SimpleContentSpec.Restriction(_)) ->
                    failwith "Not implemented: restriction in complexType-s simpleContent."
                | ComplexContent(Extension(spec)) ->
                    match context.GetRuntimeType(SchemaType(spec.Base)) with
                    | ProvidedType(_) as baseTy -> providedTy |> Cls.setParent (baseTy.AsCodeTypeReference()) |> ignore
                    | _ -> failwithf "Only complex types can be inherited! (%A)" spec.Base
                    Some(spec.Content)
                | ComplexContent(Restriction(_)) ->
                    failwith "Not implemented: restriction in complexType-s complexContent"
                | Particle(spec) ->
                    Some(spec)
                | Empty ->
                    None
            specContent
            |> Option.fold (fun _ content -> providedTy |> addTypeProperties (collectComplexTypeContentProperties choiceNameGen seqNameGen context content)) ()
        | EmptyDefinition -> ()

    let removeFaultDescription (definition: SchemaTypeDefinition) =
        let isFault content =
            let areFaultElements (el1: ElementSpec) (el2: ElementSpec) =
                el1.Name = Some("faultCode") && el2.Name = Some("faultString")
            match content with
            | Sequence({ Content = [Element(el1); Element(el2)] }) -> areFaultElements el1 el2 || areFaultElements el2 el1
            | _ -> false
        let filterFault (particles: ParticleContent list) =
            particles |> List.filter (isFault >> not)
        match definition with
        | ComplexDefinition({ Content = Particle({ Content = Some(ComplexTypeParticle.Sequence(sequence)) } as particle) } as spec) ->
            let newParticle =
                match sequence.Content with
                | [ Choice(choice) ] ->
                    match choice.Content |> filterFault with
                    | [Sequence(content)] -> ComplexTypeParticle.Sequence(content)
                    | [] | [_] as content -> ComplexTypeParticle.Sequence({ choice with Content = content })
                    | content -> ComplexTypeParticle.Choice({ choice with Content = content })
                | content -> ComplexTypeParticle.Sequence({ sequence with Content = filterFault content })
            ComplexDefinition({ spec with Content = Particle({ particle with Content = Some(newParticle) }) })
        | EmptyDefinition | ComplexDefinition(_) | SimpleDefinition(_) -> definition

    let buildResponseElementType (context: TypeBuilderContext) (elementName: XName) =
        let elementSpec = elementName |> context.GetElementSpec
        match elementSpec.Definition with
        | Explicit(typeDefinition) ->
            match typeDefinition with
            | Definition(definition) ->
                let runtimeType = context.GetOrCreateType(SchemaElement(elementName))
                definition |> removeFaultDescription |> build context runtimeType
                runtimeType
            | Name(typeName) ->
                context.GetRuntimeType(SchemaType(typeName))
        | Reference(_) -> failwith "Root level element references are not allowed."

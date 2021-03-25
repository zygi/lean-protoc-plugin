import Std.Data.RBMap
import LeanProtocPlugin.Helpers
import LeanProtocPlugin.ProtoGenM
import LeanProtocPlugin.google.protobuf.compiler.plugin
import LeanProtocPlugin.google.protobuf.descriptor

import LeanProto

open google.protobuf.compiler
open google.protobuf

def enumDerivingList := ["Repr", "Inhabited", "BEq"]
def messageDerivingList := ["InhabitedMut", "BEq"]
-- def messageDerivingList := ["Repr", "InhabitedMut", "BEq"]


def outputFilePath (fd: FileDescriptorProto) (root: String) : String := do
  let f := filePathPartsToLeanPackageParts $ fd.name.get!.stripFileEnding.splitOn "/"
  root ++ "/" ++ ("/".intercalate f) ++ ".lean"

def grpcOutputFilePath (fd: FileDescriptorProto) (root: String) : String := do
  let f := filePathPartsToLeanPackageParts $ fd.name.get!.stripFileEnding.splitOn "/"
  root ++ "/" ++ ("/".intercalate f) ++ "GRPC.lean"

def messageFullLeanPath (t: ASTPath) : ProtoGenM String := do
  if t.file.name == (← read).currentFile.name then 
    t.leanName
  else
    return t.leanModule ++ "." ++ t.leanName

def enumFullLeanPath (t: (ASTPath × EnumDescriptorProto)) : ProtoGenM String := do
  let n := protoEnumNameToLean t.snd.name.get!
  return match t.fst.revMessages.head? with
  | some _ => (← messageFullLeanPath t.fst) ++ "_" ++ n
  | none => (← messageFullLeanPath t.fst) ++ n

def oneofFullLeanPath (t: (ASTPath × OneofDescriptorProto)) : ProtoGenM String := do
  let n := protoOneofNameToLean t.snd.name.get!
  return match t.fst.revMessages.head? with
  | some _ => (← messageFullLeanPath t.fst) ++ "_" ++ n
  | none => (← messageFullLeanPath t.fst) ++ n


def oneofFullProtoPath (t: (ASTPath × OneofDescriptorProto)) : String := do
  t.fst.protoFullPath ++ "." ++ t.snd.name.getI


def messageDeserFunctionName (t: ASTPath) : ProtoGenM String := do
  return (← messageFullLeanPath t) ++ "_deserializeAux"

def messageSerFunctionName (t: ASTPath) : ProtoGenM String := do
  return (← messageFullLeanPath t) ++ "_serializeAux"

namespace google.protobuf
def FieldDescriptorProto.isSingular(fd: FieldDescriptorProto) :=
  fd.label == some FieldDescriptorProto_Label.LABEL_OPTIONAL || 
  fd.label == some FieldDescriptorProto_Label.LABEL_REQUIRED

def FieldDescriptorProto.isMessage(fd: FieldDescriptorProto) :=
 fd.type.get! ==  FieldDescriptorProto_Type.TYPE_MESSAGE

end google.protobuf

-- Field types
def bareFieldTypeName(fd: FieldDescriptorProto) : ProtoGenM String := do match (← fd.type.unwrap) with
| FieldDescriptorProto_Type.TYPE_INT32 => "Int"
| FieldDescriptorProto_Type.TYPE_INT64 => "Int"
| FieldDescriptorProto_Type.TYPE_UINT32 => "Nat"
| FieldDescriptorProto_Type.TYPE_UINT64 => "Nat"
| FieldDescriptorProto_Type.TYPE_SINT32 => "Int"
| FieldDescriptorProto_Type.TYPE_SINT64 => "Int"
| FieldDescriptorProto_Type.TYPE_FIXED32 => "Int"
| FieldDescriptorProto_Type.TYPE_FIXED64 => "Int"
| FieldDescriptorProto_Type.TYPE_SFIXED32 => "Int"
| FieldDescriptorProto_Type.TYPE_SFIXED64 => "Int"
| FieldDescriptorProto_Type.TYPE_DOUBLE => "Float"
| FieldDescriptorProto_Type.TYPE_FLOAT => "Float"
| FieldDescriptorProto_Type.TYPE_BOOL => "Bool"
| FieldDescriptorProto_Type.TYPE_STRING => "String"
| FieldDescriptorProto_Type.TYPE_BYTES => "ByteArray"
| FieldDescriptorProto_Type.TYPE_ENUM => do
  let e ← ctxFindEnum fd.typeName.get!
  -- IO.eprintln s!"Found {reprStr e}"
  enumFullLeanPath e.get!
| FieldDescriptorProto_Type.TYPE_MESSAGE => 
  let e ← ctxFindMessage fd.typeName.get!
  messageFullLeanPath e.get!
| FieldDescriptorProto_Type.TYPE_GROUP => throw $ IO.userError "Proto groups are unsupported."

-- If a field represents a map, returns its entry message type's key and value fields
def fieldMapFields(fd: FieldDescriptorProto) : ProtoGenM $ Option (FieldDescriptorProto × FieldDescriptorProto) := do
  if fd.type != some FieldDescriptorProto_Type.TYPE_MESSAGE then return none
  let res ← ctxFindMessage fd.typeName.get!
  let m ← (← res.unwrap).revMessages.head?.unwrap
  if !(m.options.getI.mapEntry.getI) then return none
  return (m.field.get! 0, m.field.get! 1)

def fullFieldTypeName(fd: FieldDescriptorProto) : ProtoGenM String := do
  match (← fieldMapFields fd) with
  | some (k, v) => do
    let k ← bareFieldTypeName k
    let v ← bareFieldTypeName v  
    return s!"(AssocList ({k}) ({v}))"
  | none => do
    let v ← bareFieldTypeName fd
    if fd.label.getD FieldDescriptorProto_Label.LABEL_OPTIONAL == FieldDescriptorProto_Label.LABEL_REPEATED then
      return s!"(Array ({v}))"
    else if fd.isMessage then
      return s!"(Option ({v}))"
    else
      return s!"({v})"

-- Deserialization functions

-- Should represent a term of type (α -> ProtoParseM α) in Lean
def bareDeserFunctionInline(fd: FieldDescriptorProto) : ProtoGenM String := do
let val ← match (← fd.type.unwrap) with
| FieldDescriptorProto_Type.TYPE_INT32 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.decodeInt32AsInt)"
| FieldDescriptorProto_Type.TYPE_INT64 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.decodeInt64AsInt)"
| FieldDescriptorProto_Type.TYPE_UINT32 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.decodeUInt32AsNat)"
| FieldDescriptorProto_Type.TYPE_UINT64 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.decodeUInt64AsNat)"
| FieldDescriptorProto_Type.TYPE_SINT32 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.decodeSInt32)"
| FieldDescriptorProto_Type.TYPE_SINT64 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.decodeSInt64)"
| FieldDescriptorProto_Type.TYPE_FIXED32 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.decodeFixedInt32AsInt)"
| FieldDescriptorProto_Type.TYPE_FIXED64 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.decodeFixedInt64AsInt)"
| FieldDescriptorProto_Type.TYPE_SFIXED32 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.decodeSFixed32)"
| FieldDescriptorProto_Type.TYPE_SFIXED64 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.decodeSFixed64)"
| FieldDescriptorProto_Type.TYPE_DOUBLE => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.decodeFloat64AsFloat)"
| FieldDescriptorProto_Type.TYPE_FLOAT => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.decodeFloat32AsFloat)"
| FieldDescriptorProto_Type.TYPE_BOOL => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.decodeBool)"
| FieldDescriptorProto_Type.TYPE_STRING => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.decodeString)"
| FieldDescriptorProto_Type.TYPE_BYTES => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.decodeByteArray)"
| FieldDescriptorProto_Type.TYPE_ENUM => do
  let e ← ctxFindEnum fd.typeName.get!
  let typeName ← enumFullLeanPath e.get!
  s!"(LeanProto.EncDec.withIgnoredState (LeanProto.EncDec.decodeEnum (α:={typeName})))"
| FieldDescriptorProto_Type.TYPE_MESSAGE => do
  let e ← ctxFindMessage fd.typeName.get!
  let deserFnName ← messageDeserFunctionName e.get!
  s!"(fun v => LeanProto.EncDec.decodeMessage ({deserFnName} v))"
| FieldDescriptorProto_Type.TYPE_GROUP => throw $ IO.userError "Proto groups are unsupported."

def isPrimitive (fd: FieldDescriptorProto) := match fd.type.getI with
| FieldDescriptorProto_Type.TYPE_MESSAGE => false
| FieldDescriptorProto_Type.TYPE_GROUP => false
| _ => true

def isTypePackable (fd: FieldDescriptorProto) := match fd.type.getI with
| FieldDescriptorProto_Type.TYPE_MESSAGE => false
| FieldDescriptorProto_Type.TYPE_GROUP => false
| FieldDescriptorProto_Type.TYPE_BYTES => false
| FieldDescriptorProto_Type.TYPE_STRING => false
| _ => true

def isRepeated(fd: FieldDescriptorProto) : Bool  :=
  fd.label.getD FieldDescriptorProto_Label.LABEL_OPTIONAL == FieldDescriptorProto_Label.LABEL_REPEATED

def isPackable(fd: FieldDescriptorProto) : Bool :=
  isRepeated fd && isTypePackable fd

def isPacked(fd: FieldDescriptorProto) : Bool :=
  isPackable fd && (fd.options.getI.packed != some false)

def fullDeserFunctionInline(fd: FieldDescriptorProto) (wiretypeVarName: String) : ProtoGenM String := do
  match (← fieldMapFields fd) with
  | some (k, v) => do
    let k ← bareDeserFunctionInline k
    let v ← bareDeserFunctionInline v  
    return s!"(LeanProto.EncDec.decodeMapEntry ({k} arbitrary) ({v} arbitrary))"
  | none => do
    let v ← bareDeserFunctionInline fd
    if isRepeated fd then
      if isPackable fd then
        return s!"(LeanProto.EncDec.decodeKeyAndMixedArray ({v} arbitrary) {wiretypeVarName})"
      else
        return s!"(LeanProto.EncDec.decodeKeyAndNonPackedArray ({v} arbitrary) {wiretypeVarName})"
    else
      return v

-- Serialization functions

def bareSerFunctionInline(fd: FieldDescriptorProto) : ProtoGenM String := do
let val ← match (← fd.type.unwrap) with
| FieldDescriptorProto_Type.TYPE_INT32 => "LeanProto.EncDec.encodeIntAsInt32"
| FieldDescriptorProto_Type.TYPE_INT64 => "LeanProto.EncDec.encodeIntAsInt64"
| FieldDescriptorProto_Type.TYPE_UINT32 => "LeanProto.EncDec.encodeNatAsUInt32"
| FieldDescriptorProto_Type.TYPE_UINT64 => "LeanProto.EncDec.encodeNatAsUInt64"
| FieldDescriptorProto_Type.TYPE_SINT32 => "LeanProto.EncDec.encodeIntAsSInt32"
| FieldDescriptorProto_Type.TYPE_SINT64 => "LeanProto.EncDec.encodeIntAsSInt64"
| FieldDescriptorProto_Type.TYPE_FIXED32 => "LeanProto.EncDec.encodeIntAsFixedInt32"
| FieldDescriptorProto_Type.TYPE_FIXED64 => "LeanProto.EncDec.encodeIntAsFixedInt64"
| FieldDescriptorProto_Type.TYPE_SFIXED32 => "LeanProto.EncDec.encodeIntAsSFixed32"
| FieldDescriptorProto_Type.TYPE_SFIXED64 => "LeanProto.EncDec.encodeIntAsSFixed64"
| FieldDescriptorProto_Type.TYPE_DOUBLE => "LeanProto.EncDec.encodeFloatAsFloat64"
| FieldDescriptorProto_Type.TYPE_FLOAT => "LeanProto.EncDec.encodeFloatAsFloat32"
| FieldDescriptorProto_Type.TYPE_BOOL => "LeanProto.EncDec.encodeBool"
| FieldDescriptorProto_Type.TYPE_STRING => "LeanProto.EncDec.encodeString"
| FieldDescriptorProto_Type.TYPE_BYTES => "LeanProto.EncDec.encodeByteArray"
| FieldDescriptorProto_Type.TYPE_ENUM => "LeanProto.EncDec.encodeEnum"
| FieldDescriptorProto_Type.TYPE_MESSAGE => do
  let e ← ctxFindMessage fd.typeName.get!
  let serFnName ← messageSerFunctionName e.get!
  s!"(LeanProto.EncDec.encodeMessage ({serFnName}))"
| FieldDescriptorProto_Type.TYPE_GROUP => throw $ IO.userError "Proto groups are unsupported."

-- Taken from the original protobuf source
section
open LeanProto.EncDec.WireType

def wireTypeForField (fd: FieldDescriptorProto) : Nat := do
  if isPacked fd then return LengthDelimited.toNat
  let wt := match fd.type.get! with 
  | FieldDescriptorProto_Type.TYPE_INT32 => Varint
  | FieldDescriptorProto_Type.TYPE_INT64 => Varint
  | FieldDescriptorProto_Type.TYPE_UINT32 => Varint
  | FieldDescriptorProto_Type.TYPE_UINT64 => Varint
  | FieldDescriptorProto_Type.TYPE_SINT32 => Varint
  | FieldDescriptorProto_Type.TYPE_SINT64 => Varint
  | FieldDescriptorProto_Type.TYPE_FIXED32 => t32Bit
  | FieldDescriptorProto_Type.TYPE_FIXED64 => t64Bit
  | FieldDescriptorProto_Type.TYPE_SFIXED32 => t32Bit
  | FieldDescriptorProto_Type.TYPE_SFIXED64 => t64Bit
  | FieldDescriptorProto_Type.TYPE_DOUBLE => t64Bit
  | FieldDescriptorProto_Type.TYPE_FLOAT => t32Bit
  | FieldDescriptorProto_Type.TYPE_BOOL => Varint
  | FieldDescriptorProto_Type.TYPE_STRING => LengthDelimited
  | FieldDescriptorProto_Type.TYPE_BYTES => LengthDelimited
  | FieldDescriptorProto_Type.TYPE_ENUM => Varint
  | FieldDescriptorProto_Type.TYPE_MESSAGE => LengthDelimited
  | FieldDescriptorProto_Type.TYPE_GROUP => panic! "Groups not supported"
  return wt.toNat
end 

def fullSerFunctionInline(fd: FieldDescriptorProto) (insideOneof: Bool) (wiretypeVarName: String) : ProtoGenM String := do
  let number := fd.number.get!
  match (← fieldMapFields fd) with
  | some (k, v) => do
    let kFn ← bareSerFunctionInline k
    let vFn ← bareSerFunctionInline v  
    let kWt := wireTypeForField k
    let vWt := wireTypeForField v
    return s!"(LeanProto.EncDec.encodeMapWithTag ({kFn}) ({vFn}) {kWt} rfl {vWt} rfl {number})"
  | none => do
    let v ← bareSerFunctionInline fd
    let wt ← wireTypeForField fd
    if fd.label.getD FieldDescriptorProto_Label.LABEL_OPTIONAL == FieldDescriptorProto_Label.LABEL_REPEATED then
      if isPacked fd then
        return s!"(LeanProto.EncDec.encodeIfNonempty (LeanProto.EncDec.encodeWithTag (LeanProto.EncDec.encodePackedArray ({v})) {wt} rfl {number}))"
      else
        return s!"(LeanProto.EncDec.encodeUnpackedArrayWithTag ({v}) {wt} rfl {number})"
    else if fd.isMessage || insideOneof then
      return s!"LeanProto.EncDec.encodeOpt (LeanProto.EncDec.encodeWithTag ({v}) {wt} rfl {number})"
    else
      return s!"LeanProto.EncDec.encodeSkipDefault (LeanProto.EncDec.encodeWithTag ({v}) {wt} rfl {number})"


inductive LogicalField where
| normal : FieldDescriptorProto -> LogicalField
| oneof : FieldDescriptorProto -> OneofDescriptorProto -> List FieldDescriptorProto -> LogicalField
deriving Repr, BEq, Inhabited

def getLogicalFields (d: DescriptorProto) : List LogicalField := do
  let mut res := []
  let mut oneofAccum := []

  -- simplify this
  for i in [:d.field.size] do
    let f := d.field[i]
    match f.oneofIndex with 
    | none => do 
      if oneofAccum != [] then
        -- Add previous oneof
        let rev := oneofAccum.reverse
        res := (LogicalField.oneof rev.head! (d.oneofDecl.get! rev.head!.oneofIndex.get!.toNat) rev) :: res
        oneofAccum := []
      res := (LogicalField.normal f) :: res
    | some n => do
      oneofAccum := f :: oneofAccum

  -- Check for accumulated oneofs once more
  if oneofAccum != [] then
    let rev := oneofAccum.reverse
    res := (LogicalField.oneof rev.head! (d.oneofDecl.get! rev.head!.oneofIndex.get!.toNat) rev) :: res

  res.reverse

-- Since enums can't have any depencencies, generate them independently
def generateEnumDeclarations (fd: FileDescriptorProto) : ProtoGenM Unit := do
  let generateEnum (e: EnumDescriptorProto) (baseLeanName: String): ProtoGenM Unit := do
    addLine s!"inductive {baseLeanName} where"
    for v in e.value do 
      addLine s!"| {v.name.get!} : {baseLeanName}"
    let derives := ", ".intercalate enumDerivingList
    addLine s!"deriving {derives}"
    addLine ""
    
    addLine s!"instance : LeanProto.ProtoEnum {baseLeanName} where"
    addLine s!"  toInt"
    for v in e.value do 
      addLine s!"  | {baseLeanName}.{v.name.get!} => {v.number.get!}"
    addLine ""

    -- For ofInt, since it's legal to have protos with identical nubmers,
    -- we check that we don't add the same number twice
    let mut seen : Std.RBTreeC Int := {}
    addLine s!"  ofInt"
    for v in e.value do 
      let num := v.number.get!
      if seen.contains num then continue
      seen := seen.insert num
      addLine s!"  | {num} => some {baseLeanName}.{v.name.get!}"
    addLine "  | _ => none"
    addLine ""
    addLine ""

  let processMessage (d: ASTPath) (_: Unit) : ProtoGenM Unit := do
    let enums := match d.revMessages.head? with | none => d.file.enumType | some m => m.enumType
    enums.forM (fun e => do generateEnum e (← enumFullLeanPath (d, e)))
  
  recurseM (← ASTPath.initM fd, ()) (wrapRecurseFn processMessage)


-- Message declarations
def generateOneofDeclaration (l: LogicalField) (baseLeanName: String): ProtoGenM Unit := do
  let (od, fields) ← match l with
    | LogicalField.oneof _ od fields => pure (od, fields)
    | _ => throw $ IO.userError "Normal logical field passed to oneof"
    
  addLine s!"inductive {baseLeanName} where"
    
  for v in fields do
    let type ← fullFieldTypeName v
    addLine s!"| {fieldNameToLean v.name.get!} : {type} -> {baseLeanName}"
  addLine ""

def generateMessageDeclaration (p: ASTPath): ProtoGenM Unit := do
  addLine s!"-- Starting {← messageFullLeanPath p}"
  match p.revMessages.head? with
  | none => return
  | some m => do
  let baseLeanName ← messageFullLeanPath p
  let logFields := getLogicalFields m

  -- first, oneofs:
  for f in logFields do
    match f with
    | LogicalField.oneof _ o _ => do 
      IO.eprintln s!"Adding enum for {baseLeanName}"
        
      generateOneofDeclaration f (← oneofFullLeanPath (p, o))
      addLine ""
    | _ => continue

  addLine s!"inductive {baseLeanName} where"
  addLine s!"| mk "     
  for lf in logFields do
    match lf with
    | LogicalField.oneof f o _ => do
      let type ← oneofFullLeanPath (p, o)
      addLine s!"  ({fieldNameToLean o.name.get!} : Option {type})"
    | LogicalField.normal f => do
      -- IO.eprintln s!"{f.name}"
      let type ← fullFieldTypeName f
      addLine s!"  ({fieldNameToLean f.name.get!} : {type})"
  addLine s!"  : {← messageFullLeanPath p}"
  addLine ""

def generateMessageDeclarations (fd: FileDescriptorProto) : ProtoGenM Unit := do
  let rec processMessage (d: ASTPath) (_: Unit) : ProtoGenM Unit := generateMessageDeclaration d  
  recurseM (← ASTPath.initM fd, ()) (wrapRecurseFn processMessage)

def generateLeanDeriveStatements (fd: FileDescriptorProto) : ProtoGenM Unit := do
  -- First, traverse the tree and build up a list of all typenames of msgs and oneofs
  let WithListState := StateT (List String) ProtoGenM
  let doOne (p: ASTPath): WithListState Unit := do
    match p.revMessages.head? with
    | none => return
    | some m => do
    let logFields := getLogicalFields m
    let mut state := [(← messageFullLeanPath p)]

    for lf in logFields do 
      match lf with
      | LogicalField.oneof f o _ => state := (← oneofFullLeanPath (p, o)) :: state 
      | LogicalField.normal f => ()
    
    set (state ++ (← get))


  -- Run the first state monad to get the list
  let initPath ← ASTPath.initM fd
  let m := recurseM (μ := WithListState) (initPath, ()) (wrapRecurseFn (fun x _ => doOne x))
  let ⟨ r, s ⟩  ← StateT.run m []
  
  let derives := ", ".intercalate messageDerivingList
  let idents := ", ".intercalate s
  addLine s!"deriving instance {derives} for {idents}"
  r

-- TODO: rewrite as proper Lean derives
def generateMessageManualDerives (fd: FileDescriptorProto) : ProtoGenM Unit := do
  let doOne (p: ASTPath): ProtoGenM Unit := do
    match p.revMessages.head? with
    | none => return
    | some m => do
    let messageTypeName ← messageFullLeanPath p
    let logFields := getLogicalFields m

    -- Accessors
    -- These are necessary because protos are mutually inductive so we can't make them structures
    for i in [:logFields.length] do
      let lf := logFields.get! i
      let (outputTypeName, origField, fieldName, isOneof) := match lf with
      | LogicalField.oneof f o _ => ((s!"(Option {·})") <$> oneofFullLeanPath (p, o), f, fieldNameToLean o.name.get!, true)
      | LogicalField.normal f => (fullFieldTypeName f, f, fieldNameToLean f.name.get!, false)

      let mut captureVars := ""
      for j in [:logFields.length] do
        captureVars := captureVars ++ s!" v{j}"
      
      -- Getter
      addLine s!"def {messageTypeName}.{fieldName} : {messageTypeName} → {← outputTypeName}"
      addLine s!"| mk{captureVars} => v{i}"
      
      -- Setter
      addLine s!"def {messageTypeName}.set_{fieldName} (orig: {messageTypeName}) (val: {← outputTypeName})"
      addLine s!"  : {messageTypeName} := match orig with"        
      let mut setVars := ""
      for j in [:logFields.length] do
        setVars := setVars ++ (if i == j then " val" else s!" v{j}")
      addLine s!"| mk{captureVars} => {messageTypeName}.mk{setVars}"

  recurseM (← ASTPath.initM fd, ()) (wrapRecurseFn (fun d _ => doOne d))

def generateDeserializers (fd: FileDescriptorProto) : ProtoGenM Unit := do
  let doOne (p: ASTPath): ProtoGenM Unit := do
    match p.revMessages.head? with
    | none => return
    | some m => do
    let messageTypeName ← messageFullLeanPath p
    let messageDeserFunctionName ← messageDeserFunctionName p
    let currentVarName := "x"

    addLine s!"partial def {messageDeserFunctionName} ({currentVarName}: {messageTypeName}) : LeanProto.EncDec.ProtoParseM {messageTypeName} := do"
    addLine s!"  if (← LeanProto.EncDec.done) then return {currentVarName}"
    addLine s!"  let (_type, key) ← LeanProto.EncDec.decodeKey"
    addLine s!"  match key with"

    let addOneofLine (f: FieldDescriptorProto) (o: OneofDescriptorProto) : ProtoGenM Unit := do
      let oneofTypeName ← oneofFullLeanPath (p, o)
      let oneofVariantName := fieldNameToLean f.name.get!
      let fieldName := fieldNameToLean o.name.get!
      let desFn ← fullDeserFunctionInline f "_type"
      let unwrapSuffix := if f.isSingular && f.isMessage then ".getD arbitrary" else ""
      let newValue := s!"(some ({oneofTypeName}.{oneofVariantName} (← {desFn} (match {currentVarName}.{fieldName} with | some ({oneofTypeName}.{oneofVariantName} q) => q{unwrapSuffix} | _ => arbitrary ))))"
      let number := f.number.get!
      addLine s!"| {number} => do {messageDeserFunctionName} ({currentVarName}.set_{fieldName} {newValue})"

    let addNormalLine (f: FieldDescriptorProto) : ProtoGenM Unit := do
      let fieldName := fieldNameToLean f.name.get!
      let desFn ← fullDeserFunctionInline f "_type"
      let number := f.number.get!
      let unwrapSuffix := if f.isSingular && f.isMessage then ".getD arbitrary" else ""
      match (← fieldMapFields f) with
      | some _ => do
        let newValue := s!"(({currentVarName}.{fieldName}.erase newPair.fst).insert newPair.fst newPair.snd)"
        addLines #[
          s!"| {number} => do",
          s!"  let newPair ← {desFn}",
          s!"  {messageDeserFunctionName} ({currentVarName}.set_{fieldName} {newValue})"
        ]
      | none =>
        let newValue := s!"(← {desFn} ({currentVarName}.{fieldName}{unwrapSuffix}))"
        addLine s!"| {number} => do {messageDeserFunctionName} ({currentVarName}.set_{fieldName} {newValue})"

    let logFields := getLogicalFields m
    for lf in logFields do
      match lf with
      | LogicalField.oneof f o fs => do
        for inner in fs do addOneofLine inner o
      | LogicalField.normal f => do addNormalLine f

    addLine s!"| 0 => do throw $ IO.userError \"Decoding message with field number 0\""
    addLine s!"| _ => do let _ ← LeanProto.EncDec.decodeUnknown _type; {messageDeserFunctionName} {currentVarName}"
    addLine ""

  recurseM (← ASTPath.initM fd, ()) (wrapRecurseFn (fun d _ => doOne d))


def generateSerializers (fd: FileDescriptorProto) : ProtoGenM Unit := do
  let doOne (p: ASTPath): ProtoGenM Unit := do
    match p.revMessages.head? with
    | none => return
    | some m => do
    let messageTypeName ← messageFullLeanPath p
    let messageSerFunctionName ← messageSerFunctionName p
    let currentVarName := "x"
    let resVarName := "res"

    let logFields := getLogicalFields m

    let mut binderNames := ""
    for i in [:logFields.length] do
      binderNames := binderNames ++ s!" v{i}"

    addLine s!"partial def {messageSerFunctionName} ({currentVarName}: ByteArray) : {messageTypeName} -> ByteArray"
    addLine s!"| {messageTypeName}.mk{binderNames} => do"
    addLine s!"  let mut {resVarName} := ByteArray.mkEmpty 0"

    for idx in [:logFields.length] do
      let lf := logFields.get! idx
      match lf with
      | LogicalField.oneof _ o fs =>
        addLine s!"  {resVarName} := match v{idx} with"
        let oneofTypeName ← oneofFullLeanPath (p, o)
        for f in fs do
          let oneofVariantName := fieldNameToLean f.name.get!
          let serFnInline ← fullSerFunctionInline f true "_type"
          addLine s!"    | some ({oneofTypeName}.{oneofVariantName} indVal) => {serFnInline} {resVarName} indVal"
        addLine s!"    | none => {resVarName}"
      | LogicalField.normal f =>
        let serFnInline ← fullSerFunctionInline f false "_type"
        addLine s!"  {resVarName} := {serFnInline} {resVarName} v{idx}"

    addLine s!"  return {resVarName}"
    addLine ""

  recurseM (← ASTPath.initM fd, ()) (wrapRecurseFn (fun d _ => doOne d))

def generateSerDeserInstances (fd: FileDescriptorProto) : ProtoGenM Unit := do
  let doOne (p: ASTPath): ProtoGenM Unit := do
    match p.revMessages.head? with
    | none => return
    | some m => do
    let messageTypeName ← messageFullLeanPath p
    let messageSerFunctionName ← messageSerFunctionName p
    let messageDeserFunctionName ← messageDeserFunctionName p

    addLine s!"instance : LeanProto.ProtoSerialize {messageTypeName} where"
    addLine s!"  serialize := {messageSerFunctionName} (ByteArray.mkEmpty 0)"
    addLine s!""

    addLine s!"instance : LeanProto.ProtoDeserialize {messageTypeName} where"
    -- TODO replace with default
    addLine s!"  deserialize b :="
    addLine s!"    let res := LeanProto.EncDec.decode b ({messageDeserFunctionName} arbitrary)"
    addLine s!"    LeanProto.EncDec.resultToExcept res"
    addLine s!""


  recurseM (← ASTPath.initM fd, ()) (wrapRecurseFn (fun d _ => doOne d))


def generateGRPC (fd: FileDescriptorProto) : ProtoGenM Unit := do
  let getEndpointInfo (md: MethodDescriptorProto) (grpcPrefix: String): ProtoGenM (String × String) := do
    let inputTypeOpt ← ctxFindMessage md.inputType.get!
    let inputType ← messageFullLeanPath inputTypeOpt.get!
    let outputTypeOpt ← ctxFindMessage md.outputType.get!
    let outputType ← messageFullLeanPath outputTypeOpt.get!

    let grpcName := grpcPrefix ++ md.name.get!
    let fieldName := protoEndpointNameToLean md.name.get!

    let (actionType, inductiveVariant) := match (md.clientStreaming.getI, md.serverStreaming.getI) with
    | (false, false) => ("LeanGRPC.ServerUnaryHandler", "unaryHandler")
    | (false, true) => ("LeanGRPC.ServerServerStreamingHandler", "serverStreamingHandler")
    | (true, false) => ("LeanGRPC.ServerClientStreamingHandler", "clientStreamingHandler")
    | (true, true) => ("LeanGRPC.ServerBiDiStreamingHandler", "biDiStreamingHandler")

    let field := s!"  {fieldName} : {actionType} σ {inputType} {outputType}"
    let handler := s!"    (\"{grpcName}\", LeanGRPC.ServerHandler.mk _ inferInstance _ inferInstance (LeanGRPC.ServerHandlerAction.{inductiveVariant} {fieldName}))"
    return (field, handler)

  let generateService (sd: ServiceDescriptorProto) : ProtoGenM Unit := do
    let grpcPrefix := "/" ++ fd.package.get! ++ "." ++ sd.name.get! ++ "/"    
    let endpointInfo ← sd.method.mapM (getEndpointInfo · grpcPrefix)
    let className := protoServiceNameToLean sd.name.get!

    addLine s!"class {className} (σ) extends LeanGRPC.Service σ where"
    for ⟨field, _⟩ in endpointInfo do addLine field

    addLine s!"  handlers_ := Std.rbmapOf ["
    let lines ← endpointInfo.mapM (fun ⟨_, entry⟩ => entry)
    addLine $ ",\n".intercalate lines.toList
    addLine s!"  ] (fun a b => a < b)"
    addLine ""

  let _ ← fd.service.mapM generateService
  

    
    

    
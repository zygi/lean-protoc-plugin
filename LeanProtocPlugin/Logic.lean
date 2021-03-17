import Std.Data.RBMap
import LeanProtocPlugin.Helpers
import LeanProtocPlugin.ProtoGenM
import LeanProtocPlugin.google.protobuf.compiler.plugin
import LeanProtocPlugin.google.protobuf.descriptor

open google.protobuf.compiler
open google.protobuf

def enumDerivingList := ["Repr", "Inhabited", "BEq"]
def oneofDerivingList := ["InhabitedMut", "BEq"]
def messageDerivingList := ["InhabitedMut", "BEq"]

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

namespace google.protobuf
def FieldDescriptorProto.isSingular(fd: FieldDescriptorProto) :=
  fd.label == some FieldDescriptorProto_Label.LABEL_OPTIONAL || 
  fd.label == some FieldDescriptorProto_Label.LABEL_REQUIRED
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
    else
      return s!"(Option ({v}))"


-- Deserialization functions

-- Should represent a term of type (α -> ProtoParseM α) in Lean
def bareDeserFunctionName(fd: FieldDescriptorProto) : ProtoGenM String := do
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

def isPackable(fd: FieldDescriptorProto) : Bool := do
  if isPrimitive fd then return true
  return fd.options.getI.packed.getI

def fullDeserFunctionName(fd: FieldDescriptorProto) (wiretypeVarName: String) : ProtoGenM String := do
  match (← fieldMapFields fd) with
  | some (k, v) => do
    let k ← bareDeserFunctionName k
    let v ← bareDeserFunctionName v  
    return s!"(LeanProto.EncDec.decodeMapEntry ({k} arbitrary) ({v} arbitrary))"
  | none => do
    let v ← bareDeserFunctionName fd
    if fd.label.getD FieldDescriptorProto_Label.LABEL_OPTIONAL == FieldDescriptorProto_Label.LABEL_REPEATED then
      if isPackable fd then
        return s!"(LeanProto.EncDec.decodeKeyAndMixedArray ({v} arbitrary) {wiretypeVarName})"
      else
        return s!"(LeanProto.EncDec.decodeKeyAndNonPackedArray ({v} arbitrary) {wiretypeVarName})"
    else
      return v


inductive LogicalField where
| normal : FieldDescriptorProto -> LogicalField
| oneof : FieldDescriptorProto -> OneofDescriptorProto -> List FieldDescriptorProto -> LogicalField
deriving Repr, BEq, Inhabited

def getLogicalFields (d: DescriptorProto) : List LogicalField := do
  let mut res := []
  -- let mut oneofsSeen : Std.RBTreeC Int := {}
  let mut oneofAccum := []

  for i in [:d.field.size] do
    let f := d.field[i]
    match f.oneofIndex with 
    | none => do 
      if oneofAccum != [] then
        -- Add previous oneof
        let rev := oneofAccum.reverse
        res := (LogicalField.oneof rev.head! (d.oneofDecl.get! rev.head!.oneofIndex.get!.toNat) rev) :: res
      res := (LogicalField.normal f) :: res
    | some n => do
      oneofAccum := f :: oneofAccum
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
  
  recurseM (ASTPath.init fd, ()) (wrapRecurseFn processMessage)


-- Message declarations
def generateOneofDeclaration (l: LogicalField) (baseLeanName: String): ProtoGenM Unit := do
  let (od, fields) ← match l with
    | LogicalField.oneof _ od fields => pure (od, fields)
    | _ => throw $ IO.userError "Normal logical field passed to oneof"
    
  addLine s!"inductive {baseLeanName} where"
    
  for v in fields do
    let type ← fullFieldTypeName v
    addLine s!"| {fieldNameToLean v.name.get!} : {type} -> {baseLeanName}"

  let derives := ", ".intercalate oneofDerivingList
  addLine s!"deriving {derives}"
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
      generateOneofDeclaration f (← oneofFullLeanPath (p, o))
      addLine ""
    | _ => continue

  addLine s!"inductive {baseLeanName} where"
  addLine s!"| mk "     
  for lf in logFields do
    match lf with
    | LogicalField.oneof f o _ => do
      let type ← oneofFullLeanPath (p, o)
      addLine s!"  ({fieldNameToLean o.name.get!} : {type})"
    | LogicalField.normal f => do
      -- IO.eprintln s!"{f.name}"
      let type ← fullFieldTypeName f
      addLine s!"  ({fieldNameToLean f.name.get!} : {type})"
  addLine s!"  : {← messageFullLeanPath p}"

  let derives := ", ".intercalate messageDerivingList
  addLine s!"deriving {derives}"
  addLine ""

def generateMessageDeclarations (fd: FileDescriptorProto) : ProtoGenM Unit := do
  let rec processMessage (d: ASTPath) (_: Unit) : ProtoGenM Unit := generateMessageDeclaration d  
  recurseM (ASTPath.init fd, ()) (wrapRecurseFn processMessage)


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
      let (outputTypeName, origField, isOneof) := match lf with
      | LogicalField.oneof f o _ => (oneofFullLeanPath (p, o), f, true)
      | LogicalField.normal f => (fullFieldTypeName f, f, false)
      let fieldName := fieldNameToLean origField.name.get!

      let mut captureVars := ""
      for j in [:logFields.length] do
        captureVars := captureVars ++ s!" v{j}"
      
      -- Getter
      addLine s!"def {messageTypeName}.{fieldName} : {messageTypeName} → {← outputTypeName}"
      addLine s!"| mk{captureVars} => v{i}"

      -- Getter or default
      if isOneof || origField.isSingular then
        addLine s!"def {messageTypeName}.{fieldName}! : {messageTypeName} → {← outputTypeName}"
        addLine s!"| mk{captureVars} => v{i}.getD arbitrary"
      
      -- Setter
      addLine s!"def {messageTypeName}.set_{fieldName} (orig: {messageTypeName}) (val: {← outputTypeName})"
      addLine s!"  : {messageTypeName} := match orig with"        
      let mut setVars := ""
      for j in [:logFields.length] do
        setVars := setVars ++ (if i == j then " val" else s!" v{j}")
      addLine s!"| mk{captureVars} => {messageTypeName}.mk{setVars}"

  recurseM (ASTPath.init fd, ()) (wrapRecurseFn (fun d _ => doOne d))

def generateDeserializers (fd: FileDescriptorProto) : ProtoGenM Unit := do
  let doOne (p: ASTPath): ProtoGenM Unit := do
    match p.revMessages.head? with
    | none => return
    | some m => do
    let messageTypeName ← messageFullLeanPath p
    let fnName ← messageDeserFunctionName p
    let messageDeserFunctionName ← messageDeserFunctionName p
    let currentVarName := "x"

    addLine s!"partial def {fnName} ({currentVarName}: {messageTypeName}) : LeanProto.EncDec.ProtoParseM {messageTypeName} := do"
    addLine s!"  if (← LeanProto.EncDec.done) then return {currentVarName}"
    addLine s!"  let (_type, key) ← LeanProto.EncDec.decodeKey"
    addLine s!"  match key with"

    let addOneofLine (f: FieldDescriptorProto) (o: OneofDescriptorProto) : ProtoGenM Unit := do
      let oneofTypeName ← oneofFullProtoPath (p, o)
      let fieldName := fieldNameToLean f.name.get!
      let desFn ← fullDeserFunctionName f "_type"
      let unwrapSuffix := if f.isSingular then ".getD arbitrary" else ""
      let newValue := s!"({oneofTypeName}.{fieldName} (← {desFn} (match {currentVarName}.{fieldName} with | {oneofTypeName}.{fieldName} q => q{unwrapSuffix} | _ => arbitrary )))"
      let number := f.number.get!
      addLine s!"| {number} => do {messageDeserFunctionName} ({currentVarName}.set_{fieldName} {newValue})"

    let addNormalLine (f: FieldDescriptorProto) : ProtoGenM Unit := do
      let fieldName := fieldNameToLean f.name.get!
      let desFn ← fullDeserFunctionName f "_type"
      let number := f.number.get!
      let unwrapSuffix := if f.isSingular then ".getD arbitrary" else ""
      match (← fieldMapFields f) with
      | some _ => do
        let newValue := s!"({currentVarName}.{fieldName}.erase newPair.fst).insert newPair.fst newPair.snd"
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

    addLine s!"| _ => do if (← LeanProto.EncDec.done) then throw $ IO.userError \"EoF before unknown value\""
    addLine s!"          {messageDeserFunctionName} {currentVarName}"
    addLine ""

  recurseM (ASTPath.init fd, ()) (wrapRecurseFn (fun d _ => doOne d))
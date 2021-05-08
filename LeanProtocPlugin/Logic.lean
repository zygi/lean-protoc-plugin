import Std.Data.RBMap
import LeanProtocPlugin.Helpers
import LeanProtocPlugin.ProtoGenM
import LeanProtocPlugin.Google.Protobuf.Compiler.Plugin
import LeanProtocPlugin.Google.Protobuf.Descriptor

import LeanProto

open LeanProtocPlugin.Google.Protobuf.Compiler
open LeanProtocPlugin.Google.Protobuf

def enumDerivingList := ["Repr", "Inhabited", "BEq"]
def messageDerivingList := ["Repr", "BEq"]
def oneofDerivingList := ["Repr", "Inhabited", "BEq"]


def outputFilePath (fd: FileDescriptorProto) (root: String) : String := do
  let f := filePathPartsToLeanPackageParts $ fd.name.stripFileEnding.splitOn "/"
  root ++ "/" ++ ("/".intercalate f) ++ ".lean"

def grpcOutputFilePath (fd: FileDescriptorProto) (root: String) : String := do
  let f := filePathPartsToLeanPackageParts $ fd.name.stripFileEnding.splitOn "/"
  root ++ "/" ++ ("/".intercalate f) ++ "GRPC.lean"

def messageFullLeanPath (t: ASTPath) : ProtoGenM String := do
  if t.file.name == (← read).currentFile.name then 
    t.leanName
  else
    return t.leanModule ++ "." ++ t.leanName

def enumFullLeanPath (t: (ASTPath × EnumDescriptorProto)) : ProtoGenM String := do
  let n := protoEnumNameToLean t.snd.name
  match t.fst.revMessages.head? with
  | some _ => do return (← messageFullLeanPath t.fst) ++ "_" ++ n
  | none => do return (← messageFullLeanPath t.fst) ++ n

def oneofFullLeanPath (t: (ASTPath × OneofDescriptorProto)) : ProtoGenM String := do
  let n := protoOneofNameToLean t.snd.name
  match t.fst.revMessages.head? with
  | some _ => do return (← messageFullLeanPath t.fst) ++ "_" ++ n
  | none => do return (← messageFullLeanPath t.fst) ++ n


def oneofFullProtoPath (t: (ASTPath × OneofDescriptorProto)) : String := do
  t.fst.protoFullPath ++ "." ++ t.snd.name

def messageDeserFunctionName (t: ASTPath) : ProtoGenM String := do
  return (← messageFullLeanPath t) ++ "_deserializeAux"

def messageSerFunctionName (t: ASTPath) : ProtoGenM String := do
  return (← messageFullLeanPath t) ++ "_serializeAux"

namespace LeanProtocPlugin.Google.Protobuf
def FieldDescriptorProto.isSingular(fd: FieldDescriptorProto) :=
  fd.label == some FieldDescriptorProto_Label.LABEL_OPTIONAL || 
  fd.label == some FieldDescriptorProto_Label.LABEL_REQUIRED

def FieldDescriptorProto.isMessage(fd: FieldDescriptorProto) :=
 fd.type ==  FieldDescriptorProto_Type.TYPE_MESSAGE


def FieldDescriptorProto.isEnum (fd: FieldDescriptorProto) := fd.type == FieldDescriptorProto_Type.TYPE_ENUM

def FieldDescriptorProto.isStringOrBytes (fd: FieldDescriptorProto) := match fd.type with
| FieldDescriptorProto_Type.TYPE_STRING => true
| FieldDescriptorProto_Type.TYPE_BYTES => true
| _ => false

def FieldDescriptorProto.isPrimitive (fd: FieldDescriptorProto) := match fd.type with
| FieldDescriptorProto_Type.TYPE_MESSAGE => false
| FieldDescriptorProto_Type.TYPE_GROUP => false
| _ => true

def FieldDescriptorProto.isTypePackable (fd: FieldDescriptorProto) := match fd.type with
| FieldDescriptorProto_Type.TYPE_MESSAGE => false
| FieldDescriptorProto_Type.TYPE_GROUP => false
| FieldDescriptorProto_Type.TYPE_BYTES => false
| FieldDescriptorProto_Type.TYPE_STRING => false
| _ => true

def FieldDescriptorProto.isRepeated(fd: FieldDescriptorProto) : Bool  :=
  fd.label == FieldDescriptorProto_Label.LABEL_REPEATED

def FieldDescriptorProto.isPackable(fd: FieldDescriptorProto) : Bool :=
  isRepeated fd && isTypePackable fd

def FieldDescriptorProto.isPacked(fd: FieldDescriptorProto) : Bool :=
  isPackable fd && (fd.options.getD arbitrary).packed

-- If a field represents a map, returns its entry message type's key and value fields
def FieldDescriptorProto.mapFields(fd: FieldDescriptorProto) : ProtoGenM $ Option (FieldDescriptorProto × FieldDescriptorProto) := do
  if fd.type != FieldDescriptorProto_Type.TYPE_MESSAGE then return none
  let res ← ctxFindMessage fd.typeName
  let m ← (← res.unwrap).revMessages.head?.unwrap
  if !((m.options.getD arbitrary).mapEntry) then return none
  return (m.field.get! 0, m.field.get! 1)

end LeanProtocPlugin.Google.Protobuf


inductive LogicalField where
| nonoptional : FieldDescriptorProto -> LogicalField
| optional : FieldDescriptorProto -> LogicalField
| repeated : FieldDescriptorProto -> LogicalField
| map : (orig: FieldDescriptorProto) -> (k: FieldDescriptorProto) -> (v: FieldDescriptorProto) -> LogicalField
| oneof : LogicalField -> (ASTPath × OneofDescriptorProto) -> Array LogicalField -> LogicalField
deriving BEq, Inhabited

-- TODO why do I need partial here?
partial def LogicalField.getFD (lf: LogicalField) : FieldDescriptorProto := match lf with
| LogicalField.nonoptional fd => fd
| LogicalField.optional fd => fd
| LogicalField.repeated fd => fd
| LogicalField.map fd _ _ => fd
| LogicalField.oneof lf' _ _ => lf'.getFD

partial def LogicalField.fieldName (lf: LogicalField) : String := match lf with
| LogicalField.oneof _ o _ => fieldNameToLean o.snd.name
| _ => fieldNameToLean lf.getFD.name

-- Lean-code for the default value of the field
partial def LogicalField.defaultLean (lf: LogicalField) : ProtoGenM String := 
  let singularDefault (f: FieldDescriptorProto) : ProtoGenM String := do
    if f.isMessage then
      return "none"
    if f.defaultValue == "" then
      return "arbitrary"
    if f.isStringOrBytes then
      return "\"" ++ f.defaultValue ++ "\""
    if f.isEnum then do
      let t ← ctxFindEnum f.typeName
      let tv ← t.unwrap
      let typeName ← enumFullLeanPath tv
      return s!"{typeName}.{f.defaultValue}"
    s!" ({f.defaultValue})"

  match lf with
  | LogicalField.nonoptional fd => singularDefault fd
  | LogicalField.optional fd => singularDefault fd
  | LogicalField.repeated _ => pure "arbitrary"
  | LogicalField.map fd _ _ => pure "arbitrary"
  | LogicalField.oneof lf' _ _ => pure "none"


def getLogicalFields (path: ASTPath) : ProtoGenM $ Array LogicalField := do
  let mut res := #[]
  let mut oneofAccum := #[]
  let d ← do try
    path.revMessages.head?.unwrap
  catch _ =>
    throw $ IO.userError "Trying to get logical fields of non-message path"

  let toLF (f: FieldDescriptorProto) : ProtoGenM LogicalField := do match (← f.mapFields) with 
        | some ⟨f1, f2⟩ => LogicalField.map f f1 f2
        | none => do
          if f.isRepeated then LogicalField.repeated f
          else if f.isMessage then LogicalField.optional f
          else LogicalField.nonoptional f

  -- TODO: simplify this

  let oneofAccumToLF (oneofs: Array FieldDescriptorProto) : ProtoGenM LogicalField := do
      -- Add previous oneof
      let oneofIdx := (oneofs.get! 0).oneofIndex.toNat
      let revLF ← oneofs.mapM toLF
      return (LogicalField.oneof (revLF.get! 0) (path, d.oneofDecl.get! oneofIdx) revLF)

  for i in [:d.field.size] do
    let f := d.field[i]
    match f.oneofIndex with
    | (-1) => do 
      if oneofAccum.size > 0 then
        res := res.push (← oneofAccumToLF oneofAccum) 
        oneofAccum := #[]

      res := res.push (← toLF f)
    | _ => do
      oneofAccum := oneofAccum.push f 

  -- Check for accumulated oneofs once more
  if oneofAccum.size > 0 then
    res := res.push (← oneofAccumToLF oneofAccum) 
    oneofAccum := #[]

  return res

-- Field types
def bareFieldTypeName(fd: FieldDescriptorProto) : ProtoGenM String := do match fd.type with
| FieldDescriptorProto_Type.TYPE_INT32 => "_root_.Int"
| FieldDescriptorProto_Type.TYPE_INT64 => "_root_.Int"
| FieldDescriptorProto_Type.TYPE_UINT32 => "_root_.Nat"
| FieldDescriptorProto_Type.TYPE_UINT64 => "_root_.Nat"
| FieldDescriptorProto_Type.TYPE_SINT32 => "_root_.Int"
| FieldDescriptorProto_Type.TYPE_SINT64 => "_root_.Int"
| FieldDescriptorProto_Type.TYPE_FIXED32 => "_root_.UInt32"
| FieldDescriptorProto_Type.TYPE_FIXED64 => "_root_.UInt64"
| FieldDescriptorProto_Type.TYPE_SFIXED32 => "_root_.Int"
| FieldDescriptorProto_Type.TYPE_SFIXED64 => "_root_.Int"
| FieldDescriptorProto_Type.TYPE_DOUBLE => "_root_.Float"
| FieldDescriptorProto_Type.TYPE_FLOAT => "_root_.Float"
| FieldDescriptorProto_Type.TYPE_BOOL => "_root_.Bool"
| FieldDescriptorProto_Type.TYPE_STRING => "_root_.String"
| FieldDescriptorProto_Type.TYPE_BYTES => "_root_.ByteArray"
| FieldDescriptorProto_Type.TYPE_ENUM => do
  let e ← ctxFindEnum fd.typeName
  enumFullLeanPath (← e.unwrap)
| FieldDescriptorProto_Type.TYPE_MESSAGE => 
  let e ← ctxFindMessage fd.typeName
  messageFullLeanPath (← e.unwrap)
| FieldDescriptorProto_Type.TYPE_GROUP => throw $ IO.userError "Proto groups are unsupported."

def fullFieldTypeName(lf: LogicalField) : ProtoGenM String := match lf with
  | LogicalField.map _ k v => do
    let k ← bareFieldTypeName k
    let v ← bareFieldTypeName v  
    return s!"(_root_.Std.AssocList ({k}) ({v}))"
  | LogicalField.repeated v => return s!"(_root_.Array ({← bareFieldTypeName v}))"
  | LogicalField.optional v => return s!"(_root_.Option ({← bareFieldTypeName v}))"
  | LogicalField.nonoptional v => return s!"({← bareFieldTypeName v})"
  | LogicalField.oneof f od _ => do
    return s!"(Option ({← oneofFullLeanPath od}))"

-- Should represent a term of type (α -> ProtoParseM α) in Lean
def bareDeserFunctionInline(fd: FieldDescriptorProto) : ProtoGenM String := do
let val ← match fd.type with
| FieldDescriptorProto_Type.TYPE_INT32 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.parseInt32AsInt)"
| FieldDescriptorProto_Type.TYPE_INT64 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.parseInt64AsInt)"
| FieldDescriptorProto_Type.TYPE_UINT32 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.parseUInt32AsNat)"
| FieldDescriptorProto_Type.TYPE_UINT64 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.parseUInt64AsNat)"
| FieldDescriptorProto_Type.TYPE_SINT32 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.parseSInt32)"
| FieldDescriptorProto_Type.TYPE_SINT64 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.parseSInt64)"
| FieldDescriptorProto_Type.TYPE_FIXED32 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.parseFixedUInt32)"
| FieldDescriptorProto_Type.TYPE_FIXED64 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.parseFixedUInt64)"
| FieldDescriptorProto_Type.TYPE_SFIXED32 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.parseSFixed32)"
| FieldDescriptorProto_Type.TYPE_SFIXED64 => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.parseSFixed64)"
| FieldDescriptorProto_Type.TYPE_DOUBLE => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.parseFloat64AsFloat)"
| FieldDescriptorProto_Type.TYPE_FLOAT => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.parseFloat32AsFloat)"
| FieldDescriptorProto_Type.TYPE_BOOL => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.parseBool)"
| FieldDescriptorProto_Type.TYPE_STRING => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.parseString)"
| FieldDescriptorProto_Type.TYPE_BYTES => "(LeanProto.EncDec.withIgnoredState LeanProto.EncDec.parseByteArray)"
| FieldDescriptorProto_Type.TYPE_ENUM => do
  let e ← ctxFindEnum fd.typeName
  let typeName ← enumFullLeanPath (← e.unwrap)
  s!"(LeanProto.EncDec.withIgnoredState (LeanProto.EncDec.parseEnum (α:={typeName})))"
| FieldDescriptorProto_Type.TYPE_MESSAGE => do
  let e ← ctxFindMessage fd.typeName
  let deserFnName ← messageDeserFunctionName (← e.unwrap)
  s!"(fun v => LeanProto.EncDec.parseMessage ({deserFnName} v))"
| FieldDescriptorProto_Type.TYPE_GROUP => throw $ IO.userError "Proto groups are unsupported."

-- Returns function of type (α -> ProtoParseM α)
partial def fullDeserFunctionInline(lf: LogicalField) (wiretypeVarName: String) : ProtoGenM String := match lf with
  | LogicalField.map _ k v => do
    let k ← bareDeserFunctionInline k
    let v ← bareDeserFunctionInline v
    let desFn := s!"(LeanProto.EncDec.parseMapEntry ({k} arbitrary) ({v} arbitrary))"
    return s!"(fun s => do let newPair ← {desFn}; return (s.erase newPair.fst).insert newPair.fst newPair.snd)"
  
  | LogicalField.repeated fd => do
      if fd.isPackable then
        s!"(LeanProto.EncDec.parseKeyAndMixedArray ({← bareDeserFunctionInline fd} arbitrary) {wiretypeVarName})"
      else
        s!"(LeanProto.EncDec.parseKeyAndNonPackedArray ({← bareDeserFunctionInline fd} arbitrary) {wiretypeVarName})"
  | LogicalField.optional fd => return s!"(fun s => some <$> ({← bareDeserFunctionInline fd} (s.getD arbitrary)))"
  | LogicalField.nonoptional fd => bareDeserFunctionInline fd
  | LogicalField.oneof lf' od _ => do
    let desFn ← fullDeserFunctionInline lf' wiretypeVarName
    let oneofTypeName ← oneofFullLeanPath od
    let f ← lf'.getFD
    let oneofVariantName := fieldNameToLean f.name
    return s!"(fun s => (some ∘ {oneofTypeName}.{oneofVariantName}) <$> ({desFn} (match s with | some ({oneofTypeName}.{oneofVariantName} q) => q | _ => arbitrary ) ) )"

-- Serialization functions

-- Returns a function (α -> ProtoSerAction)
def bareSerFunctionInline(fd: FieldDescriptorProto) : ProtoGenM String := do
let val ← match fd.type with
| FieldDescriptorProto_Type.TYPE_INT32 => "LeanProto.EncDec.serializeIntAsInt32"
| FieldDescriptorProto_Type.TYPE_INT64 => "LeanProto.EncDec.serializeIntAsInt64"
| FieldDescriptorProto_Type.TYPE_UINT32 => "LeanProto.EncDec.serializeNatAsUInt32"
| FieldDescriptorProto_Type.TYPE_UINT64 => "LeanProto.EncDec.serializeNatAsUInt64"
| FieldDescriptorProto_Type.TYPE_SINT32 => "LeanProto.EncDec.serializeIntAsSInt32"
| FieldDescriptorProto_Type.TYPE_SINT64 => "LeanProto.EncDec.serializeIntAsSInt64"
| FieldDescriptorProto_Type.TYPE_FIXED32 => "LeanProto.EncDec.serializeFixedUInt32"
| FieldDescriptorProto_Type.TYPE_FIXED64 => "LeanProto.EncDec.serializeFixedUInt64"
| FieldDescriptorProto_Type.TYPE_SFIXED32 => "LeanProto.EncDec.serializeIntAsSFixed32"
| FieldDescriptorProto_Type.TYPE_SFIXED64 => "LeanProto.EncDec.serializeIntAsSFixed64"
| FieldDescriptorProto_Type.TYPE_DOUBLE => "LeanProto.EncDec.serializeFloatAsFloat64"
| FieldDescriptorProto_Type.TYPE_FLOAT => "LeanProto.EncDec.serializeFloatAsFloat32"
| FieldDescriptorProto_Type.TYPE_BOOL => "LeanProto.EncDec.serializeBool"
| FieldDescriptorProto_Type.TYPE_STRING => "LeanProto.EncDec.serializeString"
| FieldDescriptorProto_Type.TYPE_BYTES => "LeanProto.EncDec.serializeByteArray"
| FieldDescriptorProto_Type.TYPE_ENUM => "LeanProto.EncDec.serializeEnum"
| FieldDescriptorProto_Type.TYPE_MESSAGE => do
  let e ← ctxFindMessage fd.typeName
  let serFnName ← messageSerFunctionName (← e.unwrap)
  s!"(LeanProto.EncDec.serializeMessage ({serFnName}))"
| FieldDescriptorProto_Type.TYPE_GROUP => throw $ IO.userError "Proto groups are unsupported."

section
open LeanProto.EncDec.WireType

-- Taken from the original protobuf source
def wireTypeForField (fd: FieldDescriptorProto) : Nat := do
  if fd.isPacked then return LengthDelimited.toNat
  let wt := match fd.type with 
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

def wireTypeFromNat (n: Nat) := s!"(LeanProto.EncDec.WireType.ofLit {n} rfl)"

partial def fullSerFunctionInline(lf: LogicalField) (wiretypeVarName: String) (writeIfDefault := false) : ProtoGenM String := do
  match lf with 
  | LogicalField.nonoptional fd => do
    let v ← bareSerFunctionInline fd
    let wt ← wireTypeForField fd
    let body := s!"(LeanProto.EncDec.serializeWithTag ({v}) {wireTypeFromNat wt} {fd.number})"
    if writeIfDefault then body else s!"LeanProto.EncDec.serializeSkipDefault {body}"
  | LogicalField.optional fd => 
    let v ← bareSerFunctionInline fd
    let wt ← wireTypeForField fd
    return s!"LeanProto.EncDec.serializeOpt (LeanProto.EncDec.serializeWithTag ({v}) {wireTypeFromNat wt} {fd.number})"
  | LogicalField.repeated fd =>
    let v ← bareSerFunctionInline fd
    let wt ← wireTypeForField fd
    if fd.isPacked then
      return s!"(LeanProto.EncDec.serializeIfNonempty (LeanProto.EncDec.serializeWithTag (LeanProto.EncDec.serializePackedArray ({v})) {wireTypeFromNat wt} {fd.number}))"
    else
      return s!"(LeanProto.EncDec.serializeUnpackedArrayWithTag ({v}) {wireTypeFromNat wt} {fd.number})"
  | LogicalField.map field k v =>
    let kFn ← bareSerFunctionInline k
    let vFn ← bareSerFunctionInline v  
    let kWt := wireTypeForField k
    let vWt := wireTypeForField v
    return s!"(LeanProto.EncDec.serializeMapWithTag ({kFn}) ({vFn}) {wireTypeFromNat kWt} {wireTypeFromNat vWt} {field.number})"
  | LogicalField.oneof _ o lfs => 
    let oneofTypeName ← oneofFullLeanPath o
    let mut res := "(fun s => match s with\n"
    for lf' in lfs do
      let oneofVariantName := fieldNameToLean lf'.getFD.name
      let serFnInline ← fullSerFunctionInline lf' "_type" true
      res := res ++ s!"    | some ({oneofTypeName}.{oneofVariantName} indVal) => {serFnInline} indVal"
    res := res ++ s!"    | none => ())"
    return res

-- Since enums can't have any depencencies, generate them independently
def generateEnumDeclarations (fd: FileDescriptorProto) : ProtoGenM Unit := do
  let generateEnum (e: EnumDescriptorProto) (baseLeanName: String): ProtoGenM Unit := do
    addLine s!"inductive {baseLeanName} where"
    for v in e.value do 
      addLine s!"| {v.name} : {baseLeanName}"
    let derives := ", ".intercalate enumDerivingList
    addLine s!"deriving {derives}"
    addLine ""
    
    addLine s!"instance : LeanProto.ProtoEnum {baseLeanName} where"
    addLine s!"  toInt"
    for v in e.value do 
      addLine s!"  | {baseLeanName}.{v.name} => {v.number}"
    addLine ""

    -- For ofInt, since it's legal to have protos with identical numbers,
    -- we check that we don't add the same number twice
    let mut seen : Std.RBTreeC Int := {}
    addLine s!"  ofInt"
    for v in e.value do 
      let num := v.number
      if seen.contains num then continue
      seen := seen.insert num
      addLine s!"  | {num} => some {baseLeanName}.{v.name}"
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
    addLine s!"| {fieldNameToLean v.getFD.name} : {type} -> {baseLeanName}"
  addLine ""

def generateMessageDeclaration (p: ASTPath): ProtoGenM Unit := do
  addLine s!"-- Starting {← messageFullLeanPath p}"
  match p.revMessages.head? with
  | none => return
  | some m => do
  let baseLeanName ← messageFullLeanPath p
  let logFields ← getLogicalFields p

  -- first, oneofs:
  for lf in logFields do
    match lf with
    | LogicalField.oneof _ po _ => do         
      generateOneofDeclaration lf (← oneofFullLeanPath po)
      addLine ""
    | _ => continue

  addLine s!"inductive {baseLeanName} where"
  addLine s!"| mk "     
  for lf in logFields do
    let type ← fullFieldTypeName lf
    addLine s!"  ({lf.fieldName} : {type})"
  addLine s!"  : {← messageFullLeanPath p}"
  addLine ""

def generateMessageDeclarations (fd: FileDescriptorProto) : ProtoGenM Unit := do
  let rec processMessage (d: ASTPath) (_: Unit) : ProtoGenM Unit := generateMessageDeclaration d  
  recurseM (← ASTPath.initM fd, ()) (wrapRecurseFn processMessage)

def generateLeanDeriveStatements (fd: FileDescriptorProto) : ProtoGenM Unit := do
  -- First, traverse the tree and build up a list of all typenames of msgs and oneofs
  let WithListState := StateT ((List String) × (List String)) ProtoGenM
  let doOne (p: ASTPath): WithListState Unit := do
    match p.revMessages.head? with
    | none => return
    | some m => do
    let logFields ← getLogicalFields p
    
    let mut oneofState := []
  
    for lf in logFields do 
      match lf with
      | LogicalField.oneof f o _ => oneofState := (← oneofFullLeanPath o) :: oneofState 
      | _ => ()
    
    let curr ← get
    set (((← messageFullLeanPath p) :: curr.fst), (oneofState ++ curr.snd))


  -- Run the first state monad to get the list
  let initPath ← ASTPath.initM fd
  let m := recurseM (μ := WithListState) (initPath, ()) (wrapRecurseFn (fun x _ => doOne x))
  let ⟨ r, ⟨ msgs, oneofs ⟩ ⟩  ← StateT.run m ([], [])
  
  if msgs.length > 0 then
    addLine s!"deriving instance {", ".intercalate messageDerivingList} for {", ".intercalate msgs}"
  if oneofs.length > 0 then
    addLine s!"deriving instance {", ".intercalate oneofDerivingList} for {", ".intercalate oneofs}"
  r

-- TODO: rewrite as proper Lean derives
def generateMessageManualDerives (fd: FileDescriptorProto) : ProtoGenM Unit := do
  let doOne (p: ASTPath): ProtoGenM Unit := do
    match p.revMessages.head? with
    | none => return
    | some m => do
    let messageTypeName ← messageFullLeanPath p
    let logFields ← getLogicalFields p

    -- mkDefault
    addLine s!"def {messageTypeName}.mkDefault : {messageTypeName} := "        
    let mut setVars := ""
    for lf in logFields do
      let newVar ← lf.defaultLean
      setVars := setVars ++ " " ++ newVar
    addLine s!"  {messageTypeName}.mk{setVars}"

    addLine s!"instance : Inhabited {messageTypeName} where "   
    addLine s!"  default := {messageTypeName}.mkDefault"   


    -- Accessors
    -- These are necessary because protos are mutually inductive so we can't make them structures
    for i in [:logFields.size] do
      let lf := logFields.get! i
      let (outputTypeName, origField, isOneof) := match lf with
      | LogicalField.oneof f o _ => ((s!"(Option {·})") <$> oneofFullLeanPath o, f.getFD, true)
      | _ => do let f ← lf.getFD; return (fullFieldTypeName lf, f, false)

      let mut captureVars := ""
      for j in [:logFields.size] do
        captureVars := captureVars ++ s!" v{j}"
      
      -- Getter
      addLine s!"def {messageTypeName}.{lf.fieldName} : {messageTypeName} → {← outputTypeName}"
      addLine s!"| mk{captureVars} => v{i}"
      
      -- Setter
      addLine s!"def {messageTypeName}.set_{lf.fieldName} (orig: {messageTypeName}) (val: {← outputTypeName})"
      addLine s!"  : {messageTypeName} := match orig with"        
      let mut setVars := ""
      for j in [:logFields.size] do
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
    addLine s!"  let (_type, key) ← LeanProto.EncDec.parseKey"
    addLine s!"  match key with"

    let genLine (fd: FieldDescriptorProto) (fieldName: String) (desFn: String) :=
      addLine s!"| {fd.number} => do {messageDeserFunctionName} ({currentVarName}.set_{fieldName} (← {desFn} {currentVarName}.{fieldName}))"

    let logFields ← getLogicalFields p
    for lf in logFields do
      match lf with
      | LogicalField.oneof f o fs => do
        for inner in fs do
          let desFn ← fullDeserFunctionInline (LogicalField.oneof inner o fs) "_type"
          genLine inner.getFD lf.fieldName desFn 
      | _ => do 
        let desFn ← fullDeserFunctionInline lf "_type"
        genLine lf.getFD lf.fieldName desFn 

    addLine s!"| 0 => do throw $ IO.userError \"Decoding message with field number 0\""
    addLine s!"| _ => do let _ ← LeanProto.EncDec.parseUnknown _type; {messageDeserFunctionName} {currentVarName}"
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

    let logFields ← getLogicalFields p

    let mut binderNames := ""
    for idx in [:logFields.size] do
      binderNames := binderNames ++ s!" v{idx}"

    addLine s!"partial def {messageSerFunctionName} : {messageTypeName} -> LeanProto.EncDec.ProtoSerAction"
    addLine s!"| {messageTypeName}.mk{binderNames} => do"
    for idx in [:logFields.size] do
      let lf := logFields.get! idx
      let serFn ← fullSerFunctionInline lf "_type"
      addLine s!"  {serFn} v{idx}"
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
    addLine s!"  serialize x := do"
    addLine s!"    let res := LeanProto.EncDec.serialize ({messageSerFunctionName} x)"
    addLine s!"    LeanProto.EncDec.resultStateToExcept res"
    addLine s!""

    addLine s!"instance : LeanProto.ProtoDeserialize {messageTypeName} where"
    addLine s!"  deserialize b :="
    addLine s!"    let res := LeanProto.EncDec.parse b ({messageDeserFunctionName} ({messageTypeName}.mkDefault))"
    addLine s!"    LeanProto.EncDec.resultToExcept res"
    addLine s!""

  recurseM (← ASTPath.initM fd, ()) (wrapRecurseFn (fun d _ => doOne d))

def generateGRPC (fd: FileDescriptorProto) : ProtoGenM Unit := do
  let getEndpointInfo (md: MethodDescriptorProto) (grpcPrefix: String): ProtoGenM (String × String) := do
    let inputTypeOpt ← ctxFindMessage md.inputType
    let inputType ← messageFullLeanPath (← inputTypeOpt.unwrap)
    let outputTypeOpt ← ctxFindMessage md.outputType
    let outputType ← messageFullLeanPath (← outputTypeOpt.unwrap)

    let grpcName := grpcPrefix ++ md.name
    let fieldName := protoEndpointNameToLean md.name

    let (actionType, inductiveVariant) := match (md.clientStreaming, md.serverStreaming) with
    | (false, false) => ("LeanGRPC.ServerUnaryHandler", "unaryHandler")
    | (false, true) => ("LeanGRPC.ServerServerStreamingHandler", "serverStreamingHandler")
    | (true, false) => ("LeanGRPC.ServerClientStreamingHandler", "clientStreamingHandler")
    | (true, true) => ("LeanGRPC.ServerBiDiStreamingHandler", "biDiStreamingHandler")

    let field := s!"  {fieldName} : {actionType} σ {inputType} {outputType}"
    let handler := s!"    (\"{grpcName}\", LeanGRPC.ServerHandler.mk _ inferInstance _ inferInstance (LeanGRPC.ServerHandlerAction.{inductiveVariant} {fieldName}))"
    return (field, handler)

  let generateService (sd: ServiceDescriptorProto) : ProtoGenM Unit := do
    let grpcPrefix := "/" ++ fd.package ++ "." ++ sd.name ++ "/"    
    let endpointInfo ← sd.method.mapM (getEndpointInfo · grpcPrefix)
    let className := protoServiceNameToLean sd.name

    addLine s!"class {className} (σ) extends LeanGRPC.Service σ where"
    for ⟨field, _⟩ in endpointInfo do addLine field

    addLine s!"  handlers_ := Std.rbmapOf ["
    let lines ← endpointInfo.mapM (fun ⟨_, entry⟩ => entry)
    addLine $ ",\n".intercalate lines.toList
    addLine s!"  ] Ord.compare"
    addLine ""

  let _ ← fd.service.mapM generateService
  

    
    

    
import Std.Data.RBMap
import LeanProtocPlugin.Helpers
import LeanProtocPlugin.Google.Protobuf.Compiler.Plugin
import LeanProtocPlugin.Google.Protobuf.Descriptor

open LeanProtocPlugin.Google.Protobuf.Compiler
open LeanProtocPlugin.Google.Protobuf


structure ASTPath where
  protected mk ::
  file : FileDescriptorProto
  revMessages : List DescriptorProto -- reverse-ordered path to this message, revMessages.hd being the actual message
  leanModule : String
  leanName : String
  protoFullPath : String
  deriving BEq, Inhabited

structure ProtoGenContext where
  namespacePrefix : String
  fileProtoMap : Std.RBMapC String FileDescriptorProto
  currentFile : FileDescriptorProto
  -- These are keyed by protobuf-fully-qualified-name
  enumDescriptorMap : Std.RBMapC String (ASTPath × EnumDescriptorProto)
  oneofDescriptorMap : Std.RBMapC String (ASTPath × OneofDescriptorProto)
  messageDescriptorMap : Std.RBMapC String ASTPath

structure ProtoGenState where
  lines : Array String := #[]

abbrev ProtoGenM := ReaderT ProtoGenContext $ StateT ProtoGenState $ IO

def addLine (l: String) : ProtoGenM Unit := do let s ← get; set $ ProtoGenState.mk $ s.lines.push l
def addLines (l: Array String) : ProtoGenM Unit := do let s ← get; set $ ProtoGenState.mk $ s.lines++l


def Option.unwrap (x: Option α) : ProtoGenM α := match x with
  | some v => v
  | none => throw $ IO.userError "Tried unwrapping Option that is none"

def ctxFindEnum (s: String) : ProtoGenM $ Option (ASTPath × EnumDescriptorProto) := do
  let ctx ← read; return ctx.enumDescriptorMap.find? s
def ctxFindOneof (s: String) : ProtoGenM $ Option (ASTPath × OneofDescriptorProto) := do
  let ctx ← read; return ctx.oneofDescriptorMap.find? s
def ctxFindMessage (s: String) : ProtoGenM $ Option ASTPath := do
  let ctx ← read; return ctx.messageDescriptorMap.find? s

def ASTPath.init (file: FileDescriptorProto) (rootPackage: String): ASTPath :=
  { file := file,
    revMessages := [],
    leanModule := rootPackage ++ "." ++ protoPackageToLeanPackagePrefix file.package,
    leanName := "",
    protoFullPath := "." ++ file.package}

def ASTPath.initM (file: FileDescriptorProto): ProtoGenM ASTPath := do
  return ASTPath.init file (← read).namespacePrefix

def ASTPath.addMessage (m: ASTPath) (m2 : DescriptorProto) : ASTPath :=
  let newMessageName := protoMessageNameToLean m2.name
  let newPathChunk := if m.leanName == "" then newMessageName else "_" ++ newMessageName
  { m with
    file := m.file,
    revMessages := m2 :: m.revMessages,
    leanName := m.leanName ++ newPathChunk,
    protoFullPath := m.protoFullPath ++ "." ++ m2.name }

partial def recurseM [Monad μ] (curr: α) (action: α -> μ (List α)) : μ PUnit := do
  let children ← action curr
  children.forM fun nested => recurseM nested action

def isMapEntry (d: DescriptorProto) := ((DescriptorProto.options d).getD arbitrary).mapEntry == false

def wrapRecurseFn [Monad μ] (fn: ASTPath -> σ -> μ σ) (withMaps := false) (new: (ASTPath × σ)) : μ (List (ASTPath × σ)) := do
    let r ← fn new.fst new.snd;
    let children := match new.fst.revMessages.head? with
      | some m => 
        let nested := if withMaps then m.nestedType else m.nestedType.filter isMapEntry
        nested.toList
      | none => new.fst.file.messageType.toList
    return children.map (fun v => (new.fst.addMessage v, r))
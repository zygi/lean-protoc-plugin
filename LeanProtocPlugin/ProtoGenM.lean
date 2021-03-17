import Std.Data.RBMap
import LeanProtocPlugin.Helpers
import LeanProtocPlugin.google.protobuf.compiler.plugin
import LeanProtocPlugin.google.protobuf.descriptor

open google.protobuf.compiler
open google.protobuf



structure ASTPath where
  protected mk ::
  file : FileDescriptorProto
  revMessages : List DescriptorProto -- reverse-ordered path to this message, revMessages.hd being the actual message
  leanModule : String
  leanName : String
  protoFullPath : String
  deriving Repr, Inhabited

structure ProtoGenContext where
  namespacePrefix : String
  fileProtoMap : Std.RBMapC String FileDescriptorProto
  currentFile : FileDescriptorProto
  -- These are keyed by protobuf-fully-qualified-name
  enumDescriptorMap : Std.RBMapC String (ASTPath × EnumDescriptorProto)
  oneofDescriptorMap : Std.RBMapC String (ASTPath × OneofDescriptorProto)
  messageDescriptorMap : Std.RBMapC String ASTPath
  deriving Repr

structure ProtoGenState where
  lines : Array String := #[]

abbrev ProtoGenM := ReaderT ProtoGenContext $ StateT ProtoGenState $ IO

def addLine (l: String) : ProtoGenM Unit := do let s ← get; set $ ProtoGenState.mk $ s.lines.push l
def addLines (l: Array String) : ProtoGenM Unit := do let s ← get; set $ ProtoGenState.mk $ s.lines++l


def EStateM.Result.unwrap (r: EStateM.Result IO.Error σ α) : IO α := match r with
| Result.ok r _ => pure r
| Result.error e _ => throw e

def Except.unwrap (x: Except IO.Error α) : IO α := match x with
  | Except.ok v => v
  | Except.error e => throw e

def Option.unwrap (x: Option α) : ProtoGenM α := match x with
  | some v => v
  | none => throw $ IO.userError "Tried unwrapping Option that is none"

def ctxFindEnum (s: String) : ProtoGenM $ Option (ASTPath × EnumDescriptorProto) := do
  let ctx ← read; return ctx.enumDescriptorMap.find? s
def ctxFindOneof (s: String) : ProtoGenM $ Option (ASTPath × OneofDescriptorProto) := do
  let ctx ← read; return ctx.oneofDescriptorMap.find? s
def ctxFindMessage (s: String) : ProtoGenM $ Option ASTPath := do
  let ctx ← read; return ctx.messageDescriptorMap.find? s

def ASTPath.init (file: FileDescriptorProto) : ASTPath :=
  { file := file,
    revMessages := [],
    leanModule := protoPackageToLeanPackagePrefix file.package.get!,
    leanName := "",
    protoFullPath := "." ++ file.package.getI}

def ASTPath.addMessage (m: ASTPath) (m2 : DescriptorProto) : ASTPath :=
  let newMessageName := protoMessageNameToLean m2.name.get!
  let newPathChunk := if m.leanName == "" then newMessageName else "_" ++ newMessageName
  { m with
    file := m.file,
    revMessages := m2 :: m.revMessages,
    leanName := m.leanName ++ newPathChunk,
    protoFullPath := m.protoFullPath ++ "." ++ m2.name.get! }

-- Careful around these -- unless I provide the full type params via @ I get
-- stack overflows
partial def recurseM [Monad μ] (curr: α) (action: α -> μ (List α)) : μ PUnit := do
  let children ← action curr
  children.forM fun nested => recurseM nested action

def wrapRecurseFn [Monad μ] (fn: ASTPath -> σ -> μ σ) (new: (ASTPath × σ)) : μ (List (ASTPath × σ)) := do
    let r ← fn new.fst new.snd;
    let children := match new.fst.revMessages.head? with
      | some m => m.nestedType.toList
      | none => new.fst.file.messageType.toList
    return children.map (fun v => (new.fst.addMessage v, r))
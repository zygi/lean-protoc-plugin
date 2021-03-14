import LeanProtocPlugin.google.protobuf.compiler.plugin
import LeanProtocPlugin.google.protobuf.descriptor

import LeanProto
import LeanProtocPlugin.Helpers

import Lean.Elab.Term
import Lean.Hygiene
import Init.System.IO

open google.protobuf.compiler
open google.protobuf
open IO (FS.Stream)

open Lean
open Lean.Elab.Term

def decodeToIO (x: EStateM.Result IO.Error σ α) : IO α := match x with
  | EStateM.Result.ok v _ => v
  | EStateM.Result.error e _ => throw e


def decodeExToIO (x: Except IO.Error α) : IO α := match x with
  | Except.ok v => v
  | Except.error e => throw e

partial def IO.FS.Stream.readBinToEnd [Monad m] [MonadLiftT IO m] (h : FS.Stream) : m ByteArray := do
  let rec loop (acc : ByteArray) : m ByteArray := do
    if ← h.isEof then
      return acc
    else
      let buf ← h.read 1024
      loop (acc ++ buf)
  loop ByteArray.empty

def defaultImports : Unhygienic Syntax := `(
  import LeanProto
  import Std.Data.AssocList
  import Lean.Elab.Deriving.InhabitedMut
)

def importDeps (f: FileDescriptorProto) (namespacePrefix: String) : Unhygienic Syntax := do
  let paths := f.dependency.map (filePathToPackage ∘ fun s => namespacePrefix ++ "." ++ s)
  let idents := paths.map (mkIdent ·)
  let importCommands ← idents.mapM (fun ident => `(import $ident:ident))
  -- `( $importCommands:«import»* )
  `( constant A : Int := 5 )

def doWork (file: FileDescriptorProto) : CodeGeneratorResponse_File := do
  return arbitrary

def main(argv: List String): IO UInt32 := do
  let i ← IO.getStdin
  let o ← IO.getStdout
  
  let bytes ← i.readBinToEnd
  let request ← decodeExToIO $ LeanProto.ProtoDeserialize.deserialize (α:=CodeGeneratorRequest) bytes

  let namespacePrefix := request.parameter.get!

  IO.println s!"{reprStr request}"

  let response := LeanProto.ProtoSerialize.serialize (arbitrary (α := CodeGeneratorResponse))
  o.write $ response

  return 0
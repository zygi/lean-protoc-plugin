import AssertCmd
import Std.Data.RBTree
import Std.Data.RBMap
import Init.System.IO

instance : MonadLift (Except ε) (EIO ε) :=
  ⟨fun e => match e with | Except.ok r => pure r | Except.error e => fun s => EStateM.Result.error e s ⟩

def String.toCamelCase (s: String) (firstCap : Bool) : String := do
  let mut newString := ""
  let mut sawUnderscoreBefore := false
  for idx in [:s.length] do
    if s[idx] == '_' then
      sawUnderscoreBefore := true
      continue
    if s[idx].isLower && (sawUnderscoreBefore || (idx == 0 && firstCap)) then
      newString := newString.push s[idx].toUpper
    else
      newString := newString.push s[idx]
    
    sawUnderscoreBefore := false
  return newString

#assert (String.toCamelCase "_one_two__three_" true) == "OneTwoThree"
#assert (String.toCamelCase "one_two__three_" true) == "OneTwoThree"
#assert (String.toCamelCase "one_two__three_" false) == "oneTwoThree"

def rbtreeOfC [LT α] [(a b : α) → Decidable (a < b)] (a: Array α) :=
  Std.rbtreeOf a.toList (fun x y => decide $ LT.lt x y)

def rbmapOfC [LT α] [(a b : α) → Decidable (a < b)] (a: Array (α × β)) :=
  Std.rbmapOf a.toList (fun x y => decide $ LT.lt x y)

abbrev Std.RBMapC (α β) [LT α] [(a b : α) → Decidable (a < b)] := Std.RBMap α β (fun x y => decide $ LT.lt x y)
abbrev Std.RBTreeC (α) [LT α] [(a b : α) → Decidable (a < b)] := Std.RBTree α (fun x y => decide $ LT.lt x y)

def String.stripFileEnding (s: String) := (s.dropRightWhile (· != '.')).dropRight 1

def packageNamePieceToLean (s: String) := (s.toCamelCase true)
def fileTopNameToLean (s: String) := (s.toCamelCase true)
def protoMessageNameToLean (s: String) := (s.toCamelCase true)
def protoEnumNameToLean (s: String) := (s.toCamelCase true)
def protoOneofNameToLean (s: String) := (s.toCamelCase true) ++ "Oneof"
def protoEndpointNameToLean (s: String) := (s.toCamelCase true)
def protoServiceNameToLean (s: String) := (s.toCamelCase false) ++ "Service"

def reservedNames := rbtreeOfC #["syntax", "begin", "end", "structure", "inductive", "class", "where"]

def fieldNameToLean (s: String) :=
  let mapped := (s.toCamelCase false)
  if reservedNames.contains mapped then "_"++mapped else mapped

def filePathPartsToLeanPackageParts (x: List String) := 
  let rec work : List String -> List String
    | List.nil => List.nil
    | s :: List.nil => fileTopNameToLean s :: List.nil
    | s :: xs => packageNamePieceToLean s :: work xs 
  work x

def filePathToPackage (s: String) : String := do
  let f := filePathPartsToLeanPackageParts $ s.stripFileEnding.splitOn "/"
  ".".intercalate f

#assert (filePathToPackage "google/protobuf/compiler/plugin.proto") == "Google.Protobuf.Compiler.Plugin"

def protoPackageToLeanPackagePrefix (s: String) : String :=
  let trimmed := if s.get 0 == '.' then s.drop 1 else s 
  trimmed.splitOn "." |> (·.map packageNamePieceToLean) |> (".".intercalate ·)


partial def IO.FS.Stream.readBinToEnd [Monad m] [MonadLiftT IO m] (h : IO.FS.Stream) : m ByteArray := do
  let rec loop (acc : ByteArray) : m ByteArray := do
    if ← h.isEof then
      return acc
    else
      let buf ← h.read 1024
      loop (acc ++ buf)
  loop ByteArray.empty
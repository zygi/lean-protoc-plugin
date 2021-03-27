import ConformanceProto

import LeanProto
import Init.System.IO

open ConformanceProto.Conformance
open ConformanceProto.ProtobufTestMessages.Proto3
open LeanProto.ProtoSerialize
open LeanProto.ProtoDeserialize

instance : MonadLift (Except ε) (EIO ε) :=
  ⟨fun e => match e with | Except.ok r => pure r | Except.error e => fun s => EStateM.Result.error e s ⟩

def decodeToIO (x: EStateM.Result IO.Error σ α) : IO α := match x with
  | EStateM.Result.ok v _ => v
  | EStateM.Result.error e _ => throw e

def mkParseError (x: String) := ConformanceResponse.mk $ ConformanceResponse_ResultOneof.parseError x
def mkSkipped (x: String) := ConformanceResponse.mk $ ConformanceResponse_ResultOneof.skipped x
def mkSerializeError (x: String) := ConformanceResponse.mk $ ConformanceResponse_ResultOneof.serializeError x
def mkRuntimeError (x: String) := ConformanceResponse.mk $ ConformanceResponse_ResultOneof.runtimeError x

def doWork (i: ConformanceRequest) : IO ConformanceResponse := do
  try
    let mt := i.messageType

    if mt == "conformance.FailureSet" then
      let fs := FailureSet.mk #[]
      let res ← serialize fs
      return ConformanceResponse.mk
        (ConformanceResponse_ResultOneof.protobufPayload $ res)

    if mt != "protobuf_test_messages.proto3.TestAllTypesProto3" then
      return mkSkipped "only doing proto3 for now"

    if i.requestedOutputFormat != WireFormat.PROTOBUF then
      return (mkSkipped s!"Unsupported output format")
      
    let payload ← do match i.payload with
        | ConformanceRequest_PayloadOneof.protobufPayload x => x
        | _ => return mkSkipped s!"Unsupported payload type"

    let inp : TestAllTypesProto3 ← do try
        deserialize payload (α := TestAllTypesProto3)
      catch e =>
        return mkParseError s!"{e}"
    
    let res ← try
      serialize inp
    catch e =>
      return mkSerializeError s!"Serialize error {e}"

    -- Not part of the conformance test but check that ser ∘ deser ≈ id
    -- if (inp != (← decodeExToIO $ deserialize res (α := TestAllTypesProto3))) then
    --   return mkRuntimeError s!"Runtime error, received request \"{LeanProto.Utils.byteArrayToHex payload}\", decoded: \"{LeanProto.Utils.byteArrayToHex res}\""    

    pure $ ConformanceResponse.mk
      (ConformanceResponse_ResultOneof.protobufPayload res)
  catch e =>
    return mkRuntimeError s!"Runtime error {e}"
  
partial def Loop (i o: IO.FS.Stream) : IO Unit := do
  if (← i.isEof) then
    return

  let sizeBytes ← i.read 4

  if sizeBytes.size == 0 then
    -- Empirically, the binary exists through this. But why? We should have been at Eof and should
    -- have returned above.
    return

  let size ← decodeToIO $ LeanProto.EncDec.parse sizeBytes LeanProto.EncDec.parseFixedUInt32
  
  let bytes ← i.read size.toUSize
  let request ← deserialize (α:=ConformanceRequest) bytes
  let response ← doWork request
  let serResponse ← LeanProto.ProtoSerialize.serialize response

  let sizePrefix ← LeanProto.EncDec.resultStateToExcept (LeanProto.EncDec.serialize $ LeanProto.EncDec.serializeFixedUInt32 serResponse.size.toUInt32)
  o.write $ sizePrefix
  o.write (← serialize response)
  o.flush
  Loop i o


def main(args: List String) : IO UInt32 := do
  let i ← IO.getStdin
  let o ← IO.getStdout
  Loop i o
  return 0
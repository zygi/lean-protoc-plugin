import ConformanceProto

import LeanProto
import Init.System.IO

open ConformanceProto.Conformance
open ConformanceProto.ProtobufTestMessages.Proto3
open LeanProto.ProtoSerialize
open LeanProto.ProtoDeserialize

def decodeToIO (x: EStateM.Result IO.Error σ α) : IO α := match x with
  | EStateM.Result.ok v _ => v
  | EStateM.Result.error e _ => throw e
  
def decodeExToIO (x: Except IO.Error α) : IO α := match x with
  | Except.ok v => v
  | Except.error e => throw e

def mkParseError (x: String) := ConformanceResponse.mk $ ConformanceResponse_ResultOneof.parseError x
def mkSkipped (x: String) := ConformanceResponse.mk $ ConformanceResponse_ResultOneof.skipped x
def mkSerializeError (x: String) := ConformanceResponse.mk $ ConformanceResponse_ResultOneof.serializeError x
def mkRuntimeError (x: String) := ConformanceResponse.mk $ ConformanceResponse_ResultOneof.runtimeError x

def doWork (i: ConformanceRequest) : IO ConformanceResponse := do
  let mt := i.messageType
  -- IO.eprintln mt

  if mt == "conformance.FailureSet" then
    let fs := FailureSet.mk #[]
    return ConformanceResponse.mk
      (ConformanceResponse_ResultOneof.protobufPayload $ serialize fs)

  if mt != "protobuf_test_messages.proto3.TestAllTypesProto3" then
    return mkSkipped "only doing proto3 for now"

  if i.requestedOutputFormat != WireFormat.PROTOBUF then
    return (mkSkipped s!"Unsupported output format")
    
  let payload ← do match i.payload with
      | ConformanceRequest_PayloadOneof.protobufPayload x => x
      | _ => return mkSkipped s!"Unsupported payload type"

  let inp : TestAllTypesProto3 ← do try
      decodeExToIO $ deserialize payload (α := TestAllTypesProto3)
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
  
partial def Loop (i o: IO.FS.Stream) : IO Unit := do
  -- IO.eprintln "Starting iteration"
  if (← i.isEof) then 
    IO.eprintln "Got EOF"
    return
  -- IO.eprintln s!"as size"
  let sizeBytes ← i.read 4
  -- IO.eprintln s!"Received sizeBytes {sizeBytes}"
  -- IO.eprintln s!"Received sizeBytes {LeanProto.Utils.byteArrayToHex sizeBytes}"
  let size ← decodeToIO (LeanProto.EncDec.decode sizeBytes LeanProto.EncDec.decodeFixedUInt32)
  -- IO.eprintln s!"Received size {size}"
  let bytes ← i.read size.toUSize
  -- IO.eprintln s!"{LeanProto.Utils.byteArrayToHex bytes}"
  let request ← decodeExToIO $ deserialize (α:=ConformanceRequest) bytes
  -- IO.eprintln s!"Decoded request"
  let response ← doWork request
  let serResponse := LeanProto.ProtoSerialize.serialize response
  -- IO.eprintln s!"Outputting size {serResponse.size.toUInt32}"
  o.write $ (LeanProto.EncDec.encodeFixedUInt32 (ByteArray.mkEmpty 0) serResponse.size.toUInt32)
  -- IO.eprintln s!"Outputting {LeanProto.Utils.byteArrayToHex $ serialize response}"
  o.write $ serialize response
  o.flush
  Loop i o


def main(args: List String) : IO UInt32 := do
  let i ← IO.getStdin
  let o ← IO.getStdout
  Loop i o
  return 0
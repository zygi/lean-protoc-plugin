## Protocol buffers in Lean 4

A third-party protocol buffer code generator for Lean 4. 

### Current state
Currently the generated code assumes proto3 semantics. Code for protocol buffers defined as proto2
is generated as though they were proto3. This means there's no distinction between optional-unset
fields and fields containing default values.

What works:
- Proto3 codegen, mostly passes the conformance suite
- Default values

What doesn't work yet:
- Reflection
- Textproto
- Full proto2 support
- `{...}` notation for modifying records: because protos can be mutually inductive, they are not structs and don't support the extra conveniences Lean has for structs.
- Well-known protos
- Trying to serialize protos with Int fields holding values bigger than int64_t is UB 

So far little attention has been paid to performance. It's not awful but there are a few low hanging
fruit:
- When deserializing, remove unnecessary ByteArray copies and replace them with SubByteArray when that's
  in the stdlib (e.g. [here](https://github.com/zygi/lean-proto/blob/fe6dfe2ccc8c07c09b54dac21a672aea90b200c8/LeanProto.lean#L576))
- Add native implementations of some bit-fiddling functions
- Sprinkle some `inline` and `specialize` annotations
- Change from `StateT` to `StateRefT`

### How to use
Because LeanProto currently depends on some external native code, using it is only supported from Nix.
This package exports Nix build rules that make it easy to package generated code:

```nix
  {
    inputs.leanprotocplugin.url = "path:../";
    # ...
    outputs = { ... }: {
      # Import the build rules. Note that generated packages will depend on leanproto -- the runtime
      # library -- so if you want to pin the leanproto version, pass the pinned version here.
      leanProtoPackageLib = import "${leanprotocplugin}/leanProtoPackage.nix"
        { inherit pkgs system leanPkgs leanproto; generator = leanprotocplugin.packages.${system}; };

      # The root path to your proto definitions
      protoSrc = ./proto;

      # Package configuration
      protoPkgParams = {
        inpPathsStrList = [
          "${protoSrc}/definitions.proto"
        ];
        inpRootStr = "${protoSrc}";
        # The generated file package path will be prefixed by `rootPackageNameStr`. In this case,
        # the generated structure will be 
        # .
        # ├── Proto.lean
        # └── Proto/
        #     └── Definitions.lean
        # 
        # where Proto.lean just pulls in all the deeper files.
        rootPackageNameStr = "Proto";
      };

      # This line defines a Lean package of the generated code. You can use it in your Lean package's
      # deps. 
      protoLeanPkg = leanProtoPackageLib.leanProtoPackage protoPkgParams;

      # This line defines a target that produces generated source code. Useful for debugging,
      # if the generated code isn't building.
      protoLeanSrc = leanProtoPackageLib.runGenerator protoPkgParams;

    };
  }
```

For a full (though not super simple) example, see [the conformance test runner's flake.nix](lean-proto-conformance-test/flake.nix).


### Generated code reference
- Message fields are of type `Option a`
- Other singular fields are just `a`
- Repeated fields are `Vector a`
- Maps are `AssocList a`. When Lean has derive functions for Hash or HasLt, this will be changed to a more performant map-like container.
- Enums are enumerated types.
- Oneofs are generated as inductive types. `n` fields in a message belonging to a single oneof become one field, the oneof, represented as `Option <generatedOneofType>`, that is an inductive type with `n` variants.
- Message types have
  - accessors `<MessageType>.<fieldName> : <MessageType> -> <FieldType>`
  - setters `<MessageType>.set_<fieldName> : <MessageType> -> <FieldType> -> <MessageType>`
  - `Inhabited` instances that respect the default values of protobuf fields

The interfaces: 
```lean
class LeanProto.ProtoEnum (α: Type u) where
  toInt : α -> Int
  ofInt : Int -> Option α

class LeanProto.ProtoSerialize (α: Type u) where
  serialize : α -> Except IO.Error ByteArray
  serializeToHex : α -> Except IO.Error String := fun a => Utils.byteArrayToHex <$> (serialize a)

class LeanProto.ProtoDeserialize (α: Type u) where
  deserialize : ByteArray -> Except IO.Error α
  deserializeFromHex : String -> Except IO.Error α := fun x => do
    match Utils.hexToByteArray x with
    | none => Except.error $ IO.userError "Failed to parse hex string"
    | some val => deserialize val 
```

Generated code example: 
```lean
-- Enums
inductive WireFormat where
| UNSPECIFIED : WireFormat
| PROTOBUF : WireFormat
| JSON : WireFormat
| JSPB : WireFormat
| TEXT_FORMAT : WireFormat
deriving Repr, Inhabited, BEq

instance : LeanProto.ProtoEnum WireFormat where
  toInt := ...
  ofInt := ...

-- Oneofs 
inductive ConformanceRequest_PayloadOneof where
| protobufPayload : (ByteArray) -> ConformanceRequest_PayloadOneof
| jsonPayload : (String) -> ConformanceRequest_PayloadOneof
| jspbPayload : (String) -> ConformanceRequest_PayloadOneof
| textPayload : (String) -> ConformanceRequest_PayloadOneof

-- Messages 
inductive ConformanceRequest where
| mk 
  (payload : (Option (ConformanceRequest_PayloadOneof)))
  (requestedOutputFormat : (WireFormat))
  (messageType : (String))
  (testCategory : (TestCategory))
  (jspbEncodingOptions : (Option (JspbEncodingConfig)))
  (printUnknownFields : (Bool))
  : ConformanceRequest
```

`.proto` files in this package are taken from https://github.com/protobuf/protobuf.

This is not a Google product.

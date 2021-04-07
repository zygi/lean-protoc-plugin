{
  description = "LeanProto conformance test";

  # Note: when running this, Lean takes an impractically long time to derive Repr instances for 
  # the conformance test protos so I comment out "Repr" from `messageDerivingList` in 
  # LeanProtocPlugin/Logic.lean. TODO: make `messageDerivingList` a parameter users can provide
  # to the plugin.

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;

  inputs.lean.url = github:leanprover/lean4;
  inputs.flake-utils.url = github:numtide/flake-utils;

  inputs.leanproto.url = github:zygi/lean-proto;
  inputs.leanproto.inputs.lean.follows = "lean";

  inputs.leanprotocplugin.url = "path:../";
  inputs.leanprotocplugin.inputs.lean.follows = "lean";

  outputs = { self, lean, flake-utils, leanproto, nixpkgs, leanprotocplugin }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
      };
      leanPkgs = lean.packages.${system};

      leanProtoPackageLib = import "${leanprotocplugin}/leanProtoPackage.nix"
        { inherit pkgs system leanPkgs leanproto; generator = leanprotocplugin.packages.${system}; };
      src = ./proto;

      protoPkgParams = {
        inpPathsStrList = [
          "${src}/google/protobuf/test_messages_proto3.proto"
          "${src}/google/protobuf/any.proto"
          "${src}/google/protobuf/duration.proto"
          "${src}/google/protobuf/field_mask.proto"
          "${src}/google/protobuf/struct.proto"
          "${src}/google/protobuf/timestamp.proto"
          "${src}/google/protobuf/wrappers.proto"
          "${src}/conformance.proto"
        ];
        inpRootStr = "${src}";
        rootPackageNameStr = "ConformanceProto";
      };
      conformanceProtoPkg = leanProtoPackageLib.leanProtoPackage protoPkgParams;
      confSrc = leanProtoPackageLib.runGenerator protoPkgParams;

      # Looks like the conformance test runner isn't in the default nix protobuf package
      conformanceTestRunner = pkgs.lib.overrideDerivation pkgs.protobuf (params: {
        postBuild = ''
          cd conformance && make test_cpp && cd .
        '';
      });

      pkg = leanPkgs.buildLeanPackage {
        name = "LeanProtoConformanceTest"; # must match the name of the top-level .lean file
        src = ./.;
        deps = [ leanproto.packages.${system} lean.packages.${system}.Lean conformanceProtoPkg ];
      };

      runConformanceTest =
        let enforceRecommended = true;
        in
        pkgs.writeShellScriptBin "runconformancetest" ''
          LD_LIBRARY_PATH=${pkgs.protobuf}/lib ${conformanceTestRunner}/bin/conformance-test-runner \
              --failure_list ${./.}/failing_tests.txt \
              ${if enforceRecommended then "--enforce_recommended " else ""}\
              ${pkg.executable}/bin/leanprotoconformancetest
        '';
    in
    {
      packages = {
        inherit confSrc runConformanceTest;
      };
      defaultPackage = runConformanceTest;
    });
}


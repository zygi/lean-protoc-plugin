{
  description = "LeanProto test";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;

  inputs.lean.url = "path:../../lean4";
  inputs.flake-utils.url = github:numtide/flake-utils;
  
  inputs.leanproto.url = "path:../../leanproto";
  inputs.leanproto.inputs.lean.follows = "lean";

  inputs.leanprotocplugin.url = "path:../";
  inputs.leanprotocplugin.inputs.lean.follows = "lean";

  inputs.leanShell.url = "path:../../lean-nix-helpers/leanShell.nix";
  inputs.leanShell.flake = false;

  outputs = { self, lean, flake-utils, leanproto, leanShell, nixpkgs, leanprotocplugin}: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
      };
      leanPkgs = lean.packages.${system};

      leanProtoPackageLib = import "${leanprotocplugin}/leanProtoPackage.nix"
        { inherit pkgs system leanPkgs leanproto; generator = leanprotocplugin.packages.${system}; };  
      src = ./proto;    

      protoPkgParams = {
        inpPathsStrList = ["${src}/google/protobuf/test_messages_proto3.proto"
        "${src}/google/protobuf/any.proto"
        "${src}/google/protobuf/duration.proto"
        "${src}/google/protobuf/field_mask.proto"
        "${src}/google/protobuf/struct.proto"
        "${src}/google/protobuf/timestamp.proto"
        "${src}/google/protobuf/wrappers.proto" "${src}/conformance.proto"] ;
         inpRootStr = "${src}"; 
         rootPackageNameStr = "ConformanceProto";
        };
      conformanceProtoPkg = leanProtoPackageLib.leanProtoPackage protoPkgParams;
      confSrc = leanProtoPackageLib.runGenerator protoPkgParams;         

      conformanceTestRunner = pkgs.lib.overrideDerivation pkgs.protobuf (params: {
        postBuild = ''
          cd conformance && make test_cpp && cd .
        '';
      });

      pkg = leanPkgs.buildLeanPackage {
        name = "LeanProtoTest";  # must match the name of the top-level .lean file
        src = ./.;
        deps = [leanproto.packages.${system} lean.packages.${system}.Lean conformanceProtoPkg];
        # pluginDeps = [leanproto-native.packages.${system}.sharedLib];
      };

      runConformanceTest = pkgs.stdenv.mkDerivation rec {
        inherit (system);
        name = "run-conformance-test";
        nativeBuildDeps = [pkgs.protobuf];
        enforceRecommended = false;
        buildCommand = ''
          echo "Required.Proto3.ProtobufInput.UnknownVarint.ProtobufOutput" > ./failing_tests.txt
          # I still don't understand how nix infers shared libs :(
          LD_LIBRARY_PATH=${pkgs.protobuf}/lib ${conformanceTestRunner}/bin/conformance-test-runner \
            --failure_list ${if enforceRecommended then "--enforce_recommended " else ""}\
            ./failing_tests.txt ${pkg.executable}/bin/leanprototest
        '';
      };
    in {
      packages = pkg // {
        inherit (leanPkgs) lean;
        eee = conformanceProtoPkg.modRoot;
        inherit confSrc;
        # conf = confqq;
        inherit conformanceTestRunner;
        pb = pkgs.protobuf;
        inherit runConformanceTest;
      };

      defaultPackage = pkg.modRoot;


      devShell = (import leanShell { inherit pkgs leanPkgs; nix = leanPkgs.nix; leanPkg = pkg; }).shell;
    });
}


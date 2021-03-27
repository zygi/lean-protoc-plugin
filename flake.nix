{
  description = "LeanProtocPlugin";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;

  inputs.lean.url = github:leanprover/lean4;
  inputs.flake-utils.url = github:numtide/flake-utils;

  inputs.leanproto.url = github:zygi/lean-proto;
  inputs.leanproto.inputs.lean.follows = "lean";

  inputs.assrt-command.url = github:pnwamk/lean4-assert-command;
  inputs.assrt-command.inputs.lean.follows = "lean";

  outputs = { self, lean, flake-utils, leanproto, assrt-command, nixpkgs }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
      };
      leanPkgs = lean.packages.${system};

      pkg = leanPkgs.buildLeanPackage {
        name = "LeanProtocPlugin"; # must match the name of the top-level .lean file
        src = ./.;
        deps = [ leanproto.packages.${system} assrt-command.packages.${system} ];
      };

      # For easier testing, set up derivations that will build some proto files.
      leanProtoPackageLib = import ./leanProtoPackage.nix
        { inherit pkgs system leanPkgs leanproto; generator = pkg; };
      protoGenParams = {
        inpPathsStrList = [
          "${pkg.src}/proto/google/protobuf/descriptor.proto"
          "${pkg.src}/proto/google/protobuf/compiler/plugin.proto"
        ];
        inpRootStr = "${pkg.src}/proto";
        rootPackageNameStr = "LeanProtocPlugin";
      };
      runGeneratorPkg = leanProtoPackageLib.leanProtoPackage protoGenParams;
      runGenerator = leanProtoPackageLib.runGenerator protoGenParams;
    in
    {
      packages = pkg // {
        inherit (leanPkgs) lean runGeneratorPkg runGenerator;
      };

      defaultPackage = pkg.modRoot;
    });
}


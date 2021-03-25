{
  description = "LeanProtocPlugin";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;

  inputs.leanShell.url = "path:../lean-nix-helpers/leanShell.nix";
  inputs.leanShell.flake = false;

  inputs.lean.url = "path:../lean4";
  inputs.flake-utils.url = github:numtide/flake-utils;

  inputs.leanproto.url = "path:../leanproto";
  inputs.leanproto.inputs.lean.follows = "lean";

  inputs.assrt-command.url = github:pnwamk/lean4-assert-command;
  inputs.assrt-command.inputs.lean.follows = "lean";

  outputs = { self, lean, flake-utils, leanproto, assrt-command, nixpkgs, leanShell, nix }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
      };
      leanPkgs = lean.packages.${system};
      pkg = leanPkgs.buildLeanPackage {
        name = "LeanProtocPlugin"; # must match the name of the top-level .lean file
        src = ./.;
        deps = [ leanproto.packages.${system} assrt-command.packages.${system} ];
        # pluginDeps = [leanproto-native.packages.${system}.sharedLib];
      };



      leanProtoPackageLib = import ./leanProtoPackage.nix { inherit pkgs system leanPkgs leanproto; generator = pkg; };
      runGeneratorPkg = (pkgs.lib.traceValSeqN 2 leanProtoPackageLib).leanProtoPackage 
        {inpPathsStrList = [ "${pkg.src}/proto/google/protobuf/descriptor.proto" "${pkg.src}/proto/google/protobuf/compiler/plugin.proto" ];
         inpRootStr = "${pkg.src}/proto"; 
         rootPackageNameStr = "Generated";
        };
    in
    {
      packages = pkg // {
        inherit (leanPkgs) lean;

        runGenerator = leanProtoPackageLib.runGenerator {inpPathsStrList = [ "${pkg.src}/proto/google/protobuf/descriptor.proto" "${pkg.src}/proto/google/protobuf/compiler/plugin.proto" ];
         inpRootStr = "${pkg.src}/proto"; 
         rootPackageNameStr = "Generated";
        };
        print-lean-deps = leanPkgs.print-lean-deps;
        pkgMR = runGeneratorPkg.modRoot;
        pb = pkgs.protobuf;
      };

      defaultPackage = pkg.modRoot;

      devShell = (import leanShell { inherit pkgs leanPkgs; nix = leanPkgs.nix; leanPkg = pkg; }).shell;
    });
}


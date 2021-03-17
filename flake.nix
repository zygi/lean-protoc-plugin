{
  description = "LeanProtocPlugin";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;

  inputs.leanrulewrapper.url = "path:../lean-nix-helpers/leanRuleWrapper.nix";
  inputs.leanrulewrapper.flake = false;

  inputs.leanShell.url = "path:../lean-nix-helpers/leanShell.nix";
  inputs.leanShell.flake = false;

  inputs.lean.url = "path:../lean4";
  inputs.flake-utils.url = github:numtide/flake-utils;

  inputs.leanproto.url = "path:../leanproto";
  inputs.leanproto.inputs.lean.follows = "lean";

  inputs.assrt-command.url = github:pnwamk/lean4-assert-command;
  inputs.assrt-command.inputs.lean.follows = "lean";

  outputs = { self, lean, flake-utils, leanproto, assrt-command, nixpkgs, leanrulewrapper, leanShell, nix}: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
      };
      leanPkgs = lean.packages.${system};
      pkg = leanPkgs.buildLeanPackage {
        name = "LeanProtocPlugin";  # must match the name of the top-level .lean file
        src = ./.;
        deps = [leanproto.packages.${system} assrt-command.packages.${system}];
        # pluginDeps = [leanproto-native.packages.${system}.sharedLib];
      };
      runGenerator = inpPathsStrList: inpRootStr: namespaceStr: pluginPathStr: pkgs.stdenv.mkDerivation {
        inherit system;
        name = "protoc-lean";
        buildCommand = ''
          mkdir -p $out/${namespaceStr}
          ln -s ${pkgs.lib.traceVal pluginPathStr} ./protoc-gen-lean 
          ${pkgs.protobuf}/bin/protoc --plugin=protoc-gen-lean -I${inpRootStr} --lean_out=$out/${namespaceStr} \
            --lean_opt=${namespaceStr}  ${pkgs.lib.concatStringsSep " " inpPathsStrList}
        '';
      };
    in {
      packages = pkg // {
        inherit (leanPkgs) lean;

        runGenerator = runGenerator ["${pkg.src}/proto/google/protobuf/descriptor.proto"] 
          "${pkg.src}/proto" "Generated" "${pkg.executable}/bin/${pkg.executable.name}";
        print-lean-deps = leanPkgs.print-lean-deps;
      } // ((import leanrulewrapper) { inherit pkg system nixpkgs; nix = leanPkgs.nix; });

      defaultPackage = pkg.modRoot;

      devShell = import leanShell { inherit pkgs leanPkgs; nix = leanPkgs.nix; leanPkg = pkg; };
    });
}


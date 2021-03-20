{ pkgs ? import <nixpkgs> { }, leanPkgs, generator, leanproto, system}:
with pkgs;
let runGenerator = inpPathsStrList: inpRootStr: rootPackageNameStr: pkgs.stdenv.mkDerivation {
        inherit (system);
        name = "protoc-lean";
        buildCommand = ''
          mkdir -p $out/${rootPackageNameStr}
          ln -s ${generator.executable}/bin/leanprotocplugin ./protoc-gen-lean 
          ${pkgs.protobuf}/bin/protoc --plugin=protoc-gen-lean -I${inpRootStr} --lean_out=$out \
            --lean_opt=${rootPackageNameStr}  ${pkgs.lib.concatStringsSep " " inpPathsStrList}
        '';
      };
leanProtoPackage = inpPathsStrList: inpRootStr: rootPackageNameStr: leanPkgs.buildLeanPackage {
        name = rootPackageNameStr;  # must match the name of the top-level .lean file
        src = runGenerator inpPathsStrList inpRootStr rootPackageNameStr;
        deps = [leanproto.packages.${system}];
      };
in {
    inherit runGenerator leanProtoPackage;
}
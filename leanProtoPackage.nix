{ pkgs ? import <nixpkgs> { }, leanPkgs, generator, leanproto, system, leangrpc ? false}:
with pkgs;
let runGenerator = {inpPathsStrList, inpRootStr, rootPackageNameStr, genGRPC ? false}:
        assert lib.assertMsg ((genGRPC && (builtins.isAttrs leangrpc)) || (!genGRPC && !leangrpc))
          "Generating gRPC services; must pass leangrpc as argument";        
        pkgs.stdenv.mkDerivation {
          inherit (system);
          name = "protoc-lean";
          buildCommand = ''
            mkdir -p $out/${rootPackageNameStr}
            ln -s ${generator.executable}/bin/leanprotocplugin ./protoc-gen-lean 
            ${pkgs.protobuf}/bin/protoc --plugin=protoc-gen-lean -I${inpRootStr} --lean_out=$out \
              --lean_opt=${rootPackageNameStr}  ${pkgs.lib.concatStringsSep " " inpPathsStrList}
          '';
      };
leanProtoPackage = {inpPathsStrList, inpRootStr, rootPackageNameStr, genGRPC ? false}@args: 
        assert lib.assertMsg ((genGRPC && (builtins.isAttrs leangrpc)) || (!genGRPC && !leangrpc))
          "Generating gRPC services; must pass leangrpc as argument"; 
        leanPkgs.buildLeanPackage {
          name = rootPackageNameStr;  # must match the name of the top-level .lean file
          src = runGenerator args;
          deps = [leanproto.packages.${system}] ++ (if genGRPC then [leangrpc.packages.${system}] else []);
        };
in {
    inherit runGenerator leanProtoPackage;
}
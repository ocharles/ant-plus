{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytes, bytestring, digest, stdenv
      , tagged, text, usb, vector
      }:
      mkDerivation {
        pname = "ant-plus";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base bytes bytestring digest tagged text usb vector
        ];
        executableHaskellDepends = [ base bytestring usb vector ];
        homepage = "https://github.com/ocharles/ant-plus";
        description = "Haskell bindings to ANT+ devices";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv

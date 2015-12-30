{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytes, bytestring, digest, stdenv
      , tagged, text, usb, vector, managed, async, stm
      }:
      mkDerivation {
        pname = "ant-plus";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base bytes bytestring digest tagged text usb vector async stm
        ];
        executableHaskellDepends = [ base bytestring usb vector managed ];
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

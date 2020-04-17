{ mkDerivation, attoparsec, base, bytestring, h2decays, hpack
, optparse-generic, pipes, process, stdenv, transformers
}:
mkDerivation {
  pname = "h2pythia";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    attoparsec base bytestring h2decays optparse-generic pipes process
    transformers
  ];
  prePatch = "hpack";
  homepage = "https://github.com/cbpark/h2pythia#readme";
  description = "Generating input files to run Pythia 8 for the heavy Higgs processes in the 2HDM";
  license = stdenv.lib.licenses.gpl3;
}

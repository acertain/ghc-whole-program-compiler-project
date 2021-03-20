{ mkDerivation, aeson, ansi-wl-pprint, base, binary, bytestring
, containers, directory, exceptions, filemanip, filepath, hashable
, lib, optparse-applicative, primitive, serialise, text
, transformers, unordered-containers, zip
}:
mkDerivation {
  pname = "external-stg";
  version = "0.1.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint base binary bytestring containers directory
    filemanip filepath hashable text transformers unordered-containers
    zip
  ];
  executableHaskellDepends = [
    aeson ansi-wl-pprint base bytestring containers directory
    exceptions filepath optparse-applicative primitive serialise text
    zip
  ];
  description = "A library to dump GHC's STG representation";
  license = lib.licenses.bsd3;
}

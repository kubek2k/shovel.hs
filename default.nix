{ mkDerivation, amazonka, amazonka-sqs, base, lens
, optparse-applicative, stdenv, text
}:
mkDerivation {
  pname = "shovel-hs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    amazonka amazonka-sqs base lens optparse-applicative text
  ];
  license = stdenv.lib.licenses.mit;
}

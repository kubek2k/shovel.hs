{ mkDerivation, base, stdenv, amazonka, amazonka-sqs, lens, text}:
mkDerivation {
  pname = "shovel-hs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base amazonka amazonka-sqs lens text ];
  license = stdenv.lib.licenses.mit;
}

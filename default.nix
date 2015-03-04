{ mkDerivation, aeson, base, bytestring, conduit-extra
, http-conduit, lens, lens-aeson, mtl, persistent-redis, regex-pcre
, shakespeare, stdenv, text, transformers, vector, wreq, yesod
, yesod-auth, yesod-auth-oauth2
}:
mkDerivation {
  pname = "approvd";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring conduit-extra http-conduit lens lens-aeson
    mtl persistent-redis regex-pcre shakespeare text transformers
    vector wreq yesod yesod-auth yesod-auth-oauth2
  ];
  homepage = "https://github.com/madjar/approvd";
  license = stdenv.lib.licenses.unfree;
}

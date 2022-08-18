{ lib }: let

  inherit (lib.libreg)
    packumenter
    packumentClosure
    importManifestNpm
  ;

  descs = [
    "@babel/core@^7.18.9"
    "@babel/plugin-proposal-class-properties@^7.18.6"
    "@babel/preset-env@^7.18.9"
    "@babel/preset-typescript@^7.18.6"
    "@types/jest@^28.1.6"
    "babel-loader@^8.2.2"
    "typescript@^4.7.4"
    "webpack@^4.35.3"
    "webpack-cli@^3.3.5"
  ];

in ( builtins.foldl' ( p: p ) packumenter descs ).packuments

{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
}:
with nixpkgs;
let
  reflexPlatformSrc = fetchGit {
    url = https://github.com/reflex-frp/reflex-platform;
    rev = "716879f16d53c93766e7ed9af17416fccb2edfe1";
  };

  reflex-platform = import reflexPlatformSrc {};
in
reflex-platform.project({ pkgs, ... }: {
  packages = {
    reflex-chartjs    = ./reflex-chartjs;
    reflex-jexcel     = ./relfex-jexcel;
    reflex-jsoneditor = ./reflex-jsoneditor;
    reflex-codemirror = ./reflex-codemirror;
    reflex-select2    = ./reflex-select2;
  };

  shells = {
    ghcjs = [ "reflex-chartjs"
              "reflex-jexcel"
              "reflex-jsoneditor"
              "reflex-codemirror"
              "reflex-select2"
            ];
  };
})


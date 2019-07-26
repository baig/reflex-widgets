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
    reflex-chartjs      = ./reflex-chartjs;
    reflex-jexcel       = ./reflex-jexcel;
    reflex-jsoneditor   = ./reflex-jsoneditor;
    reflex-codemirror   = ./reflex-codemirror;
    reflex-jdenticon    = ./reflex-jdenticon;
    reflex-select2      = ./reflex-select2;
    reflex-fileapi      = ./reflex-fileapi;
    reflex-mdl          = ./reflex-mdl;
    reflex-tensorflowjs = ./reflex-tensorflowjs;
    reflex-utils        = ./reflex-utils;
  };

  shells = {
    ghcjs = [ "reflex-chartjs"
              "reflex-jexcel"
              "reflex-jsoneditor"
              "reflex-codemirror"
              "reflex-select2"
              "reflex-jdenticon"
              "reflex-utils"
              "reflex-fileapi"
              "reflex-tensorflowjs"
            ];
  };
})


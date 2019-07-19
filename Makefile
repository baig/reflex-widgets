NIX_FLAGS=--cores 0 -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz

chartjs:
	nix-build ${NIX_FLAGS} -A ghcjs.reflex-chartjs

codemirror:
	nix-build ${NIX_FLAGS} -A ghcjs.reflex-codemirror

jsoneditor:
	nix-build ${NIX_FLAGS} -A ghcjs.reflex-jsoneditor

jexcel:
	nix-build ${NIX_FLAGS} -A ghcjs.reflex-jexcel

fileapi:
	nix-build ${NIX_FLAGS} -A ghcjs.reflex-fileapi

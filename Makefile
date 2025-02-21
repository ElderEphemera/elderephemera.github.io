site:
	nix-build -A site

test:
	nix-build -A site --arg checkNoDrafts false

ghcid:
	nix-shell builder --run "cd builder; ghcid -c 'cabal repl'"

.PHONY: site test ghcid

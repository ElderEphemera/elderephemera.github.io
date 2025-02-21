site:
	nix-build -A site

test:
	nix-build -A site --arg checkNoDrafts false

ghcid:
	nix-shell builder --run "cd builder; ghcid -c 'cabal repl'"

host:
	nix-shell -p busybox --run 'httpd -f -v -p 8080 -h result'

.PHONY: site test ghcid host

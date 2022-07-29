site:
	nix-build -A site

test:
	nix-build -A site --arg checkNoDrafts false

.PHONY: site test

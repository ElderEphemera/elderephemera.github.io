site:
	nix-build -A site

test:
	nix-build -A site --arg checkAll false

.PHONY: site test

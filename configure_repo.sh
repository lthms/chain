#!/bin/sh

echo "[*] Setting up git hooks"
rm -f .git/hooks/pre-commit
ln -sf ../../hooks/pre-commit .git/hooks/pre-commit

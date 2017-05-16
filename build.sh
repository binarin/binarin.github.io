#!/usr/bin/env bash
set -euo pipefail

git worktree prune
if ! git worktree list | grep -q .git/publish-wt; then
    git worktree add .git/publish-wt master
fi

echo ======================================================================
cabal build
echo ======================================================================
./dist/build/site/site build

cd .git/publish-wt
git ls-files | xargs rm -f

echo ======================================================================
rsync -va ../../_site/ . | sed '0,/^$/d'

git add .
git commit -m "Updated on $(date -R) from $(git -C ../../ rev-parse --short HEAD)"

#!/usr/bin/env bash

PROJECT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../" && pwd )"
cd "$PROJECT_DIR"

echo "Rebuilding.."
ghc site.hs > /dev/null
./site rebuild > /dev/null
echo "..Done"

echo "Deploying.."
rm -rf _deploy/*
cp -r _site/* _deploy/
cd _deploy

git add .
git commit -am "Updated site @ $(date +"%F %T")"  > /dev/null
git push origin gh-pages  > /dev/null
echo "..Done"

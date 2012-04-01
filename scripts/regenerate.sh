#!/usr/bin/env bash

PROJECT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../" && pwd )"

cd $PROJECT_DIR

ghc --make $PROJECT_DIR/site.hs
$PROJECT_DIR/site rebuild
$PROJECT_DIR/scripts/cleanUrls.sh

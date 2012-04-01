#!/usr/bin/env bash

PROJECT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../" && pwd )"
SITE_DIR=${1:-$("mktemp" -d /tmp/_site.XXXXXXXXX)}

cd $PROJECT_DIR

$PROJECT_DIR/site rebuild
$PROJECT_DIR/scripts/cleanUrls.sh
mv _site/* $SITE_DIR/

#!/usr/bin/env bash

project_root=$(git rev-parse --show-toplevel)
export GIT_DEPLOY_DIR=$project_root/_site

$project_root/scripts/deploy.sh && \
    git push origin master && \
    git push origin gh-pages

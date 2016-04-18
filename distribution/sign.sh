#!/usr/bin/env bash

if [ "$TRAVIS_BRANCH" = 'develop' ] && [ "$TRAVIS_PULL_REQUEST" == 'false' ]; then
    echo $SIGNING_KEY
    echo $SIGNING_KEY | base64 --decode > distribution/signingkey.asc
    gpg --fast-import cd/signingkey.asc
fi
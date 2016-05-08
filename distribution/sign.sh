#!/usr/bin/env bash

if [ "$TRAVIS_BRANCH" = 'develop' ] && [ "$TRAVIS_PULL_REQUEST" == 'false' ]; then
    openssl aes-256-cbc -K $ENCPRYPTED_KEY -iv $ENCPRYPTED_IV -in distribution/signingkey.asc.enc -out distribution/signingkey.asc -d
    gpg --fast-import distribution/signingkey.asc
fi
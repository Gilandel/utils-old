#!/usr/bin/env bash

if [ "$TRAVIS_BRANCH" = 'develop' ] && [ "$TRAVIS_PULL_REQUEST" == 'false' ]; then
    openssl aes-256-cbc -K $ENCPRYPTED_KEY -iv $ENCPRYPTED_IV -in distribution/signingkey.gpg.enc -out distribution/signingkey.gpg -d
    gpg --fast-import distribution/signingkey.gpg
fi
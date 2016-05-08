#!/usr/bin/env bash

if [ "$TRAVIS_BRANCH" = 'develop' ] && [ "$TRAVIS_PULL_REQUEST" == 'false' ]; then
	mkdir -p target/gpg
    openssl aes-256-cbc -K $ENCPRYPTED_KEY -iv $ENCPRYPTED_IV -in distribution/signingkey.asc.enc -out target/gpg/signingkey.asc -d
    gpg --fast-import target/gpg/signingkey.asc
fi
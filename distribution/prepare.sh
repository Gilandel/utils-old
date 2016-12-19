#!/usr/bin/env bash

if [ "$TRAVIS_BRANCH" = 'master' ] && [ "$TRAVIS_PULL_REQUEST" == 'false' ]; then
	echo "Prepare signing for SNAPSHOT"
	mkdir -p target/gpg
    openssl aes-256-cbc -K $ENCPRYPTED_KEY -iv $ENCPRYPTED_IV -in distribution/signingkey.asc.enc -out target/gpg/signingkey.asc -d
    gpg --batch --fast-import target/gpg/signingkey.asc
elif [ "${#TRAVIS_TAG}" -gt 0 ]; then
	echo "Prepare signing for RELEASE"
	mkdir -p target/gpg
    openssl aes-256-cbc -K $ENCPRYPTED_KEY -iv $ENCPRYPTED_IV -in distribution/signingkey.asc.enc -out target/gpg/signingkey.asc -d
    gpg --batch --fast-import target/gpg/signingkey.asc
else
	echo "No signature required"
fi

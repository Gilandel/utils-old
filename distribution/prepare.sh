#!/usr/bin/env bash

if [ "$TRAVIS_PULL_REQUEST" == 'false' ]; then
	if [ "$TRAVIS_BRANCH" = 'master' ] || [ "$TRAVIS_BRANCH" = 'release' ]; then
	    echo "Prepare signing for SNAPSHOT"
	    
	    # Decrypt GPG used to sign outputs
	    mkdir -p target/gpg
	    openssl aes-256-cbc -K $ENCPRYPTED_KEY -iv $ENCPRYPTED_IV -in distribution/signingkey.asc.enc -out target/gpg/signingkey.asc -d
	    gpg --batch --fast-import target/gpg/signingkey.asc
	    
	    # Decrypt SSH key so we can push release to GitHub
	    openssl aes-256-cbc -K $ENCPRYPTED_KEY -iv $ENCPRYPTED_IV -in distribution/pushingkey.enc -out ${HOME}/.ssh/id_rsa -d
		chmod 600 ${HOME}/.ssh/id_rsa
	fi
else
    echo "No signature required"
fi

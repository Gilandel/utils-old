#!/usr/bin/env bash

if [ "$TRAVIS_BRANCH" = 'master' ] && [ "$TRAVIS_PULL_REQUEST" = 'false' ]; then
	echo "Build and deploy SNAPSHOT"
	mvn deploy -DskipTests=true -P sign,build-extras --settings distribution/settings.xml
elif [ "$TRAVIS_BRANCH" = "$CURRENT_TAG" ]; then
	echo "Build and deploy RELEASE"
	mvn release:prepare release:perform -DskipTests=true -P sign,build-extras --settings distribution/settings.xml
else
	echo "Only build"
fi

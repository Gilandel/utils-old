#!/usr/bin/env bash

if [ "$TRAVIS_BRANCH" = 'develop' ] && [ "$TRAVIS_PULL_REQUEST" == 'false' ]; then
	mvn deploy -DskipTests=true -P sign,build-extras --settings distribution/settings.xml
fi
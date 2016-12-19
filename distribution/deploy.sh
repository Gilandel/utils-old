#!/usr/bin/env bash

if [ "$TRAVIS_BRANCH" = 'master' ] && [ "$TRAVIS_PULL_REQUEST" = 'false' ]; then
	if [ "${#TRAVIS_TAG}" -gt 0 ]; then
		mvn release:prepare release:perform -DskipTests=true -P sign,build-extras --settings distribution/settings.xml
	else
		mvn deploy -DskipTests=true -P sign,build-extras --settings distribution/settings.xml
	fi
fi

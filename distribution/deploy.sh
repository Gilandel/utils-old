#!/usr/bin/env bash

if [ "$TRAVIS_BRANCH" = 'master' ] && [ "$TRAVIS_PULL_REQUEST" = 'false' ]; then
	echo "Build and deploy SNAPSHOT"
	mvn deploy -DskipTests=true -P sign,build-extras --settings distribution/settings.xml
	exit $?
elif [ "${#TRAVIS_TAG}" -gt 0 ]; then
	export MAVEN_SETTINGS=distribution/settings.xml
	echo "Build and deploy RELEASE"
	git config --global user.email "gilles@landel.fr"
	git config --global user.name "Gilles Landel"
	mvn release:prepare release:perform --batch-mode -DcheckModificationExcludeList=distribution/prepare.sh,distribution/deploy.sh -DskipTests=true -P sign,build-extras --settings distribution/settings.xml
	exit $?
else
	echo "Only build"
fi

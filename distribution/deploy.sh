#!/usr/bin/env bash

if [ "$TRAVIS_BRANCH" = 'master' ] && [ "$TRAVIS_PULL_REQUEST" = 'false' ]; then
	echo "Build and deploy SNAPSHOT"
	mvn deploy -DskipTests=true -P sign,build-extras --settings distribution/settings.xml
	exit $?
elif [ "${#TRAVIS_TAG}" -gt 0 ]; then
	echo "Build and deploy RELEASE"
	git config --global user.email "$GIT_EMAIL"
	git config --global user.name "$GIT_USER"
	git rebase
	mvn release:prepare release:perform --batch-mode -DcheckModificationExcludeList=distribution/prepare.sh,distribution/deploy.sh -DskipTests=true -P sign,build-extras --settings distribution/settings.xml
	exit $?
else
	echo "Only build"
fi

#!/usr/bin/env bash

if [ "$TRAVIS_BRANCH" = 'master' ] && [ "$TRAVIS_PULL_REQUEST" = 'false' ]; then
	if [ "$RELEASE" = "true" ]; then
		echo "Build and deploy RELEASE"
		git config --global user.email "$GIT_EMAIL"
		git config --global user.name "$GIT_USER"
		mvn release:prepare release:perform --batch-mode -DcheckModificationExcludeList=distribution/prepare.sh,distribution/deploy.sh -DskipTests=true -P sign,build-extras --settings distribution/settings.xml
		exit $?
	else
		echo "Build and deploy SNAPSHOT"
		mvn deploy -DskipTests=true -P sign,build-extras --settings distribution/settings.xml
		exit $?
	fi
else
	echo "Only build"
fi

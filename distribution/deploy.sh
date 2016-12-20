#!/usr/bin/env bash

if [ "$TRAVIS_BRANCH" = 'master' ] && [ "$TRAVIS_PULL_REQUEST" = 'false' ]; then
    echo "Build and deploy SNAPSHOT"
    mvn deploy -DskipTests=true -P sign,build-extras --settings distribution/settings.xml
    exit $?
elif [ "${#TRAVIS_TAG}" -gt 0 ]; then
    echo "Prepare RELEASE $TRAVIS_TAG"
    git config --global user.email "$GIT_EMAIL"
    git config --global user.name "$GIT_USER"
    mvn release:clean release:prepare --batch-mode -DskipTests=true -P sign,build-extras --settings distribution/settings.xml
    echo "Perform RELEASE $TRAVIS_TAG"
    git tag
    mvn release:perform --batch-mode -DskipTests=true -P sign,build-extras --settings distribution/settings.xml
    exit $?
else
    echo "Only build"
fi

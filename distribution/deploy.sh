#!/usr/bin/env bash

if [ "$TRAVIS_BRANCH" = 'master' ] && [ "$TRAVIS_PULL_REQUEST" = 'false' ]; then
    echo "Build and deploy SNAPSHOT"
    
    mvn deploy -DskipTests=true -P sign,build-extras --settings distribution/settings.xml
elif [ "$TRAVIS_BRANCH" = 'release' ]; then
    echo "Prepare and perform RELEASE"
    
    git config --global user.email "$GIT_EMAIL"
    git config --global user.name "$GIT_USER"
    
    git checkout release
    
    mvn release:clean release:prepare release:perform -X -B -DskipTests=true -P sign,build-extras --settings distribution/settings.xml
    
    git fetch origin +master:master
    git checkout master
    git merge release
    git push origin master
else
    echo "Only build"
fi

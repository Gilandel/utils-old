#!/usr/bin/env bash

if [ "$TRAVIS_BRANCH" = 'master' ] && [ "$TRAVIS_PULL_REQUEST" = 'false' ]; then
    echo "Build and deploy SNAPSHOT"
    
    mvn deploy -DskipTests=true -P sign,build-extras --settings distribution/settings.xml
elif [ "$TRAVIS_BRANCH" = 'release' ]; then
    echo "Prepare and perform RELEASE"
    
    git config --global user.email "$GIT_EMAIL"
    git config --global user.name "$GIT_USER"
    
    # Travis checkout the commit as detached head (which is normally what we
	# want) but maven release plugin does not like working in detached head
	# mode. This might be a problem if other commits have already been pushed
	# to the release branch, but in that case we will have problem anyway.
    git checkout release
    
    # Prepare and release
    mvn release:clean release:prepare release:perform -B -DskipTests=true -P sign,build-extras --settings distribution/settings.xml
    
    # Merge the release branch with master
    git fetch origin +master:master
    git checkout master
    git merge release
    git push origin master
else
    echo "Only build"
fi

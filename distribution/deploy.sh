#!/usr/bin/env bash

set -ev

GPG_HOME=${HOME}/.gnupg
DISTRIBUTION_HOME=${HOME}/build/Gilandel/utils/distribution
MVN_SETTINGS=${DISTRIBUTION_HOME}/settings.xml

mkdir -p ${GPG_HOME}
cp ${DISTRIBUTION_HOME}/*.gpg ${GPG_HOME}

if [ "$TRAVIS_BRANCH" = 'master' ] && [ "$TRAVIS_PULL_REQUEST" = 'false' ]; then
	echo "Build and deploy SNAPSHOT"
	
	mvn deploy -DskipTests=true -P sign,build-extras --settings ${MVN_SETTINGS}
elif [ "$TRAVIS_BRANCH" = 'release' ]; then
	if [[ `git log --format=%B -n 1` == *"[maven-release-plugin]"* ]]; then
		echo "Do not release commits created by maven release plugin"
	else
		echo "Prepare and perform RELEASE"
		
		# Decrypt SSH key so we can push release to GitHub
		openssl aes-256-cbc -K $ENCPRYPTED_KEY -iv $ENCPRYPTED_IV -in distribution/pushingkey.enc -out ${HOME}/.ssh/id_rsa -d
		chmod 600 ${HOME}/.ssh/id_rsa
		
		git config --global user.email "$GIT_EMAIL"
		git config --global user.name "$GIT_USER"
		
		# Travis checkout the commit as detached head (which is normally what we
		# want) but maven release plugin does not like working in detached head
		# mode. This might be a problem if other commits have already been pushed
		# to the release branch, but in that case we will have problem anyway.
		git checkout release
		
		# Prepare and release
		mvn release:clean release:prepare -B -DskipTests=true -P sign,build-extras --settings ${MVN_SETTINGS} -Darguments="--settings ${MVN_SETTINGS}"
		mvn release:perform -B -DskipTests=true -P sign,build-extras --settings ${MVN_SETTINGS} -Darguments="--settings ${MVN_SETTINGS}"
		
		# Merge the release branch with master
		git fetch origin +master:master
		git checkout master
		git merge release
		git push origin master
	fi
else
	echo "Only build"
fi

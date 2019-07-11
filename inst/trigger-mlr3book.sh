echo "TRAVIS_BRANCH=$TRAVIS_BRANCH TRAVIS_PULL_REQUEST=$TRAVIS_PULL_REQUEST"
if [[ ($TRAVIS_BRANCH == master) &&
      ($TRAVIS_PULL_REQUEST == false) ]] ; then
  curl -LO --retry 3 https://raw.github.com/mernst/plume-lib/master/bin/trigger-travis.sh
  sh trigger-travis.sh mlr-org mlr3book $TRAVIS_ACCESS_TOKEN "trigger from mlr3"
fi

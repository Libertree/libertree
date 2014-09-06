#!/bin/bash

git stash
for dir in libertree-client-rb libertree-frontend-ramaze libertree-backend-rb libertree-model-rb; do
    echo "updating subtree: $dir"
    git subtree pull --squash --prefix=$dir https://github.com/Libertree/$dir master
done
git stash pop

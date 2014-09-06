#!/bin/bash

if [[ -z $1 ]]; then
    for THEME in $(find themes/ -maxdepth 1 -mindepth 1 -type d -printf '%f '); do
        echo "building theme \`$THEME'"
        bundle exec "sass --update themes/$THEME/scss:themes/$THEME/css"
    done
else
    if [[ ! -d "themes/$1/scss" ]]; then
        echo "The source directory \`themes/$1/scss' does not exist."
        exit 1
    fi

    if [[ ! -d "themes/$1/css" ]]; then
        echo "The target directory \`themes/$1/css' does not exist."
        exit 1
    fi

    bundle exec "sass --update themes/$1/scss:themes/$1/css"
fi


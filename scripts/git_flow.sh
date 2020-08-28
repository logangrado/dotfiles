#!/bin/bash

function gfm () {
    if [ ! -n "$1" ]; then
        echo "Please specify a branch to merge into"
        return
    fi
    
    branch=$(git branch | sed -n -e 's/^\* \(.*\)/\1/p')

    if [[ "$branch" == "$1" ]]; then
        echo "Cannot merge branch $branch into itself"
        return
    fi
    
    echo "Merging $branch into $1"
    
    git checkout $1
    git merge --no-ff --no-edit $branch
    git branch -d $branch
}

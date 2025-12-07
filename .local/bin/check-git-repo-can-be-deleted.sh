#!/usr/bin/bash

git status --short

git clean -ndX

git clean -nd

git stash list

git log --branches --not --remotes --simplify-by-decoration --decorate --oneline

git for-each-ref --format='%(refname:short) %(upstream:short)' refs/heads | while read branch upstream; do
    if [ -z "$upstream" ]; then
        echo "No upstream configured for branch $branch"
        continue
    fi

    counts=$(git rev-list --left-right --count "$branch...$upstream")
    local_ahead=$(echo $counts | awk '{print $1}')
    remote_ahead=$(echo $counts | awk '{print $2}')
    if [ "$local_ahead" -eq 0 ] && [ "$remote_ahead" -eq 0 ]; then
        # All good, don't output anything
        continue
    elif [ "$local_ahead" -gt 0 ] && [ "$remote_ahead" -eq 0 ]; then
        echo "$branch is ahead of $upstream by $local_ahead commit(s)"
    elif [ "$local_ahead" -eq 0 ] && [ "$remote_ahead" -gt 0 ]; then
        echo "$branch is behind $upstream by $remote_ahead commit(s)"
    else
        echo "$branch has diverged from $upstream (Ahead: $local_ahead, Behind: $remote_ahead)"
    fi
done

#!/bin/bash

POLL_TIME_SECS=1
FIND_WATCHED_FILES=$1
CMD=$2

function watched_files_md5sum {
    MD5SUM=$(bash -c "$FIND_WATCHED_FILES" | xargs cat | md5sum)
    echo $MD5SUM
}

#CUR_MD5=$(watched_files_md5sum)
NEW_MD5=$(watched_files_md5sum)

I=0

while(true)
do
    if [[ $CUR_MD5 != $NEW_MD5 ]]
    then
        ((I++))
        echo "$(date) watched files changed running >>> $CMD <<<"
        $CMD
    fi
    CUR_MD5=$NEW_MD5
    NEW_MD5=$(watched_files_md5sum)
    sleep $POLL_TIME_SECS
done

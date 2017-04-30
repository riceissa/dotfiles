#!/bin/bash

d=$(date -Isecond)
if p=$(mocp -Q "%file	%song	%album	%artist	%ct" 2>/dev/null); then
    echo "$d	$p" >> ~/.mocp-playlog
fi

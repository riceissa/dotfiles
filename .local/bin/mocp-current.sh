#!/bin/bash

d=$(date -Isecond)
echo -n "$d	" >> ~/.mocp-playlog
p=$(mocp -Q "%file	%song	%album	%artist	%ct")
echo -n "$p" >> ~/.mocp-playlog
echo "" >> ~/.mocp-playlog

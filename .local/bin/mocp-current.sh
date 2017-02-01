#!/bin/bash

d=$(date -Isecond)
echo -n "$d	" >> ~/.mocp-playlog
p=$(mocp -Q "%file	%title	%artist	%artist	%ct")
echo -n "$p" >> ~/.mocp-playlog
echo "" >> ~/.mocp-playlog

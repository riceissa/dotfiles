#!/bin/bash

d=$(date -Isecond)
echo -n "$d	" >> ~/.mocp-playlog
mocp -Q "%file	%title	%artist	%artist	%ct" >> ~/.mocp-playlog

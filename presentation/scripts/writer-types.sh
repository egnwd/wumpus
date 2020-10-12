#!/bin/bash
git show state:src/Wumpus.hs\
  | grep -e "::" \
  | grep -e "\[String\]" \
  | sed 's/\[String\]/([String], ())/g'\
  | sed -E 's/(\[String\]), (.*)\)/|\\mylib{\1}|, |\\mylibo{\2}|)/g'

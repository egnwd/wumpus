#!/bin/bash
git show state:src/Wumpus.hs\
  | tail -n +74\
  | head -n 14\
  | sed -E "s/ (: logs'*)/| \\\\mylib{\1}|/g"\
  | grep -ve "^\s*--"

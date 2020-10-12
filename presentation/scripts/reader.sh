#!/bin/bash
git show vanilla:src/Wumpus.hs\
  | tail -n +29\
  | head -n 22\
  | sed 's/ wc /` \\mylib{wc} `/g'\
  | sed 's/ wc/` \\mylib{wc} `/g'\
  | grep -ve "^\s*--"

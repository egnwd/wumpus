#!/bin/bash
git show vanilla:src/Wumpus.hs\
  | grep -e "::" \
  | grep -e "WorldConfig" \
  | sed -E 's/(WorldConfig) -> (.*)$/|\\mylib{\1}| -> |\\mylibo{\2}|/g'

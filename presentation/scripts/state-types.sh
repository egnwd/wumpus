#!/bin/bash
git show reader:src/Wumpus.hs\
  | grep -e "::" \
  | grep -e "GameState" \
  | sed 's/m GameState/m (GameState, ())/g'\
  | sed -E  's/(GameState)([^\(]*)\((GameState), (.*)\)$/|\\mylib{\1}|\2(|\\mylib{\3}, \\mylibo{\4}|)/g'\
  | grep "mylib"

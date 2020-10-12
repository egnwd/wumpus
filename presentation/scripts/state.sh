#!/bin/bash
git show reader:src/Wumpus.hs\
  | tail -n +35\
  | head -n 24\
  | sed -E "s/([ \(])(gs'*)/\`\1\\\\mylib{\2}\`/g"\
  | grep -ve "^\s*--"

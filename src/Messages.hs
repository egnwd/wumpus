module Messages
  ( youAreInCave
  , tunnelsLeadTo
  , moveOrShoot
  , moveOrShootInstr
  , whereTo
  , whereToInstr
  , senseWumpus
  , senseBats
  , sensePits
  , loseWumpus
  , losePits
  , loseArrows
  , encounterWumpus
  , encounterBats
  , winWumpus
  , missed
  , win
  , lose
  ) where

import Data.List

youAreInCave cave = "YOU ARE IN CAVE " ++ show cave ++ "!!"

tunnelsLeadTo tunnels = "TUNNELS LEAD TO " ++ intercalate " " (map show tunnels)

moveOrShoot = "MOVE OR SHOOT? "
moveOrShootInstr = "YOU MAY MOVE (m) OR SHOOT (s)"

whereTo = "WHERE TO? "
whereToInstr tunnels = "NOT POSSIBLE -\n" ++ tunnelsLeadTo tunnels

senseWumpus = "I SMELL A WUMPUS!"
senseBats   = "BATS NEARBY!"
sensePits   = "I FEEL A DRAFT!"

loseWumpus = "TSK TSK TSK - WUMPUS GOT YOU!"
losePits   = "YYYIIIIEEEE . . . FELL IN PIT!"
loseArrows = "AGH! OUT OF ARROWS!"

encounterWumpus = "....OOPS! I BUMPED A WUMPUS!"
encounterBats = "ZAP--SUPER BAT SNATCH!! ELSEWHEREVILLE FOR YOU!"

winWumpus = "AHA! YOU GOT THE WUMPUS!"

missed = "MISSED!"

win  = "HEE HEE HEE - The Wumpus'll get you next time!!"
lose ="HA HA HA - YOU LOSE!"

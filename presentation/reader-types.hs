loop :: GameState -> |\mylib{WorldConfig}| -> |\mylibo{IO GameState}|
sense :: GameState -> |\mylib{WorldConfig}| -> |\mylibo{[String]}|
execute :: Action -> GameState -> |\mylib{WorldConfig}| -> |\mylibo{(GameState, [String])}|
emptyCave :: GameState -> |\mylib{WorldConfig}| -> |\mylibo{(GameState, Cave)}|
anotherCave :: GameState -> |\mylib{WorldConfig}| -> |\mylibo{(GameState, Cave)}|
getMoveEvent :: Action -> GameState -> |\mylib{WorldConfig}| -> |\mylibo{Maybe MoveEvent}|


loop :: (World m, MonadIO m) => GameState -> m GameState
loop` \mylib{gs}` = do
  m <- asks maze
  let cave = gCave` \mylib{gs}`
  let tunnels = sort (m ! cave)

  putStrLn $ Msg.youAreInCave cave
  sense` \mylib{gs}` >>= traverse_ putStrLn
  putStrLn (Msg.tunnelsLeadTo tunnels)

  action <- getAction tunnels
  d <- asks isDebug
  let shouldDebug l = d || (not $ isPrefixOf "[DEBUG]" l)
  `(\mylib{gs'}`, logs) <- execute action` \mylib{gs}`
  traverse_ putStrLn $ filter shouldDebug logs

  case gameOver` \mylib{gs'}` of
    Nothing -> loop` \mylib{gs'}`
    Just Win -> putStrLn Msg.win >> return` \mylib{gs'}`
    Just Lose -> putStrLn Msg.lose >> return` \mylib{gs'}`

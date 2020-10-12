loop :: GameState -> WorldConfig -> IO GameState
loop gs` \mylib{wc} `= do
  let m = maze` \mylib{wc} `
  let cave = gCave gs
  let tunnels = sort (m ! cave)

  putStrLn $ Msg.youAreInCave cave
  mapM_ putStrLn $ sense gs` \mylib{wc} `
  putStrLn $ Msg.tunnelsLeadTo tunnels

  action <- getAction tunnels
  let (gs', logs) = execute action gs` \mylib{wc} `
  let shouldDebug l = (isDebug` \mylib{wc} `) || (not $ isPrefixOf "[DEBUG]" l)
  mapM_ putStrLn $ filter shouldDebug logs

  case gameOver gs' of
    Nothing -> loop gs'` \mylib{wc} `
    Just Win -> putStrLn Msg.win >> return gs'
    Just Lose -> putStrLn Msg.lose >> return gs'


execute :: (Game m) => Action -> m [String]
execute a@(Move c) = do
  eCave <- emptyCave

  updateHistory a
  let logs = ["[DEBUG] Updated cave to Cave " ++ show c]
  modify (\s -> s { gCave = c })
  let logs' =  ("[DEBUG] Updated history with action " ++ show a)| \mylib{: logs}|
  getMoveEvent a >>= \case
      Just Wumpus -> (modify $ \s -> s { gameOver = Just Lose }) >> (return $ Msg.encounterWumpus| \mylib{: logs'}|)
      Just Pit    -> (modify $ \s -> s { gameOver = Just Lose }) >> (return $ Msg.losePits| \mylib{: logs'}|)
      Just Bat    -> (modify $ \s -> s { gCave = eCave }) >> (return $ Msg.encounterBats : ("[DEBUG] Updated history with action " ++ show eCave)| \mylib{: logs'}|)
      Nothing     -> return logs'

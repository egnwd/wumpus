module Wumpus.Utils
  ( printMapM_
  , putStrLn
  ) where

import Control.Monad.Reader
import Prelude hiding (putStrLn)
import qualified Prelude as P

printMapM_ :: (MonadIO m, Foldable t) => t String -> m ()
printMapM_ = liftIO . mapM_ putStrLn

putStrLn :: (MonadIO m) => String -> m ()
putStrLn = liftIO . P.putStrLn

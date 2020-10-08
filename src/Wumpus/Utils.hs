module Wumpus.Utils
  ( printMapM_
  , putStrLn
  , (<!>)
  , (<:>)
  , (<++>)
  ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Array
import Prelude hiding (putStrLn)
import qualified Prelude as P

printMapM_ :: (MonadIO m, Foldable t) => t String -> m ()
printMapM_ = liftIO . mapM_ putStrLn

putStrLn :: (MonadIO m) => String -> m ()
putStrLn = liftIO . P.putStrLn

(<!>) :: (Applicative f, Ix i) => f (Array i r) -> f i -> f r
(<!>) = liftA2 (!)

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

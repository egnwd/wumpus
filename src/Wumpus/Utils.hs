module Wumpus.Utils
  ( printMapM_
  , putStrLn
  , (<!>)
  , (<:>)
  , (<++>)
  , anyM
  , logMsg
  , logMsgs
  , logDebug
  ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Array
import Prelude hiding (putStrLn)
import qualified Prelude as P

printMapM_ :: (MonadIO m, Foldable t) => t String -> m ()
printMapM_ = liftIO . mapM_ putStrLn

putStrLn :: (MonadIO m) => String -> m ()
putStrLn = liftIO . P.putStrLn

(<!>) :: (Applicative f, Ix i) => f (Array i r) -> f i -> f r
(<!>) = liftA2 (!)
{-# INLINE (<!>) #-}

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)
{-# INLINE (<:>) #-}

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)
{-# INLINE (<++>) #-}

anyM :: (Monad m, Foldable t) => m (a -> Bool) -> m (t a) -> m Bool
anyM = liftA2 any

logMsg :: Monad m => String -> WriterT [String] m ()
logMsg = tell . return

logMsgs :: Monad m => [String] -> WriterT [String] m ()
logMsgs = tell

logDebug :: Monad m => String -> WriterT [String] m ()
logDebug = logMsg . ("[DEBUG] " ++)

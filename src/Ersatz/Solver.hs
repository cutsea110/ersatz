{-# LANGUAGE CPP #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2014, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ersatz.Solver
  ( module Ersatz.Solver.DepQBF
  , module Ersatz.Solver.Minisat
  , solveWith
  ) where

import Control.Monad
import Control.Monad.State
import Data.Default
import Ersatz.Codec
import Ersatz.Problem
import Ersatz.Solution
import Ersatz.Solver.DepQBF
import Ersatz.Solver.Minisat

#if __GLASGOW_HASKELL__ < 710
solveWith :: (Monad m, Alternative n, MonadPlus n, HasSAT s, Default s, Codec a) => Solver s m -> StateT s m a -> m (Result, n (Decoded a))
#else
solveWith :: (MonadIO m, Monad m, MonadPlus n, HasSAT s, Default s, Show s, Codec a) => Solver s m -> StateT s m a -> m (Result, n (Decoded a))
#endif
solveWith solver m = do
  (a, problem) <- runStateT m def
{-
  liftIO $ do
    putStrLn "==========================="
    putStrLn $ show problem
-}
  (res, litMap) <- solver problem
{-
  liftIO $ do
    putStrLn $ show res
    putStrLn $ show litMap
    putStrLn "==========================="
-}
  return (res, decode (solutionFrom litMap problem) a)

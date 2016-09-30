--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2014, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# language OverloadedStrings #-}

module Ersatz.Solver.Minisat
  ( minisat
  , cryptominisat
  , minisatPath
  ) where

import Data.ByteString.Builder
import Control.Exception (IOException, handle)
import Control.Monad.IO.Class
import Data.IntMap (IntMap)
import Ersatz.Problem
import Ersatz.Solution
import Ersatz.Solver.Common
import qualified Data.IntMap.Strict as IntMap
import System.IO
import System.Process (readProcessWithExitCode)

import qualified Data.ByteString.Char8 as B
import Data.List ( foldl' )

data SolutionFormat = Minisat
                    | Cryptominisat
                    deriving (Show, Read, Eq)

-- | 'Solver' for 'SAT' problems that tries to invoke the @minisat@ executable from the @PATH@
minisat :: MonadIO m => Solver SAT m
minisat = minisatPath (parseSolution Minisat) "minisat"

-- | 'Solver' for 'SAT' problems that tries to invoke the @cryptominisat@ executable from the @PATH@
cryptominisat :: MonadIO m => Solver SAT m
cryptominisat = minisatPath (parseSolution Cryptominisat) "cryptominisat5"

-- | 'Solver' for 'SAT' problems that tries to invoke a program that takes @minisat@ compatible arguments.
--
-- The 'FilePath' refers to the path to the executable.
minisatPath :: MonadIO m => (B.ByteString -> IntMap Bool) -> FilePath -> Solver SAT m
minisatPath parser path problem = liftIO $
  withTempFiles ".cnf" "" $ \problemPath solutionPath -> do
    withFile problemPath WriteMode $ \fh ->
      hPutBuilder fh (dimacs problem)

    (exit, _out, _err) <-
      readProcessWithExitCode path [problemPath, solutionPath] []

    sol <- parseSolutionFile parser solutionPath

    return (resultOf exit, sol)

parseSolutionFile :: (B.ByteString -> IntMap Bool) -> FilePath -> IO (IntMap Bool)
parseSolutionFile parser path = handle handler (parser <$> B.readFile path)
  where
    handler :: IOException -> IO (IntMap Bool)
    handler _ = return IntMap.empty

parseSolution :: SolutionFormat -> B.ByteString -> IntMap Bool
parseSolution Minisat s =
  case B.words s of
    x : ys | x == "SAT" ->
          foldl' ( \ m y -> let Just (v,_) = B.readInt y
                            in  if 0 == v then m else IntMap.insert (abs v) (v>0) m
                 ) IntMap.empty ys
    _ -> IntMap.empty -- WRONG (should be Nothing)
parseSolution Cryptominisat s = go [] $ B.lines s
  where
    go xs (y:ys)
      | "c " `B.isPrefixOf` y = go xs ys
      | y == "s SATISFIABLE" = go xs ys
      | "v " `B.isPrefixOf` y = go (B.drop 2 y:xs) ys
      | otherwise = IntMap.empty -- WRONG (should be Nothing)
    go xs [] = foldl' ( \ m y -> let Just (v,_) = B.readInt y
                                 in if 0 == v then m else IntMap.insert (abs v) (v>0) m
                      ) IntMap.empty $ concatMap B.words $ reverse xs

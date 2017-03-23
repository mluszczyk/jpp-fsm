{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.List
import System.Environment
import System.Exit
import Text.Read

import Auto

newtype Alpha = Alpha Char deriving (Eq,Enum,Show,Ord)
instance Bounded Alpha where
    minBound = Alpha 'A'
    maxBound = Alpha 'Z'

parseTransitionLine :: String -> Maybe (Int, [Alpha], [Int])
parseTransitionLine s = case words s of
  (wState : wSymbols : wordsStates) -> do
     state <- readMaybe wState
     states <- mapM readMaybe wordsStates
     pure (state, map Alpha wSymbols, states)
  _ -> Nothing

parseProblemInput :: String -> Maybe (Int, [Int], [Int], [(Int, [Alpha], [Int])], [Alpha])
parseProblemInput str = case filter (not . null) (lines str) of
  lStateNum : lInitStates : lAccStates : rest -> do
     let lWord = last rest
         linesTransitions = init rest
     num <- readMaybe lStateNum
     initStates <- readMaybe lInitStates
     accStates <- readMaybe lAccStates
     transitions <- mapM parseTransitionLine linesTransitions
     pure (num, initStates, accStates, transitions, map Alpha lWord)
  _ -> Nothing

extractStates :: [Int] -> [Int] -> [(Int, [Alpha], [Int])] -> [Int]
extractStates initStates acceptingStates transitions =
  nub (initStates ++
       acceptingStates ++
       map (\(s, _, _) -> s) transitions ++
       concatMap (\(_, _, d) -> d) transitions)

isValidProblemInput :: Int -> [Int] -> [Int] -> [Int] -> [(Int, [Alpha], [Int])] -> [Alpha] -> Bool
isValidProblemInput numStates states initStates accStates transitions word =
  (numStates >= 0) &&
  (length states <= numStates) &&
  all isTransitionCorrect transitions &&
  all isAlphaInRange word

  where isAlphaInRange a = (minBound <= a) && (a <= maxBound)
        isTransitionCorrect :: (Int, [Alpha], [Int]) -> Bool
        isTransitionCorrect (s, aa, ss) =
          all isAlphaInRange aa &&
          not (null aa) &&
          not (null ss)

unpackTransitions :: [(a, [b], c)] -> [(a, b, c)]
unpackTransitions = concatMap (\(a, bList, c) -> [(a, b, c) | b <- bList])

handle :: String -> String
handle string = maybe "BAD INPUT" go mProblemInput
  where
    mProblemInput = case parseProblemInput string of
      Just (numStates, initStates, accStates, transitions, word) ->
        let states = extractStates initStates accStates transitions in
        if isValidProblemInput numStates states initStates accStates transitions word then
          Just (states, initStates, accStates, transitions, word)
        else
          Nothing
      _ -> Nothing
    go (states, initStates, accStates, transitions, word) = show $ accepts auto word
      where
        auto = fromLists states initStates accStates (unpackTransitions transitions)

main = do
   args <- getArgs
   if length args == 1 then
     do
       content <- readFile $ head args
       putStrLn $ handle content
   else
       die "usage: RunAuto file_name"



{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

isValidProblemInput :: (Int, [Int], [Int], [(Int, [Alpha], [Int])], [Alpha])
                       -> Bool
isValidProblemInput (numStates, initStates, accStates, transitions, word) =
  all isStateInRange initStates &&
  all isStateInRange accStates &&
  all isTransitionCorrect transitions &&
  all isAlphaInRange word

  where isStateInRange s = (1 <= s) && (s <= numStates)
        isAlphaInRange a = (minBound <= a) && (a <= maxBound)
        isTransitionCorrect :: (Int, [Alpha], [Int]) -> Bool
        isTransitionCorrect (s, aa, ss) =
          isStateInRange s &&
          all isStateInRange ss &&
          all isAlphaInRange aa

unpackTransitions :: [(a, [b], c)] -> [(a, b, c)]
unpackTransitions = concatMap (\(a, bList, c) -> [(a, b, c) | b <- bList])

handle :: String -> String
handle string = maybe "BAD INPUT" go mProblemInputValidated
  where
    mProblemInputValidated = maybe mProblemInput (\i -> if isValidProblemInput i then Just i else Nothing) mProblemInput
    mProblemInput = parseProblemInput string
    go (a, b, c, d, e) = show $ accepts auto word
      where
        auto = fromLists [1..a] b c (unpackTransitions d)
        word = e

main = do
   args <- getArgs
   if length args == 1 then
     do
       content <- readFile $ head args
       putStrLn $ handle content
   else
       die "usage: RunAuto file_name"



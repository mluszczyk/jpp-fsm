{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.List
import Data.Maybe (isNothing, catMaybes)
import System.Environment
import System.Exit
import Text.Read

import Auto

newtype Alpha = Alpha Char deriving (Eq,Enum,Show)
instance Bounded Alpha where
    minBound = Alpha 'A'
    maxBound = Alpha 'Z'


parseTransitionLine :: String -> Maybe (Int, [Alpha], [Int])
parseTransitionLine s =
  case words s of
    (wState : wSymbols : wordsStates) ->
        let mState = readMaybe wState :: Maybe Int in
        let mStates = unpackListMaybe (map (readMaybe :: String -> Maybe Int)
                                       wordsStates) in
        case (mState, mStates) of
            (Just state, Just states) -> Just (state, map Alpha wSymbols, states)
            _ -> Nothing
    _ -> Nothing

unpackListMaybe :: [Maybe x] -> Maybe [x]
unpackListMaybe l = if any isNothing l then Nothing else Just (catMaybes l)

parseProblemInput :: String -> Maybe (Int, [Int], [Int], [(Int, [Alpha], [Int])], [Alpha])
parseProblemInput str = case filter (not . null) (lines str) of
  lStateNum : lInitStates : lAccStates : rest -> do
     let lWord = last rest
         linesTransitions = init rest
     num <- readMaybe lStateNum
     initStates <- readMaybe lInitStates
     accStates <- readMaybe lAccStates
     transitions <- mapM parseTransitionLine linesTransitions
     pure $ (num, initStates, accStates, transitions, Alpha <$> lWord)
  _ -> Nothing


unpackTransitions :: [(a, [b], c)] -> [(a, b, c)]
unpackTransitions = concatMap (\(a, bList, c) -> [(a, b, c) | b <- bList])

handle :: String -> String
handle string = maybe "BAD INPUT" go (parseProblemInput string)
  where
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



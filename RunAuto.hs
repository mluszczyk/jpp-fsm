{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Text.Read
import System.Environment
import Data.List
import Data.List.Split
import System.Exit
import Data.Maybe (isNothing, catMaybes)

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

parseProblemInput :: String -> Either String (Int, [Int], [Int], [(Int, [Alpha], [Int])], [Alpha])
parseProblemInput str =
    let list = filter (/= "") $ splitOn "\n" str in
    if length list < 4 then Left "too few lines" else
        let (lStateNum : lInitStates : lAccStates : rest) = list in
        let lWord = last rest in
        let linesTransitions = init rest in
        let mNum = readMaybe lStateNum :: Maybe Int in
        let mInitStates = readMaybe lInitStates :: Maybe [Int] in
        let mAccStates = readMaybe lAccStates :: Maybe [Int] in
        let mTransitions = (unpackListMaybe $ map parseTransitionLine linesTransitions) in
        case (mNum, mInitStates, mAccStates, mTransitions) of
           (Just num, Just initStates, Just accStates, Just transitions) ->
              Right (num, initStates, accStates, transitions, map Alpha lWord)
           (Nothing, _, _, _) -> Left "number of states"
           (_, Nothing, _, _) -> Left "list of init states"
           (_, _, Nothing, _) -> Left "list of accepting states"
           (_, _, _, Nothing) -> Left "transitions"

unpackTransitions :: [(a, [b], c)] -> [(a, b, c)]
unpackTransitions = concatMap (\(a, bList, c) -> [(a, b, c) | b <- bList])

handle :: String -> String
handle string = either ("BAD INPUT on " ++) go (parseProblemInput string)
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



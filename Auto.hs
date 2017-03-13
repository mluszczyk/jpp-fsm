module Auto (Auto, accepts, emptyA, epsA, symA, leftA, sumA, thenA, fromLists, toLists) where

import Data.List;

data Auto a q = A { states      :: [q]
                  , initStates  :: [q]
                  , isAccepting :: q -> Bool
                  , transition  :: q -> a -> [q]
                  }

walk :: Eq q => Auto a q -> a -> [q] -> [q]
walk auto letter states = nub [end | start <- states, end <- (transition auto start letter)]

accepts_helper :: Eq q => Auto a q -> [a] -> [q] -> Bool
accepts_helper auto _ [] = False
accepts_helper auto [] cur_states = any (isAccepting auto) cur_states
accepts_helper auto (word_head:word_tail) cur_states =
    accepts_helper auto word_tail (walk auto word_head cur_states)

accepts :: Eq q => Auto a q -> [a] -> Bool
accepts auto word = accepts_helper auto word (initStates auto)

emptyA :: Auto a ()
emptyA = A { states = [()]
           , initStates = [()]
           , isAccepting = \_ -> False
           , transition = \_ _ -> []
           }

epsA :: Auto a ()
epsA = A { states = [()]
         , initStates = [()]
         , isAccepting = \_ -> True
         , transition = \_ _ -> []
         }

symA :: Eq a => a -> Auto a Bool
symA l = A { states = [False, True]
           , initStates = [False]
           , isAccepting = \s -> s
           , transition = \s l' -> (if ((s == False) && (l == l')) then [True] else [])
           }

leftA :: Auto a q -> Auto a (Either q r)
leftA auto = A { states = map Left (states auto)
               , initStates = map Left (initStates auto)
               , isAccepting = either (isAccepting auto) (\_ -> False)
               , transition = \s l -> either (\s' -> map Left (transition auto s' l)) (\_ -> []) s
               }

sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
sumA auto1 auto2 = A { states = (map Left (states auto1)) ++ (map Right (states auto2))
                     , initStates = (map Left (initStates auto1)) ++ (map Right (initStates auto2))
                     , isAccepting = either (isAccepting auto1) (isAccepting auto2)
                     , transition = \state letter ->
                        either (\l -> map Left (transition auto1 l letter)) (\r -> map Right (transition auto2 r letter)) state
                     }

-- adds init states of auto2 if in an accepting state of auto1
teleport :: Auto a q1 -> Auto a q2 -> [q1] -> [Either q1 q2]
teleport auto1 auto2 leftStates =
    (map Left leftStates) ++
    if (any (isAccepting auto1) leftStates) then
        (map Right (initStates auto2)) else []

thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA auto1 auto2 = A { states = (map Left (states auto1)) ++ (map Right (states auto2))
                      , initStates = teleport auto1 auto2 (initStates auto1)
                      , isAccepting = either (\_ -> False) (isAccepting auto2)
                      , transition = \state letter -> either
                         (\l -> teleport auto1 auto2 (transition auto1 l letter))
                         (\r -> map Right (transition auto2 r letter))
                         state
                      }

fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q,a,[q])] -> Auto a q
fromLists states' initStates' acceptingStates' transitions' =
  A { states = states'
    , initStates = initStates'
    , isAccepting = \state -> any (== state) acceptingStates'
    , transition = \state letter ->
        case filter (\(start, letter', end_list) -> start == state && letter == letter') transitions' of
          [] -> []
          ((_, _, end_states):[]) -> end_states
          _ -> error "incorrect definition"
    }

toLists :: (Enum a,Bounded a) => Auto a q -> ([q],[q],[q],[(q,a,[q])])
toLists auto =
  ( states auto
  , initStates auto
  , filter (isAccepting auto) (states auto)
  , [(start, letter, transitions) |
        start <- states auto,
        letter <- [minBound..],
        let transitions = transition auto start letter,
        not (null transitions)
    ]
  )

instance (Show a, Enum a, Bounded a, Show q) => Show (Auto a q) where
  show auto =
    "fromLists " ++ (show states) ++
    " " ++ (show init) ++
    " " ++ (show accepting) ++
    " " ++ (show transitions)
    where (states, init, accepting, transitions) = toLists auto

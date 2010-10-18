-- Write a program that accepts an NFA m1 as input and 
-- generates a state table for DFA m2 equivalent to m1
module NFA where
import qualified Data.Set as S
import qualified Data.Map as M
import qualified DFA as D
import Data.Maybe
import Data.List hiding (union)
import Control.Monad
import Control.Applicative
import Debug.Trace

data NFA a st = NFA 
    { states :: States st
    , alpha  :: S.Set a
    , trans  :: Delta a st
    , start  :: st
    , end    :: States st
    } deriving Show

type Delta a st = M.Map (st, Maybe a) (States st)
type States st = S.Set st

(<!>) val m = (M.!) m val

transform f = S.fromList . concatMap (S.toList . f) . S.toList
powerset = filterM $ const [True, False]

-- toDFA :: (Show a, Show st, Ord a, Ord st) => NFA a st -> D.DFA a Int
toDFA nfa = D.DFA (cmpct) (alpha nfa) (M.fromList trans') start' end'
    where
        q = S.fromList $ S.fromList <$> (powerset . S.toList . states $ nfa)
        trans' = do
            as <- S.toList . alpha $ nfa
            qs <- S.toList q
            let r = foldl' S.union S.empty [transClosure (trans nfa) as qss | qss <- S.toList qs]
            guard $ not . S.null $ r
            return ((qs, as), r)
        rtrans = map (\((a,b),c)->((a, b), c)) . filter (\((a,_),_) -> M.member a cmpct') $ trans'
        cmpct  = (S.singleton $ epsilons (trans nfa) (start nfa)) `S.union` (S.fromList $ trans' >>= \(_,b) -> return b)
        cmpct' = M.fromList $ zip (S.toList cmpct) [0..]
        start' = epsilons (trans nfa) (start nfa)
        end'   = S.filter (not . S.null . S.intersection (end nfa)) cmpct 
   
transClosure m a st = epsilons m `transform` during
    where
        pre    = epsilons m st
        during = (\x -> fromMaybe S.empty $ M.lookup (x, Just a) m) `transform` pre

epsilons m st = epsilons' m st (S.singleton st)
epsilons' m st ss = if curr == ss
                        then ss
                        else S.union curr $ epsilons m `transform` look
    where 
        look = fromMaybe S.empty $ M.lookup (st, Nothing) m
        curr = look `S.union` ss

toReg nfa = D.toReg . toDFA $ nfa

test = NFA
    (S.fromList [1,2,3])
    (S.fromList "ab")
    (M.fromList 
        [ ((1, Just 'b'), S.singleton 2)
        , ((1, Nothing), S.singleton 3)
        , ((2, Just 'a'), S.fromList [2,3])
        , ((2, Just 'b'), S.singleton 3)
        , ((3, Just 'a'), S.singleton 1)
        ])
    1
    (S.singleton 1)
    
test' = NFA
    (S.fromList [1..8])
    (S.fromList [0,1])
    (M.fromList
        [ ((1, Nothing), S.singleton 2)
        , ((2, Just 0), S.fromList [3,6])
        , ((3, Just 0), S.singleton 4)
        , ((4, Just 1), S.singleton 5)
        , ((6, Just 1), S.singleton 7)
        , ((7, Nothing), S.singleton 1)
        , ((7, Just 0), S.singleton 8)
        , ((8, Nothing), S.singleton 1)
        ])
    1
    (S.fromList [1,5,7,8])
    
test'' = NFA
    (S.fromList [0..4])
    (S.fromList [0,1])
    (M.fromList
        [ ((0, Just 1), S.fromList [1,2])
        , ((0, Just 0), S.fromList [4])
        , ((1, Just 1), S.singleton 0)
        , ((2, Just 1), S.singleton 3)
        , ((3, Just 0), S.singleton 0)
        ])
    0
    (S.singleton 4)
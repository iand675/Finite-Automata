module DFA where
import Regex
import qualified GNFA as G
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.List hiding (union)
import Control.Monad
import Control.Applicative

data DFA a st = DFA 
    { states :: States st
    , alpha  :: S.Set a
    , trans  :: Delta a st
    , start  :: st
    , accept :: States st
    } deriving Show
    
type Delta a st = M.Map (st, a) st
type States st  = S.Set st

-- helper functions
stateList = S.toList . states
transList = M.toAscList . trans
isEnd x = S.member x . accept
(<!>) val m = (M.!) m val 

-- performs single state lookup given input character,
-- returning the result state or else nothing if the lookup fails
delta :: (Ord st, Ord a) => Delta a st -> st -> a -> Maybe st
delta d from via = M.lookup (from, via) d

-- runs continual state lookups across input string, short circuiting on reject
run :: (Ord st, Ord a) => Delta a st -> st -> [a] -> Maybe st
run d s = foldM (delta d) s

-- returns true if given dfa accepts given input, else false
accepts :: (Ord a, Ord st) => DFA a st -> [a] -> Bool
accepts d s = fromMaybe False $ (`isEnd` d) <$> run (trans d) (start d) s

-- uses ints as return state type to keep things simple and pretty
-- union :: (Ord a, Ord st1, Ord st2) => DFA a st1 -> DFA a st2 -> DFA a Int
union dfa1 dfa2 = DFA cmpct sigma delta st f
    where
        -- qMap used to keep repeated unions from having type mismatches
        -- also keeps the storage size of each element q from exploding
        qMap  = S.fromList $ (,) <$> stateList dfa1 <*> stateList dfa2
        sigma = S.union (alpha dfa1) (alpha dfa2)
        st    = (start dfa1, start dfa2)
        -- uses list monad to build cartesian product, filtering mismatched inputs out
        delta = M.fromList $ do 
                    ((r1,a1),st1) <- transList dfa1
                    ((r2,a2),st2) <- transList dfa2
                    guard $ a1 == a2
                    return (((r1,r2), a1), (st1,st2))
        cmpct = S.fromList $ concatMap (\((a,b),c) -> [a,c]) (M.toList delta)
        f     = S.fromList 
                    [(r1,r2)
                    | r1 <- stateList dfa1
                    , r2 <- stateList dfa2
                    , r1 `isEnd` dfa1 || r2 `isEnd` dfa2
                    ]
                    
--toGNFA :: (Ord a, Enum st) => DFA a st -> G.GNFA a st
toGNFA dfa = G.GNFA q sigma delta st f
    where
        q = S.insert st . S.insert f . states $ dfa
        sigma = alpha dfa
        delta   = M.fromList $ delta' : delta'' ++ old4
        delta'  = ((st, start dfa), Epsilon)
        delta'' = map (\x -> ((x, f), Epsilon)) $ S.toList (accept dfa)
        convert ((a, b), c) = ((a, c), Atom b)
        old = M.toList $ trans dfa
        old' = map convert old
        old'' = groupBy (\x y -> fst x == fst y) old'
        old3 (k,x) = if length x > 1 then (k, foldl1' Union x) else (k, head x)
        old4 = map old3 $ foo old''
        st = succ $ S.findMax (states dfa)
        f = succ st  

foo :: [[((a,a), Regex b)]] -> [((a,a), [Regex b])]
foo x = map baz x
    where
        baz :: [((a,a), Regex b)] -> ((a,a), [Regex b])
        baz y = (head zap, zop)
            where
                (zap, zop) = unzip y
        
toReg dfa = G.toRegex $ toGNFA dfa

-- regex is 1*01*01*
test1 = DFA 
    (S.fromList [1..3]) 
    (S.fromList [0,1]) 
    (M.fromList 
        -- format is ((current state, input), resulting state)
        [((1,1),1),((1,0),2),((2,1),2),((2,0),3),((3,1),3){-,((3,0),1)-}]) 
     1
    (S.fromList [3])

-- regex is 1*01*01*
test4 = DFA 
    (S.fromList [1..3]) 
    (S.fromList [0,1]) 
    (M.fromList 
        -- format is ((current state, input), resulting state)
        [((1,1),1),((1,0),2),((2,1),2),((2,0),3),((3,1),3),((3,0),1)]) 
     1
    (S.fromList [3])

-- regex is 1*01*0(01*01*0|1)*
test2 = DFA 
    (S.fromList [1..2])
    (S.fromList [0,1])
    (M.fromList 
        [((1,0),1),((1,1),2),((2,0),2),((2,1),2)])
    1
    (S.fromList [2])

-- regex is 0*10*
test3 = DFA 
    (S.fromList [1,2])
    (S.fromList [0,1])
    (M.fromList 
        [((1,0),1),((1,1),2),((2,0),2)])
    1
    (S.fromList [2])


printDFA d = do
    let tl = show . S.toList
    let f = map (\((a,b),c) -> (a,b,c))
    let f' (a,b,c) = show a ++ "\t" ++ show b ++ "\t" ++ show c
    putStrLn $ "states: " ++ (tl $ states d)
    putStrLn $ "trans: (from, via, to)"
    mapM_ (putStrLn . f') (f $ M.toList $ trans d)
    putStrLn $ "start: " ++ (show $ start d)
    putStrLn $ "accept: " ++ (tl $ accept d)
 
m1 = DFA
    (S.fromList [1,2,3])
    (S.fromList "ab")
    (M.fromList [((1,'a'),2),((1,'b'),3),((2,'a'),3),((2,'b'),1),((3,'a'),3),((3,'b'),3)])
    1
    (S.fromList [3])
m2 = DFA
    (S.fromList [1,2,3])
    (S.fromList "ab")
    (M.fromList [((1,'a'),1),((1,'b'),2),((2,'a'),3),((2,'b'),2),((3,'a'),2),((3,'b'),1)])
    1
    (S.fromList [2])
    
m1' = DFA
    (S.fromList [1..3])
    (S.fromList [0,1])
    (M.fromList [((1,0),2),((1,1),1),((2,0),3),((2,1),2),((3,0),1),((3,1),3)])
    1
    (S.fromList [1])

m2' = DFA
    (S.fromList [1,2])
    (S.fromList [0,1])
    (M.fromList [((1,0),1),((1,1),2),((2,0),2),((2,1),2)])
    1
    (S.fromList [2])

g1 :: DFA Char Int
g1 = DFA
    (S.fromList [1,2])
    (S.fromList "ab")
    (M.fromList [((1,'a'),1),((1,'b'),2),((2,'a'),2),((2,'b'),2)])
    1
    (S.singleton 2)
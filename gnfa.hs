module GNFA where
import Regex
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Debug.Trace
import Data.Maybe

data GNFA a st = GNFA
    { states :: S.Set st
    , alpha  :: S.Set a
    , trans  :: Delta a st
    , start  :: st
    , accept :: st
    } deriving Show

type Delta a st = M.Map (st, st) (Regex a)

printGNFA d = do
    let tl = show . S.toList
    -- let f = map (\((a,b),c) -> (a,b,c))
    putStrLn $ "states: " ++ (show $ states d)
    putStrLn $ "trans: (from, via, to)"
    mapM_ (putStrLn . show) (M.toList $ trans d)
    putStrLn $ "start: " ++ (show $ start d)
    putStrLn $ "accept: " ++ (show $ accept d)


toRegex g = if 1 == (M.size $ trans g) 
    then head . M.elems . trans $ g
    else toRegex' g

check m x = trace m . traceShow x $ x
valid x = x /= Empty

toRegex' g = toRegex $ g{states=ripped, trans=trans'}
    where
        (rip, ripped) = S.deleteFindMin . states $ g
        noAccept  = S.toList $ S.delete (accept g) ripped
        noStart   = S.toList $ S.delete (start g) ripped
        old = trans g
        (<!>) val def = fromMaybe def $ M.lookup val old
        combine r1 r2 r3 r4 = case valid r1 of
            True  -> case valid r2 of
                True  -> case valid r3 of
                    True  -> case valid r4 of
                        True  -> ((r1 `Concat` (Kleene r2)) `Concat` r3) `Union` r4
                        False -> (r1 `Concat` (Kleene r2)) `Concat` r3
                    False -> case valid r4 of
                        True  -> r4
                        False -> Empty
                False -> case valid r3 of
                    True  -> case valid r4 of
                        True  -> (r1 `Concat` r3) `Union` r4
                        False -> r1 `Concat` r3
                    False -> case valid r4 of
                        True  -> r1 `Union` r4
                        False -> r1
            False -> case valid r4 of
                True  -> r4
                False -> Empty                     
        trans' = M.fromList $ do
            qi <- noAccept
            qj <- noStart
            let r1 = (qi, rip)  <!> Empty
            let r2 = (rip, rip) <!> Empty
            let r3 = (rip, qj)  <!> Empty
            let r4 = (qi, qj)   <!> Empty
            let rxpr = combine r1 r2 r3 r4
            guard . valid $ rxpr
            return ((qi,qj), rxpr)
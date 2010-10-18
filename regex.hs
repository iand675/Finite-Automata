module Regex where

data Regex a = Atom a
             | Epsilon
             | Empty
             | Union (Regex a) (Regex a)
             | Concat (Regex a) (Regex a)
             | Kleene (Regex a)
             deriving (Eq)

instance (Eq a, Show a) => Show (Regex a) where
    show x = filter (/= '\'') (showRegex x)

showRegex (Atom a)     = show a
showRegex (Epsilon)    = "epsilon"
showRegex (Empty)      = "empty"
showRegex (Union a b)  = "(" ++ showRegex a ++ "|" ++ showRegex b ++ ")"
showRegex (Concat Epsilon b) = showRegex b
showRegex (Concat a Epsilon) = showRegex a
showRegex (Concat a b) = case a of
    Atom _ -> case b of
        Kleene y -> if a == y 
            then showRegex a ++ "+"
            else showRegex a ++ showRegex b
        _        -> showRegex a ++ showRegex b
    _      ->       showRegex a ++ showRegex b
showRegex (Kleene Epsilon) = ""
showRegex (Kleene a)   = showRegex a ++ "*"

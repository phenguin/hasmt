module Hasmt.Util where

compose :: [a -> a] -> a -> a
compose = foldr (.) id

addFst :: a -> b -> (a, b)
addFst x y = (x, y)

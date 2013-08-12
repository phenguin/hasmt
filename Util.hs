module Util where

compose :: [a -> a] -> a -> a
compose = foldr (.) id

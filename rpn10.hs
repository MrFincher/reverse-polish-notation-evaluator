{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
import Data.Maybe 

data Tok = Op Op | Lit Int deriving Show

data Op = Div | Mult | Add | Sub deriving Eq

instance (Show Op) where
  show Div = "/"
  show Mult = "*"
  show Add = "+"
  show Sub = "-"

ops :: [Tok]
ops = map Op [Div,Mult,Add,Sub]

toks :: [Tok]
toks = Lit 10 : ops

eval :: [Tok] -> Maybe [Tok]
eval [] = Just [Lit 0]
eval [Lit l] = Just [Lit l]
eval (Lit a : Lit b : Op o : xs) = (:xs) . Lit <$> aux o a b >>= eval
eval _ = Nothing

aux Div _ 0 = Nothing
aux Div a b = Just (div a b)
aux Mult a b = Just (a*b)
aux Add a b = Just (a+b)
aux Sub a b = Just (a-b)
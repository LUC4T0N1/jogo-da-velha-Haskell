module Compartilhado.Compartilhado where

data Quadrado = X | O | Vazio deriving (Eq, Show, Ord)

type Tabuleiro = [[Quadrado]]

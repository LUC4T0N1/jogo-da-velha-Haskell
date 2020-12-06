module Compartilhado.Logica where

import Compartilhado.Compartilhado
import Compartilhado.Interface
import Data.Array
import Data.List (transpose)

validarCoordenada :: (Int, Int) -> Bool
validarCoordenada = inRange ((0, 0), (2, 2))

quadradoVazio :: Tabuleiro -> Int -> Bool
quadradoVazio t i = concat t !! i == Vazio

cortar :: Int -> [a] -> [[a]]
cortar n [] = []
cortar n xs = take n xs : cortar n (drop n xs)

manipulaCoord :: (Int, Int) -> Int
manipulaCoord c
  | c == (0, 0) = 0
  | c == (0, 1) = 1
  | c == (0, 2) = 2
  | c == (1, 0) = 3
  | c == (1, 1) = 4
  | c == (1, 2) = 5
  | c == (2, 0) = 6
  | c == (2, 1) = 7
  | c == (2, 2) = 8

jogada :: Tabuleiro -> Int -> Quadrado -> Tabuleiro
jogada g i p = cortar 3 (xs ++ [p] ++ ys)
  where
    (xs, Vazio : ys) = splitAt i (concat g)

cheio :: Tabuleiro -> Bool
cheio = all (/= Vazio) . concat

diagonalizar :: Tabuleiro -> [Quadrado]
diagonalizar g = [g !! n !! n | n <- [0 .. 2]]

vencer :: Quadrado -> [[Quadrado]] -> Bool
vencer j t = logica t || logica (transpose t) || logica [diagonalizar t, diagonalizar (map reverse t)]
  where
    logica = any (all (== j))

coordenadaClicada :: (Float, Float) -> (Int, Int)
coordenadaClicada (x, y) =
  ( floor ((y + (fromIntegral alturaTela * 0.5)) / alturaQuadrado),
    floor ((x + (fromIntegral larguraTela * 0.5)) / larguraQuadrado)
  )
module Main where

import Compartilhado.Compartilhado
import Compartilhado.Interface
import Graphics.Gloss (Display (InWindow), makeColor, play)
import Graphics.Gloss.Data.Color (makeColor)
import PvC.InterfacePvC (controladorInterfacePvC)
import PvC.JogoPvC (comecoPvC)
import PvC.LogicaPvC
import PvP.InterfacePvP (controladorInterfacePvP)
import PvP.JogoPvP (comecoPvP)
import PvP.LogicaPvP (controlaJogadas)
import System.IO (hFlush, stdout)

tela :: Display
tela = InWindow "Jogo" (larguraTela, alturaTela) (200, 100)

data OpcoesMenu = PvP | PvCFacil | PvCDificil | Repetir deriving (Show)

execPvP :: IO ()
execPvP = do
  play tela (makeColor 0 0 0 200) 10 comecoPvP controladorInterfacePvP controlaJogadas (const id)

execPvCDificil :: IO ()
execPvCDificil = do
  play tela (makeColor 0 0 0 200) 10 comecoPvC controladorInterfacePvC controlaJogadasDificil (const id)

execPvCFacil :: IO ()
execPvCFacil = do
  play tela (makeColor 0 0 0 200) 10 comecoPvC controladorInterfacePvC controlaJogadasFacil (const id)

execRepetir :: IO ()
execRepetir = do
  putStrLn "Inválido, digite novamente"
  main

menu :: IO OpcoesMenu
menu = do
  putStrLn "Bem vindo ao jogo da velha!"
  putStrLn "Digite 0 para inciar o modo de dois jogadores"
  putStrLn "Digite 1 para jogar contra o computador no modo fácil"
  putStrLn "Digite 2 para jogar contra o computador no modo difícil"
  hFlush stdout
  c <- getChar
  case c of
    '0' -> return PvP
    '1' -> return PvCFacil
    '2' -> return PvCDificil
    _ -> return Repetir

main :: IO ()
main = do
  tipoJogo <- menu
  case tipoJogo of
    PvCFacil -> execPvCFacil
    PvCDificil -> execPvCDificil
    PvP -> execPvP
    Repetir -> execRepetir

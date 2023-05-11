module Lib where

import Data.Set as Set
import Graphics.WorldTurtle

data LSysteme = LSysteme
  { variables :: Set Char,
    constantes :: Set Char,
    axiome :: String,
    regles :: Char -> String
  }

generer :: LSysteme -> Int -> String
generer ls 0 = axiome ls
generer ls n = Prelude.foldl (\acc elm -> acc ++ regles ls elm) "" (generer ls (n - 1))

executer ls n = runTurtle ( setSpeed 0 >> (Prelude.foldl (\acc e -> acc >> (case e of 'F' -> forward 5; '+' -> left 90; '-' -> right 90; _ -> forward 0)) (return ()) (generer ls n)))

koch :: LSysteme
koch = LSysteme {variables = Set.singleton 'F', constantes = Set.fromList "+-", axiome = "F", regles = \c -> case c of 'F' -> "F+F-F-F+F"; _ -> [c]}

testKoch :: IO ()
testKoch = do
  executer koch 3

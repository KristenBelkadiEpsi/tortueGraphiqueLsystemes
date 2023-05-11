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

transcrire :: String -> Float -> Float -> TurtleCommand ()
transcrire chaine pasDeplacement pasRotation = Prelude.foldl (\acc elm -> acc >> case elm of 'F' -> forward pasDeplacement; '+' -> left pasRotation; '-' -> right pasRotation; '|' -> left 180; _ -> return ()) (return ()) chaine

executer :: LSysteme -> Int -> Float -> Float -> IO ()
executer ls n pasDeplacement pasRotation = runTurtle (setSpeed 0 >> setInvisible >> transcrire (generer ls n) pasDeplacement pasRotation)

koch :: LSysteme
koch = LSysteme {variables = Set.singleton 'F', constantes = Set.fromList "+-", axiome = "F", regles = \c -> case c of 'F' -> "F+F-F-F+F"; _ -> [c]}

testLSysteme :: LSysteme
testLSysteme = LSysteme {variables = Set.singleton 'F', constantes = Set.fromList "+-", axiome = "F+F+F+F+", regles = \c -> case c of 'F' -> "F+FF-F-FF+F"; _ -> [c]}

testKoch :: IO ()
testKoch = do
  executer testLSysteme 3 10 90

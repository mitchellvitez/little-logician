module Lib
    ( someFunc
    ) where

import Core
import Parse
import Reduce

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment
import Text.Megaparsec

someFunc :: IO ()
someFunc = do
  args <- getArgs
  when (length args /= 1) $ error "expected one filename argument"
  file <- readFile $ head args
  let content = T.pack file
  let program = parse parseProgram "" content
  case program of
    Left bundle -> error $ errorBundlePretty bundle
    Right parsed -> do
      case parse parseProgramText "" content of
        Left bundle -> error $ errorBundlePretty bundle
        Right goalText -> do
          let results = zip goalText $ map pipeline parsed
          mapM_ (putStrLn . T.unpack . showResult) results
          showFinal (length . filter (==True) . map snd $ results) (length results)

showFinal :: Int -> Int -> IO ()
showFinal some all =
  putStrLn $ amount <> " goals obtained"
  where
    amount =
      if some == all then "all " <> show all
      else show some <> " out of " <> show all

showResult :: (Text, Bool) -> Text
showResult (name, proven) =
  name <> " " <> if proven then "obtains" else "does not obtain"

display :: Goal -> Text
display (Goal props concl) = T.concat
  [ T.intercalate ", " (map displayProp props)
  , " ⊢ "
  , displayProp concl
  ]

displayProp :: Prop -> Text
displayProp (Var x)       = x
displayProp PropTrue      = "T"
displayProp PropFalse     = "F"
displayProp (Or p q)      = displayProp p <> " ∨ " <> displayProp q
displayProp (And p q)     = displayProp p <> " ∧ " <> displayProp q
displayProp (Not p)       = "¬" <> displayProp p
displayProp (Implies p q) = displayProp p <> " → " <> displayProp q
displayProp (Iff p q)     = displayProp p <> " ↔ " <> displayProp q

module Lib
    ( someFunc
    ) where

import Core
import Parse
import Reduce

import Control.Monad (when)
import Control.Monad.Trans.Writer.Lazy
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment
import Text.Megaparsec

someFunc :: IO ()
someFunc = do
  args <- getArgs
  when (length args < 1) $ error "expected one filename argument"
  let write = length args > 1 && args !! 1 `elem` ["w", "write"]
  file <- readFile $ head args
  let content = T.pack file
  let program = parse parseProgram "" content
  case program of
    Left bundle -> error $ errorBundlePretty bundle
    Right parsed -> do
      case parse parseProgramText "" content of
        Left bundle -> error $ errorBundlePretty bundle
        Right goalText ->
          -- Writer was added later, a lot of the code is just copied
          if write then do
            let results = zip goalText $ map (runWriter . wpipeline) parsed
            mapM_ (putStrLn . T.unpack . wshowResult) results
            showFinal (length . filter (==True) . map (fst . snd) $ results) (length results)
          else do
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

wshowResult :: (Text, (Bool, Text)) -> Text
wshowResult (name, (proven, note)) =
  note <> showResult (name, proven)

showResult :: (Text, Bool) -> Text
showResult (name, proven) =
  name <> " " <> if proven then "obtains" else "does not obtain"

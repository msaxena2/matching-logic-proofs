{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Text.Prettyprint.Doc  
import Data.Text.Prettyprint.Doc.Render.Text
import ProofUtils (onePlusOneProof) 
import Data.Void
import Data.Text.Prettyprint.Doc.Render.String 


main :: IO ()
main = (putStrLn . renderString . layoutPretty (LayoutOptions (Unbounded))) (pretty onePlusOneProof)

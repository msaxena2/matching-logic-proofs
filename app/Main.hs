module Main where
import Data.Text.Prettyprint.Doc  
import ProofUtils 
import Data.Void


main :: IO ()

main = print $ viaShow (renderProof onePlusOneProof)

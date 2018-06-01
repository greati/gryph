module Main where

import Syntactic.Parser
import Syntactic.Syntax
import Syntactic.Values
import Execution.Semantic
import Execution.Memory
import Execution.RepetitionSemantic as RS
import qualified Execution.Graph as G
import Execution.GraphSemantic
import Data.Time.Clock

main :: IO ()
main = undefined

igryph :: Filename -> IO()
igryph s = do
                us <- parseFile s
                exec memory programMemory scopes us


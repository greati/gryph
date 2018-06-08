module Main where

import Syntactic.Parser
import Syntactic.Syntax
import Syntactic.Values
import Execution.Semantic
import Execution.Memory
import qualified Execution.Graph as G
import Execution.GraphSemantic
import Data.Time.Clock
import Control.DeepSeq

main :: IO ()
main = do
            igryph "../examples/structs.gph"
            return ()

igryph :: Filename -> IO()
igryph s = do
                us <- parseFile s
                exec memory programMemory scopes us


module Main where

import System.Environment
import Syntactic.Parser
import Syntactic.Syntax
import Syntactic.Values
import Execution.Semantic
import Execution.Memory
import qualified Execution.Graph as G
import Data.Time.Clock
import Control.DeepSeq

main :: IO ()
main = do
            args <- getArgs
            case args of
                [] -> error "Please, provide a gryph file (.gph)"
                [x] -> do
                        igryph x
                        return ()

igryph :: Filename -> IO()
igryph s = do
                us <- parseFile s
                exec memory programMemory scopes us
                return $ ()


{-# LANGUAGE BangPatterns #-}
module Execution.Semantic where

import Syntactic.Values   as V
import Syntactic.Syntax   as S
import Execution.Memory
import Data.Time.Clock
import Data.List as L
import Execution.Graph    as G
import Syntactic.Types
import qualified Data.Map.Strict as M
import qualified Data.Set        as Se
import Control.DeepSeq
import Data.Char
import Syntactic.Parser

type Filename = String

-- | Get current system time to produce scope ids
getCurSeconds :: IO Integer                                                                                   
getCurSeconds = do                                                                                            
                    t <- getCurrentTime                                                                       
                    i <- return $ utctDayTime t                                                               
                    return $ diffTimeToPicoseconds i


-- |Scopes in the execution.
scopes :: Scopes
scopes = [GlobalScope]

-- |Execute a program represented as a list of program units.
exec :: Memory -> ProgramMemory -> Scopes -> [ProgramUnit] -> IO(Memory, ProgramMemory, Scopes) 
exec m pm ss [] = if m == m then return (m,pm,ss) else return (m,pm,ss)
exec m pm ss (u:us) = if m == m 
                      then do
                        (m', pm', ss') <- execUnit u m pm ss
                        exec m' pm' ss' us
                      else do
                        (m', pm', ss') <- execUnit u m pm ss
                        exec m' pm' ss' us 

-- |Executes a program unit.
execUnit :: ProgramUnit -> Memory -> ProgramMemory -> Scopes -> IO (Memory, ProgramMemory, Scopes)
execUnit (SubprogramDecl sub) m pm ss = do 
                                            pm' <- execSubDecl sub m pm ss
                                            return $ (m, pm', ss)
execUnit (StructDecl struct) m pm ss = 
                                do
                                    pm' <- execStructDecl m pm ss struct
                                    return $ (m, pm', ss)

execUnit (Stmt stmt) m pm ss = do 
                                    (m', ss', v) <- execStmt stmt m pm ss
                                    return $ (m', pm, ss')
execUnit (Use path) m pm ss = do
                                    f <- parseFile path
                                    let decls = filter (\x -> case x of    
                                                                (Use _) -> False
                                                                (Stmt _) -> False
                                                                _ -> True) f
                                    exec m pm ss decls

-- |Executes a subprogram declaration.
execSubDecl :: Subprogram -> Memory -> ProgramMemory -> Scopes -> IO (ProgramMemory)
execSubDecl s m pm ss = do  
                            (si,sc) <- interpretSubDeclaration s m pm ss
                            case declareSubprogram si sc pm of
                                Left i -> error i
                                Right pm' -> return $ pm'

-- |Executes a struct declaration.
execStructDecl :: Memory -> ProgramMemory -> Scopes -> StructDecl -> IO (ProgramMemory)
execStructDecl m pm ss s = do
                                (si, sc) <- interpretStructDecl m pm ss s
                                case declareStruct pm si sc of
                                    Left i -> error i
                                    Right pm' -> return $ pm'

type Scoper = (Integer -> Scope)

-- | Executes a block of statement, creating a new scope and declaring variables. When finish, clear the scope.
-- It also allows taking a list of declarations in the form [(Name,Cell)]
execBlockSimple :: Block -> Memory -> ProgramMemory -> Scoper -> Scopes -> [(Name,Cell)] -> IO (Memory, Scopes, Maybe Value)
--execBlockSimple ([]) m pm _ (s:ss) _ = return (m, ss, Nothing)
execBlockSimple b@(st:sts) m pm scoper ss decls =  
    do  time <- getCurSeconds
        let     newScope = scoper (time)
                ss' = (newScope:ss) 
                m' = case elabVars m decls newScope of
                    Left i -> error i
                    Right m'' -> m'' in
                        do 
                            execBlock' b m' pm ss' newScope Nothing

execBlock' [] m pm (s:ss) newScope v = return (clearScope s m, ss, v)
execBlock' (st:sts) m pm ss' newScope v = do 
                (m'',ss'', v'') <- execStmt st m pm ss'
                do
                    if newScope == head ss'' then
                        execBlock' sts m'' pm ss'' newScope v''
                    else 
                        return $ (clearScope newScope m'',ss'',v'')

-- | Executes a block for a given scope
execBlock :: Block -> Memory -> ProgramMemory -> Scopes -> Scope -> IO (Memory, Scopes, Maybe Value)
execBlock [] m pm ss newScope = return (m, ss, Nothing)
execBlock (st:sts) m pm ss' newScope = do 
                (m'',ss'', v'') <- execStmt st m pm ss'
                do
                    if newScope == head ss'' then
                        execBlock sts m'' pm ss'' newScope
                    else 
                        return $ (m'',ss'',v'')

-- |Executes any statement.
execStmt :: Stmt -> Memory -> ProgramMemory -> Scopes -> IO (Memory, Scopes, Maybe Value)
execStmt d@(DeclStmt _) m pm ss = do
                            m' <- varDeclStmt d m pm ss
                            return $ (m', ss, Nothing)
execStmt a@(AttrStmt _ _) m pm ss = do
                            m' <- execAttrStmt a m pm ss
                            return $ (m', ss, Nothing)
execStmt (PrintStmt e) m pm ss = do
                                (v, m', ss') <- eval m pm ss e
                                case v of
                                    (String str) -> putStr $ parseString str
                                    (Char c) -> putChar $ c
                                    v -> putStr $ show v
                                return $ (m', ss', Nothing)
execStmt (PrintLnStmt e) m pm ss = do
                                (v, m', ss') <- eval m pm ss e
                                case v of
                                    (String str) -> putStrLn $ parseString str
                                    (Char c) -> do {putChar c; putStrLn ""}
                                    v -> putStrLn $ show v
                                return $ (m', ss', Nothing)
execStmt (ReadStmt i) m pm ss = do
                                value <- getLine
                                case updateVar m ((\(Ident i) -> i) i) ss (makeCompatibleAssignTypes pm GString (String value)) of
                                    Left e -> error e
                                    Right m' -> return (m', ss, Nothing)
                                
execStmt (IfStmt e (IfBody ifbody) elsebody) m pm ss' = 
    do 
            (v, m'', ss'') <- eval m pm ss' e
            let test = case makeBooleanFromValue v of
                            Left i -> error i
                            Right i -> i in 
                do 
                    if test then
                            do execBlockSimple ifbody m'' pm BlockScope ss'' []
                    else
                        case elsebody of
                            NoElse -> do return (m'',ss'', Nothing)
                            ElseBody block -> do execBlockSimple block m'' pm BlockScope ss'' []

execStmt (DfsStmt ids graph starter body) m pm ss =
    if length ids > 3
    then error "There are too many identifiers"
    else do
        (g, gtype,m'',ss'') <- evalWithType m pm ss graph
        case g of
            (V.Graph g'@(G.Graph vertices edges)) ->
                if Se.null vertices
                then return (m'', ss'', Nothing)
                else
                    case starter of
                        Nothing -> do 
                            let v1 = head $ Se.toList vertices
                            time <- getCurSeconds 
                            let newScope = IterationScope time
                                ss' = (newScope:ss'') in
                                    dfsStmt ids g' [(v1, v1, (Integer 1))] (Se.empty) body m'' pm ss' newScope 
                        Just v  -> do
                            (v', m''', ss''') <- eval m'' pm ss'' v 
                            let v1@(G.Vertex id1 _) = G.getVertexFromValue g' v' False
                            if id1 == -1
                            then error $ "The Vertex " ++ (show v') ++ " doesn't exist"
                            else do
                                time <- getCurSeconds 
                                let newScope = IterationScope time
                                    ss' = (newScope:ss''') in
                                        dfsStmt ids g' [(v1, v1, (Integer 1))] (Se.empty) body m''' pm ss' newScope
            _ -> error "The expression must be a graph"
    where
        dfsStmt ids graph stack vis body m pm ss' newScope =
            if stack == []
            then return (m, ss', Nothing)
            else do 
                let h@(v@(Vertex id _), dad, step) = (head stack)
                if Se.member v vis
                then if length stack == 1
                     then 
                        return (m, ss', Nothing)
                     else 
                        dfsStmt ids graph (tail stack) vis body m pm ss' newScope
                else do 
                        let new_vis = Se.insert v vis
                        let nameCell = getNameCellGraph ids (head stack) 
                        let m' = (case elabVars m nameCell newScope of
                                        Left i -> error i
                                        Right m'' -> m'') in
                                    do
                                        let new_stack = fillStack graph h (tail stack)
                                        if length new_stack == 0
                                        then do 
                                            (m'',ss'',v) <- execBlock body m' pm ss' newScope
                                            return (clearScope newScope m'', tail ss'', v)
                                        else do 
                                            (m'',ss'',v) <- execBlock body m' pm ss' newScope
                                            if (newScope == head ss'') then
                                                dfsStmt ids graph new_stack new_vis body (clearScope newScope m'') pm ss'' newScope
                                            else 
                                                return (clearScope newScope m'', ss'', v)

        fillStack g (v, _, step) stack = fillStack' (G.getEdges g v) step stack
        fillStack' [] _ stack = stack
        fillStack' (G.Edge v1 v2 _: xs) step stack =
            let (Integer step') = step in
                fillStack' xs step ((v2, v1, Integer (step' + 1)) : stack)

execStmt (BfsStmt ids graph starter body) m pm ss =
    if length ids > 3
    then error "There are too many identifiers"
    else do
        (g, gtype,m''',ss''') <- evalWithType m pm ss graph
        case g of
            (V.Graph g'@(G.Graph vertices edges)) ->
                if Se.null vertices
                then return (m''', ss''', Nothing)
                else
                    case starter of
                        Nothing -> do 
                            let v1 = head $ Se.toList vertices
                            time <- getCurSeconds 
                            let newScope = IterationScope time
                                ss' = (newScope:ss''') in
                                    bfsStmt ids g' [(v1, v1, (Integer 1))] (Se.fromList [v1]) body m''' pm ss' newScope 
                        Just v  -> do
                            (v', m', ss'') <- eval m''' pm ss''' v 
                            let v1@(G.Vertex id1 _) = G.getVertexFromValue g' v' False
                            if id1 == -1
                            then error $ "The Vertex " ++ (show v') ++ " doesn't exist"
                            else do
                                time <- getCurSeconds 
                                let newScope = IterationScope time
                                    ss' = (newScope:ss'') in
                                        bfsStmt ids g' [(v1, v1, (Integer 1))] (Se.fromList [v1]) body m' pm ss' newScope
            _ -> error "The expression must be a graph"
    where
        bfsStmt ids graph queue vis body m pm ss' newScope = 
            do let nameCell = getNameCellGraph ids (head queue) 
               let m' = (case elabVars m nameCell newScope of
                    Left i -> error i
                    Right m'' -> m'') in
                do
                    let h@(v@(Vertex id _), dad, step) = (head queue)
                    let (new_vis, new_queue) = fillQueue graph h vis queue
                    if length new_queue == 1
                    then do 
                        (m'',ss'',v) <- execBlock body m' pm ss' newScope
                        return (clearScope newScope m'', tail ss'', v)
                    else do 
                        (m'',ss'',v) <- execBlock body m' pm ss' newScope
                        if (newScope == head ss'') then
                            bfsStmt ids graph (tail new_queue) new_vis body (clearScope newScope m'') pm ss'' newScope
                        else 
                            return $ (clearScope newScope m'', ss'', v)

        fillQueue g (v, _, step) vis queue = fillQueue' (G.getEdges g v) vis step queue
        fillQueue' [] vis _ queue = (vis, queue)
        fillQueue' (G.Edge v1 v2 _: xs) vis step queue =
            if Se.member v2 vis
            then fillQueue' xs vis step queue  
            else let (Integer step') = step in
                    fillQueue' xs (Se.insert v2 vis) step (queue ++ [(v2, v1, Integer (step' + 1))])

execStmt (ForStmt ids vs body) m pm ss = 
    do 
            let new_vs = replicateList ((length ids) - (length vs)) vs 
            (vss, m', ss') <- (getLists m pm ss [new_vs])
            vss' <- over vss
            time <- getCurSeconds
            let newScope = IterationScope time
                ss' = (newScope:ss) in
                    forStmt ids vss' body m pm ss' newScope
            where
                getLists m pm ss (xs:[])  = do (xss, m',ss') <- (evalList m pm ss xs)
                                               case xss of
                                                    xss'@( (List list) : _ ) -> do return (xss', m',ss')
                                                    xss'@((String s) : _ )   -> do return $ (makeListChars xss', m', ss')
                                                    maps@((Map map) : _ )    -> do return ([(List(makeMap(M.toList x))) | Map x <- maps], m',ss')
                                                    gs@((V.Graph _) : _)     -> do return ([ List $ removeVerticesId $ G.getVertices g | V.Graph g <- gs], m', ss')
                                                    _                        -> error "Wrong pattern!"
                getLists m pm ss (xs:xss) = do (xss', m',ss')   <- (evalList m pm ss xs)
                                               (xss'', m'', ss'') <- (getLists m' pm ss' xss)
                                               return ((xss' ++ xss''), m'', ss'')

                replicateList 0 xs = xs
                replicateList n xs | n < 0     = error "There aren't enough identifiers." 
                                   | otherwise = replicateList (n-1) xs ++ [last xs]

                makeListChars [] = []
                makeListChars ((String s) : xs) = [List [Char x | x <- s]] ++ (makeListChars xs)

                makeMap :: [(Value, Value)] -> [Value]
                makeMap []     = []
                makeMap [x]    = [Pair x]
                makeMap (x:xs) = (Pair x) : makeMap xs

                removeVerticesId :: [G.Vertex Value] -> [Value]
                removeVerticesId [] = []
                removeVerticesId (G.Vertex _ v : xs) = v : removeVerticesId xs

                forStmt ids vs body m pm ss' newScope = 
                    do
                        let nameCell = getNameCell ids (head vs) 
                        let m' = (case elabVars m nameCell newScope of
                                Left i -> error i
                                Right m'' -> m'') in
                                    if length vs == 1
                                    then do
                                            (m'',ss'',v) <- execBlock body m' pm ss' newScope
                                            return (clearScope newScope m'', tail ss'', v)
                                    else do
                                        if length vs > 0
                                        then do
                                            (m'',ss'',v) <- execBlock body m' pm ss' newScope
                                            if (newScope == head ss'') then
                                                forStmt ids (tail vs) body (clearScope newScope m'') pm ss'' newScope
                                            else return $ (clearScope newScope m'', ss'', v)
                                        else do
                                            return (m, tail ss', Nothing)

execStmt (WhileStmt e body) m pm ss =  --let ss' = (IterationScope (length ss):ss) in 
                        do  
                            time <- getCurSeconds
                            let newScope = IterationScope (time)
                                ss' = (newScope:ss) in
                                    repeatWhile body m pm ss' newScope

    where
        repeatWhile body' m pm ss newScope =
            do 
                    (v, m', ss') <- eval m pm ss e
                    let test = case makeBooleanFromValue v of
                                    Left i -> error i
                                    Right i -> i in 
                        if test then 
                            do
                                    do 
                                        (m'',ss'',v') <- execBlock body' m' pm ss' newScope
                                        if (newScope == head ss'') then 
                                            repeatWhile body (clearScope newScope m'') pm ss'' newScope
                                        else return $ (clearScope newScope m'', ss'',v')
                        else
                           return (m', tail ss', Nothing) 

execStmt (SubCallStmt (SubprogCall (Ident i) as)) m pm ss = do  
                                                                arguments <- processSubArgs as [] m pm ss
                                                                selected <- return $ selectSubForCall i arguments pm
                                                                do
                                                                    case selected of
                                                                        Nothing -> error ("No subprogram found for call to " ++ i)
                                                                        Just sub -> do 
                                                                                (m',ss',mv) <- execSubprogram m pm scopes sub arguments
                                                                                return $ (m',ss,mv)

execStmt (ReturnStmt e') m pm ss = 
    case e' of
        Just e -> 
            do  (v, m'', ss'') <- eval m pm ss e
                let (ss', m') = case clearScopesUntilSub m'' ss'' of
                                    Nothing -> error "Return called outside subprogram scope"
                                    Just v' -> v' in return $ (m',ss', Just v)
        Nothing -> 
            let (ss', m') = case clearScopesUntilSub m ss of
                                Nothing -> error "Return called outside subprogram scope"
                                Just v' -> v' in return $ (m',ss', Nothing)

execStmt (BreakStmt) m pm ss = do
                                    return $ (m',ss'',Nothing)
                                    where (ss'', m') = case clearScopesUntilIter m ss of
                                            Nothing -> error "Break called outside iteration scope"
                                            Just v' -> v'

execStmt (AddStmt e1 e2) m pm ss =
                                    do
                                        (e1', m', ss') <- eval m pm ss e1
                                        (e2', e2type, m''', ss''') <- evalWithType m' pm ss' e2
                                        case e2' of
                                            (V.Graph g@(G.Graph vertices _)) -> do
                                                case e2type of
                                                    GGraphEmpty -> do let v  = G.getVertexFromValue g e1' True
                                                                      let g' = G.insertVertex g v
                                                                      m'' <- execAttrStmt' m''' pm ss''' [] e2 (V.Graph g', e2type)
                                                                      return (m'', ss''', Nothing)
                                                    GGraphVertexEdge typeVertice _ -> if not $ checkCompatType typeVertice (getType e1')
                                                                                      then error $ "Incompatible types " ++ (show $ getType e1')  ++ " and " ++ show typeVertice
                                                                                      else do let v  = G.getVertexFromValue g e1' True
                                                                                              let g' = G.insertVertex g v
                                                                                              m'' <- execAttrStmt' m''' pm ss''' [] e2 (V.Graph g', e2type)
                                                                                              return (m'', ss''', Nothing)                                            
                                            (List l) -> do 
                                                case e2type of
                                                    GListEmpty -> do 
                                                                    m'' <- execAttrStmt' m' pm ss' [] e2 (List ([e1']), e2type)
                                                                    return (m'', ss', Nothing)
                                                    GList lt   -> do
                                                                    if checkCompatType lt (getType e1')
                                                                    then do
                                                                        m'' <- execAttrStmt' m' pm ss' [] e2 (List (l ++ [e1']), e2type)
                                                                        return (m'', ss', Nothing)
                                                                    else
                                                                        error $ "Incompatible types between " ++ (show lt) ++ " and " ++ (show $ getType e1')
                                            _ -> error "Wrong pattern"

execStmt (DelStmt e1 e2) m pm ss =
                                    do
                                        (e1', m', ss') <- eval m pm ss e1
                                        (e2', e2type, m''', ss''') <- evalWithType m' pm ss' e2
                                        case e2' of
                                            (V.Graph g@(G.Graph vertices _)) -> do
                                                let v@(Vertex id _)  = G.getVertexFromValue g e1' False
                                                if id == -1
                                                then error $ "The Vertex " ++ (show e1') ++ " doesn't exist"
                                                else do
                                                    let g' = G.deleteVertex v g
                                                    m'' <- execAttrStmt' m''' pm ss''' [] e2 (V.Graph g', e2type)
                                                    return (m'', ss''', Nothing)                                                                                               
                                            (List l) -> do
                                                case e1' of
                                                    Integer i ->
                                                        if i < 0 || i >= fromIntegral(length l)
                                                        then error $ "Index " ++ (show e1') ++ " out of bounds"
                                                        else
                                                            do  m'' <- execAttrStmt' m''' pm ss''' [] e2 (List ((take (fromIntegral i) l) ++ (drop ((fromIntegral i)+1) l)), e2type)
                                                                return (m'', ss''', Nothing)
                                                    _         -> error $ "Incompatible type of index"
                                            (V.Map d) -> do
                                                case e2type of
                                                    GDictEmpty -> error "Empty dictionary"
                                                    GDict ks _ ->
                                                        if checkCompatType ks (getType e1')
                                                        then 
                                                            if M.notMember e1' d
                                                            then error $ "The key " ++ (show e1') ++ " isn't present in dictionary"
                                                            else do
                                                                m'' <- execAttrStmt' m''' pm ss''' [] e2 (Map (M.delete e1' d), e2type)
                                                                return (m'', ss''', Nothing)
                                                        else error "Incompatible type of index"
                                                    _          -> error "Unknown type" 
                                            _ -> error "Wrong pattern"

execStmt (AddEdgeStmt weight (S.Edge typeEdge e1 e2) g) m pm ss = 
    do
        (g', gtype, m''''', ss''''') <- evalWithType m pm ss g
        case g' of
            (V.Graph g1@(G.Graph vertices _)) -> do 
                (e1', m', ss') <- eval m''''' pm ss''''' e1
                (e2', m'', ss'') <- eval m' pm ss' e2
                if getType e1' == getType e2'
                then do
                    let v1@(G.Vertex id1 _) = G.getVertexFromValue g1 e1' False
                    if id1 == -1
                    then error $ "The Vertex " ++ (show e1') ++ " doesn't exist"
                    else do
                        let v2@(G.Vertex id2 _) = G.getVertexFromValue g1 e2' False
                        if id2 == -1
                            then error $ "The Vertex " ++ (show e2') ++ " doesn't exist"
                        else
                            case weight of
                                Nothing -> do let w = (Integer 1)
                                              g'' <- addEdge g1 typeEdge v1 v2 w
                                              m''' <- execAttrStmt' m'' pm ss'' [] g (g'', gtype)
                                              return (m''', ss'', Nothing) 
                                Just w  -> do (w', m''', ss''') <- eval m'' pm ss'' w
                                              g'' <- addEdge g1 typeEdge v1 v2 w'
                                              m'''' <- execAttrStmt' m''' pm ss''' [] g (g'', gtype)
                                              return (m'''', ss''', Nothing)
                else error $ "Incompatible types " ++ (show $ getType e1')  ++ " and " ++ (show $ getType e2')
            _ -> error "Wrong pattern" 

execStmt (DelEdgeStmt (S.Edge typeEdge e1 e2) g) m pm ss = 
    do
        (g', gtype, m''''', ss''''') <- evalWithType m pm ss g
        case g' of
            (V.Graph g1@(G.Graph vertices _)) -> do
                (e1', m', ss')   <- eval m''''' pm ss''''' e1
                (e2', m'', ss'') <- eval m' pm ss' e2
                if getType e1' == getType e2'
                then do
                    let v1@(G.Vertex id1 _) = G.getVertexFromValue g1 e1' False
                    if id1 == -1
                    then error $ "The Vertex " ++ (show e1') ++ " doesn't exist"
                    else do
                        let v2@(G.Vertex id2 _) = G.getVertexFromValue g1 e2' False
                        if id2 == -1
                            then error $ "The Vertex " ++ (show e2') ++ " doesn't exist"
                        else
                            case typeEdge of
                                DoubleEdge -> do let ed1  = G.Edge v1 v2 (Integer 1)
                                                 let ed2  = G.Edge v2 v1 (Integer 1)
                                                 if G.isEdgePresent g1 ed1
                                                 then
                                                    if G.isEdgePresent g1 ed2
                                                    then do
                                                        let g''  = G.deleteEdge g1 ed1
                                                        let g''' = G.deleteEdge g'' ed2
                                                        m''' <- execAttrStmt' m'' pm ss'' [] g (V.Graph g''', gtype)
                                                        return (m''', ss'', Nothing)
                                                    else 
                                                        error $ "There isn't any Edge between " ++ (show e2') ++ " and " ++ (show e1')
                                                 else 
                                                    error $ "There isn't any Edge between " ++ (show e1') ++ " and " ++ (show e2')

                                RightEdge -> do let ed1  = G.Edge v1 v2 (Integer 1)
                                                let g''  = G.deleteEdge g1 ed1
                                                if G.isEdgePresent g1 ed1
                                                then do
                                                    m''' <- execAttrStmt' m'' pm ss'' [] g (V.Graph g'', gtype)
                                                    return (m''', ss'', Nothing)
                                                else 
                                                    error $ "There isn't any Edge between " ++ (show e1') ++ " and " ++ (show e2')

                                LeftEdge -> do let ed2  = G.Edge v2 v1 (Integer 1)
                                               let g'' = G.deleteEdge g1 ed2
                                               if G.isEdgePresent g1 ed2
                                               then do
                                                    m''' <- execAttrStmt' m'' pm ss'' [] g (V.Graph g'', gtype)
                                                    return (m''', ss, Nothing)
                                               else 
                                                    error $ "There isn't any Edge between " ++ (show e2') ++ " and " ++ (show e1')
                else error $ "Incompatible types " ++ (show $ getType e1')  ++ " and " ++ (show $ getType e2')
            _ -> error "Wrong pattern" 

-- | Parse a string with escaped chars
parseString :: String -> String
parseString [] = []
parseString s = (c:cs)
        where [(c,s')] = readLitChar s
              cs = parseString s'


-- | Produce a register value to store in memory from a struct declaration
makeDefaultSetter :: ProgramMemory -> StructIdentifier -> Value
makeDefaultSetter pm si = Setter si (M.fromList (makeSetList pm sc))
    where
        makeSetList pm [] = []
        makeSetList pm ((n, t, mv):ds) = 
                                        case mv of
                                            Nothing -> (n, (t, def)) : makeSetList pm ds
                                                where def = case t of
                                                            GUserType t' -> makeDefaultSetter pm t'
                                                            _ -> defaultValue pm t
                                            Just v -> (n, (t, memval)) : makeSetList pm ds
                                                where memval = case v of
                                                                Setter i _ -> applySetter pm (makeDefaultSetter pm t') v t'
                                                                    where (GUserType t') = t
                                                                _ -> v
        (_,sc) = fetchStructDecl pm si

-- | Interpret struct declaration
interpretStructDecl :: Memory -> ProgramMemory -> Scopes -> StructDecl -> IO (StructIdentifier, StructContent)
interpretStructDecl m pm ss decl@(Struct (GUserType  i) _) = 
        do 
            sc <- interpretStructDecl' m pm ss decl
            let sc' = sc \\ (nubBy (\(i1,_,_) (i2,_,_) -> i1 == i2) sc)
            if sc' /= [] then error $ "Duplicity of field(s) " ++ (concat (map (\(i,_,_)->(i ++ " ")) sc')) ++ " in definition of struct " ++ i
            else return $ (i, sc)
    where
        interpretStructDecl' m pm ss (Struct t []) = return $ []
        interpretStructDecl' m pm ss (Struct t (d:ds)) = do
                                                            d' <- interpretVarDeclaration m pm ss d
                                                            remain <- interpretStructDecl' m pm ss (Struct t ds)
                                                            return $ d' ++ remain
            --case stmt of
                
-- | Interpret var declarations
interpretVarDeclaration :: Memory -> ProgramMemory -> Scopes -> VarDeclaration -> IO [(Name, GType, Maybe Value)]
interpretVarDeclaration m pm ss (VarDeclaration ns t es) = interpret m pm ss (VarDeclaration ns t es'')
    where 
        es'' = case fillReplicate ns es of
                    Nothing -> error "More expressions than identifiers in declaration"
                    Just l -> l
        interpret m pm ss (VarDeclaration [] t []) = do
                                                        return $ []
        interpret m pm ss (VarDeclaration ((Ident n):ns) t []) = do
                                                                remain <- interpret m pm ss (VarDeclaration ns t [])
                                                                return $ (n,t, Nothing) : remain
        interpret m pm ss (VarDeclaration ((Ident n):ns) t (e:es)) = do
                                                                (v, m', ss') <- eval m pm ss e
                                                                remain <- interpret m' pm ss' (VarDeclaration ns t es)
                                                                return $ (n, t, Just v) : remain
                
-- | Given two lists, l1 and l2, error if |l1|<|l2|, and fill l2 with its last element if |l2|<|l1|
fillReplicate :: [a] -> [b] -> Maybe [b]
fillReplicate [] [] = Just []
fillReplicate (_:xs) [] = Just []
fillReplicate [] (_:ys) = Nothing
fillReplicate (_:xs'@(x:xs)) [y] = 
                case fillReplicate xs' [y] of
                    (Just r) -> Just $ y : r
                    Nothing -> Nothing
fillReplicate (x:xs) (y:ys) = 
                case fillReplicate xs ys of
                    (Just r) -> Just $ y : r
                    Nothing -> Nothing



-- | Clear until subprogram scope.
clearScopesUntilSub :: Memory -> Scopes -> Maybe (Scopes, Memory)
clearScopesUntilSub m ss = clearScopesUntilType m (SubScope (-1)) ss

-- | Clear until an iteration scope.
clearScopesUntilIter :: Memory -> Scopes -> Maybe (Scopes, Memory)
clearScopesUntilIter m ss = clearScopesUntilType m (IterationScope (-1)) ss

-- | Clear scopes until the type of scope s is reached.
clearScopesUntilType :: Memory -> Scope -> Scopes -> Maybe (Scopes, Memory)
clearScopesUntilType m _ [] = Nothing
clearScopesUntilType m _ [GlobalScope] = Nothing
clearScopesUntilType m t@(SubScope _) (s@(SubScope _):ss) = Just (ss, m')
    where m' = clearScope t m
clearScopesUntilType m t@(SubScope _) (_:ss) = clearScopesUntilType m' t ss
    where m' = clearScope t m
clearScopesUntilType m t@(IterationScope _) (s@(IterationScope _):ss) = Just (ss, m')
    where m' = clearScope t m
clearScopesUntilType m t@(IterationScope _) (_:ss) = clearScopesUntilType m' t ss
    where m' = clearScope t m
clearScopesUntilType m t (s@(BlockScope _):ss) = clearScopesUntilType m' t ss
    where m' = clearScope t m

-- | Executes a subprogram
execSubprogram :: Memory -> ProgramMemory -> Scopes -> (SubIdentifier, SubContent) -> ProcessedActualParams -> IO (Memory, Scopes, Maybe Value)
execSubprogram m pm ss sub@(ident,content@(_,_,block)) as = do
                                                                (m'',ss'',v) <- execBlockSimple block m pm SubScope ss declarations
                                                                return (m'',ss'',v)
                                                        where declarations = prepareSubcallElabs m pm ss content as
                            
-- | Prepare elaboration for subprogram call.
-- Calls must be in the form: 
--      f(x1,x2,x3,...,xn,n1,n2,n3,...,nm)
-- where xi is an ordered parameter, and nj is a named parameter.
-- Cases follow this semantic:
--      - non single identifier parameter passing (expressions but identifiers alone): 1, [1,2], a+2...
--          - just take the value
--      - single identifier parameter passing: a,b,c...
--          - check if formal asks for a reference or a value and answers accordingly
--      - a named parameter: a=2, b=2,...
--          - remove parameter from parameters list
-- When actual arguments finalize, it is important to check optional formal parameters declarations
prepareSubcallElabs :: Memory -> ProgramMemory -> Scopes -> SubContent -> ProcessedActualParams -> [(Name, Cell)]
prepareSubcallElabs m pm ss ([], mt, b) [] = []
prepareSubcallElabs m pm ss ((f@(n,pt,mv)):fs, mt, b) [] = 
                    case mv of
                        Nothing -> prepareSubcallElabs m pm ss (fs, mt, b) []
                        Just v -> (n, (getType v, v')) : prepareSubcallElabs m pm ss (fs, mt, b) []
                            where (_,v') = makeCompatibleAssignTypes pm (getParamGType pt) v
prepareSubcallElabs m pm ss (fs'@((f@(n,pt,mv)):fs), mt, b) ((a,t):as) = case a of
                                  Right v -> (n, (getParamGType pt, v')) : prepareSubcallElabs m pm ss (fs, mt, b) as
                                            where (_,v') = makeCompatibleAssignTypes pm (getParamGType pt) v
                                  Left ((Ident i), Left ci@(n',s')) -> case pt of 
                                                                (GRef _) -> (n, v') : prepareSubcallElabs m pm ss (fs, mt, b) as
                                                                    where v' = case fetchCellByScope m n' s' of
                                                                                    Left i -> error i
                                                                                    Right ci'@(t,v'') -> case v'' of 
                                                                                                --Register _ -> (t,Ref ci)
                                                                                                Value _ -> (t,Ref ci)
                                                                                                Ref ci'' -> (t, Ref ci'')
                                                                (GType _) -> (n, (getParamGType pt, v')) : prepareSubcallElabs m pm ss (fs, mt, b) as
                                                                    where   (_,v') = makeCompatibleAssignTypes pm (getParamGType pt) v''
                                                                            v'' = case getVarScopeValue m n' s' of
                                                                                    Left i -> error i
                                                                                    Right v'' -> v''
                                  Left ((Ident i), Right (Just v)) ->   
                                                    if existsFormal i fs' then 
                                                        (i, (t, v')) : prepareSubcallElabs m pm ss (removeFormal i fs', mt, b) as
                                                    else 
                                                        error $ "Named parameter " ++ i ++ " doesnt match any unbound formal parameter in the call"
                                                        where (_,v') = makeCompatibleAssignTypes pm (getParamGType pt) v

-- | Remove a formal parameter from a list of formal parameters
removeFormal :: Name -> [FormalParameter] -> [FormalParameter]
removeFormal _ [] = []
removeFormal n ((f@(n',_,_)):fs) = if n /= n' then f:removeFormal n fs else fs

-- | Tests if there exists a formal parameter with the given name
existsFormal :: Name -> [FormalParameter] -> Bool
existsFormal n = or . map (\(n',_,_) -> n == n')


-- | Process list of actual parameters from a subprogram call.
--
processSubArgs :: [SubprogArg] -> [Identifier] -> Memory -> ProgramMemory -> Scopes -> IO [(Either (Identifier, Either CellIdentifier (Maybe Value)) Value, GType)]
processSubArgs [] _ _ _ _ = return []
processSubArgs (a:as) ids m pm ss = case a of
                                       ArgExpr expr -> do   remaining <- processSubArgs as ids m pm ss
                                                            if ids == [] then
                                                                case expr of 
                                                                    ArithTerm (IdTerm id@(Ident i)) -> return $ (Left (id, Left ci), t): remaining
                                                                        where 
                                                                              (ci, (t,tc)) = case fetchVar m i ss of
                                                                                                Left err -> error err
                                                                                                Right cell -> cell
                                                                    _ -> do 
                                                                            (ev, tv, m', ss') <- evalWithType m pm ss expr
                                                                            --(ev, m', ss') <- eval m pm ss expr
                                                                            case ev of 
                                                                                --setter@(Setter _) -> return $ (Right setter, GAnonymousStruct) : remaining
                                                                                _ -> return $ (Right ev, tv) : remaining
                                                            else error "Optional parameter before ordered parameter"
                                       ArgIdentAssign (IdentAssign [i] expr) -> do
                                                                        --(ev, m', ss') <- eval m pm ss expr
                                                                        (ev, tv, m', ss') <- evalWithType m pm ss expr
                                                                        remaining <- processSubArgs as (i:ids) m' pm ss'
                                                                        if elem i ids then error "Multiple assignment to same parameter"
                                                                            else case ev of
                                                                                --setter@(Setter _) -> () : remaining
                                                                                _ -> return $ (Left (i, Right (Just ev)), tv) : remaining
 
data AccessType = ListIndex Value | DictIndex Value | StructField Name deriving (Show, Eq)

evalWithType :: Memory -> ProgramMemory -> Scopes -> ArithExpr -> IO (Value, GType, Memory, Scopes)
evalWithType m pm ss e =
    case e of
        (ArithTerm (IdTerm (Ident i))) -> 
                case fetchVarValueType m i ss of
                    Left i -> error i
                    Right i@(t,v) -> return $ (v,t, m, ss)
        _ -> do
                (v', m', ss') <- eval m pm ss e 
                return $ (v', getType v', m', ss')

execAttrStmt' :: Memory -> ProgramMemory -> Scopes -> [AccessType] -> ArithExpr -> (Value,GType) -> IO Memory
execAttrStmt' m pm ss as lhs rhs@(vr,tr) = 
    do 
        case lhs of
            (ArithTerm (IdTerm (Ident i))) -> do
                                                let var = fetchVarValueType m i ss in
                                                    case var of
                                                        Left e -> error e
                                                        Right (tl,val) -> do 
                                                                v' <- return $ backwardAccessUpdate m pm ss as i val tl tr vr
                                                                case updateVar m i ss (tl, Value v') of 
                                                                    Right m' -> do return m'   
                                                                    Left i -> error i
            (ListAccess remaining index) -> do
                                                (index', m', ss') <- eval m pm ss index
                                                execAttrStmt' m' pm ss' ((ListIndex index'):as) remaining rhs
            (DictAccess remaining index) -> do
                                                (index', m', ss') <- eval m pm ss index
                                                execAttrStmt' m' pm ss' ((DictIndex index'):as) remaining rhs
            (StructAccess remaining (Ident field)) -> 
                                            do
                                                execAttrStmt' m pm ss ((StructField field):as) remaining rhs
            _ -> error $ "Not a valid lhs"

    where
        backwardAccessUpdate m pm ss [] ident memval memtype rightType v =  if checkCompatType (memtype) (rightType) then v else
                                                          error $ "Incompatible types " ++ show (memtype) ++ " and " ++ show rightType
        backwardAccessUpdate m pm ss (a:as) ident memval memtype rightType v = 
            case a of
                ListIndex index -> case index of
                                        (Integer int) -> 
                                                case memval of
                                                    (List xs) -> List (setElemList xs int v'')
                                                        where v'' = backwardAccessUpdate m pm ss as ident (xs !! fromInteger int) (uncapsulate Nothing memtype) rightType v
                                                    (String xs) -> String (setElemList xs int c)
                                                        where v''@(Char c) = backwardAccessUpdate m pm ss as ident (Char (xs !! fromInteger int)) (uncapsulate Nothing memtype) rightType v
                                                    _ -> error "You must access a list or a string." 
                                        _ -> error "List index must be an integer." 
                DictIndex index -> case memval of
                                        (Map m') -> case as of
                                                    [] ->  Map (M.insert index v'' m')
                                                       where v'' = if checkCompatType (uncapsulate Nothing memtype) (getType v) then v else error $ "Incompatible types " ++ show (uncapsulate Nothing memtype) ++ " and " ++ show rightType
                                                    _  -> Map (M.insert index v'' m')
                                                       where v'' = backwardAccessUpdate m pm ss as ident (m' M.! index) (uncapsulate Nothing memtype) rightType v
                                        _ -> error "You must access a dictionary"
                StructField field -> case memval of
                                        (Setter si m') -> if M.notMember field m' then error $ field ++ " not in the struct"
                                                        else Setter si (M.insert field (t,v'') m')
                                                where v'' = backwardAccessUpdate m pm ss as ident v''' t rightType v
                                                      (t,v''') = m' M.! field
                                        _ -> error "You must access a struct field"

-- | Auxiliar for execting attribute statements
execAttrStmt :: Stmt -> Memory -> ProgramMemory -> Scopes -> IO Memory
execAttrStmt (AttrStmt [] []) m pm ss = return m
execAttrStmt (AttrStmt t'@(t:ts) v'@(v:vs)) m pm ss = do
                                                    let v'' = case fillReplicate t' v' of
                                                                Nothing -> error "Too many expressions in multiple attribution"
                                                                Just v'' -> v''
                                                    execMultiAttrStmt t' v'' m pm ss
    where
        execMultiAttrStmt [] [] m pm ss = return m
        execMultiAttrStmt t'@(t:ts) v'@(v:vs) m pm ss = do
                                                                        rhs@(vr,tr,m'',ss'') <- evalWithType m pm ss v
                                                                        m' <- execAttrStmt' m'' pm ss'' [] t (vr,tr)
                                                                        execMultiAttrStmt ts vs m' pm ss''
                                                                                                        
-- |From value, guaarantee boolean value
makeBooleanFromValue :: Value -> Either String Bool
makeBooleanFromValue (Bool v) = Right v
makeBooleanFromValue _        = Left "Expected boolean value"

{--
-- | Check types for compatibility
checkCompatType :: GType -> GType -> Bool
checkCompatType t t' = if t == t' then True
                    else case (t,t') of
                        (GFloat, GInteger)  -> True
                        _                   -> checkCompatType' t t'

checkCompatType' :: GType -> GType -> Bool
checkCompatType' t t' = if t == t' then True
                     else case (t,t') of
                        (GGraphVertexEdge v1 _, GGraphVertexEdge v2 GEdgeEmpty) -> checkCompatType' v1 v2--(v1 == v2)
                        (GGraphVertexEdge v2 GEdgeEmpty, GGraphVertexEdge v1 _) -> checkCompatType' v1 v2--(v1 == v2)(v1 == v2)
                        (GGraphVertexEdge _ _, GGraphEmpty) -> True
                        (GGraphEmpty, GGraphVertexEdge _ _) -> True
                        (GGraphVertexEdge v1 e1, GGraphVertexEdge v2 e2) -> checkCompatType' e1 e2 && checkCompatType' v1 v2--(e1 == e2) && (v1 == v2)
                        (GList _, GListEmpty)     -> True
                        (GListEmpty ,GList _)     -> True
                        (GDict _ _, GDictEmpty)   -> True
                        (GDictEmpty, GDict _ _ )  -> True
                        (GDict k1 v1, GDict k2 v2)  -> checkCompatType' k1 k2 && checkCompatType' v1 v2--(e1 == e2) && (v1 == v2)
                        (GList l, GList l') -> checkCompatType' l l'
                        --(GList l, GList l') -> checkCompatType (GList l) l'
                        --(GList l, GList l') -> checkCompatType (l) $ GList l'
                        (GUserType _, GAnonymousStruct) -> True
                        --(GAnonymousStruct, GUserType _) -> True
                        (GPair p1 p2, GPair p1' p2') -> (checkCompatType' p1 p1') && (checkCompatType' p2 p2')
                        (GTriple p1 p2 p3, GTriple p1' p2' p3') -> (checkCompatType' p1 p1') && (checkCompatType' p2 p2') && (checkCompatType' p3 p3')
                        (GQuadruple p1 p2 p3 p4, GQuadruple p1' p2' p3' p4') -> (checkCompatType' p1 p1') && (checkCompatType' p2 p2') && (checkCompatType' p3 p3') && (checkCompatType' p4 p4')
                        _                   -> False

--}

                        
-- | Given type t and value v, return (t,v) if they are compatible.
makeCompatibleAssignTypes :: ProgramMemory -> GType -> Value -> (GType, MemoryValue)
makeCompatibleAssignTypes pm t@(GList _) v@(List []) = (t, Value v)
makeCompatibleAssignTypes pm t@(GUserType u) v@(Setter is m) = (t, Value $ applySetter pm (makeDefaultSetter pm u) v u)
makeCompatibleAssignTypes pm t@(GGraphEmpty) v =
    let tv = getType v
    in case tv of
        GGraphEmpty          -> (t, Value v)
        GGraphVertexEdge _ _ -> (tv, Value v)
        v'                   -> error ("Incompatible types " ++ show t ++ " and " ++ show v')

makeCompatibleAssignTypes pm t@(GGraphVertexEdge vertices edges) v = 
    let tv = getType v
    in case tv of
        GGraphEmpty                        -> (t,Value v)
        GGraphVertexEdge vertex GEdgeEmpty -> if checkCompatType vertices vertex--vertices == vertex 
                                              then (t,Value v)
                                              else error ("Incompatible types " ++ show t ++ " and " ++ show v)
        GGraphVertexEdge vertex edge       -> if checkCompatType vertices vertex && checkCompatType edges edge--vertices == vertex && edges == edge
                                              then (t,Value v)
                                              else error ("Incompatible types " ++ show t ++ " and " ++ show tv)
        v'                                 -> error ("Incompatible types " ++ show t ++ " and " ++ show v')
makeCompatibleAssignTypes pm t v = (t, Value $ coerceAssignByType pm t v)

coerceAssignByType :: ProgramMemory -> GType -> Value -> Value
coerceAssignByType pm GFloat i'@(Integer i) = coerce i'
coerceAssignByType pm (GUserType ut) s'@(Setter "" m) = applySetter pm (makeDefaultSetter pm ut) s' ut
coerceAssignByType pm t v = if checkCompatType t t' then v
                   else error ("Incompatible types " ++ show t ++ " and " ++ show t')
                    where
                        t' = getType v

-- | Given a setter, a user type name and a register (memory value), produce a new register
-- The value is the setter.
applySetter :: ProgramMemory -> Value -> Value -> StructIdentifier -> Value
applySetter pm (Setter ir r) (Setter is msetter) si = Setter ir (updateRegister (M.toList msetter) r)
        where
                updateRegister :: [(String, (GType, Value))] -> M.Map Name (GType, Value) -> M.Map Name (GType, Value)
                updateRegister [] m = m
                updateRegister ((s,(t,v)):ss) m 
                            | M.member s m = case v of
                                                (Setter is' setter') -> updateRegister ss m''
                                                    where m'' = M.update (\k -> Just (t',applySetter pm r' (Setter is' setter') is')) s m
                                                          (t',r') = m M.! s
                                                _ -> updateRegister ss m''  
                                                    where (t',_) = m M.! s
                                                          m'' = M.update (\k -> Just (t'',v')) s m  
                                                          (t'', Value v') = makeCompatibleAssignTypes pm t' v
                            | otherwise = error $ "Type " ++ si ++ " doesn't have field " ++ s     

-- | Set the i-element of a list.
setElemList :: Integral i => [a] -> i -> a -> [a]
setElemList [] i k 
    | i >= 0 = error "Index out of range"
    | otherwise = []
setElemList (x:xs) 0 k = k : xs
setElemList (x:xs) i k = x : setElemList xs (i-1) k

-- |Executes a declaration statement.
varDeclStmt :: Stmt -> Memory -> ProgramMemory -> Scopes -> IO Memory
varDeclStmt (DeclStmt (VarDeclaration (x:xs) t [])) m pm ss = do 
                                                            case elabVar (head ss) ((\(Ident x) -> x) x) (makeCompatibleAssignTypes pm t (defaultValue pm t)) m of
                                                                (Left i) -> error i
                                                                (Right m') -> varDeclStmt (DeclStmt (VarDeclaration xs t [])) m' pm ss 
varDeclStmt (DeclStmt (VarDeclaration [] t [])) m pm ss = do 
                                                                return m
varDeclStmt (DeclStmt (VarDeclaration [] t (_:es))) m pm ss =     do 
                                                                    error "Too many expressions in right side."
varDeclStmt (DeclStmt (VarDeclaration (x:xs'@(y:xs)) t (e:[]))) m pm ss = do 
                                                                do
                                                                    (v, m', ss') <- eval m pm ss e
                                                                    case elabVar (head ss) ((\(Ident x) -> x) x) (makeCompatibleAssignTypes pm t v) m' of
                                                                        (Left i) -> error i
                                                                        (Right i) -> varDeclStmt (DeclStmt (VarDeclaration xs' t (e:[]))) i pm ss'
varDeclStmt (DeclStmt (VarDeclaration (x:xs) t (e:es))) m pm ss = do 
                                                                    (v, m', ss') <- eval m pm ss e
                                                                    case elabVar (head ss) ((\(Ident x) -> x) x) (makeCompatibleAssignTypes pm t v) m' of
                                                                        (Left i) -> error i
                                                                        (Right i) -> varDeclStmt (DeclStmt (VarDeclaration xs t es)) i pm ss'

-- | Interpret subprogram declaration
interpretSubDeclaration :: Subprogram -> Memory -> ProgramMemory -> Scopes -> IO (SubIdentifier, SubContent)
interpretSubDeclaration (Subprogram (Ident i) ps t b) m pm ss = 
    do 
        formals <- formalParams ps
        let formals' = formals \\ (nubBy (\(i1,_,_) (i2,_,_) -> i1 == i2) formals)
        if formals' /= [] then error $ "Duplicity of formal parameter(s) " ++ (concat (map (\(i,_,_)->(i ++ " ")) formals')) ++ " in subprogram " ++ i
        else return $ ((i, paramTypes ps), (formals, t, b)) 
    where   
            paramTypes [] = []
            paramTypes ((ParamDeclaration ids gt _):ps) = replicate (length ids) gt ++ paramTypes ps

            formalParams :: [ParamDeclaration] -> IO [(String, GParamType, Maybe Value)]
            formalParams [] = return []
            formalParams (p:ps) = do interp <- (interpretParamDeclaration p m pm ss) 

                                     remain <- formalParams ps
                                     return $ interp ++ remain


-- | Interpret formal parameters
interpretParamDeclaration :: ParamDeclaration -> Memory -> ProgramMemory -> Scopes -> IO [(String, GParamType, Maybe Value)]
interpretParamDeclaration pd@(ParamDeclaration is _ es) m pm ss 
    | length es > length is = error "Too many default values"
    | otherwise = interpretParamDeclaration' pd m pm ss
                    
    where
        interpretParamDeclaration' (ParamDeclaration [] _ []) _ _ _ = return []
        interpretParamDeclaration' (ParamDeclaration [(Ident i)] gt [e]) m pm ss = 
                                                        do  (v, m', ss') <- eval m pm ss e 
                                                            return [(i, gt, Just v)]
        interpretParamDeclaration' (ParamDeclaration ((Ident i):is) gt []) m pm ss = 
                                                        do  remain <- interpretParamDeclaration (ParamDeclaration is gt []) m pm ss
                                                            return $ (i, gt, Nothing) : remain
        interpretParamDeclaration' (ParamDeclaration ((Ident i):is) gt [e]) m pm ss = 
                                                        do  (v, m', ss') <- eval m pm ss e 
                                                            remain <- interpretParamDeclaration (ParamDeclaration is gt [e]) m' pm ss'
                                                            return $ (i, gt, Just v) : remain
        interpretParamDeclaration' (ParamDeclaration ((Ident i):is) gt (e:es)) m pm ss = 
                                                        do  (v, m', ss') <- eval m pm ss e 
                                                            remain <- interpretParamDeclaration (ParamDeclaration is gt es) m' pm ss'
                                                            return $ (i, gt, Just v) : remain
 --Get type of a List and make coercion if needed                
getListType :: [Value] -> GType
getListType [x]      = getType x
getListType (x:y:xs) = if checkCompatType' (getType x) ( getType y)
                       then getListType (y:xs)
                       else error "Invalid List Type"   
               
-- | Return the type of a given Gryph value.
getType :: Value -> GType
getType (Integer i)                   = GInteger
getType (Float f)                     = GFloat
getType (String s )                   = GString
getType (Char c)                      = GChar
getType (Bool b)                      = GBool
getType (List [])                     = GListEmpty
getType (List (x:xs))                 = GList (getListType (x:xs))
getType (Map (m))                     = if M.null m then GDictEmpty else GDict ( getType (head (M.keys m))) ( getType (head (M.elems m)))
getType (Pair (v1,v2))                = GPair (getType v1) (getType v2 )
getType (Triple (v1,v2, v3))          = GTriple (getType v1) (getType v2 ) (getType v3)
getType (Quadruple (v1,v2, v3, v4))   = GQuadruple (getType v1) (getType v2 ) (getType v3) (getType v4)
getType (Setter "" _ )                = GAnonymousStruct
getType (Setter s _ )                 = GUserType s
getType (V.Graph (G.Graph vertices edges)) = 
    if Se.null vertices
    then GGraphEmpty
    else
        if M.null edges
        then GGraphVertexEdge (getType $ getVertexValue vertices) GEdgeEmpty
        else GGraphVertexEdge (getType $ getVertexValue vertices) (getType $ getEdgeValue edges)

-- | Return the type of vertex value
getVertexValue :: (Se.Set (G.Vertex Value)) -> Value
getVertexValue vertices = v
    where
        ( G.Vertex _ v : _ ) = Se.elems vertices 

-- | Return the type of edge load
getEdgeValue :: (M.Map Int [G.Edge Value Value]) -> Value
getEdgeValue edges = v
    where
        ( (G.Edge _ _ v) : _ ) = head $ M.elems edges

-- | Return the type dictionary keys
getKeyType :: Value -> GType
getKeyType (Map m) = getType (head (M.keys m))

-- | Return the type of dictionary values
getValueType :: Value -> GType 
getValueType (Map m) = getType (head (M.elems m))

coerce:: Value -> Value
coerce (Integer x) = (Float (fromInteger x))
coerce v = v

-- | Evaluates a list of expressions
evalList :: Memory -> ProgramMemory -> Scopes -> [ArithExpr] -> IO ([Value], Memory, Scopes)
evalList m pm ss [x]      =  do {(x', m', ss') <- eval m pm ss x ; return $ ([x'], m', ss')}
evalList m pm ss (x:y:xs) =  do 
                                  (z, m', ss')  <- eval m pm ss x
                                  (y', m'', ss'') <- eval m' pm ss' y
                                  if not( checkCompatType (getType y') ( getType z) || checkCompatType (getType z) (getType y') ) then error "Type mismatch in List "
                                    else do
                                       (l, m''', ss''') <- evalList m'' pm ss''  (y:xs)
                                       return ((z:l), m''', ss''') 

-- | Evaluates a tuple
evalTuple :: Memory -> ProgramMemory -> Scopes -> [ArithExpr] -> IO [Value]
evalTuple m pm ss [x] = do {(x', m', ss') <-eval m pm ss x; return $ [x']}
evalTuple m pm ss (x:xs) = do   (z, m', ss') <- eval m pm ss x
                                t <- evalTuple m' pm ss' xs
                                return $ (z:t)

-- | Evaluates a dictionary
evalDict :: Memory -> ProgramMemory -> Scopes -> [DictEntry] -> M.Map Value Value -> IO (M.Map Value Value)
evalDict m pm ss [] m1                   = return $ M.empty
evalDict m pm ss ((k1,v1):(k2,v2):xs) m1 = 
                                         do     (ek1, m', ss') <- eval m pm ss k1
                                                (ek2, m'', ss'') <- eval m' pm ss' k2
                                                (ev1, m''', ss''') <- eval m'' pm ss'' v1
                                                (ev2, m'''', ss'''') <- eval m''' pm ss''' v2
                                                if (getType ek1) == (getType ek2) && (getType ev1 == getType ev2)
                                                 then do
                                                        d <- evalDict m'''' pm ss'''' ((k2,v2):xs) m1
                                                        return $ M.insert ek1 ev1 d
                                                else error "Dict Type mismatch"
evalDict m pm ss ((k,v):xs) m1           = 
                                        do      (k', m', ss')   <- eval m pm ss k
                                                (v', m'', ss'') <- eval m' pm ss' v
                                                d <- evalDict m'' pm ss'' xs m1
                                                return $ M.insert k' v' d

-- | Binary operation evaluator
evalBinOp ::Memory -> ProgramMemory -> Scopes -> ArithExpr ->( Value -> Value -> Value ) -> String -> IO (Value, Memory, Scopes)
evalBinOp m pm ss (ArithBinExpr _  e1 e2) f op = 
                                            do
                                                (v1, m', ss')    <- eval m pm ss e1
                                                (v2, m'', ss'')  <- eval m' pm ss' e2
                                                (k, m''', ss''') <- eval m'' pm ss'' e2
                                                case v1 of
                                                     l1@(List (x:xs)) -> if (t1 == t2) then return $ (f l1 k, m''', ss''')
                                                                         else error $ "Type mismatch " ++ show t1 ++ op ++ show t2 ++ " operation"
                                                                          where t1 = getType k
                                                                                t2 = getType x 
                                                     k -> case v2 of 
                                                           l2@(List (x:xs) ) -> if (t1 == t2) then return $ (f l2 k, m''', ss''')
                                                                         else error $ "Type mismatch " ++ show t1 ++ op ++ show t2 ++ " operation"
                                                                          where t1 = getType k
                                                                                t2 = getType x
                                                           x -> return ( f k x, m''', ss''') 
                                                           --x -> if (getType k == getType x) then return $ f k  x
                                                             --            else error "Type mismatch  operation"
-- | Convert a string to other type
fromString::  String  -> GType -> Value
fromString s (GInteger )        = Integer (read s::Integer)
fromString s (GFloat  )         = Float (read s::Double)
fromString s (GChar )           = Char (read s::Char)
fromString s (GBool )           = Bool (read s::Bool)
fromString _ _                  = error "No parser from String"


cast:: Value -> GType -> Value
cast v1 g@GString     = case v1 of
                         (List l1) -> if (getListType l1) == GChar then String (map (\(Char c) -> c)  l1) else String (show v1)  
                         (Char c)  -> String [c] 
                         _         -> String (show v1) 
cast v1 g@GInteger    = case v1 of
                         Float f   -> Integer ( floor f)
                         Bool  b   -> if b then Integer 1 else Integer 0
                         String s  -> fromString s g
                         _         -> error "No cast avaliable"
cast v1 g@GFloat      = case v1 of
                         Integer i -> Float (fromInteger i)
                         String s  -> fromString s g
                         _         -> error "No cast avaliable"
cast v1 g@GBool       = case v1 of
                         Integer i -> if i /= 0 then Bool True else Bool False
                         Float   f -> if f /= 0 then Bool True else Bool False
                         String  s -> fromString s g
                         _         -> error "No cast avaliable"

cast v1 g@(GList GFloat)  = case v1 of 
                             List [] -> List []
                             l@(List (x:xs)) -> List ((cast x GFloat):r )
                              where List r = (cast (List xs) (GList GFloat) )
                             _               -> error "No cast avaliable"
cast v1 g@(GList GInteger)  = case v1 of 
                               List [] -> List []
                               l@(List (x:xs)) -> List ((cast x GInteger):r )
                                where List r = (cast (List xs) (GList GInteger) )
                               _               -> error "No cast avaliable"
cast v1 g@(GList GBool)  = case v1 of 
                               List [] -> List []
                               l@(List (x:xs)) -> List ((cast x GBool):r )
                                where List r = (cast (List xs) (GList GBool) )
                               _               -> error "No cast avaliable"
cast v1 g@(GList GString)  = case v1 of 
                               List [] -> List []
                               l@(List (x:xs)) -> List ((cast x GString):r )
                                where List r = (cast (List xs) (GList GString) )
                               _               -> error "No cast avaliable"
cast _   _          =  error "Unknown type"

-- | Main expression evaluator
eval :: Memory -> ProgramMemory -> Scopes -> ArithExpr -> IO (Value, Memory, Scopes)
eval m pm ss (ArithTerm (LitTerm (Lit v)))      = return (v, m, ss)
eval m pm ss (ArithUnExpr MinusUnOp e)          = do {(v, m', ss') <- eval m pm ss e ; return $ (minusUn v, m', ss')}
eval m pm ss (ArithUnExpr PlusUnOp e)           = do {(v, m', ss') <- eval m pm ss e; return $ (plusUn v, m', ss')}
eval m pm ss (ArithUnExpr NotUnOp e)            = do {(v, m', ss') <- eval m pm ss e; return $ (not' v, m', ss')}
eval m pm ss (ArithBinExpr MinusBinOp  e1 e2)   = evalBinOp m pm ss (ArithBinExpr MinusBinOp e1 e2)   minusBin  (" - ")
eval m pm ss (ArithBinExpr PlusBinOp  e1 e2)    = evalBinOp m pm ss (ArithBinExpr PlusBinOp e1 e2)   plusBin    (" + ")
eval m pm ss (ArithBinExpr TimesBinOp  e1 e2)   = evalBinOp m pm ss (ArithBinExpr TimesBinOp e1 e2)  timesBin   (" * ")
eval m pm ss (ArithBinExpr DivBinOp  e1 e2)     = evalBinOp m pm ss (ArithBinExpr DivBinOp e1 e2)    divBin     (" / ")
eval m pm ss (ArithBinExpr ExpBinOp  e1 e2)     = evalBinOp m pm ss (ArithBinExpr ExpBinOp e1 e2)    expBin     (" ^ ")
eval m pm ss (ArithBinExpr ModBinOp  e1 e2)     = evalBinOp m pm ss (ArithBinExpr ModBinOp e1 e2)    modBin     (" % ")
eval m pm ss (ExprLiteral (ListLit [] ))        = return $ (List [], m, ss)
eval m pm ss (ExprLiteral (ListLit es ))        = do {(v, m', ss') <- evalList m pm ss es; if getListType v == GFloat then return $(List (map coerce v), m', ss')  else  return $ (List v, m', ss')}
eval m pm ss (ExprLiteral (DictLit de))         = do {v <- evalDict m pm ss de M.empty ; return $ (Map v, m, ss)}
eval m pm ss (ArithEqExpr Equals e1 e2)         = do {(v1, m', ss') <- eval m pm ss e1;( v2, m'', ss'') <- eval m' pm ss' e2; return $ (Bool (v1 == v2), m'', ss'') }
eval m pm ss (ArithEqExpr NotEquals e1 e2)      = do {(v1, m', ss') <- eval m pm ss e1;( v2, m'', ss'') <- eval m' pm ss' e2; return $ (Bool (v1 /= v2), m'', ss'')} 
eval m pm ss (ArithRelExpr Greater e1 e2)       = do {(v1, m', ss') <- eval m pm ss e1;( v2, m'', ss'') <- eval m' pm ss' e2; return $ (Bool (v1 > v2), m'', ss'')}  
eval m pm ss (ArithRelExpr GreaterEq e1 e2)     = do {(v1, m', ss') <- eval m pm ss e1;( v2, m'', ss'') <- eval m' pm ss' e2; return $ (Bool (v1 >= v2), m'', ss'')}  
eval m pm ss (ArithRelExpr Less e1 e2)          = do {(v1, m', ss') <- eval m pm ss e1;( v2, m'', ss'') <- eval m' pm ss' e2; return $ (Bool (v1 < v2), m'', ss'')}   
eval m pm ss (ArithRelExpr LessEq e1 e2)        = do {(v1, m', ss') <- eval m pm ss e1;( v2, m'', ss'') <- eval m' pm ss' e2; return $ (Bool (v1 <= v2), m'', ss'')}    
eval m pm ss (ArithRelExpr In e1 e2)            = do 
                                                    (e1', m', ss')   <- eval m  pm ss  e1
                                                    (e2', m'', ss'') <- eval m' pm ss' e2
                                                    case e2' of
                                                        V.Map dict -> case getType e2' of
                                                                         GDict ks _ -> if getType e1' == ks 
                                                                                       then return $ (Bool (M.member e1' dict), m'', ss'')
                                                                                       else error $ "Incompatible key type between " ++ (show $ getType e1') ++ " and " ++ (show ks) 
                                                                         _          -> error "Type error"
                                                        V.Graph g@(G.Graph vs _) -> case Se.toList vs of
                                                                                        [] -> error "Empty Graph"
                                                                                        (Vertex _ v : _) -> if getType e1' == getType v
                                                                                                            then do let G.Vertex id _ = G.getVertexFromValue g e1' False
                                                                                                                    return $ ( Bool $ not (id == (-1)), m'', ss'')
                                                                                                            else error $ "Incompatible key type between " ++ (show $ getType e1') ++ " and " ++ (show $ getType v) 
                                                        List l -> case l of
                                                                    []      -> error "Empty List"
                                                                    (x : _) -> if getType x == getType e1'
                                                                               then return $ (Bool $ L.elem e1' l, m'', ss'')
                                                                               else error $ "Incompatible key type between " ++ (show $ getType e1') ++ " and " ++ (show $ getType x) 
                                                        _          -> error $ "Operation not supported for " ++ (show $ getType e2')
eval m pm ss (LogicalBinExpr And e1 e2)         = 
                                                do      (v1, m', ss')   <- eval m pm ss e1
                                                        (v2, m'', ss'') <- eval m' pm ss' e2  
                                                        case v1 of
                                                            Bool b1 -> case v2 of
                                                                        Bool b2 -> return $ (Bool ( b1 && b2), m'', ss'')
                                                                        _ -> error "And operator rhs type error"
                                                            _ -> error "And operator lhs type error"
eval m pm ss (LogicalBinExpr Or e1 e2)          = 
                                                do      (v1, m', ss')   <- eval m pm ss e1
                                                        (v2, m'', ss'') <- eval m' pm ss' e2  
                                                        case v1 of
                                                            Bool b1 -> case v2 of
                                                                        Bool b2 -> return $ (Bool ( b1 || b2), m'', ss'')
                                                                        _ -> error "And operator rhs type error"
                                                            _ -> error "And operator lhs type error "
eval m pm ss (LogicalBinExpr Xor e1 e2)         = 
                                                do      (v1, m', ss')   <- eval m pm ss e1
                                                        (v2, m'', ss'') <- eval m' pm ss' e2  
                                                        case v1 of
                                                            Bool b1 -> case v2 of
                                                                        Bool b2 -> return $ (Bool (((not b1) &&  b2 ) || (b1 && (not b2))), m'', ss'')
                                                                        _ -> error "And operator rhs type error"
                                                            _ -> error "And operator lhs type error "
eval m pm ss (CastExpr e1 g)                    =
                                                do      (v1, m', ss') <- eval m pm ss e1 
                                                        return $ (cast v1 g, m', ss')


eval m pm ss (ExprLiteral (TupleLit te))        = 
                                                do
                                                    l <- evalTuple m pm ss te
                                                    return $ if length l == 2 then (Pair ((l !! 0), (l !! 1)), m, ss)
                                                       else if length l == 3 then (Triple ((l !! 0), (l !! 1), (l !! 2)), m, ss)
                                                            else if length  l == 4 then (Quadruple ((l !! 0), (l !! 1), (l !! 2), (l !! 3)), m, ss)
                                                             else error "Limit of Quadruples"

eval m pm ss (GraphAccess e1 e2 )               =
                                                do
                                                    (g, m', ss')   <- eval m pm ss e1
                                                    (v, m'', ss'') <- eval m' pm ss' e2
                                                    case g of
                                                        (V.Graph g@(G.Graph vertices edges)) ->
                                                            do  let (G.Vertex id v') = G.getVertexFromValue g v False
                                                                if id == -1
                                                                then error $ "The Vertex " ++ (show v') ++ " does not exist"
                                                                else do
                                                                    let e = edges M.!? id
                                                                    case e of
                                                                        Nothing  -> return $ (List [], m'', ss'')
                                                                        Just edg -> return $ (List $ makeAdjList edg, m'', ss'')
                                                        _ -> error "Access Graph mismatch"
                                                    where
                                                        makeAdjList [] = []
                                                        makeAdjList ( G.Edge _ (G.Vertex _ v ) _ : xs) = v : makeAdjList xs 

eval m pm ss (GraphEdgeAccess g (S.Edge edgetype v1 v2) ) = do
                                                (g', m', ss')  <- eval m pm ss g
                                                case g' of
                                                    (V.Graph g@(G.Graph vertices edges)) -> do
                                                        (v1', m'', ss'') <- eval m' pm ss' v1
                                                        (v2', m''', ss''') <- eval m'' pm ss'' v2
                                                        let vertex1@(G.Vertex id v1'') = G.getVertexFromValue g v1' False
                                                        if id == -1
                                                            then error $ "The Vertex " ++ (show v1'') ++ " does not exist"
                                                        else 
                                                            do  let vertex2@(G.Vertex id2 v2'') = G.getVertexFromValue g v2' False
                                                                if id2 == -1
                                                                    then error $ "The Vertex " ++ (show v2'') ++ " does not exist"
                                                                else 
                                                                    do let e = edges M.!? id
                                                                       case e of
                                                                            Nothing  -> error $ "There is no Edge between " ++ (show v1'') ++ " and " ++ (show v2'') ++ " does not exist"
                                                                            Just edg -> return $ (getWeightEdge vertex1 vertex2 edg, m''', ss''')
                                                    _ -> error "Access Graph mismatch"

                                                where
                                                    getWeightEdge v1 v2 [] = error $ "There is no Edge between " ++ (show v1) ++ " and " ++ (show v2) ++ " does not exist"
                                                    getWeightEdge v1 v2 ( G.Edge _ v w : xs) = if v2 == v then w else getWeightEdge v1 v2 xs

eval m pm ss (ListAccess e1 e2 )                = 
                                                do
                                                    (v1, m', ss')   <- eval m pm ss e1
                                                    (v2, m'', ss'') <- eval m' pm ss' e2
                                                    case v1 of
                                                        (List l) -> case v2 of
                                                                     Integer i ->  return $ (l !! (fromIntegral i), m'', ss'')
                                                                     _ -> error "Access List mismatch"
                                                        (String s) -> case v2 of
                                                                     Integer i -> return (Char $ s !! (fromIntegral i), m'', ss'')
                                                                     _ -> error "Access string mismatch"
                                                        _ -> error "Access List mismatch"
eval m pm ss (DictAccess e1 e2)                 = 
                                                do
                                                    (v1, m', ss')  <- eval m pm ss e1
                                                    (k, m'', ss'') <- eval m' pm ss' e2
                                                    case v1 of
                                                        d@(Map ma) -> if getKeyType d == getType k then case  M.lookup k ma of
                                                                                                              Nothing -> error "No key on Dict "
                                                                                                              Just k  -> return $ (k, m'', ss'')
                                                                     else error "Access Dict with invalid key type"
                                                        _ -> error "Access on Dict type mismatch"
eval m pm ss (TupleAccess e1 e2)                = 
                                                do
                                                    (v1', m', ss')   <- eval m pm ss e1
                                                    (v2', m'', ss'') <- eval m' pm ss' e2
                                                    case v1' of
                                                        Pair (v1, v2)     -> case v2' of
                                                                            Integer 0 -> return $ (v1, m'', ss'')
                                                                            Integer 1 -> return $ (v2, m'', ss'')
                                                                            _         -> error "Acessing Pair"
                                                        Triple (v1,v2,v3)-> case v2' of
                                                                            Integer 0 -> return $ (v1, m'', ss'')
                                                                            Integer 1 -> return $ (v2, m'', ss'')
                                                                            Integer 2 -> return $ (v3, m'', ss'')
                                                                            _         -> error "Acessing Pair"
                                                        Quadruple (v1,v2,v3,v4)-> case v2' of
                                                                            Integer 0 -> return $ (v1, m'', ss'')
                                                                            Integer 1 -> return $ (v2, m'', ss'')
                                                                            Integer 2 -> return $ (v3, m'', ss'')
                                                                            Integer 3 -> return $ (v4, m'', ss'')
                                                                            _         -> error "Acessing Pair"
                                                         
                                                        _ -> error "Tuple error " 
eval m pm ss (StructAccess e1 (Ident i))        = 
                                                do
                                                    (v1, m', ss') <- eval m pm ss e1
                                                    case v1 of
                                                        setter@(Setter is msetter) -> 
                                                                    if M.notMember i msetter then error $ i ++ " not a field of this user type"
                                                                    else return $ (v', m', ss')
                                                                    where (t,v') = msetter M.! i
                                                        _ -> error "Trying to access a non-struct type with {}"

eval m pm ss (ArithBinExpr PlusPlusBinOp e1 e2) = 
                                                do
                                                    (v1,t1,m',ss')  <- evalWithType m pm ss e1
                                                    (v2,t2,m'',ss'')  <- evalWithType m' pm ss' e2
                                                    case v1 of
                                                        l1@(List []) -> case v2 of 
                                                                         l2@(List [])     -> if checkCompatType' t1 t2 then return $ (plusPlusBinList l1 l2, m'', ss'') else error $ "Type mismatch " ++ (show t1) ++ " ++ " ++ (show t2)
                                                                         l2@(List (x:xs)) -> if checkCompatType' t1 t2 then return $ (plusPlusBinList l1 l2, m'', ss'') else error $ "Type mismatch " ++ (show t1) ++ " ++ " ++ (show t2)
                                                                         _                -> error $ "Type mismatch " ++ (show t1) ++ " ++ " ++ (show t2)

                                                        l1@(List (x:xs)) -> case v2 of
                                                                                l2@(List [])     -> if checkCompatType t1 t2  then return $ (plusPlusBinList l1 l2, m'', ss'') else error $ "Type mismatch " ++ (show t1) ++ " ++ " ++ (show t2)
                                                                                l2@(List (y:ys)) -> if checkCompatType t1 t2 
                                                                                                    then return $ (plusPlusBinList l1 l2, m'', ss'')
                                                                                                    else error $ "Type mismatch " ++ (show t1) ++ " ++ " ++ (show t2) 
                                                                                _                -> error $ "Type mismatch " ++ (show t1) ++ " ++ " ++ (show t2)

eval m pm ss (ArithBinExpr TimesTimesBinOp e1 e2) = 
                                                do
                                                    (v1,t1,m',ss')  <- evalWithType m pm ss e1
                                                    (v2,t2,m'',ss'')  <- evalWithType m' pm ss' e2
                                                    case v1 of
                                                       i@(Integer n) -> case v2 of 
                                                                         l2@(List l  )    -> return $ (timesTimesBin i l2, m'', ss'')
                                                                         _                -> error $ "Type mismatch " ++ (show t1) ++ " ** " ++ (show t2)
                                                       l1@(List l) -> case v2 of
                                                                                i@(Integer n)     -> return $ (timesTimesBin i l1, m'', ss'')
                                                                                _               -> error $ "Type mismatch " ++ (show t1) ++ " ** " ++ (show t2)

                                                       _                -> error $ "Type mismatch " ++ (show t1) ++ " ** " ++ (show t2)
eval m pm ss (ArithTerm (IdTerm (Ident i))) = case fetchVarValue m i ss of
                                                Left i -> error i
                                                Right i -> return $ (i, m, ss)

eval m pm ss (ArithTerm (SubcallTerm (SubprogCall (Ident i) as))) = 
                                                do  arguments <- processSubArgs as [] m pm ss
                                                    selected <- return $ selectSubForCall i arguments pm
                                                    case selected of
                                                        Nothing -> error ("No subprogram found for call to " ++ i)
                                                        Just sub -> 
                                                                    case sub of
                                                                        (_,(_, (Nothing),_)) -> error "Procedure inside expression"
                                                                        _ -> do
                                                                                (m',ss',v) <- execSubprogram m pm scopes sub arguments
                                                                                do
                                                                                    case v of
                                                                                        Nothing -> error "No return from subprogram call"
                                                                                        Just v -> return (v, m', ss)

eval m pm ss (ExprLiteral (ListCompLit lc)) = 
    do  
        (r, m', (s:ss')) <- forListComp m pm ss lc
        return $ (List r, m', ss')

eval m pm ss e@(StructInitExpr (StructInit (Ident t) ias)) =  do 
    setter <- evalSetter m pm ss e
    return $ ((applySetter pm (makeDefaultSetter pm t) setter t), m, ss)
    where
        evalSetter m pm ss (StructInitExpr (StructInit (Ident t) ias)) = 
            do case ias of
                    [] -> return $ Setter t M.empty
                    ((IdentAssign [(Ident i)] e):ias') -> do
                                                    Setter is remain <- evalSetter m pm ss (StructInitExpr (StructInit (Ident t) ias'))
                                                    if M.member i remain then error $ "Duplicity of field " ++ i ++ " in struct initialization"
                                                    else do
                                                            (v, m', ss') <- eval m pm ss e
                                                            return $ Setter t (M.insert i (getType v, v) remain) 

eval m pm ss (ExprLiteral (GraphLit exp edges )) = do 
                time <- getCurSeconds
                let     newScope = BlockScope time
                        ss' = (newScope:ss) in
                    do
                        (v',m'',(s:ss'')) <- evalGraphComp m pm ss' exp edges
                        return (v',m'',ss'') 

timesTimesBin :: Value -> Value -> Value
timesTimesBin (Integer i) (List l)   = List   (concat (replicate (fromInteger i) l) ) 

plusPlusBinList :: Value -> Value -> Value
plusPlusBinList (List []) (List l1) = List l1;
plusPlusBinList (List l1) (List []) = List l1;
plusPlusBinList (List xs'@(x:xs)) (List ys'@(y:ys)) = List (xs' ++ ys')

plusPlusBin :: Value -> Value -> Value
plusPlusBin k (List []) = (List [k])
plusPlusBin (List []) k = (List [k])
plusPlusBin k (List xs'@(x:xs)) = if not (checkCompatType (getType k) tl) || not (checkCompatType tl (getType k)) then error "Type mismatch in operation ++"
                           else List (k:xs')
                                where tl = getListType xs' 
plusPlusBin (List xs'@(x:xs)) k = if getType (k) /= tl then error "Type mismatch in operation ++"
                           else List (xs' ++ [k])
                                where tl = getType x

modBin ::  Value -> Value -> Value
modBin (Integer i) (Integer j) = (Integer (i `mod`  j)) 
modBin (List l)  i             =   List ( map ((flip modBin) i) l)
modBin  i (List l)             =   List ( map (modBin i) l)

expBin ::  Value -> Value -> Value
expBin (Integer i) (Integer j) = (Integer (i ^ j)) 
expBin (Float f) (Integer i )  = ( Float (f ** (fromInteger i)))
expBin (Integer i) ( Float f)  = ( Float ((fromInteger i) ** f))  
expBin (Float f1) (Float f2)   = ( Float (f1 ** f2))
expBin (List l)  i             =   List ( map ((flip expBin) i) l)
expBin  i (List l)             =   List ( map (expBin i) l)

divBin ::  Value -> Value -> Value
divBin (Integer i) (Integer j) = (Integer (i `div` j)) 
divBin (Float f) (Integer i )  = ( Float (f / (fromInteger i)))
divBin (Integer i) ( Float f)  = ( Float ((fromInteger i) / f))  
divBin (Float f1) (Float f2)   = ( Float (f1 / f2))
divBin (List l)  i             =   List ( map ((flip divBin) i) l)
divBin  i (List l)             =   List ( map (divBin i) l)

timesBin ::  Value -> Value -> Value
timesBin (Integer i) (Integer j) = (Integer (i*j)) 
timesBin (Float f) (Integer i )  = ( Float (f * (fromInteger i)))
timesBin (Integer i) ( Float f)  = ( Float ((fromInteger i) * f))  
timesBin (Float f1) (Float f2)   = ( Float (f1 * f2))
timesBin (List l)  i             = List ( map (timesBin i) l)
timesBin  i (List l)             = List ( map (timesBin i) l)
timesBin (Integer i) (String s)  = String (concat (replicate (fromInteger i) s) )
timesBin (String s) (Integer i)  = String (concat (replicate (fromInteger i) s) )
timesBin (String s) i            = error $ "Type mismatch GString * " ++ show (getType i) 
timesBin i (String s)            = error $ "Type mismatch " ++ show (getType i) ++ " * GString"  

plusBin ::  Value -> Value -> Value
plusBin (Integer i) (Integer j)  = Integer (i+j) 
plusBin (Float f) (Integer i )   =  Float (f + (fromInteger i))
plusBin (Integer i) ( Float f)   =  Float ((fromInteger i) +f)  
plusBin (Float f1) (Float f2)    =  Float (f1 + f2)
plusBin (List l)  i              =  List ( map (plusBin i) l)
plusBin  i (List l)              =  List ( map (plusBin i) l)
plusBin  (String s1) (String s2) =  String (s1 ++ s2)

minusBin ::  Value -> Value -> Value
minusBin (Integer i) (Integer j) = (Integer (i-j)) 
minusBin (Float f) (Integer i )  = ( Float (f - (fromInteger i)))
minusBin (Integer i) ( Float f)  = ( Float ((fromInteger i) - f))  
minusBin (Float f1) (Float f2)   = ( Float (f1 - f2))
minusBin (List l)  i             =   List ( map ((flip minusBin) i) l)
minusBin  i (List l)             =   List ( map (minusBin i) l)

plusUn :: Value -> Value
plusUn (Integer i) = Integer i
plusUn (Float i) = Float i
plusUn _ = error "Type error Unary (+) operator "

minusUn :: Value -> Value
minusUn (Integer i) = Integer (-i)
minusUn (Float i) = Float (-i)
minusUn _ = error "Type error Unary (-)  operator " 

not' :: Value -> Value
not' (Bool b) = Bool (not b)
not' _ = error "Type error Unary (not) operator "

--------------------------------------------------------------
-- |Graph Comprehension

evalGraphComp :: Memory -> ProgramMemory -> Scopes -> Maybe ArithExpr -> Maybe EdgeComp -> IO (Value, Memory, Scopes)
evalGraphComp m pm ss list edges = case list of
                                    Nothing -> case edges of
                                                    Nothing -> return $ error "Empty list of vertices!"
                                                    Just e  -> evalEdgeComp m pm ss Nothing e
                                    Just l  -> case edges of
                                                    Nothing -> do ((List xs), m', ss') <- eval m pm ss l                                                                
                                                                  return $ (V.Graph (G.fromVertices $ G.fromListToVertices $ zip [0..(length xs)] xs), m', ss')
                                                    Just e  -> evalEdgeComp m pm ss (Just l) e

evalEdgeComp :: Memory -> ProgramMemory -> Scopes -> Maybe ArithExpr -> EdgeComp -> IO (Value, Memory, Scopes)
evalEdgeComp m pm ss list (EdgeComp weight edge forIt) = do
    (m', ss', id, vs, when_exp) <- evalForIterator m pm ss forIt
    case vs of
        [] -> case list of
                Nothing -> return $ (V.Graph (G.Graph Se.empty M.empty), m', ss')
                Just l  -> do ((List xs), m'', ss'') <- eval m' pm ss' l 
                              let g = G.fromVertices $ G.fromListToVertices $ zip [0..(length xs)] xs
                              return $ (V.Graph g, m'', ss'')
        vs'@(v:vs) -> do let (Right m'') = elabVars m' (getNameCell id v) (head ss')
                         case list of
                            Nothing -> evalEdgeComp' m'' pm ss' weight edge id vs' when_exp (G.Graph Se.empty M.empty) True
                            Just l  -> do ((List xs), m''', ss''') <- eval m'' pm ss' l 
                                          let g = G.fromVertices $ G.fromListToVertices $ zip [0..(length xs)] xs
                                          evalEdgeComp' m''' pm ss''' weight edge id vs' when_exp g False                                        

evalEdgeComp' :: Memory -> ProgramMemory -> Scopes -> Maybe ArithExpr -> S.Edge -> [Identifier] -> [Value] -> Maybe ArithExpr -> (G.Graph Value Value) -> Bool -> IO (Value, Memory, Scopes)
evalEdgeComp' m pm ss weight edge id vs when_exp g new_vertices = case vs of
    [v] -> do (Right m') <- updateListIds m ss id v
              case when_exp of
                Nothing -> do
                    generateGraph m' pm ss edge weight g new_vertices
                Just when_exp -> do
                    (success, m'', ss'') <- (eval m' pm ss when_exp)
                    if success == (Bool True)
                    then do          
                        generateGraph m'' pm ss'' edge weight g new_vertices
                    else do 
                        return $ (V.Graph g, m'', ss'')
    (v:vs) -> do (Right m') <- updateListIds m ss id v
                 case when_exp of
                    Nothing -> do
                        (V.Graph g', m'', ss'') <- generateGraph m' pm ss edge weight g new_vertices
                        evalEdgeComp' m'' pm ss'' weight edge id vs Nothing g' new_vertices
                    Just when_exp -> do
                        (success, m'', ss'') <- (eval m' pm ss when_exp)
                        if success == (Bool True)
                        then do          
                            (V.Graph g', m''', ss''') <- generateGraph m'' pm ss'' edge weight g new_vertices
                            evalEdgeComp' m''' pm ss''' weight edge id vs (Just when_exp) g' new_vertices
                        else do 
                            evalEdgeComp' m'' pm ss'' weight edge id vs (Just when_exp) g new_vertices 

generateGraph :: Memory -> ProgramMemory -> Scopes -> S.Edge -> Maybe ArithExpr -> (G.Graph Value Value) -> Bool -> IO (Value, Memory, Scopes)
generateGraph m pm ss (S.Edge tp exp1 exp2) weight g new_vertices = do
    (e1, m', ss')   <- eval m pm ss exp1
    (e2, m'', ss'') <- eval m' pm ss' exp2
    let v1@(Vertex id1 _) = G.getVertexFromValue g e1 new_vertices
    if id1 == -1
    then return $ (V.Graph g, m'', ss'')
    else do
        let g' = G.insertVertex g v1
        let v2@(Vertex id2 _) = G.getVertexFromValue g' e2 new_vertices
        if id2 == -1
        then return $ (V.Graph g, m'', ss'')
        else do
            let g'' = G.insertVertex g' v2
            case weight of
                Nothing -> do let w = (Integer 1)
                              r <- addEdge g'' tp v1 v2 w
                              return (r, m'', ss'')
                Just weight -> do (w, m''', ss''') <- eval m'' pm ss'' weight
                                  r <- addEdge g'' tp v1 v2 w
                                  return (r, m''', ss''')

addEdge :: G.Graph Value Value -> EdgeType -> G.Vertex Value -> G.Vertex Value -> Value -> IO Value
addEdge g@(G.Graph _ es) tp v1@(Vertex id1 _) v2@(Vertex id2 _) weight = do
    if checkEdgeType weight (M.toList es)
    then 
        case tp of
            LeftEdge -> do return (V.Graph (G.insertEdge g (G.Edge v2 v1 weight)))
            RightEdge -> do return (V.Graph (G.insertEdge g (G.Edge v1 v2 weight))) 
            DoubleEdge -> do 
                let g' = G.insertEdge g (G.Edge v1 v2 weight)
                return (V.Graph (G.insertEdge g' (G.Edge v2 v1 weight)))
    else 
        error "Weight type is different"

-- |Check the type of the graph edges and new edge
checkEdgeType :: Value -> [(Int, [G.Edge Value Value])] -> Bool
checkEdgeType _ []                                     = True
checkEdgeType w ( ( _ , ( G.Edge _ _ wx : _ ) ) : xs ) = (getType w == getType wx) && (checkEdgeType w xs)

--------------------------------------------------------------
-- |List Comprehension

forListComp :: Memory -> ProgramMemory -> Scopes -> ListComp -> IO ([Value], Memory, Scopes)
forListComp m pm ss lc = do 
                            time <- getCurSeconds
                            let     newScope = BlockScope time
                                    ss' = (newScope:ss) in
                                    do
                                        (m', ss'', exp, id, vs, when_exp) <- evalListComp m pm ss' lc
                                        case vs of
                                            []         -> return ([], m', ss'')
                                            vs'@(v:vs) -> do let (Right m'') = elabVars m' (getNameCell id v) (head ss')
                                                             forListComp' m'' pm ss'' exp id vs' when_exp

forListComp' :: Memory -> ProgramMemory -> Scopes -> ArithExpr -> [Identifier] -> [Value] -> Maybe ArithExpr -> IO ([Value], Memory, Scopes)
forListComp' m pm ss exp id [v] when_exp = do
                                                (Right m') <- updateListIds m ss id v
                                                case when_exp of
                                                    Nothing -> do
                                                        (e, m'', ss'') <- eval m' pm ss exp 
                                                        return ([e], m'', ss'')
                                                    Just when_exp -> do
                                                        (success, m'', ss'') <- (eval m' pm ss when_exp)
                                                        if success == (Bool True)
                                                        then do
                                                            (e, m''', ss''')  <- eval m'' pm ss'' exp 
                                                            return ([e], m''', ss''')
                                                        else do 
                                                            return ([], m'', ss'')

forListComp' m pm ss exp id (v:vs) when_exp = do
                                                (Right m') <- updateListIds m ss id v
                                                case when_exp of
                                                    Nothing -> do
                                                        (e, m'', ss'') <- eval m' pm ss exp
                                                        (r, m''', ss''') <- forListComp' m'' pm ss'' exp id vs when_exp
                                                        return (e : r, m''', ss''')
                                                    Just when_exp -> do
                                                        (success, m'', ss'')  <- (eval m' pm ss when_exp)
                                                        if success == (Bool True)
                                                        then do
                                                            (e, m''', ss''') <- eval m'' pm ss'' exp
                                                            (r, m'''', ss'''') <- forListComp' m''' pm ss''' exp id vs (Just when_exp)
                                                            return (e : r, m'''', ss'''')
                                                        else do 
                                                            forListComp' m'' pm ss'' exp id vs (Just when_exp)

evalListComp :: Memory -> ProgramMemory -> Scopes -> ListComp -> IO (Memory, Scopes, ArithExpr, [Identifier], [Value], Maybe ArithExpr)
evalListComp m pm ss (ListComp expression forIt ) = do
        (m', ss', is, xss, when_exp) <- evalForIterator m pm ss forIt
        return (m', ss', expression, is, xss, when_exp)

evalForIterator :: Memory -> ProgramMemory -> Scopes -> ForIterator -> IO (Memory, Scopes, [Identifier], [Value], Maybe ArithExpr)
evalForIterator m pm ss (ForIterator is xs when_exp) = do
        let new_xs = replicateList ((length is) - (length xs)) xs
        (xss, m', ss') <- (getLists m pm ss [new_xs])
        xss' <- (over xss)
        if when_exp == []
        then do
            return (m', ss', is, xss', Nothing)
        else do
            return (m', ss', is, xss', (Just (head when_exp)))            
    where getLists m pm ss (xs:[])  = do (xss, m', ss') <- (evalList m pm ss xs)
                                         return (xss, m', ss')
          getLists m pm ss (xs:xss) = do (xss', m', ss') <- (evalList m pm ss xs) 
                                         (xss'', m'', ss'') <- (getLists m' pm ss' xss)
                                         return ((xss' ++ xss''), m'', ss'')
          replicateList 0 xs = xs
          replicateList n xs | n < 0     = error "There aren't enough identifiers." 
                             | otherwise = replicateList (n-1) xs ++ [last xs]

over :: [Value] -> IO [Value]
over []       = do return [] 
over [(List xs)]       = do return [(List [x]) | x <- xs]
over (List [x]: xss)   = do xss' <- (over xss)
                            xss'' <- (join x xss')
                            return xss''
over (List (x:xs):xss) = do xss'  <- (over xss)
                            xss'' <- (join x xss') 
                            xss''' <- (over ((List xs): xss))
                            return (xss'' ++ xss''')

join :: Value -> [Value] -> IO [Value]
join v []     = do return []
join v [(List xs)]     = do return [ List (v : xs) ]
join v ((List xs):xss) = do xss' <- (join v xss)
                            return ((List (v : xs)) : xss')

getNameCell :: [Identifier] -> Value -> [(Name,Cell)]
getNameCell [(Ident id)] (List [v]) = [ (id, ((getType v), (Value v)) ) ]
getNameCell ((Ident id):ids) (List (v:vs)) = (id, ((getType v), (Value v)) ) : (getNameCell ids (List vs))                                          

getNameCellGraph :: [Identifier] -> (Vertex Value, Vertex Value, Value) -> [(Name,Cell)]
getNameCellGraph [(Ident id1)] (Vertex _ a,_,_)                                    = [ (id1, ((getType a), (Value a)) ) ]
getNameCellGraph [(Ident id1), (Ident id2)] (Vertex _ a,Vertex _ b,_)              = [ (id1, ((getType a), (Value a)) ), (id2, ((getType b), (Value b)) ) ]
getNameCellGraph [(Ident id1), (Ident id2), (Ident id3)] (Vertex _ a,Vertex _ b,c) = [ (id1, ((getType a), (Value a)) ), (id2, ((getType b), (Value b)) ), (id3, ((getType c), (Value c)) )]

updateListIds :: Memory -> Scopes -> [Identifier] -> Value -> IO (Either String Memory)
updateListIds m ss [(Ident id)] (List [v])        = do 
                                                        let r = updateVar m id ss ((getType v), (Value v))
                                                        return r
updateListIds m ss ((Ident id):ids) (List (v:vs)) = do
                                                        let (Right m') = updateVar m id ss ((getType v), (Value v))
                                                        r <- updateListIds m' ss ids (List vs)
                                                        return r


uncapsulate ::(Maybe Integer) -> GType -> GType
uncapsulate Nothing (GDict t1 t2) = t2
uncapsulate Nothing (GList t1)    = t1
uncapsulate (Just 0) (GPair t1 _) = t1
uncapsulate (Just 1) (GPair _ t2) = t2
uncapsulate (Just 0) (GTriple t1 _ _) = t1
uncapsulate (Just 1) (GTriple _ t2 _) = t2
uncapsulate (Just 2) (GTriple _ _ t3) = t3
uncapsulate (Just 0) (GQuadruple t1 _ _ _) = t1
uncapsulate (Just 1) (GQuadruple _ t2 _ _) = t2
uncapsulate (Just 2) (GQuadruple _ _ t3 _) = t3
uncapsulate (Just 3) (GQuadruple _ _ _ t4) = t4
uncapsulate  _ x             = x

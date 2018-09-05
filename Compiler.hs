module Compiler where

import Language
import Parser
import Utils

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = [Addr]

data TiDump = DummyTiDump deriving(Show)

initialTiDump :: TiDump
initialTiDump = DummyTiDump

type TiHeap = Heap Node

data Node = NAp Addr Addr
        | NSuperComb Name [Name] CoreExpr
        | NNum Int
        deriving(Show)

type TiGlobals = ASSOC Name Addr

type TiStats = Int

tiStatInitial :: TiStats
tiStatInitial = 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps s = s + 1

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps s = s

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats fn (stack, dump, heap, globals, stats) = 
    (stack, dump, heap, globals, fn stats)

compile :: CoreProgram -> TiState
compile program = (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)    
    where 
        sc_defs = program ++ preludeDefs ++ extraPreludeDefs
        (initial_heap, globals) = buildInitialHeap sc_defs
        initial_stack = [address_of_main]
        address_of_main = aLookUp globals "main" (error "main is not defined")

extraPreludeDefs :: CoreProgram
extraPreludeDefs = []        

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap sc_defns = mapAccuml allocateSc hInitial sc_defns

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
        where (heap', addr) = hAlloc heap (NSuperComb name args body)

eval :: TiState -> [TiState]
eval state = state : rest_states
    where
        rest_states | tiFinal state = []
                    | otherwise = eval next_state
        next_state = doAdim (step state)


doAdim :: TiState -> TiState
doAdim state = applyToStats tiStatIncSteps state

tiFinal :: TiState -> Bool
tiFinal ([sole_addr], dump, heap, globals, stats) = 
    isDataNode (hLookUp heap sole_addr)
tiFinal ([], dump, heap, globals, stats) = error "Empty stack"
tiFinal state = False

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode _ = False

step :: TiState -> TiState
step state = dispatch (hLookUp heap (hd stack))
    where 
        (stack, dump, heap, globals, stats) = state
        dispatch (NNum n) = numStep state n
        dispatch (NAp a1 a2) = apStep state a1 a2
        dispatch (NSuperComb sc args body) = scStep state sc args body 

numStep :: TiState -> Int -> TiState
numStep _ _ = error "Number applied as a function"

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2 = 
    (a1 : stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body = (new_stack, dump, new_heap, globals, stats)
    where
        new_stack = result_addr : (drop (length arg_names + 1) stack)
        (new_heap, result_addr) = instantiate body heap env
        env = arg_bindings ++ globals
        arg_bindings = zip2 arg_names (getargs heap stack)

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc : stack) = map get_arg stack
    where
        get_arg addr = arg where (NAp fun arg) = hLookUp heap addr

instantiate :: CoreExpr -> TiHeap -> ASSOC Name Addr -> (TiHeap, Addr)
instantiate (ENum n) heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env = hAlloc heap2 (NAp a1 a2)
    where (heap1, a1) = instantiate e1 heap env
          (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env = 
    (heap, aLookUp env v (error ("Undefined name " ++ (show v))))
instantiate (EConstr tag arity) heap env = 
    instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env =
    instantiateLet isrec defs body heap env
instantiate (ECase e alts) heap env = 
    error "Can't instantiate case exprs"

instantiateConstr tag arity heap env = 
    error "Can't instantiate constructors yet"
instantiateLet isrec defs body heap env = 
    error "Can't instantiate let(rec)s yet"           

showResults :: [TiState] -> String
showResults states = 
    iDisplay (iConcat [iLayn (map showState states), 
                       showStats (last states)])

showState :: TiState -> ISeq
showState (stack, dump, heap, globals, stats) = 
    iConcat [showStack heap stack, iNewline, showHeap heap, iNewline]

showHeap :: TiHeap -> ISeq
showHeap (n, rem, ls) = 
    iConcat [
        iStr "---------- Heap ----------", iNewline,
        iIndent (iInterleave iNewline (map show_heap_item ls)),
        iStr "--------------------------"
    ]
    where
        show_heap_item (addr, node) = iConcat [showFWAddr addr, iStr ": ", showNode node] 


showStack :: TiHeap -> TiStack -> ISeq
showStack heap stack = 
    iConcat [
        iStr "========== Stk ==========", iNewline,
        iIndent (iInterleave iNewline (map show_stack_item stack)),
        iStr "========================="
    ]
    where
        show_stack_item addr = 
            iConcat [showFWAddr addr, iStr ": ",
                     showStkNode heap (hLookUp heap addr)
            ]

showStkNode :: TiHeap -> Node -> ISeq
showStkNode heap (NAp fun_addr arg_addr) = 
    iConcat [iStr "NAp ", showFWAddr fun_addr,
             iStr " ", showFWAddr arg_addr, iStr " (",
             showNode (hLookUp heap arg_addr), iStr ")"]
showStkNode heap node = showNode node

showNode :: Node -> ISeq
showNode (NAp a1 a2) = 
    iConcat [iStr "NAp ", iStr (showAddr a1), iStr " ", iStr (showAddr a2)]
showNode (NSuperComb name args body) = iStr ("NSuperComb " ++ name)
showNode (NNum n) = iConcat [iStr "NNum ", iNum n]

showFWAddr :: Addr -> ISeq
showFWAddr addr = iStr (space (4 - length str) ++ str)
    where str = showAddr addr

showStats :: TiState -> ISeq
showStats (stack, dump, heap, globals, stats) = 
    iConcat [iNewline, iNewline, iStr "Total number of steps = ",
             iNum (tiStatGetSteps stats)]    

runProg :: String -> String
runProg = showResults . eval . compile . parser             

test2 :: IO ()
test2 = 
    do str <- readFile "./tests/test2.ak"
       mapM_ (\s -> putStrLn (show s ++ "\n")) $ (eval.compile.parser) str

test3 :: IO ()
test3 = 
    do str <- readFile "./tests/test3.ak"
       putStrLn $ runProg str

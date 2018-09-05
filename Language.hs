module Language where

import Data.Time

data Expr a = 
    EVar Name
    | ENum Int
    | EConstr Int Int
    | EAp (Expr a) (Expr a)
    | ELet IsRec [(a, Expr a)] (Expr a)
    | ECase (Expr a) [Alter a]
    | ELam [a] (Expr a)
    deriving Show

type Name = String
type CoreExpr = Expr Name
type IsRec = Bool

recursive :: IsRec
recursive = True

nonRecursive :: IsRec 
nonRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, rhs) <- defns]

rhsOf :: [(a, b)] -> [b]
rhsOf defns = [rhs | (name, rhs) <- defns]

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr _ = False

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

type Defn a = (a, Expr a)
type CoreDefn = Defn Name

preludeDefs:: CoreProgram
preludeDefs = 
    [("I", ["x"], EVar "x"),
    ("K", ["x", "y"], EVar "x"),
    ("K1", ["x", "y"], EVar "y"),
    ("S", ["f", "g", "x"], 
        EAp
        (EAp (EVar "f") (EVar "x"))
        (EAp (EVar "g") (EVar "x"))),
    ("compose", ["f", "g", "x"], 
        EAp
        (EVar "f")
        (EAp (EVar "g") (EVar "x"))),
    ("twice", ["f"], 
        EAp
        (EAp (EVar "compose") (EVar "f"))
        (EVar "f"))
    ]

-- -- test function
-- mkMulitAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
-- mkMulitAp n e1 e2 = foldl EAp e1 (take n e2s)
--     where e2s = e2 : e2s

-- run :: Int -> IO ()
-- run n = do 
--     start <- getCurrentTime
--     putStrLn $ pprExpr (mkMulitAp n (EVar "f") (EVar "x"))
--     stop <- getCurrentTime
--     print $ diffUTCTime stop start

iNil :: ISeq 
iNil = INil

iStr :: String -> ISeq
iStr = IStr

iAppend :: ISeq -> ISeq -> ISeq
iAppend INil seq = seq
iAppend seq INil = seq
iAppend seq1 seq2 = IAppend seq1 seq2

iNewline :: ISeq
iNewline = INewline

iIndent :: ISeq -> ISeq
iIndent = IIndent

iDisplay :: ISeq -> String   
iDisplay seq = flatten 0 [(seq, 0)]

flatten :: Int -> [(ISeq, Int)] -> String
flatten _ [] = ""
flatten col ((IStr s, indent) : seqs) = 
    s ++ (flatten ((length s) + col) seqs)
flatten col ((IAppend seq1 seq2, indent) : seqs) = flatten col ((seq1, indent) : (seq2, indent): seqs)
flatten col ((INil, indent) : seqs) = flatten col seqs
flatten col ((INewline, indent) : seqs) = 
    '\n' : (space indent) ++ (flatten indent seqs)
flatten col ((IIndent seq, indent): seqs) = 
    flatten col ((seq, col) : seqs)    

space :: Int -> [Char]
space 0 = []
space n = ' ' : space (n-1)

iConcat :: [ISeq] -> ISeq
iConcat [] = iNil
iConcat (x : xs) = x `iAppend` (iConcat xs)

iInterleave :: ISeq -> [ISeq] -> ISeq
iInterleave _ [] = iNil
iInterleave sep (x : xs) = x `iAppend` sep `iAppend` (iInterleave sep xs)

pprExpr :: CoreExpr -> ISeq
pprExpr (EVar v) = iStr v
pprExpr (ENum n) = iStr (show n)
pprExpr (EAp (EAp (EVar "+") e1) e2) = 
    iConcat [ pprAExpr e1, iStr " + ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "-") e1) e2) = 
    iConcat [ pprAExpr e1, iStr " - ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "*") e1) e2) = 
    iConcat [ pprAExpr e1, iStr " * ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "/") e1) e2) = 
    iConcat [ pprAExpr e1, iStr " / ", pprAExpr e2]
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2) 
pprExpr (ELet isrec defns expr) = 
    iConcat [iStr keyword, iNewline, 
        iStr " ", iIndent (pprDefns defns), iNewline,
        iStr "in ", pprExpr expr]
    where keyword   | not isrec = "let"
                    | isrec = "letrec"
pprExpr (ECase e alts) = 
    iConcat [iStr "case ", pprExpr e, iStr " of",
        iNewline, iStr " ", iIndent (pprAlts alts)]
pprExpr (ELam as e) = 
    iConcat [iStr "\\ ", iInterleave (iStr " ") (map iStr as), 
        iStr ". ", pprExpr e]

pprAlts :: [CoreAlt] -> ISeq
pprAlts alts = iInterleave sep (map pprAlt alts)
    where sep = iConcat [iStr " ;", iNewline]  

pprAlt :: CoreAlt -> ISeq
pprAlt (n, as, e) = 
    iConcat [iStr "<", iStr (show n), iStr "> ", 
        iInterleave (iStr " ") (map iStr as), 
        iStr " -> ", pprExpr e]

pprAExpr :: CoreExpr -> ISeq
pprAExpr e 
    | isAtomicExpr e = pprExpr e
    | otherwise = iConcat [iStr "(", pprExpr e , iStr ")"] 

pprDefns :: [(Name, CoreExpr)] -> ISeq
pprDefns defns = iInterleave sep (map pprDefn defns)
    where sep = iConcat [iStr ";", iNewline]

pprDefn :: (Name, CoreExpr) -> ISeq
pprDefn (name, expr) = 
    iConcat [iStr name, iStr " = ", iIndent (pprExpr expr)]  

pprCoreScDefn :: CoreScDefn -> ISeq
pprCoreScDefn (name, as, e) = 
    iConcat [iStr name, iStr " ", iInterleave (iStr " ") (map iStr as), 
        iStr " = ", pprExpr e]

pprProgram :: CoreProgram -> ISeq 
pprProgram cs = iInterleave sep (map pprCoreScDefn cs)
    where sep = iConcat [iStr ";", iNewline]    

data ISeq = INil
    | IStr String
    | IAppend ISeq ISeq
    | IIndent ISeq
    | INewline

pprint prog = iDisplay (pprProgram prog)

iNum :: Int -> ISeq
iNum n = iStr (show n)

iFWNum :: Int -> Int -> ISeq
iFWNum width n = 
    iStr (space (width - length digits) ++ digits)
    where digits = show n

iLayn :: [ISeq] -> ISeq
iLayn seqs = iConcat (map lay_item (zip [1..] seqs))
    where lay_item (n, seq) = 
            iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline] 

sampleExpr = ELet False [("x", (ENum 2)), ("y", (ENum 4))] (EAp (EAp (EVar "+") sampleExpr1) (EVar "y"))
sampleExpr1 = ECase (EVar "x") [(1, ["n"], (ENum 5)), (2, ["m", "p"], (ENum 5))]
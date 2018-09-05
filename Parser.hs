module Parser where

import Data.Char
import Language

type Token = (Int, String)

clex :: Int -> String -> [Token]
clex _ [] = []
clex n (c : cs) 
    | isWhiteSpace c = if c == '\n' 
                       then clex (n + 1) cs
                       else clex n cs
    | isAlpha c = (n, var_tok) : clex n rest_cs
    where var_tok = c : takeWhile isIdChar cs
          rest_cs = dropWhile isIdChar cs
clex n (c : cs) 
    | isDigit c = (n, num_token) : clex n rest_cs
    where num_token = c : takeWhile isDigit cs
          rest_cs = dropWhile isDigit cs    
clex n (c : c1 : cs)
    | c == '-' && c1 == '-' = clex n $ dropWhile (\ch -> ch /= '\n') cs
    | (c : [c1]) `elem` twoCharOps = (n, c : [c1]) : clex n cs
    | otherwise = (n, [c]) : clex n (c1 : cs)
clex n (c : cs) = (n, [c]): clex n cs

twoCharOps :: [String]
twoCharOps = ["==", "<=", ">=", "~=", "->"]

isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || (c == '_')

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \n\t"

type Parser a = [Token] -> [(a, [Token])]

pLit :: String -> Parser String
pLit s = pSat (== s)

pVar :: Parser String
pVar = pSat isVariable

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks = [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks, (v2, toks2) <- p2 toks1]    

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks = [ (combine v1 v2 v3, toks3) | (v1, toks1) <- p1 toks, (v2, toks2) <- p2 toks1, (v3, toks3) <- p3 toks2]    

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 toks = [ (combine v1 v2 v3 v4, toks4) | (v1, toks1) <- p1 toks, (v2, toks2) <- p2 toks1, (v3, toks3) <- p3 toks2, (v4, toks4) <- p4 toks3]    

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty []) 

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)  

pEmpty :: a -> Parser a
pEmpty v toks = [(v, toks)]

pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks = [(f v, toks1) | (v, toks1) <- p toks]

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p pSep = pThen (:) p pRest
    where pRest = (pThen (\sep ls -> ls) pSep (pOneOrMoreWithSep p pSep)) `pAlt` (pEmpty []) 

pSat :: (String -> Bool) -> Parser String
pSat fn (tok: toks)
    | fn (snd tok) = [(snd tok, toks)]
    | otherwise = []
pSat fn [] = []

isVariable :: String -> Bool
isVariable [] = False
isVariable (c : cs) = isAlpha c && all isIdChar cs

pNum :: Parser Int
pNum = pSat isNum `pApply` read

isNum :: String -> Bool
isNum [] = False
isNum (c : cs) = isDigit c && all isDigit cs

syntax :: [Token] -> CoreProgram
syntax = take_first_parse . pProgram
         where 
         take_first_parse ((prog, []) : others) = prog
         take_first_parse (parse : others) = take_first_parse others
         take_first_parse other = error "Syntax error"  


pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr
      where mk_sc v vs eq expr = (v, vs, expr)  

pDefn :: Parser CoreDefn
pDefn = pThen3 mk_sc pVar (pLit "=") pExpr
        where mk_sc v _ expr = (v, expr)

pAlter :: Parser CoreAlt
pAlter = pThen4 mk_sc pAlterNum (pZeroOrMore pVar) (pLit "->") pExpr 
         where mk_sc num vs _ expr = (num, vs, expr)   
       
pAlterNum :: Parser Int
pAlterNum = pThen3 mk_sc (pLit "<") pNum (pLit ">")
            where mk_sc _ num _ = num  

pBinop :: Parser String
pBinop = pArithOp `pAlt` pRelop `pAlt` pBoolOp

pArithOp :: Parser String
pArithOp = pSat (\s -> s `elem` ["+", "-", "*", "/"])

pRelop :: Parser String
pRelop = pSat (\s -> s `elem` ["<", "<=", "==", "~=", ">=", ">"])

pBoolOp :: Parser String
pBoolOp = pSat (\s -> s `elem` ["&", "|"])

pAExpr :: Parser CoreExpr 
pAExpr = (pVar `pApply` EVar) `pAlt` (pNum `pApply` ENum)  `pAlt` pConstr `pAlt` pExprInParen

pConstr :: Parser CoreExpr
pConstr = pThen3 mk_sc pConstrStart pConstrMid pConstrEnd
          where mk_sc _ (n1, n2) _ = EConstr n1 n2 

pConstrStart :: Parser String
pConstrStart = pThen mk_sc (pLit "Pack") (pLit "{")
               where mk_sc s1 s2 = s1 ++ s2 

pConstrMid :: Parser (Int, Int)
pConstrMid = pThen3 mk_sc pNum (pLit ",") pNum
             where mk_sc n1 _ n2 = (n1, n2)

pConstrEnd :: Parser String
pConstrEnd = pLit "}"

pExpr :: Parser CoreExpr
pExpr =  pLet `pAlt` pCase `pAlt` pLambda `pAlt` pExpr1

data PartialExpr = NoOp | FoundOp Name CoreExpr deriving(Show)

pExpr1 :: Parser CoreExpr
pExpr1  = pThen assembleOp pExpr2 pExpr1c

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e NoOp = e
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2

pExpr1c :: Parser PartialExpr
pExpr1c = (pThen FoundOp (pLit "|") pExpr1) `pAlt` (pEmpty NoOp) 
          
pExpr2 :: Parser CoreExpr
pExpr2 = pThen assembleOp pExpr3 pExpr2c

pExpr2c :: Parser PartialExpr
pExpr2c = (pThen FoundOp (pLit "&") pExpr2) `pAlt` (pEmpty NoOp)

pExpr3 :: Parser CoreExpr
pExpr3 = pThen assembleOp pExpr4 pExpr3c

pExpr3c :: Parser PartialExpr
pExpr3c = (pThen FoundOp pRelop pExpr4) `pAlt` (pEmpty NoOp)

pExpr4 :: Parser CoreExpr
pExpr4 = pThen assembleOp pExpr5 pExpr4c

pExpr4c :: Parser PartialExpr
pExpr4c = (pThen FoundOp (pLit "+") pExpr4) `pAlt` (pThen FoundOp (pLit "-") pExpr5) `pAlt` (pEmpty NoOp)

pExpr5 :: Parser CoreExpr
pExpr5 = pThen assembleOp pExpr6 pExpr5c

pExpr5c :: Parser PartialExpr
pExpr5c = (pThen FoundOp (pLit "*") pExpr5) `pAlt` (pThen FoundOp (pLit "/") pExpr6) `pAlt` (pEmpty NoOp)

pExpr6 :: Parser CoreExpr
pExpr6 = pApp 

pApp :: Parser CoreExpr
pApp = (pOneOrMore pAExpr) `pApply` mk_ap_chain

mk_ap_chain :: [CoreExpr] -> CoreExpr
mk_ap_chain [c] = c
mk_ap_chain (c : cs) = EAp c (mk_ap_chain cs) 

pLet :: Parser CoreExpr
pLet = pThen4 mk_sc pLetAlt (pOneOrMoreWithSep pDefn (pLit ";")) (pLit "in") pExpr 
       where mk_sc isRec defns _ expr = ELet isRec defns expr

pLetAlt :: Parser Bool
pLetAlt = ((pLit "let") `pAlt` (pLit "letrec")) `pApply` (\s -> if s == "letrec" then True else False)

pCase :: Parser CoreExpr
pCase = pThen4 mk_sc (pLit "case") pExpr (pLit "of") (pOneOrMoreWithSep pAlter (pLit ";"))
        where mk_sc _ expr _ alts = ECase expr alts

pLambda :: Parser CoreExpr
pLambda = pThen4 mk_sc (pLit "\\") (pOneOrMore pVar) (pLit ".") pExpr 
          where mk_sc _ as _ expr = ELam as expr

pExprInParen :: Parser CoreExpr
pExprInParen = pThen3 mk_sc (pLit "(") pExpr (pLit ")") 
               where mk_sc _ expr _ = expr 

parser :: String -> CoreProgram
parser = syntax . (clex 1)

test :: IO ()
test = 
    do str <- readFile "./tests/test2.ak"
       print $ clex 1 str

test1 :: IO ()
test1 = 
    do str <- readFile "./tests/test1.ak"
       print $ parser str

module Utils where

type Heap a = (Int, [Int], [(Int, a)])
type Addr = Int

shownum n = show n

hd :: [a] -> a
hd = head

tl :: [a] -> [a]
tl = tail 

zip2 :: [a] -> [b] -> [(a, b)]
zip2 = zip

hInitial :: Heap a
hInitial = (0, [1..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, (next : free), ls) n = ((size + 1, free, (next, n) : ls), next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (size, free, ls) addr n = (size, free, (addr, n) : remove ls addr)

hFree :: Heap a -> Addr -> Heap a
hFree (size, free, ls) addr = (size - 1, addr : free, remove ls addr)

hLookUp :: Heap a -> Addr -> a
hLookUp (size, free, ls) addr = aLookUp ls addr (error ("can't find node " ++ showAddr addr ++ " in heap"))

hAddresses :: Heap a -> [Addr]
hAddresses (size, free, ls) = [addr | (addr, n) <- ls]

hSize :: Heap a -> Int
hSize (size, free, ls) = size

hNull :: Addr
hNull = 0

hIsnull :: Addr -> Bool
hIsnull addr  = addr == 0

showAddr :: Addr -> [Char]
showAddr addr = "#" ++ (show addr)

remove :: [(Int, a)] -> Int -> [(Int, a)]
remove [] addr = error ("Attempt to update or free non existent address #" ++ showAddr addr)
remove ((a, n) : ls) addr 
    | a == addr = ls
    | otherwise = (a, n) : remove ls addr

type ASSOC a b = [(a, b)]
 
aLookUp :: Eq a => ASSOC a b -> a -> b -> b
aLookUp [] k def = def
aLookUp ((k1, v1) : ls) k def
    | k1 == k = v1
    | otherwise = aLookUp ls k def

aDomain :: ASSOC a b -> [a]
aDomain ls = [k | (k, v) <- ls]

aRange :: ASSOC a b -> [b]
aRange ls = [v | (k, v) <- ls]

aEmpty :: ASSOC a b
aEmpty = []     

mapAccuml :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccuml f acc [] = (acc, [])
mapAccuml f acc (x : xs) = (a, c : cs)  
    where (acc', c) = f acc x
          (a, cs) = mapAccuml f acc' xs 
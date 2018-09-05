## Template Instantiation Machine

tim has been built to understand evaluation of functional language. 

Finished with following stages -  
1. Lexer
2. Pretty printer
3. Parser
4. Building intial heap
5. Working on state transitions

**Sample Execution**

```
-- ./tests/test.3ak

M x = x;
main = I M 3

```
*Execution trace*

```
 1) ========== Stk ==========
      #2: NSuperComb main
    =========================
    ---------- Heap ----------
      #8: NSuperComb twice
      #7: NSuperComb compose
      #6: NSuperComb S
      #5: NSuperComb K1
      #4: NSuperComb K
      #3: NSuperComb I
      #2: NSuperComb main
      #1: NSuperComb M
    --------------------------

 2) ========== Stk ==========
     #11: NAp   #3  #10 (NAp #1 #9)
    =========================
    ---------- Heap ----------
     #11: NAp #3 #10
     #10: NAp #1 #9
      #9: NNum 3
      #8: NSuperComb twice
      #7: NSuperComb compose
      #6: NSuperComb S
      #5: NSuperComb K1
      #4: NSuperComb K
      #3: NSuperComb I
      #2: NSuperComb main
      #1: NSuperComb M
    --------------------------

 3) ========== Stk ==========
      #3: NSuperComb I
     #11: NAp   #3  #10 (NAp #1 #9)
    =========================
    ---------- Heap ----------
     #11: NAp #3 #10
     #10: NAp #1 #9
      #9: NNum 3
      #8: NSuperComb twice
      #7: NSuperComb compose
      #6: NSuperComb S
      #5: NSuperComb K1
      #4: NSuperComb K
      #3: NSuperComb I
      #2: NSuperComb main
      #1: NSuperComb M
    --------------------------

 4) ========== Stk ==========
     #10: NAp   #1   #9 (NNum 3)
    =========================
    ---------- Heap ----------
     #11: NAp #3 #10
     #10: NAp #1 #9
      #9: NNum 3
      #8: NSuperComb twice
      #7: NSuperComb compose
      #6: NSuperComb S
      #5: NSuperComb K1
      #4: NSuperComb K
      #3: NSuperComb I
      #2: NSuperComb main
      #1: NSuperComb M
    --------------------------

 5) ========== Stk ==========
      #1: NSuperComb M
     #10: NAp   #1   #9 (NNum 3)
    =========================
    ---------- Heap ----------
     #11: NAp #3 #10
     #10: NAp #1 #9
      #9: NNum 3
      #8: NSuperComb twice
      #7: NSuperComb compose
      #6: NSuperComb S
      #5: NSuperComb K1
      #4: NSuperComb K
      #3: NSuperComb I
      #2: NSuperComb main
      #1: NSuperComb M
    --------------------------

 6) ========== Stk ==========
      #9: NNum 3
    =========================
    ---------- Heap ----------
     #11: NAp #3 #10
     #10: NAp #1 #9
      #9: NNum 3
      #8: NSuperComb twice
      #7: NSuperComb compose
      #6: NSuperComb S
      #5: NSuperComb K1
      #4: NSuperComb K
      #3: NSuperComb I
      #2: NSuperComb main
      #1: NSuperComb M
    --------------------------



Total number of steps = 5
```
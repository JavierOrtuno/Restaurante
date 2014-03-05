DEF INPUT PARAM iintx1 AS INT. 
DEF INPUT PARAM iinty1 AS INT.
DEF INPUT PARAM iintx2 AS INT.
DEF INPUT PARAM iinty2 AS INT.
DEF OUTPUT PARAM odecTotal AS DEC.

odecTotal = EXP (iintx2 - iintx1,2) + EXP (iinty2 - iinty1,2).
odecTotal = SQRT(odecTotal).

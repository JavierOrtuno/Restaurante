DEF INPUT PARAM iinta AS INT. 
DEF INPUT PARAM iintb AS INT.
DEF INPUT PARAM iintc AS INT.
DEF OUTPUT PARAM odecTotal  AS DEC.
DEF OUTPUT PARAM odecTotal2 AS DEC.
DEF VAR vdecraiz  AS DEC.

vdecraiz = EXP (iintb,2) - ( ( 4 * iinta ) * iintc ).
vdecraiz = SQRT(vdecraiz).
odecTotal  = ( ( - iintb + vdecraiz ) / ( 2 * iinta ) ).
odecTotal2 = ( ( - iintb - vdecraiz ) / ( 2 * iinta ) ).

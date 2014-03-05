{include.i}

DEF VAR vdecX1 AS DEC.
DEF VAR vdecX2 AS DEC.
DEF VAR vintA AS INT.
DEF VAR vintB AS INT.
DEF VAR vintC AS INT.

UPDATE vintA.
UPDATE vintB.
UPDATE vintC.

vdecX1  = Cuadraticas(vintA,vintB,vintC).

MESSAGE vdecX1 VIEW-AS ALERT-BOX.


DEF VAR vintOpcion AS INT.
DEF VAR odecTotal AS DEC.
DEF VAR vchrNombre AS CHAR EXTENT 10.
DEF VAR ichrlista AS CHAR.
DEF VAR vintidx AS INT.

FUNCTION Extent_to_lista CHAR(ichrlista AS CHAR):
      DO vintidx = 1 TO 10:
          ichrlista = ichrlista + vchrNombre[vintidx] + ",".
      END.
      ichrlista = TRIM(ichrlista,",").
      RETURN ichrlista.
END.

PROCEDURE Nombres.
     DO vintidx = 1 TO 10.
      UPDATE vchrNombre[vintidx].
     END.
     ichrlista = Extent_to_lista(ichrlista).
     RUN pContratantes2.p(ichrlista).
     MESSAGE ichrlista VIEW-AS ALERT-BOX.
END.

PROCEDURE Distancia.
    DEF VAR iintx1 AS INT. 
    DEF VAR iinty1 AS INT.
    DEF VAR iintx2 AS INT.
    DEF VAR iinty2 AS INT.
        UPDATE iintx1.
        UPDATE iinty1.
        UPDATE iintx2.
        UPDATE iinty2.
        RUN pDistancias.p(iintx1,iinty1,iintx2,iinty2, OUTPUT odecTotal).
        MESSAGE odecTotal VIEW-AS ALERT-BOX.
END.

PROCEDURE Formula.
    DEF VAR iinta AS INT. 
    DEF VAR iintb AS INT.
    DEF VAR iintc AS INT.
    DEF VAR odecTotal2 AS DEC.
       UPDATE iinta.
       UPDATE iintb.
       UPDATE iintc.
       RUN pFormula.p(iinta,iintb,iintc, OUTPUT odecTotal, OUTPUT odecTotal2).
       MESSAGE odecTotal SKIP odecTotal2 VIEW-AS ALERT-BOX.
END.

DISPLAY "Seleccione la operacion deseada:" 
    SKIP "1.-Pedir nombres" 
    SKIP "2.-Calcular Distancia" 
    SKIP "3.-Resolver formula" 
    SKIP "4.-Calcular combinaciones".
    UPDATE vintOpcion.

CASE vintOpcion:
    WHEN 1 THEN
        RUN Nombres.
    WHEN 2 THEN
        RUN Distancia.
    WHEN 3 THEN
        RUN Formula.
    WHEN 4 THEN
        RUN pFuncion.p.
END CASE.






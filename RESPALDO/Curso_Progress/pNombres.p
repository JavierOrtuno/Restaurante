DEF VAR vchrNombre AS CHAR EXTENT 10.
DEF VAR vchrNombre2 AS CHAR EXTENT 10.
DEF VAR ichrlista AS CHAR.
DEF VAR ichrlista2 AS CHAR.
DEF VAR vintidx AS INT.
DEF VAR vintidx2 AS INT.

PROCEDURE Nombres.
     DEF VAR vintcontm AS INT.
     DEF VAR vintcontf AS INT.
     FOR EACH CALL_click WHERE gender = 'M' OR gender = 'F' BY lastname1:
       IF gender = 'M' THEN
       DO:
         IF CALL_click.tperson = 'F' THEN
         DO:
           vintidx = vintidx + 1.
           vchrNombre[vintidx] = call_click.NAME + " " + call_click.lastname1.
         END.
         ELSE
         DO:
           vintidx2 = vintidx2 + 1.
           vchrNombre2[vintidx2] = call_click.NAME + " " + call_click.lastname1.
         END.
         vintcontm = vintcontm + 1.
       END.
       ELSE
       DO:
         IF CALL_click.tperson = 'F' THEN
         DO:
           vintidx = vintidx + 1.
           vchrNombre[vintidx] = call_click.NAME + " " + call_click.lastname1.
         END.
         ELSE
         DO:
           vintidx2 = vintidx2 + 1.
           vchrNombre2[vintidx2] = call_click.NAME + " " + call_click.lastname1.
         END.
         vintcontf = vintcontf + 1.
       END.
     END.
     DO vintidx = 1 TO 10:
          ichrlista = ichrlista + vchrNombre[vintidx] + "~n".
     END.
     ichrlista = TRIM(ichrlista,",").
     DO vintidx = 1 TO 10:
          ichrlista2 = ichrlista2 + vchrNombre2[vintidx] + "~n".
     END.
     ichrlista = TRIM(ichrlista,",").
     MESSAGE "Numero de hombres es:" vintcontm SKIP "Numero de mujeres es:" vintcontf VIEW-AS ALERT-BOX.
     MESSAGE "Personas fisicas:" ichrlista SKIP "Personas Morales:" ichrlista2 VIEW-AS ALERT-BOX.
     IF vintcontm > vintcontf THEN
       MESSAGE "Hay mas hombres que mujeres" VIEW-AS ALERT-BOX.
     ELSE
       MESSAGE "Hay mas mujeres que hombres" VIEW-AS ALERT-BOX.
END.

RUN Nombres.

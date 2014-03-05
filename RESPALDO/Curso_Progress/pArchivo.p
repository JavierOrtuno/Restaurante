DEF VAR vchrNombre AS CHAR EXTENT 10.
DEF VAR vchrNombre2 AS CHAR EXTENT 10.
DEF VAR ichrlista AS CHAR.
DEF VAR ichrlista2 AS CHAR.
DEF VAR vintidx AS INT.
DEF VAR vintidx2 AS INT.
DEF VAR archivo AS CHAR.

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
          ichrlista = ichrlista + vchrNombre[vintidx] + ",".
     END.
     ichrlista = TRIM(ichrlista,",").
     DO vintidx = 1 TO 10:
          ichrlista2 = ichrlista2 + vchrNombre2[vintidx] + ",".
     END.
     ichrlista = TRIM(ichrlista,",").
     archivo = "C:\Curso_Progress\Datos\call_click.txt".
     OUTPUT TO VALUE (archivo).
     PUT "Primer Reporte Semillero" AT 30 SKIP.
     PUT "Existen " vintcontm " hombres" .
     PUT "Existen " AT 50 vintcontf " mujeres" SKIP .
     DO vintidx=1 TO NUM-ENTRIES (ichrlista):
         PUT ENTRY (vintidx,ichrlista) FORMAT "x(20)" .
         PUT ENTRY (vintidx,ichrlista2) AT 50 FORMAT "X(20)" SKIP.
     END.
     IF vintcontm > vintcontf THEN
     DO:
       PUT "Hay mas hombres que mujeres" AT 30 SKIP.
       PUT "Viva la familia" AT 30.
     END.
     ELSE
     DO:
       PUT "Hay mas mujeres que hombres" AT 30 SKIP.
       PUT "Viva la familia" AT 30.
     END.
END.

RUN Nombres.

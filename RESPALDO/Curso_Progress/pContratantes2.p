DEF INPUT PARAM inchrlista AS CHAR.

PROCEDURE lista_to_extent.
  DEF VAR vintIdx AS INT.
  DEF VAR vchrNombre AS CHAR EXTENT 10.

  DO vintIdx = 1 TO NUM-ENTRIES(inchrlista):
    vchrNombre[vintIdx] = ENTRY (vintIdx,inchrlista).
/*    IF vchrNombres[vIntIndice] MATCHES "*EL" THEN
        vintCuantosEL = vintCuantosEL + 1.
    IF vchrNombres[vIntIndice] MATCHES "* *" THEN
        vintDosNom = vintDosNom + 1.*/
  END.
END.



DEF VAR vIntIndice AS INT. 
DEF VAR vchrNombres AS CHAR EXTENT 10.

DO vIntIndice = 1 TO 10.
    UPDATE vchrNombres[vIntIndice].
END.

REPEAT vIntIndice = 1 TO 10.
    MESSAGE "El nombre" vIntIndice "es:" vchrNombres[vIntIndice] VIEW-AS ALERT-BOX.
END.

/*10 lineas 3min*/

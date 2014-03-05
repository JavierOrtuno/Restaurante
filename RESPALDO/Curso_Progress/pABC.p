DEF VAR vchrcurp AS CHAR FORMAT "X(20)".
DEF VAR vlogband AS LOG.

MESSAGE "curp:" UPDATE vchrcurp.

FIND FIRST CALL_click WHERE curp = vchrcurp NO-ERROR.
IF AVAILABLE CALL_click THEN
DO:
      MESSAGE "deseas modificar?" vchrcurp VIEW-AS ALERT-BOX BUTTONS YES-NO SET vlogband.
      IF vlogband = TRUE THEN
      DO:
         UPDATE CALL_click.curp.
      END.
      ELSE
      DO:
          MESSAGE "deseas eliminar?" vchrcurp VIEW-AS ALERT-BOX BUTTONS YES-NO SET vlogband.
          IF vlogband = TRUE THEN
          DO:
              FIND CURRENT CALL_click EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
              DELETE CALL_click.
          END.
          ELSE
          DO:
              MESSAGE "bye" VIEW-AS ALERT-BOX.
          END.
      END.
END.
ELSE
DO:
    MESSAGE "no existe " vchrcurp " deseas agregar?" VIEW-AS ALERT-BOX BUTTONS YES-NO SET vlogband.
    IF vlogband = TRUE THEN
    DO:
        INSERT CALL_click EXCEPT curp.
        ASSIGN curp = vchrcurp.
    END.
    ELSE
    DO:
        MESSAGE "bye" VIEW-AS ALERT-BOX.
    END.
END.





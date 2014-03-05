&SCOPED-DEFINE FormRFC "!(4)-9(6)-!!9"

DEF VAR vchrTest AS CHAR.

DEF FRAME Dummy vchrTest.

FUNCTION Valid-Format LOG(inchrValor   AS CHAR,
                          inchrFormato AS CHAR):
 DEF VAR vlogValidFormat AS LOG.

 ASSIGN vchrTest:FORMAT IN FRAME Dummy = inchrFormato
        vchrTest:SCREEN-VALUE          = inchrValor NO-ERROR.

 vlogValidFormat = (ERROR-STATUS:GET-NUMBER(1) = 0).

 RETURN vlogValidFormat.
END.

MESSAGE "Válido Leopoldo: "      Valid-Format("Leopoldo",{&FormRFC}) SKIP
        "Válido GUGL680818IS6: " Valid-Format("GUGL-680818-IS6",{&FormRFC}) VIEW-AS ALERT-BOX.

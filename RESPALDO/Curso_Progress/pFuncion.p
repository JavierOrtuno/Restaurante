DEF VAR vintn AS INT. 
DEF VAR vintTotal AS INT.

FUNCTION factorial INT (vintn AS INT):
    DEF VAR vintIdx AS INT.
    DEF VAR vintfac AS INT INIT 1.
    DO vintIdx = 2 TO vintn:
        vintfac = vintfac * vintIdx.
    END.
    RETURN vintfac.
END.

UPDATE vintn.

vintTotal = factorial(vintn).

MESSAGE "El resultado x1:" vintTotal VIEW-AS ALERT-BOX

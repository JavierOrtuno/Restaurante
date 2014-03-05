DEF VAR archivo AS CHAR.
archivo = "C:\Curso.Progress\datos\call_click.csv".
OUTPUT TO VALUE (archivo).
FOR EACH CALL_click NO-LOCK.
    EXPORT DELIMITER "," CALL_click.
END.

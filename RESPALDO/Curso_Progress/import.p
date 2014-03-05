DELETE FROM CALL_click.
INPUT FROM VALUE("C:\Curso.Progress\datos\call_click.txt").
REPEAT :
    CREATE CALL_click.
    IMPORT DELIMITER "," CALL_click.
END.

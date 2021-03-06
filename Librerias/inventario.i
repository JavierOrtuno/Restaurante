&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*------------------------------------------------------------------------
    Library     : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-DescontarExistencia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DescontarExistencia Method-Library 
FUNCTION DescontarExistencia RETURNS LOGICAL
  ( vintproducto AS INT, vintcantidad AS INT, vinttipo AS INT, vintuser AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Genera_Lote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Genera_Lote Method-Library 
FUNCTION Genera_Lote RETURNS CHARACTER
      ( vdtefecha AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Insertar_Bitacora) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Insertar_Bitacora Method-Library 
FUNCTION Insertar_Bitacora RETURNS LOGICAL
  ( vintmov AS INT, vintuser AS INT, vintstock AS INT, vintcant AS INT /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Method-Library 
/* ************************* Included-Libraries *********************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-DescontarExistencia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DescontarExistencia Method-Library 
FUNCTION DescontarExistencia RETURNS LOGICAL
  ( vintproducto AS INT, vintcantidad AS INT, vinttipo AS INT, vintuser AS INT) :
/*------------------------------------------------------------------------------
  Purpose: Descontar en el stock, los ingredientes usados ya sea en desperdicio
  o consumo
    Notes:  El parametro vintipo es 1- consumo, 2- desperdicio
------------------------------------------------------------------------------*/
DEF VAR vinttotal  AS INT.
DEF VAR vdteactual AS DATE.

vdteactual = TODAY.
CASE vinttipo:
    WHEN 1 THEN DO:
        Insertar_Bitacora(3,vintuser,vintproducto,vintcantidad).
        FOR EACH stock WHERE stock.id_producto = vintproducto AND stock.f_caducidad > vdteactual AND stock.cantidad > 0 BY f_caducidad BY cantidad.
            IF stock.cantidad >= vintcantidad THEN DO:
                vinttotal = stock.cantidad - vintcantidad.
                ASSIGN stock.cantidad = vinttotal.
                LEAVE.
            END.
            ELSE DO:
                vintcantidad = vintcantidad - stock.cantidad.
                ASSIGN stock.cantidad = 0.
                IF vintcantidad = 0 THEN DO:
                    LEAVE.
                END.
            END.
        END.
    END.
    WHEN 2 THEN DO:
        FOR EACH stock WHERE id_producto = vintproducto AND f_caducidad < vdteactual AND cantidad > 0 BY f_caducidad BY cantidad.
            IF cantidad >= vintcantidad THEN DO:
                vinttotal = cantidad - vintcantidad.
                ASSIGN stock.cantidad = vinttotal.
                LEAVE.
            END.
            ELSE DO:
                vintcantidad = vintcantidad - cantidad.
                ASSIGN stock.cantidad = 0.
                IF vintcantidad = 0 THEN DO:
                    LEAVE.
                END.
            END.
        END.
    END.
END CASE.
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Genera_Lote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Genera_Lote Method-Library 
FUNCTION Genera_Lote RETURNS CHARACTER
      ( vdtefecha AS DATE ) :
    /*------------------------------------------------------------------------------
      Purpose:  Genera el numero de lote para el ingreso de un producto en el stock
        Notes:  
    ------------------------------------------------------------------------------*/
    DEF VAR vintnumlote AS INT.
    DEF VAR vchrlote    AS CHAR.
    DEF VAR vchrmes     AS CHAR.
    DEF VAR vchrdia     AS CHAR.
    DEF VAR vchrano     AS CHAR.
    DEF VAR vdteactual  AS DATE.

    vdteactual = TODAY.
    FIND LAST stock WHERE f_ingreso = vdtefecha NO-LOCK NO-ERROR.
    IF AVAILABLE stock THEN DO:
      vintnumlote = INT(SUBSTR (stock.lote,LENGTH(TRIM(stock.lote)) - 2)).
      vintnumlote = vintnumlote + 1.
      IF vintnumlote < 10 THEN DO:
          vchrlote = "00" + string(vintnumlote).
      END.
      ELSE DO:
         IF vintnumlote < 100 THEN DO:
             vchrlote = "0" + string(vintnumlote).
         END.
         END.
      IF  int(DAY(vdteactual)) < 10 THEN DO:
          vchrdia = "0" + STRING(DAY(vdteactual)).
      END.
      ELSE DO:
          vchrdia = STRING(DAY(vdteactual)).
      END.
      IF  int(MONTH(vdteactual)) < 10 THEN DO:
          vchrmes = "0" + STRING(MONTH(vdteactual)).
      END.
      ELSE DO:
          vchrmes = STRING(MONTH(vdteactual)).
      END.
      vchrano = (SUBSTR(STRING(YEAR(vdteactual)),3)).
      vchrlote = "LT-" + vchrdia + vchrmes + vchrano + vchrlote.
    END.
    ELSE DO:
        IF  int(DAY(vdteactual)) < 10 THEN DO:
          vchrdia = "0" + STRING(DAY(vdteactual)).
        END.
        ELSE DO:
          vchrdia = STRING(DAY(vdteactual)).
        END.
        IF  int(MONTH(vdteactual)) < 10 THEN DO:
          vchrmes = "0" + STRING(MONTH(vdteactual)).
        END.
        ELSE DO:
          vchrmes = STRING(MONTH(vdteactual)).
        END.
        vchrano = (SUBSTR(STRING(YEAR(vdteactual)),3)).
        vchrlote = "LT-" + vchrdia + vchrmes + vchrano + "001".
    END.

      RETURN vchrlote.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Insertar_Bitacora) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Insertar_Bitacora Method-Library 
FUNCTION Insertar_Bitacora RETURNS LOGICAL
  ( vintmov AS INT, vintuser AS INT, vintstock AS INT, vintcant AS INT /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Inserta el tipo de movimiento en el bitacora
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR vdteactual AS DATE.

vdteactual = TODAY.

CREATE bitacora_stock.
ASSIGN bitacora_stock.id_bit_stock = NEXT-VALUE(sec_bitacora_stock)
       bitacora_stock.fecha = vdteactual
       bitacora_stock.id_movimiento = vintmov
       bitacora_stock.id_usuario = vintuser
       bitacora_stock.id_stock = vintstock
       bitacora_stock.cantidad = vintcant.
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


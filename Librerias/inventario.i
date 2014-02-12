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
  ( vintproducto AS INT, vintcantidad AS INT, vinttipo AS INT  )  FORWARD.

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
  ( vintproducto AS INT, vintcantidad AS INT, vinttipo AS INT  ) :
/*------------------------------------------------------------------------------
  Purpose: Descontar en el stock, los ingredientes usados ya sea en desperdicio
  o consumo
    Notes:  El parametro vintipo es 1- consumo, 2- desperdicio
------------------------------------------------------------------------------*/
DEF VAR vdteactual AS DATE.
DEF VAR vinttotal  AS INT.

vdteactual = TODAY.
CASE vintproducto:
    WHEN 1 THEN DO:
        FOR EACH stock WHERE id_producto = vintproducto AND f_caducidad > vdteactual AND cantidad > 0 BY f_caducidad BY cantidad.
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
    WHEN 2 THEN DO:

    END.
END CASE.
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


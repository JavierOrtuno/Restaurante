&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Area Include 
FUNCTION Area RETURNS DECIMAL
  (vdecA AS DEC, vdecB AS DEC, vdecC AS DEC)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Cuadraticas Include 
FUNCTION Cuadraticas RETURNS DECIMAL
  (vinta AS INT, vintb AS INT, vintc AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Distancias Include 
FUNCTION Distancias RETURNS DECIMAL
    (vintx1 AS INT, vinty1 AS INT, vintx2 AS INT, vinty2 AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Factorial Include 
FUNCTION Factorial RETURNS INTEGER
    (vintn AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Integral Include 
FUNCTION Integral RETURNS DECIMAL 
    ( vdecA AS DEC, vdecB AS DEC, vdecC AS DEC, vdecX1 AS DEC, vdecX2 AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 14.91
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Sebastian Include 
PROCEDURE Sebastian :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Area Include 
FUNCTION Area RETURNS DECIMAL
  (vdecA AS DEC, vdecB AS DEC, vdecC AS DEC) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR vdecPeri AS DEC.
DEF VAR vdecArea AS DEC.

vdecPeri = (vdecA + vdecB + vdecC) / 2.
vdecArea = SQRT (vdecPeri * (vdecPeri - vdecA) * (vdecPeri - vdecB) * (vdecPeri - vdecC)).

  RETURN vdecArea.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Cuadraticas Include 
FUNCTION Cuadraticas RETURNS DECIMAL
  (vinta AS INT, vintb AS INT, vintc AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR vdecraiz AS DEC.
DEF VAR odecTotal AS DEC.
DEF VAR odecTotal2 AS DEC.

vdecraiz = EXP (vintb,2) - ( ( 4 * vinta ) * vintc ).
vdecraiz = SQRT(vdecraiz).
odecTotal  = ( ( - vintb + vdecraiz ) / ( 2 * vinta ) ).
odecTotal2 = ( ( - vintb - vdecraiz ) / ( 2 * vinta ) ).

  RETURN odecTotal.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Distancias Include 
FUNCTION Distancias RETURNS DECIMAL
    (vintx1 AS INT, vinty1 AS INT, vintx2 AS INT, vinty2 AS INT) :

DEF VAR vdecTotal AS DEC.

vdecTotal = EXP (vintx2 - vintx1,2) + EXP (vinty2 - vinty1,2).
vdecTotal = SQRT(vdecTotal).

MESSAGE vdecTotal VIEW-AS ALERT-BOX.

  RETURN vdecTotal.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Factorial Include 
FUNCTION Factorial RETURNS INTEGER
    (vintn AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEF VAR vintIdx AS INT.
    DEF VAR vintfac AS INT INIT 1.
    DO vintIdx = 2 TO vintn:
        vintfac = vintfac * vintIdx.
    END.

  RETURN vintfac.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Integral Include 
FUNCTION Integral RETURNS DECIMAL 
    ( vdecA AS DEC, vdecB AS DEC, vdecC AS DEC, vdecX1 AS DEC, vdecX2 AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR vdecf1 AS DEC.
DEF VAR vdecf2 AS DEC.

vdecf1 = (((1 / 3) * vdecA) * EXP (vdecX1,3)) + (((1 / 2) * vdecB) * EXP (vdecX1,2)) + (vdecC * vdecX1).
vdecf2 = (((1 / 3) * vdecA) * EXP (vdecX2,3)) + (((1 / 2) * vdecB) * EXP (vdecX2,2)) + (vdecC * vdecX2).

    RETURN vdecf1.   /* Function return value. */
    RETURN vdecf2.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


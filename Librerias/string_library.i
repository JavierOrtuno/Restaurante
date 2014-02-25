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
&SCOPED-DEFINE cListCharLetras "Á,É,Í,Ó,Ú,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z"
&SCOPED-DEFINE cListCharNumeros "1,2,3,4,5,6,7,8,9,0"

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fillLeft) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fillLeft Method-Library 
FUNCTION fillLeft RETURNS CHARACTER
    ( INPUT vcharString AS CHARACTER,
      INPUT vcharFill AS CHARACTER,
      INPUT vintPosition AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isLetter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isLetter Method-Library 
FUNCTION isLetter RETURNS LOGICAL
    (INPUT vcharCadena AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isNumber) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isNumber Method-Library 
FUNCTION isNumber RETURNS LOGICAL
    (INPUT vcharCadena AS CHARACTER)  FORWARD.

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
         HEIGHT             = 15.52
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

&IF DEFINED(EXCLUDE-fillLeft) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fillLeft Method-Library 
FUNCTION fillLeft RETURNS CHARACTER
    ( INPUT vcharString AS CHARACTER,
      INPUT vcharFill AS CHARACTER,
      INPUT vintPosition AS INTEGER ) :
    /*------------------------------------------------------------------------------
        Purpose: Validar el tamaño de una Cadena y rrellenar con un caracter
            enviado a la izquierda de la cadena, en base a las posiciones requeridas,
            si es mayor de esas posiciones, truncara la cadena cortandole el principio de la misma.  
        Notes:  
        Parameters:
            vcharString: Cadena a Validar
            vcharFill: Caracter para Rellenar
            vintPosition: Posiciones desadas en la salida
        Author: I.S.C. Fco. Javier Ortuño Colchado
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vintSize AS INTEGER.
    DEFINE VARIABLE vintCount AS INTEGER.
    DEFINE VARIABLE vcharOutputString AS CHARACTER.
    DEFINE VARIABLE vcharLeftFill AS CHARACTER.

    IF LENGTH(TRIM(vcharString)) <> 0 AND
       TRIM(vcharString) <> "0" AND 
       TRIM(vcharString) <> "" THEN
        vcharString = LEFT-TRIM(vcharString, "0").

    vintSize = LENGTH(vcharString) * 2.    
    vintSize = vintSize - (NUM-ENTRIES(vcharString, ".") - 1).
    vintSize = vintSize - (NUM-ENTRIES(vcharString, "*") - 1).        
    vintSize = vintSize - (NUM-ENTRIES(vcharString, "-") - 1).
    vintSize = vintSize - (NUM-ENTRIES(vcharString, "(") - 1).
    vintSize = vintSize - (NUM-ENTRIES(vcharString, ")") - 1).
    vintSize = vintSize - (NUM-ENTRIES(vcharString, "!") - 1).
    vintSize = vintSize - (NUM-ENTRIES(vcharString, "^") - 1).

    IF vintSize < vintPosition THEN DO:
        DO vintCount = vintSize TO vintPosition:
            vcharLeftFill = vcharLeftFill + vcharFill.
        END.
        vcharOutputString = vcharLeftFill + vcharString.
    END.
    ELSE
        IF vintSize > vintPosition THEN
            vcharOutputString = SUBSTR(vcharString, vintSize - vintPosition, vintSize).
        ELSE
            vcharOutputString = vcharString.   
        
    RETURN vcharOutputString.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isLetter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isLetter Method-Library 
FUNCTION isLetter RETURNS LOGICAL
    (INPUT vcharCadena AS CHARACTER) :
    /*------------------------------------------------------------------------------
        Purpose: Validar Solo Lettras.
        Notes: Función para Validar si una Cadena contiene solo Letras.
        Author: I.S.C. Fco. Javier Ortuño C.
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vlogResponse AS LOGICAL INITIAL TRUE.
    DEFINE VARIABLE vintCont AS INTEGER.
    DEFINE VARIABLE vintSizeCadena AS INTEGER.    

    vintSizeCadena = LENGTH(vcharCadena).

    DO vintCont = 1 TO vintSizeCadena:
        IF (LOOKUP (SUBSTRING (vcharCadena, vintCont, 1), {&cListCharLetras}) > 0) = FALSE THEN DO:
            vlogResponse = FALSE.
            LEAVE.
        END.
    END.

    RETURN vlogResponse.    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isNumber) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isNumber Method-Library 
FUNCTION isNumber RETURNS LOGICAL
    (INPUT vcharCadena AS CHARACTER) :
    /*------------------------------------------------------------------------------
        Purpose: Validar Solo Números.
        Notes: Función para Validar si una Cadena contiene solo Números.
        Author: I.S.C. Fco. Javier Ortuño C.
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vlogResponse AS LOGICAL INITIAL TRUE.
    DEFINE VARIABLE vintCont AS INTEGER.
    DEFINE VARIABLE vintSizeCadena AS INTEGER.

    vintSizeCadena = LENGTH(vcharCadena).

    DO vintCont = 1 TO vintSizeCadena:
        IF (LOOKUP (SUBSTRING (vcharCadena, vintCont, 1), {&cListCharNumeros}) > 0) = FALSE THEN DO:
            vlogResponse = FALSE.
            LEAVE.
        END.
    END.

    RETURN vlogResponse.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


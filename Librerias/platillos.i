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

&IF DEFINED(EXCLUDE-getCatClasificacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCatClasificacion Method-Library 
FUNCTION getCatClasificacion RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCodigoPlatillo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCodigoPlatillo Method-Library 
FUNCTION getCodigoPlatillo RETURNS CHARACTER
    ( INPUT vintIdMenu AS INTEGER, 
      INPUT vcharCaracter AS CHARACTER, 
      INPUT vintElementos AS INTEGER )  FORWARD.

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


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-addIngredientes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addIngredientes Method-Library 
PROCEDURE addIngredientes :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pinIntMenu AS INTEGER.
    DEFINE INPUT PARAMETER pinCharIngredientes AS CHARACTER.
    DEFINE VARIABLE vintCount AS INTEGER.
    MESSAGE pinCharIngredientes VIEW-AS ALERT-BOX.
    DO vintCount = 1 TO NUM-ENTRIES(pinCharIngredientes):
        CREATE INGREDIENTE.
        ASSIGN 
            INGREDIENTE.ID_INGREDIENTE = NEXT-VALUE(SEC_INGREDIENTE)
            INGREDIENTE.CANTIDAD = 0
            INGREDIENTE.ID_MENU = pinIntMenu
            INGREDIENTE.ID_PRODUCTO = INTEGER(ENTRY(vintCount, pinCharIngredientes)). 
        MESSAGE ENTRY(vintCount, pinCharIngredientes) VIEW-AS ALERT-BOX.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addPlatillo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addPlatillo Method-Library 
PROCEDURE addPlatillo :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:           
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pinCharPlatillo AS CHARACTER.
    DEFINE INPUT PARAMETER pinDecPrecio AS DECIMAL.
    DEFINE INPUT PARAMETER pinIntClasificacion AS INTEGER.
    DEFINE INPUT PARAMETER pinCharIngredientes AS CHARACTER.
    DEFINE VARIABLE vintIdMenu AS INTEGER.    
    DEFINE VARIABLE vcharCodigo AS CHARACTER.
    
    vintIdMenu = NEXT-VALUE(SEC_MENU).
    vcharCodigo = getCodigoPlatillo(vintIdMenu, "0", 4).

    CREATE MENU.
    ASSIGN 
        MENU.ID_MENU = vintIdMenu
        MENU.CODIGO = vcharCodigo
        MENU.DESCRIPCION = pinCharPlatillo
        MENU.PRECIO = pinDecPrecio
        MENU.ID_CLASIFICACION = pinIntClasificacion.   

    RUN addIngredientes(vintIdMenu, pinCharIngredientes).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-updatePlatillo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updatePlatillo Method-Library 
PROCEDURE updatePlatillo :
/*--    ----------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getCatClasificacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCatClasificacion Method-Library 
FUNCTION getCatClasificacion RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
        Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcharCatalogo AS CHARACTER INITIAL "".

    FOR EACH CLASIFICACION NO-LOCK:
        vcharCatalogo = vcharCatalogo + 
            CLASIFICACION.DESCRIPCION + "," +
            STRING(CLASIFICACION.ID_CLASIFICACION) + ",".
    END.    

    RETURN TRIM(vcharCatalogo, ",").
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCodigoPlatillo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCodigoPlatillo Method-Library 
FUNCTION getCodigoPlatillo RETURNS CHARACTER
    ( INPUT vintIdMenu AS INTEGER, 
      INPUT vcharCaracter AS CHARACTER, 
      INPUT vintElementos AS INTEGER ) :
    /*------------------------------------------------------------------------------
        Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vintCount AS INTEGER.
    DEFINE VARIABLE vcharCodigo AS CHARACTER INITIAL "PT-".    

    DO vintCount = LENGTH(STRING(vintIdMenu)) TO vintElementos:
        vcharCodigo = vcharCodigo + vcharCaracter.
    END.
    vcharCodigo = vcharCodigo + STRING(vintIdMenu).

    RETURN vcharCodigo.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


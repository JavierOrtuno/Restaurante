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

&IF DEFINED(EXCLUDE-canDeletePlatillo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD canDeletePlatillo Method-Library 
FUNCTION canDeletePlatillo RETURNS LOGICAL
    ( INPUT vrowID AS ROWID )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCatClasificacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCatClasificacion Method-Library 
FUNCTION getCatClasificacion RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getClasificacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getClasificacion Method-Library 
FUNCTION getClasificacion RETURNS CHARACTER
    ( INPUT vintIdClasificacion AS INTEGER )  FORWARD.

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
        Purpose: Agregar Ingredientes a un Platillo
        Parameters:  
            INPUT pinIntMenu (ID DE MENU)
            INPUT pinCharIngredientes (LISTA DE INGREDIENTES)
        Author: I.S.C. Fco. Javier Ortuño Colchado  
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pinIntMenu AS INTEGER.
    DEFINE INPUT PARAMETER pinCharIngredientes AS CHARACTER.
    DEFINE VARIABLE vintCount AS INTEGER.
    
    IF NUM-ENTRIES(pinCharIngredientes, "|") > 1 THEN DO:
    
        DO vintCount = 1 TO NUM-ENTRIES(pinCharIngredientes, "|"):
            CREATE INGREDIENTE.
            ASSIGN 
                INGREDIENTE.ID_INGREDIENTE = NEXT-VALUE(SEC_INGREDIENTE)
                INGREDIENTE.CANTIDAD = INTEGER(ENTRY(1, ENTRY(vintCount, pinCharIngredientes, "|")))
                INGREDIENTE.ID_MENU = pinIntMenu
                INGREDIENTE.ID_PRODUCTO = INTEGER(ENTRY(2, ENTRY(vintCount, pinCharIngredientes, "|"))).
        END.

    END.
    ELSE DO:
        
        CREATE INGREDIENTE.
        ASSIGN 
            INGREDIENTE.ID_INGREDIENTE = NEXT-VALUE(SEC_INGREDIENTE)
            INGREDIENTE.CANTIDAD = INTEGER(ENTRY(1, pinCharIngredientes))
            INGREDIENTE.ID_MENU = pinIntMenu
            INGREDIENTE.ID_PRODUCTO = INTEGER(ENTRY(2, pinCharIngredientes)).

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addPlatillo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addPlatillo Method-Library 
PROCEDURE addPlatillo :
/*------------------------------------------------------------------------------
        Purpose: Agregar un Platillo
        Parameters: 
            INPUT pinCharPlatillo (NOMBRE DEL PLATILLO)
            INPUT pinDecPrecio (PRECIO DEL PLATILLO)
            INPUT pinIntClasificacion (CLASIFICACION DEL PLATILLO)
            INPUT pinCharIngredientes (LISTA DE INGREDIENTES)
        Author: I.S.C. Fco. Javier Ortuño Colchado
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
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pinRowId AS ROWID.
    DEFINE INPUT PARAMETER pinCharPlatillo AS CHARACTER.
    DEFINE INPUT PARAMETER pinDecPrecio AS DECIMAL.
    DEFINE INPUT PARAMETER pinIntClasificacion AS INTEGER.
    DEFINE INPUT PARAMETER pinCharIngredientes AS CHARACTER.    

    FIND FIRST MENU WHERE ROWID(MENU) = pinRowId.
    ASSIGN         
        MENU.DESCRIPCION = pinCharPlatillo
        MENU.PRECIO = pinDecPrecio
        MENU.ID_CLASIFICACION = pinIntClasificacion.
        
    DELETE FROM INGREDIENTE WHERE INGREDIENTE.ID_MENU = MENU.ID_MENU.
    RUN addIngredientes(MENU.ID_MENU, pinCharIngredientes).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-canDeletePlatillo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION canDeletePlatillo Method-Library 
FUNCTION canDeletePlatillo RETURNS LOGICAL
    ( INPUT vrowID AS ROWID ) :
    /*------------------------------------------------------------------------------
        Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vlogResponse AS LOGICAL INITIAL TRUE.

    FIND FIRST MENU WHERE ROWID(MENU) = vrowID.

    IF CAN-FIND(CONSUMO WHERE CONSUMO.ID_MENU = MENU.ID_MENU) THEN
        vlogResponse = FALSE.
    
    RETURN vlogResponse.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCatClasificacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCatClasificacion Method-Library 
FUNCTION getCatClasificacion RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
        Purpose: Obtener Catálogo de Clasificaciones
        Author: I.S.C. Fco. Javier Ortuño Colchado
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

&IF DEFINED(EXCLUDE-getClasificacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getClasificacion Method-Library 
FUNCTION getClasificacion RETURNS CHARACTER
    ( INPUT vintIdClasificacion AS INTEGER ) :
    /*------------------------------------------------------------------------------
        Purpose: Obtener descripción de una Clasificación
        Author: I.S.C. Fco. Javier Ortuño Colchado    
    ------------------------------------------------------------------------------*/
    FIND FIRST CLASIFICACION WHERE CLASIFICACION.ID_CLASIFICACION = vintIdClasificacion NO-ERROR.

    RETURN CLASIFICACION.DESCRIPCION.
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
        Purpose: Generar Código de un Platillo.
        Author: I.S.C. Fco. Javier Ortuño Colchado
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


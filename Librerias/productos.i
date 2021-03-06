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

&IF DEFINED(EXCLUDE-canDeleteProducto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD canDeleteProducto Method-Library 
FUNCTION canDeleteProducto RETURNS LOGICAL
    ( INPUT vrowID AS ROWID )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCatProducto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCatProducto Method-Library 
FUNCTION getCatProducto RETURNS CHARACTER
    ( INPUT vcharInitial AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCatUnidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCatUnidad Method-Library 
FUNCTION getCatUnidad RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCodProducto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCodProducto Method-Library 
FUNCTION getCodProducto RETURNS CHARACTER
    ( INPUT vintIdProducto AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescProducto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescProducto Method-Library 
FUNCTION getDescProducto RETURNS CHARACTER
    ( INPUT vintIdProducto AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUnidadMedida) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUnidadMedida Method-Library 
FUNCTION getUnidadMedida RETURNS CHARACTER
    ( INPUT vintIdUnidad AS INTEGER )  FORWARD.

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

&IF DEFINED(EXCLUDE-addProducto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addProducto Method-Library 
PROCEDURE addProducto :
/*------------------------------------------------------------------------------
        Purpose: Agregar un Producto a Base de Datos     
        Parameters:  
            INPUT pinIntIdProducto (ID PRODUCTO)
            INPUT pinCharCodigo (CODIGO DE PRODUCTO)
            INPUT pinCharDescripcion (DESC DE PRODUCTO)
            INPUT pinIntCantidad (CANTIDAD MINIMA)
            INPUT pinIntUnidad (UNIDAD MEDIDA)
        Author: I.S.C. Fco. Javier Ortu�o Colchado
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pinIntIdProducto AS INTEGER.
    DEFINE INPUT PARAMETER pinCharCodigo AS CHARACTER.
    DEFINE INPUT PARAMETER pinCharDescripcion AS CHARACTER.
    DEFINE INPUT PARAMETER pinIntCantidad AS INTEGER.
    DEFINE INPUT PARAMETER pinIntUnidad AS INTEGER.
    
    CREATE PRODUCTO.
    ASSIGN 
        PRODUCTO.ID_PRODUCTO = pinIntIdProducto
        PRODUCTO.CODIGO = TRIM(pinCharCodigo)
        PRODUCTO.DESCRIPCION = TRIM(pinCharDescripcion)
        PRODUCTO.CANT_MINIMA = pinIntCantidad
        PRODUCTO.ID_UNIDAD = pinIntUnidad.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-updateProducto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateProducto Method-Library 
PROCEDURE updateProducto :
/*------------------------------------------------------------------------------
        Purpose: Actualizar un Producto a Base de Datos     
        Parameters:  
            INPUT pinIntRowId (ROWID dDE PRODUCTO)
            INPUT pinCharCodigo (CODIGO DE PRODUCTO)
            INPUT pinCharDescripcion (DESC DE PRODUCTO)
            INPUT pinIntCantidad (CANTIDAD MINIMA)
            INPUT pinIntUnidad (UNIDAD MEDIDA)
        Author: I.S.C. Fco. Javier Ortu�o Colchado
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pinIntRowId AS ROWID.
    DEFINE INPUT PARAMETER pinCharCodigo AS CHARACTER.
    DEFINE INPUT PARAMETER pinCharDescripcion AS CHARACTER.
    DEFINE INPUT PARAMETER pinIntCantidad AS INTEGER.
    DEFINE INPUT PARAMETER pinIntUnidad AS INTEGER.
    
    FIND PRODUCTO WHERE ROWID(PRODUCTO) = pinIntRowId NO-ERROR.
    ASSIGN 
        PRODUCTO.CODIGO = TRIM(pinCharCodigo)
        PRODUCTO.DESCRIPCION = TRIM(pinCharDescripcion)
        PRODUCTO.CANT_MINIMA = pinIntCantidad
        PRODUCTO.ID_UNIDAD = pinIntUnidad.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-canDeleteProducto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION canDeleteProducto Method-Library 
FUNCTION canDeleteProducto RETURNS LOGICAL
    ( INPUT vrowID AS ROWID ) :
    /*------------------------------------------------------------------------------
        Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vlogResponse AS LOGICAl INITIAL TRUE.

    FIND FIRST PRODUCTO WHERE ROWID(PRODUCTO) = vrowID.
    IF CAN-FIND(STOCK WHERE STOCK.ID_PRODUCTO = PRODUCTO.ID_PRODUCTO) OR 
       CAN-FIND(INGREDIENTE WHERE INGREDIENTE.ID_PRODUCTO = PRODUCTO.ID_PRODUCTO) 
        THEN
            vlogResponse = FALSE.

    RETURN vlogResponse.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCatProducto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCatProducto Method-Library 
FUNCTION getCatProducto RETURNS CHARACTER
    ( INPUT vcharInitial AS CHARACTER ) :
    /*------------------------------------------------------------------------------
        Purpose:  Obtener Cat�logo de Productos        
        Author: I.S.C. Fco. Javier Ortu�o Colchado
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcharCatalogo AS CHARACTER.

    IF TRIM(vcharInitial) = "" THEN DO:
        FOR EACH PRODUCTO NO-LOCK BY PRODUCTO.DESCRIPCION:
            vcharCatalogo = vcharCatalogo + 
                PRODUCTO.DESCRIPCION + "," +
                STRING(PRODUCTO.ID_PRODUCTO) + ",".
        END.
    END.    
    ELSE DO:
        FOR EACH PRODUCTO WHERE PRODUCTO.DESCRIPCION BEGINS TRIM(vcharInitial) NO-LOCK BY PRODUCTO.DESCRIPCION:
            vcharCatalogo = vcharCatalogo + 
                PRODUCTO.DESCRIPCION + "," +
                STRING(PRODUCTO.ID_PRODUCTO) + ",".
        END.
    END.
    
    RETURN TRIM(vcharCatalogo, ",").
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCatUnidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCatUnidad Method-Library 
FUNCTION getCatUnidad RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
        Purpose: Retorna Estructura del Cat�logo de Unidades de Medida 
        Notes: Formato(DESCRIPCION, ID, DESCRIPCION, ID ...)
        Author: I.S.C. Fco. Javier Ortu�o Colchado
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcharCatalogo AS CHARACTER.

    FOR EACH UNIDAD_MEDIDA NO-LOCK:
        vcharCatalogo = vcharCatalogo + 
            UNIDAD_MEDIDA.DESCRIPCION + "," +
            STRING(UNIDAD_MEDIDA.ID_UNIDAD) + ",".
    END.
    
    RETURN TRIM(vcharCatalogo, ",").
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCodProducto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCodProducto Method-Library 
FUNCTION getCodProducto RETURNS CHARACTER
    ( INPUT vintIdProducto AS INTEGER ) :
    /*------------------------------------------------------------------------------
        Purpose: Funci�n para Generar el Siguiente C�digo del Producto en DB        
        Author: I.S.C. Fco. Javier Ortu�o Colchado
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vintCount AS INTEGER.
    DEFINE VARIABLE vcharIdProd AS CHARACTER.
    DEFINE VARIABLE vcharCodigo AS CHARACTER INITIAL "PR-".    

    vcharIdProd = STRING(vintIdProducto).
    vintCount = LENGTH(vcharIdProd).
    DO vintCount = vintCount + 1 TO 4:
        vcharCodigo = vcharCodigo + "0".
    END.
    vcharCodigo = vcharCodigo + vcharIdProd.

    RETURN vcharCodigo.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDescProducto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescProducto Method-Library 
FUNCTION getDescProducto RETURNS CHARACTER
    ( INPUT vintIdProducto AS INTEGER ) :
    /*------------------------------------------------------------------------------
        Purpose:  Obtener la Descripci�n de un Producto
        Author: I.S.C. Fco. Javier Ortu�o Colchado
    ------------------------------------------------------------------------------*/
    FIND FIRST PRODUCTO WHERE ID_PRODUCTO = vintIdProducto NO-ERROR.

    RETURN PRODUCTO.DESCRIPCION.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUnidadMedida) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUnidadMedida Method-Library 
FUNCTION getUnidadMedida RETURNS CHARACTER
    ( INPUT vintIdUnidad AS INTEGER ) :
    /*------------------------------------------------------------------------------
        Purpose: Obtener la Descripci�n de la Unidad de Medida por ID        
        Author: I.S.C. Fco. Javier Ortu�o Colchado
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcharUnidadMedida AS CHARACTER INITIAL "".

    FIND FIRST UNIDAD_MEDIDA 
        WHERE UNIDAD_MEDIDA.ID_UNIDAD = vintIdUnidad NO-LOCK NO-ERROR.
    vcharUnidadMedida = UNIDAD_MEDIDA.DESCRIPCION.

    RETURN vcharUnidadMedida.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


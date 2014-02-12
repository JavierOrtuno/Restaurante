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
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pinIntIdProducto AS INTEGER.
    DEFINE INPUT PARAMETER pinCharCodigo AS CHARACTER.
    DEFINE INPUT PARAMETER pinCharDescripcion AS CHARACTER.
    DEFINE INPUT PARAMETER pinIntCantidad AS INTEGER.
    DEFINE INPUT PARAMETER pinIntUnidad AS INTEGER.
    
    CREATE PRODUCTO.
        ASSIGN 
            PRODUCTO.ID_PRODUCTO = pinIntIdProducto
            PRODUCTO.CODIGO = pinCharCodigo
            PRODUCTO.DESCRIPCION = pinCharDescripcion
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
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getCatUnidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCatUnidad Method-Library 
FUNCTION getCatUnidad RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
        Purpose: Retorna Estructura del Catálogo de Unidades de Medida 
        Notes: Formato(DESCRIPCION, ID, DESCRIPCION, ID ...)
        Author: I.S.C. Fco. Javier Ortuño Colchado
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
        Purpose: Función para Generar el Siguiente Código del Producto en DB
        Notes:  
        Author: I.S.C. Fco. Javier Ortuño Colchado
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

&IF DEFINED(EXCLUDE-getUnidadMedida) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUnidadMedida Method-Library 
FUNCTION getUnidadMedida RETURNS CHARACTER
    ( INPUT vintIdUnidad AS INTEGER ) :
    /*------------------------------------------------------------------------------
        Purpose: Obtener la Descripción de la Unidad de Medida por ID
        Notes: 
        Author: I.S.C. Fco. Javier Ortuño Colchado
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcharUnidadMedida AS CHARACTER INITIAL "".

    FIND FIRST UNIDAD_MEDIDA 
        WHERE UNIDAD_MEDIDA.ID_UNIDAD = vintIdUnidad NO-LOCK.    
    vcharUnidadMedida = UNIDAD_MEDIDA.DESCRIPCION.

    RETURN vcharUnidadMedida.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


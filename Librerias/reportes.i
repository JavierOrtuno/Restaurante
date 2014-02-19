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

&IF DEFINED(EXCLUDE-getDescClasificacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescClasificacion Method-Library 
FUNCTION getDescClasificacion RETURNS CHARACTER
  ( INPUT vintIdClasificacion AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getReporteMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReporteMenu Method-Library 
FUNCTION getReporteMenu RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

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

&IF DEFINED(EXCLUDE-getDescClasificacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescClasificacion Method-Library 
FUNCTION getDescClasificacion RETURNS CHARACTER
  ( INPUT vintIdClasificacion AS INTEGER ) :
    /*------------------------------------------------------------------------------
        Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    FIND FIRST CLASIFICACION WHERE CLASIFICACION.ID_CLASIFICACION = vintIdClasificacion.

    RETURN CLASIFICACION.DESCRIPCION.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getReporteMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getReporteMenu Method-Library 
FUNCTION getReporteMenu RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
        Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcharReporte AS CHARACTER.
    DEFINE VARIABLE vintBandera AS INTEGER.
    DEFINE VARIABLE vlogPrev AS LOGICAL.
    
    FOR EACH MENU BY MENU.ID_CLASIFICACION:
        IF vintBandera <> MENU.ID_CLASIFICACION THEN DO:
            IF vlogPrev = TRUE THEN DO:
                vcharReporte = vcharReporte + "</ul>~n</div>~n".
                vlogPrev = FALSE.
            END.
            vcharReporte = vcharReporte + "<div class='clasificacion shadow'>~n".
            vcharReporte = vcharReporte + "<div class='titulo text-shadow'>" + getDescClasificacion(MENU.ID_CLASIFICACION) + "</div>~n".
            vcharReporte = vcharReporte + "<ul>~n".
            vcharReporte = vcharReporte + "<li>" + MENU.DESCRIPCION + "<div>$" + STRING(MENU.PRECIO) + "</div></li>~n".
            vlogPrev = TRUE.
        END.
        ELSE
            vcharReporte = vcharReporte + "<li>" + MENU.DESCRIPCION + "<div>$" + STRING(MENU.PRECIO) + "</div></li>~n".
        vintBandera = MENU.ID_CLASIFICACION.
    END.
    IF vlogPrev = TRUE THEN
        vcharReporte = vcharReporte + "</ul>~n</div>~n".
    RETURN vcharReporte.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


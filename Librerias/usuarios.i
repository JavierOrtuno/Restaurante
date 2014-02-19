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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addUsuario Include 
PROCEDURE addUsuario :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER inIntIdUsuario AS INTEGER.
    DEFINE INPUT PARAMETER inCharUsuario AS CHARACTER.
    DEFINE INPUT PARAMETER inCharContrasena AS CHARACTER.
    
    CREATE USUARIO.
    ASSIGN 
        USUARIO.ID_USUARIO = inIntIdUsuario
        USUARIO.USUARIO = inCharUsuario
        USUARIO.CONTRASENIA = inCharContrasena.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteUsuario Include 
PROCEDURE deleteUsuario :
/*------------------------------------------------------------------------------
        Purpose:
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER inIntRowId AS ROWID.
    DEFINE INPUT PARAMETER inCharUsuario AS CHARACTER.
    DEFINE INPUT PARAMETER inCharContra AS CHARACTER.
    
    FIND USUARIO WHERE ROWID(USUARIO) = inIntRowId.
    DELETE Usuario.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateUsuario Include 
PROCEDURE updateUsuario :
/*------------------------------------------------------------------------------
        Purpose:
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER inIntRowId AS ROWID.
    DEFINE INPUT PARAMETER inCharUsuario AS CHARACTER.
    DEFINE INPUT PARAMETER inCharContra AS CHARACTER.
    
    FIND USUARIO WHERE ROWID(USUARIO) = inIntRowId.
    ASSIGN 
        USUARIO.USUARIO = inCharUsuario.
        USUARIO.CONTRASENIA = inCharContra.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


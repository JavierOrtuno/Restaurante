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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addPersona Include 
FUNCTION addPersona RETURNS INTEGER
  ( INPUT inCharNombre AS CHAR,  INPUT inCharApat AS CHAR,
    INPUT inCharAmat AS CHAR,    INPUT inCharSexo AS CHAR,
    INPUT inCharFecha AS CHAR,   INPUT inCharCurp AS CHAR,
    INPUT inCharRfc AS CHAR,     INPUT inCharDomicilio AS CHAR,
    INPUT inCharCorreo AS CHAR
     )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addUsuario Include 
FUNCTION addUsuario RETURNS INTEGER
  ( INPUT inCharUsuario AS CHARACTER,
    INPUT inCharContrasena AS CHARACTER )  FORWARD.

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
         HEIGHT             = 13.43
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteUsuario Include 
PROCEDURE deleteUsuario :
/*------------------------------------------------------------------------------
        Purpose:
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER inIntRowId AS ROWID.
    
    FIND USUARIO WHERE ROWID(USUARIO) = inIntRowId.
       FIND Empleado WHERE Empleado.id_Usuario = usuario.Id_Usuario.
           FIND Persona WHERE Persona.Id_Persona = Empleado.Id_Persona.
           DELETE Persona.
           DELETE Usuario.
           DELETE Empleado.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updatePersona Include 
PROCEDURE updatePersona :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER inIntRowId AS ROWID.          DEFINE INPUT PARAMETER inintIdRol AS INT.            
    DEFINE INPUT PARAMETER inCharNombre AS CHARACTER.    DEFINE INPUT PARAMETER inCharApat AS CHARACTER.      
    DEFINE INPUT PARAMETER inCharAmat AS CHARACTER.      DEFINE INPUT PARAMETER inCharSexo AS CHARACTER.      
    DEFINE INPUT PARAMETER inCharFecha AS CHARACTER.     DEFINE INPUT PARAMETER inCharCurp AS CHARACTER.      
    DEFINE INPUT PARAMETER inCharRfc AS CHARACTER.       DEFINE INPUT PARAMETER inCharDomicilio AS CHARACTER. 
    DEFINE INPUT PARAMETER inCharCorreo AS CHARACTER.
            

    FIND USUARIO WHERE ROWID(USUARIO) = inIntRowId.
       FIND FIRST Empleado WHERE Empleado.id_Usuario = usuario.Id_Usuario.
       ASSIGN
           EMPLEADO.ID_ROL = inintIdRol.
           FIND FIRST Persona WHERE Persona.Id_Persona = Empleado.Id_Persona.

    ASSIGN 
        PERSONA.NOMBRE = inCharNombre
        PERSONA.A_PATERNO = inCharApat            PERSONA.A_MATERNO = inCharAmat
        PERSONA.SEXO = inCharSexo                 PERSONA.F_NACIMIENTO = DATE(inCharFecha)
        PERSONA.CURP = inCharCurp                 PERSONA.RFC = inCharRfc
        PERSONA.DOMICILIO = inCharDomicilio       PERSONA.CORREO = inCharCorreo.

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
    DEFINE INPUT PARAMETER inCharUsuario AS CHAR.
    DEFINE INPUT PARAMETER inCharContra AS CHAR.
    
    FIND USUARIO WHERE ROWID(USUARIO) = inIntRowId.

    ASSIGN 
        USUARIO.USUARIO = inCharUsuario.
        USUARIO.CONTRASENIA = inCharContra.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addPersona Include 
FUNCTION addPersona RETURNS INTEGER
  ( INPUT inCharNombre AS CHAR,  INPUT inCharApat AS CHAR,
    INPUT inCharAmat AS CHAR,    INPUT inCharSexo AS CHAR,
    INPUT inCharFecha AS CHAR,   INPUT inCharCurp AS CHAR,
    INPUT inCharRfc AS CHAR,     INPUT inCharDomicilio AS CHAR,
    INPUT inCharCorreo AS CHAR
     ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vintpersona AS INT.

    vintpersona = NEXT-VALUE(SEC_PERSONA).

    CREATE PERSONA.
    ASSIGN 
        PERSONA.ID_PERSONA = vintpersona                      PERSONA.NOMBRE = inCharNombre
        PERSONA.A_PATERNO = inCharApat                        PERSONA.A_MATERNO = inCharAmat
        PERSONA.SEXO = inCharSexo                             PERSONA.F_NACIMIENTO = DATE(inCharFecha)
        PERSONA.CURP = inCharCurp                             PERSONA.RFC = inCharRfc
        PERSONA.DOMICILIO = inCharDomicilio                   PERSONA.CORREO = inCharCorreo.
  RETURN vintpersona.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addUsuario Include 
FUNCTION addUsuario RETURNS INTEGER
  ( INPUT inCharUsuario AS CHARACTER,
    INPUT inCharContrasena AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR vintIdUsuario AS INT.

  vintIdUsuario = NEXT-VALUE(SEC_USUARIO).

  CREATE USUARIO.
    ASSIGN 
        USUARIO.ID_USUARIO = vintIdUsuario.
        USUARIO.USUARIO = inCharUsuario.
        USUARIO.CONTRASENIA = inCharContrasena.

  RETURN vintIdUsuario.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


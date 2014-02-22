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
  ( INPUT inCharNombre AS CHARACTER,
    INPUT inCharApat AS CHARACTER,
    INPUT inCharAmat AS CHARACTER,
    INPUT inCharSexo AS CHARACTER,
    INPUT inCharFecha AS CHARACTER,
    INPUT inCharCurp AS CHARACTER,
    INPUT inCharRfc AS CHARACTER,
    INPUT inCharDomicilio AS CHARACTER,
    INPUT inCharCorreo AS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addPersona1 Include 
PROCEDURE addPersona1 :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER inCharNombre AS CHARACTER.
    DEFINE INPUT PARAMETER inCharApat AS CHARACTER.
    DEFINE INPUT PARAMETER inCharAmat AS CHARACTER.
    DEFINE INPUT PARAMETER inCharSexo AS CHARACTER.
    DEFINE INPUT PARAMETER inCharFecha AS CHARACTER.
    DEFINE INPUT PARAMETER inCharCurp AS CHARACTER.
    DEFINE INPUT PARAMETER inCharRfc AS CHARACTER.
    DEFINE INPUT PARAMETER inCharDomicilio AS CHARACTER.
    DEFINE INPUT PARAMETER inCharCorreo AS CHARACTER.

    CREATE PERSONA.
    ASSIGN 
        PERSONA.ID_PERSONA = NEXT-VALUE(SEC_PERSONA)          PERSONA.NOMBRE = inCharNombre
        PERSONA.A_PATERNO = inCharApat                        PERSONA.A_MATERNO = inCharAmat
        PERSONA.SEXO = inCharSexo                             PERSONA.F_NACIMIENTO = DATE(inCharFecha)
        PERSONA.CURP = inCharCurp                             PERSONA.RFC = inCharRfc
        PERSONA.DOMICILIO = inCharDomicilio                   PERSONA.CORREO = inCharCorreo.

    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addUsuario1 Include 
PROCEDURE addUsuario1 :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER inCharUsuario AS CHARACTER.
    DEFINE INPUT PARAMETER inCharContrasena AS CHARACTER.
    
    CREATE USUARIO.
    ASSIGN 
        USUARIO.ID_USUARIO = NEXT-VALUE(SEC_USUARIO)
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
  /*  DEFINE INPUT PARAMETER inCharUsuario AS CHARACTER.
    DEFINE INPUT PARAMETER inCharContra AS CHARACTER.*/
    
    FIND USUARIO WHERE ROWID(USUARIO) = inIntRowId.
    DELETE Usuario.

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
    DEFINE INPUT PARAMETER inIntRowId AS ROWID.
    DEFINE INPUT PARAMETER inintIdRol AS INT.
    DEFINE INPUT PARAMETER inCharNombre AS CHARACTER.
    DEFINE INPUT PARAMETER inCharApat AS CHARACTER.
    DEFINE INPUT PARAMETER inCharAmat AS CHARACTER.
    DEFINE INPUT PARAMETER inCharSexo AS CHARACTER.
    DEFINE INPUT PARAMETER inCharFecha AS CHARACTER.
    DEFINE INPUT PARAMETER inCharCurp AS CHARACTER.
    DEFINE INPUT PARAMETER inCharRfc AS CHARACTER.
    DEFINE INPUT PARAMETER inCharDomicilio AS CHARACTER.
    DEFINE INPUT PARAMETER inCharCorreo AS CHARACTER.
    MESSAGE inintIdRol VIEW-AS ALERT-BOX.
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
    DEFINE INPUT PARAMETER inCharUsuario AS CHARACTER.
    DEFINE INPUT PARAMETER inCharContra AS CHARACTER.
    
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
  ( INPUT inCharNombre AS CHARACTER,
    INPUT inCharApat AS CHARACTER,
    INPUT inCharAmat AS CHARACTER,
    INPUT inCharSexo AS CHARACTER,
    INPUT inCharFecha AS CHARACTER,
    INPUT inCharCurp AS CHARACTER,
    INPUT inCharRfc AS CHARACTER,
    INPUT inCharDomicilio AS CHARACTER,
    INPUT inCharCorreo AS CHARACTER
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


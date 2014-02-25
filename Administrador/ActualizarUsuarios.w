&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dlg_UpdateUsua
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dlg_UpdateUsua 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{usuarios.i}
{login.i}
/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER inIntEvento AS INTEGER.
DEFINE INPUT PARAMETER inRowId AS ROWID.

DEF VAR vintIdRol AS INT.
DEF VAR vIntIdUsuario AS INT.
DEF VAR vintIdPersona AS INT.

/* Local Variable Definitions ---                                       
DEFINE VARIABLE vintIdUsuario AS INTEGER.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dlg_UpdateUsua

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Fill_Nombre Fill_Usuario Fill_Apat ~
Fill_Contrasena Fill_Amat Fill_Sexo Fill_FNac Btn_Aceptar Fill_Direccion ~
Fill_Curp Fill_RFC Fill_Correo Combo_Rol RECT-1 RECT-26 RECT-27 
&Scoped-Define DISPLAYED-OBJECTS Fill_Nombre Fill_Usuario Fill_Apat ~
Fill_Contrasena Fill_Amat Fill_Sexo Fill_FNac Fill_Direccion Fill_Curp ~
Fill_RFC Fill_Correo Fill_Rol Combo_Rol 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidarRegistroUsuario Dlg_UpdateUsua 
FUNCTION ValidarRegistroUsuario RETURNS LOGICAL
    ( vcharUsu    AS CHAR,
      vcharContra AS CHAR,
      vCharNombre AS CHAR, 
      vCharApat   AS CHAR, 
      vCharAmat   AS CHAR, 
      vintIdRol   AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Aceptar 
     LABEL "Aceptar" 
     SIZE 20 BY 2.86.

DEFINE VARIABLE Combo_Rol AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "SELECCIONAR","0"
     DROP-DOWN-LIST
     SIZE 31.4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_Amat AS CHARACTER FORMAT "X(256)":U 
     LABEL "(*) Apellido Materno" 
     VIEW-AS FILL-IN 
     SIZE 30.8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_Apat AS CHARACTER FORMAT "X(256)":U 
     LABEL "(*) Apellido Paterno" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_Contrasena AS CHARACTER FORMAT "X(100)":U 
     LABEL "(*) Contraseña" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_Correo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Correo" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_Curp AS CHARACTER FORMAT "X(18)":U 
     LABEL "Curp" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_Direccion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dirección" 
     VIEW-AS FILL-IN 
     SIZE 44.4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_FNac AS DATE FORMAT "99/99/99":U INITIAL 12/12/14 
     LABEL "F. Nacimiento dd/mm/aa" 
     VIEW-AS FILL-IN 
     SIZE 27.4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_Nombre AS CHARACTER FORMAT "X(256)":U 
     LABEL "(*) Nombre" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_RFC AS CHARACTER FORMAT "X(15)":U 
     LABEL "RFC" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_Rol AS CHARACTER FORMAT "X(256)":U 
     LABEL "(*) Descripción" 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1 NO-UNDO.

DEFINE VARIABLE Fill_Sexo AS CHARACTER FORMAT "X(1)":U 
     LABEL "Sexo (M/F)" 
     VIEW-AS FILL-IN 
     SIZE 16.8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_Usuario AS CHARACTER FORMAT "X(100)":U 
     LABEL "(*) Usuario" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 8  
     SIZE 145 BY 2.38
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 72 BY 15.19.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 48.4 BY 4.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dlg_UpdateUsua
     Fill_Nombre AT ROW 6.48 COL 34.8 COLON-ALIGNED
     Fill_Usuario AT ROW 6.76 COL 101 COLON-ALIGNED
     Fill_Apat AT ROW 7.91 COL 34.8 COLON-ALIGNED
     Fill_Contrasena AT ROW 8.67 COL 101 COLON-ALIGNED
     Fill_Amat AT ROW 9.33 COL 35 COLON-ALIGNED
     Fill_Sexo AT ROW 10.76 COL 35 COLON-ALIGNED
     Fill_FNac AT ROW 12.14 COL 35 COLON-ALIGNED
     Btn_Aceptar AT ROW 13.38 COL 101
     Fill_Direccion AT ROW 13.52 COL 35 COLON-ALIGNED
     Fill_Curp AT ROW 14.95 COL 35.2 COLON-ALIGNED
     Fill_RFC AT ROW 16.29 COL 35 COLON-ALIGNED
     Fill_Correo AT ROW 17.76 COL 35.2 COLON-ALIGNED
     Fill_Rol AT ROW 19.24 COL 26.6 COLON-ALIGNED
     Combo_Rol AT ROW 19.24 COL 44 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1 COL 1
     RECT-26 AT ROW 5.71 COL 10.8
     RECT-27 AT ROW 5.71 COL 86.8
     "ACTUALIZAR USUARIOS" VIEW-AS TEXT
          SIZE 26.8 BY 1.19 AT ROW 1.62 COL 63
          BGCOLOR 8 FGCOLOR 15 FONT 12
     "Datos de Autenticación" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 5 COL 88.8
     "Datos Generales" VIEW-AS TEXT
          SIZE 19.6 BY .62 AT ROW 4.95 COL 11.4
     SPACE(115.00) SKIP(16.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Actualizar".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dlg_UpdateUsua
                                                                        */
ASSIGN 
       FRAME Dlg_UpdateUsua:SCROLLABLE       = FALSE
       FRAME Dlg_UpdateUsua:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Fill_Rol IN FRAME Dlg_UpdateUsua
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dlg_UpdateUsua
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dlg_UpdateUsua Dlg_UpdateUsua
ON WINDOW-CLOSE OF FRAME Dlg_UpdateUsua /* Actualizar */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Aceptar Dlg_UpdateUsua
ON CHOOSE OF Btn_Aceptar IN FRAME Dlg_UpdateUsua /* Aceptar */
DO:    
    DEF VAR vcharUsuario AS CHAR.                  DEF VAR vcharContrasena AS CHAR.                 DEF VAR vCharCorreo AS CHAR.        
    DEF VAR vCharNombre AS CHAR.                   DEF VAR vCharApat AS CHAR.                       DEF VAR vCharAmat AS CHAR.
    DEF VAR vCharSexo AS CHAR.                     DEF VAR vCharFecha AS CHAR INITIAL ?.            DEF VAR vCharCurp AS CHAR.           
    DEF VAR vCharRfc AS CHAR.                      DEF VAR vCharDomicilio AS CHAR.    

    vcharUsuario = Fill_Usuario:SCREEN-VALUE.      vcharContrasena = Fill_Contrasena:SCREEN-VALUE.  vcharNombre =Fill_Nombre:SCREEN-VALUE.       
    vcharApat = Fill_Apat:SCREEN-VALUE.            vcharAmat = Fill_Amat:SCREEN-VALUE.              vcharSexo = Fill_Sexo:SCREEN-VALUE.
    vcharFecha = STRING(Fill_FNac:SCREEN-VALUE).   vcharDomicilio = Fill_Direccion:SCREEN-VALUE.    vcharCurp = Fill_Curp:SCREEN-VALUE.          
    vcharRFC = Fill_Rfc:SCREEN-VALUE.              vcharCorreo = Fill_Correo:SCREEN-VALUE.
        
    IF validarRegistroUsuario(vcharUsuario, vcharContrasena, vCharNombre, vCharApat, vCharAmat, STRING(vintIdRol)) = FALSE THEN DO:
        MESSAGE "Los campos con (*) son obligatorios" VIEW-AS ALERT-BOX.
        LEAVE.
    END.
    ASSIGN 
        Fill_Usuario Fill_Contrasena Fill_Nombre Fill_Apat Fill_Amat Fill_Sexo Fill_FNac Fill_Direccion Fill_Correo Fill_Rfc Fill_Curp.
    CASE inIntEvento:
        WHEN 1 THEN DO:
            vintIdPersona = addPersona(Fill_Nombre, Fill_Apat, Fill_Amat, Fill_Sexo, STRING(Fill_FNac), Fill_Direccion, Fill_Correo, Fill_Rfc, Fill_Curp).
            vintIdUsuario = addUsuario(Fill_Usuario, getEncrypt(Fill_Contrasena)).
            RUN asignarEmpleado.
            APPLY "WINDOW-CLOSE" TO FRAME Dlg_UpdateUsua.    
        END.
        WHEN 2 THEN DO:
            RUN updatePersona(inRowId, vintIdRol, Fill_Nombre, Fill_Apat, Fill_Amat, Fill_Sexo, Fill_FNac, Fill_Direccion, Fill_Correo, Fill_Rfc, Fill_Curp).
            RUN updateUsuario(inRowId, Fill_Usuario, getEncrypt(Fill_Contrasena)).
            APPLY "WINDOW-CLOSE" TO FRAME Dlg_UpdateUsua.
        END.
        WHEN 3 THEN DO:
            RUN deleteUsuario(inRowId).
            APPLY "WINDOW-CLOSE" TO FRAME Dlg_UpdateUsua. 
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Combo_Rol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Combo_Rol Dlg_UpdateUsua
ON VALUE-CHANGED OF Combo_Rol IN FRAME Dlg_UpdateUsua
DO:  
    vintIdRol = INTEGER(combo_rol:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dlg_UpdateUsua 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  DEFINE VARIABLE vcharLista AS CHARACTER.

  FOR EACH Rol NO-LOCK:
     vcharLista = vcharLista + ROL.DESCRIPCION + "," + STRING(ROL.ID_ROL) + ",".
  END.  

  ASSIGN Combo_Rol:LIST-ITEM-PAIRS = TRIM(vcharLista, ",").

  RUN setInitial.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asignarEmpleado Dlg_UpdateUsua 
PROCEDURE asignarEmpleado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
CREATE EMPLEADO.

    ASSIGN 
        EMPLEADO.ID_EMPLEADO = NEXT-VALUE(SEC_EMPLEADO)                     
        EMPLEADO.ID_PERSONA = vintIdPersona                    
        EMPLEADO.ID_USUARIO = vintIdUsuario                           
        EMPLEADO.ID_ROL = vintIdRol.                      
               
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asignarValores Dlg_UpdateUsua 
PROCEDURE asignarValores :
/*------------------------------------------------------------------------------
        Purpose: Asigna valores de a columna indicada.    
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    FIND FIRST USUARIO WHERE ROWID(USUARIO) = inRowId.

    CASE inIntEvento:
        WHEN 2 THEN DO:  
            Fill_Usuario:SCREEN-VALUE IN FRAME {&FRAME-NAME} = USUARIO.USUARIO.
            Fill_Contrasena:SCREEN-VALUE IN FRAME {&FRAME-NAME} = USUARIO.CONTRASENIA.

            FIND FIRST Empleado WHERE Empleado.id_Usuario = usuario.Id_Usuario.
                FIND FIRST Persona WHERE Persona.Id_Persona = Empleado.Id_Persona.
                    FIND FIRST Rol WHERE Empleado.Id_Rol = Rol.Id_Rol.

            Fill_Nombre:SCREEN-VALUE = Persona.Nombre.                 Fill_Apat:SCREEN-VALUE = Persona.A_paterno.
            Fill_Amat:SCREEN-VALUE = Persona.A_materno.                Fill_Sexo:SCREEN-VALUE = Persona.Sexo.
            Fill_FNac:SCREEN-VALUE = STRING(Persona.F_Nacimiento).     Fill_Direccion:SCREEN-VALUE = Persona.Domicilio.
            Fill_Curp:SCREEN-VALUE = Persona.Curp.                     Fill_Rfc:SCREEN-VALUE = Persona.Rfc.
            Fill_Correo:SCREEN-VALUE = Persona.Correo.                 Fill_Rol:SCREEN-VALUE = Rol.Descripcion.
        END.
        WHEN 3 THEN DO: 
            Fill_Usuario:SCREEN-VALUE IN FRAME {&FRAME-NAME}= USUARIO.USUARIO.
            Fill_Contrasena:SCREEN-VALUE IN FRAME {&FRAME-NAME}= USUARIO.CONTRASENIA.

            FIND FIRST Empleado WHERE Empleado.id_Usuario = usuario.Id_Usuario.
                FIND FIRST Persona WHERE Persona.Id_Persona = Empleado.Id_Persona.
                    FIND FIRST Rol WHERE Empleado.Id_Rol = Rol.Id_Rol.

            Fill_Nombre:SCREEN-VALUE = Persona.Nombre.                 Fill_Apat:SCREEN-VALUE = Persona.A_paterno.
            Fill_Amat:SCREEN-VALUE = Persona.A_materno.                Fill_Sexo:SCREEN-VALUE = Persona.Sexo.
            Fill_FNac:SCREEN-VALUE = STRING(Persona.F_Nacimiento).     Fill_Direccion:SCREEN-VALUE = Persona.Domicilio.
            Fill_Curp:SCREEN-VALUE = Persona.Curp.                     Fill_Rfc:SCREEN-VALUE = Persona.Rfc.
            Fill_Correo:SCREEN-VALUE = Persona.Correo.                 Fill_Rol:SCREEN-VALUE = Rol.Descripcion.
            ASSIGN combo_rol:SCREEN-VALUE = STRING(Rol.ID_ROL).

            Fill_Usuario:SENSITIVE = NO.
            Fill_Contrasena:SENSITIVE = NO.

            Fill_Nombre:SENSITIVE = NO.          Fill_Apat:SENSITIVE = NO.
            Fill_Amat:SENSITIVE = NO.            Fill_Sexo:SENSITIVE = NO.
            Fill_FNac:SENSITIVE = NO.            Fill_Direccion:SENSITIVE = NO.
            Fill_Curp:SENSITIVE = NO.            Fill_Rfc:SENSITIVE = NO.
            Fill_Correo:SENSITIVE = NO.          Fill_Rol:SENSITIVE = NO.
            ASSIGN combo_rol:SENSITIVE = NO.
        END.
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dlg_UpdateUsua  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dlg_UpdateUsua.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dlg_UpdateUsua  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY Fill_Nombre Fill_Usuario Fill_Apat Fill_Contrasena Fill_Amat Fill_Sexo 
          Fill_FNac Fill_Direccion Fill_Curp Fill_RFC Fill_Correo Fill_Rol 
          Combo_Rol 
      WITH FRAME Dlg_UpdateUsua.
  ENABLE Fill_Nombre Fill_Usuario Fill_Apat Fill_Contrasena Fill_Amat Fill_Sexo 
         Fill_FNac Btn_Aceptar Fill_Direccion Fill_Curp Fill_RFC Fill_Correo 
         Combo_Rol RECT-1 RECT-26 RECT-27 
      WITH FRAME Dlg_UpdateUsua.
  VIEW FRAME Dlg_UpdateUsua.
  {&OPEN-BROWSERS-IN-QUERY-Dlg_UpdateUsua}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setInitial Dlg_UpdateUsua 
PROCEDURE setInitial :
/*------------------------------------------------------------------------------
        Purpose: Inicialización del Dialogo de Registro de usuarios   
        Parameters: <none>
        
    ------------------------------------------------------------------------------*/
    CASE inIntEvento:

        WHEN 1 THEN DO:
           
        END.

        WHEN 2 THEN DO:
            RUN asignarValores.
        END.

        WHEN 3 THEN DO:
            RUN asignarValores.
        END.

    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidarRegistroUsuario Dlg_UpdateUsua 
FUNCTION ValidarRegistroUsuario RETURNS LOGICAL
    ( vcharUsu    AS CHAR,
      vcharContra AS CHAR,
      vCharNombre AS CHAR, 
      vCharApat   AS CHAR, 
      vCharAmat   AS CHAR, 
      vintIdRol   AS CHAR ) :
    /*------------------------------------------------------------------------------

    ------------------------------------------------------------------------------*/
    IF vcharUsu = " " OR vcharContra = " " OR vcharNombre = " " OR vcharApat = " " OR vcharAmat = " " OR vintIdRol = "0" THEN
        RETURN FALSE.
    
    RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE vintIdUsuario AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dlg_UpdateUsua

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Fill_Usuario Fill_Contrasena Btn_Aceptar ~
Btn_Cancelar RECT-1 RECT-20 
&Scoped-Define DISPLAYED-OBJECTS Fill_IdUser Fill_Usuario Fill_Contrasena 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidarRegistroUsuario Dlg_UpdateUsua 
FUNCTION ValidarRegistroUsuario RETURNS LOGICAL
    ( vcharUsu AS CHARACTER, vcharContra AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Aceptar 
     LABEL "Aceptar" 
     SIZE 20 BY 2.52.

DEFINE BUTTON Btn_Cancelar 
     LABEL "Cancelar" 
     SIZE 20 BY 2.52.

DEFINE VARIABLE Fill_Contrasena AS CHARACTER FORMAT "X(100)":U 
     LABEL "Contraseña" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_IdUser AS CHARACTER FORMAT "X(10)":U 
     LABEL "ID Usuario" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_Usuario AS CHARACTER FORMAT "X(100)":U 
     LABEL "Usuario" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 8  
     SIZE 90 BY 2.38
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 58 BY 8.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dlg_UpdateUsua
     Fill_IdUser AT ROW 5.19 COL 33.2 COLON-ALIGNED
     Fill_Usuario AT ROW 7.24 COL 33.2 COLON-ALIGNED
     Fill_Contrasena AT ROW 9.52 COL 33.2 COLON-ALIGNED
     Btn_Aceptar AT ROW 13.67 COL 21
     Btn_Cancelar AT ROW 13.67 COL 51
     RECT-1 AT ROW 1 COL 1
     RECT-20 AT ROW 3.86 COL 17
     "ACTUALIZAR" VIEW-AS TEXT
          SIZE 15 BY 1.19 AT ROW 1.62 COL 38.2
          BGCOLOR 8 FGCOLOR 15 FONT 12
     SPACE(37.79) SKIP(15.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Actualizar Usuarios".


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

/* SETTINGS FOR FILL-IN Fill_IdUser IN FRAME Dlg_UpdateUsua
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dlg_UpdateUsua
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dlg_UpdateUsua Dlg_UpdateUsua
ON WINDOW-CLOSE OF FRAME Dlg_UpdateUsua /* Actualizar Usuarios */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Aceptar Dlg_UpdateUsua
ON CHOOSE OF Btn_Aceptar IN FRAME Dlg_UpdateUsua /* Aceptar */
DO:    
    DEFINE VARIABLE vcharUsuario AS CHARACTER.
    DEFINE VARIABLE vcharContrasena AS CHARACTER.

    vcharUsuario = Fill_Usuario:SCREEN-VALUE.
    vcharContrasena = Fill_Contrasena:SCREEN-VALUE.
        
    IF validarRegistroUsuario(vcharUsuario, vcharContrasena) = FALSE THEN DO:
        MESSAGE "Todos los Campos son Requeridos" VIEW-AS ALERT-BOX.
        LEAVE.
    END.
    ASSIGN 
        Fill_Usuario Fill_Contrasena.
    CASE inIntEvento:
        WHEN 1 THEN DO:
            RUN addUsuario(vintIdUsuario, Fill_Usuario, getEncrypt(Fill_Contrasena)).
            APPLY "WINDOW-CLOSE" TO FRAME Dlg_UpdateUsua.    
        END.
        WHEN 2 THEN DO:
            RUN updateUsuario(inRowId, Fill_Usuario, getEncrypt(Fill_Contrasena)).
            APPLY "WINDOW-CLOSE" TO FRAME Dlg_UpdateUsua.
        END.
        WHEN 3 THEN DO:
            RUN deleteUsuario(inRowId, Fill_Usuario, Fill_Contrasena).
            APPLY "WINDOW-CLOSE" TO FRAME Dlg_UpdateUsua. 
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar Dlg_UpdateUsua
ON CHOOSE OF Btn_Cancelar IN FRAME Dlg_UpdateUsua /* Cancelar */
DO:
    APPLY "WINDOW-CLOSE" TO FRAME Dlg_UpdateUsua.    
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
  RUN setInitial.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
            Fill_IdUser:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(INTEGER(USUARIO.ID_USUARIO)).
            Fill_Usuario:SCREEN-VALUE = UPPER(USUARIO.USUARIO).
            Fill_Contrasena:SCREEN-VALUE = UPPER(USUARIO.CONTRASENIA).
        END.

        WHEN 3 THEN DO: 
            Fill_IdUser:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(INTEGER(USUARIO.ID_USUARIO)).
            Fill_Usuario:SCREEN-VALUE = UPPER(USUARIO.USUARIO).
            Fill_Contrasena:SCREEN-VALUE = UPPER(USUARIO.CONTRASENIA).

            Fill_IdUser:SENSITIVE = NO.
            Fill_Usuario:SENSITIVE = NO.
            Fill_Contrasena:SENSITIVE = NO.

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
  DISPLAY Fill_IdUser Fill_Usuario Fill_Contrasena 
      WITH FRAME Dlg_UpdateUsua.
  ENABLE Fill_Usuario Fill_Contrasena Btn_Aceptar Btn_Cancelar RECT-1 RECT-20 
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
            vintIdUsuario = NEXT-VALUE(SEC_USUARIO). 
            Fill_IdUser:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(vintIdUsuario).
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
    ( vcharUsu AS CHARACTER, vcharContra AS CHARACTER) :
    /*------------------------------------------------------------------------------
        Purpose: Validar Registro de Producto  
        Notes: Retorna TRUE si la validación es correcta
        Author: I.S.C. Fco. Javier Ortuño Colchado
    ------------------------------------------------------------------------------*/
    IF vcharUsu = " " OR vcharContra = " " THEN
        RETURN FALSE.
    
    RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


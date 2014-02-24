&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame-Login
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame-Login 
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
/*Libreries ---                                                         */
{login.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame-Login

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Fill-Usuario Fill-Contrasena Bttn-Salir ~
Btn_OK RECT-16 RECT-17 
&Scoped-Define DISPLAYED-OBJECTS Fill-Usuario Fill-Contrasena 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Ingresar" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Bttn-Salir 
     LABEL "&Salir" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE Fill-Contrasena AS CHARACTER FORMAT "X(256)":U 
     LABEL "Contraseña" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Fill-Usuario AS CHARACTER FORMAT "X(256)":U 
     LABEL "Usuario" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 8  
     SIZE 141 BY 27.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 83 BY 12.62.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 8  
     SIZE 75.6 BY 2.14
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame-Login
     Fill-Usuario AT ROW 14.52 COL 57.2 COLON-ALIGNED
     Fill-Contrasena AT ROW 16.48 COL 56.8 COLON-ALIGNED BLANK 
     Bttn-Salir AT ROW 21.05 COL 39.2
     Btn_OK AT ROW 21.05 COL 90.6
     RECT-14 AT ROW 1 COL 1
     RECT-16 AT ROW 10.81 COL 30.6
     RECT-17 AT ROW 4.33 COL 35.2
     "Bienvenido al Sistema le Seminaré" VIEW-AS TEXT
          SIZE 70.2 BY 1.19 AT ROW 4.81 COL 38.6
          BGCOLOR 8 FGCOLOR 15 FONT 70
     SPACE(33.19) SKIP(22.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 FGCOLOR 0 
         TITLE "Login".


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
/* SETTINGS FOR DIALOG-BOX Dialog-Frame-Login
                                                                        */
ASSIGN 
       FRAME Dialog-Frame-Login:SCROLLABLE       = FALSE
       FRAME Dialog-Frame-Login:HIDDEN           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-14 IN FRAME Dialog-Frame-Login
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame-Login
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame-Login Dialog-Frame-Login
ON 0 OF FRAME Dialog-Frame-Login /* Login */
OR "s" OF FRAME {&FRAME-NAME} ANYWHERE DO:
   
    CASE CHR(LASTKEY):
        
        WHEN "s" THEN APPLY "CHOOSE" TO Bttn-Salir.

    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame-Login Dialog-Frame-Login
ON RETURN OF FRAME Dialog-Frame-Login /* Login */
ANYWHERE DO: 
  APPLY "CHOOSE" TO Btn_OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame-Login Dialog-Frame-Login
ON WINDOW-CLOSE OF FRAME Dialog-Frame-Login /* Login */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame-Login
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame-Login /* Ingresar */
DO:
    DEF VAR vintIdRol AS INT.
    DEF VAR vintIdUsuario AS INT.
    DEF VAR vcharUsuario AS CHAR.
    DEF VAR vcharContrasena AS CHAR.
    DEF VAR vcharCadEncript AS CHAR.
    
    vcharUsuario = Fill-Usuario:SCREEN-VALUE.
    vcharContrasena = Fill-Contrasena:SCREEN-VALUE.
    
    IF vcharUsuario = "" OR vcharContrasena = "" THEN DO:
      MESSAGE "No se pueden dejar campos vacíos" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
    END.
    
    ELSE DO:
      vcharCadEncript = getEncrypt(vcharContrasena).
      RUN pBuscaUsuario(vcharUsuario, vcharCadEncript).
           
      IF AVAILABLE Usuario THEN DO:
         vintIdUsuario = Usuario.ID_USUARIO.
         vintIdRol = Rol.ID_ROL.
         HIDE ALL.
         RUN pBitacora(vintIdUsuario).
         RUN MenuPpal.w(vintIdRol, vintIdUsuario).
         APPLY "WINDOW-CLOSE" TO FRAME Dialog-Frame-Login.
      END.

      ELSE DO:
        MESSAGE "Usuario o contraseña inválidos" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
      END.
    END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame-Login
ON RETURN OF Btn_OK IN FRAME Dialog-Frame-Login /* Ingresar */
DO:
  APPLY "CHOOSE" TO Btn_OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Bttn-Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bttn-Salir Dialog-Frame-Login
ON CHOOSE OF Bttn-Salir IN FRAME Dialog-Frame-Login /* Salir */
DO:
  DEF VAR vlogOK AS LOG.

  MESSAGE "¿REALMENTE DESEA SALIR?" VIEW-AS ALERT-BOX BUTTONS YES-NO SET vlogOK.
 
  IF vlogOK = YES THEN DO:
    APPLY "WINDOW-CLOSE" TO FRAME Dialog-Frame-Login.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame-Login 


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
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame-Login  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame-Login.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame-Login  _DEFAULT-ENABLE
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
  DISPLAY Fill-Usuario Fill-Contrasena 
      WITH FRAME Dialog-Frame-Login.
  ENABLE Fill-Usuario Fill-Contrasena Bttn-Salir Btn_OK RECT-16 RECT-17 
      WITH FRAME Dialog-Frame-Login.
  VIEW FRAME Dialog-Frame-Login.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame-Login}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBitacora Dialog-Frame-Login 
PROCEDURE pBitacora :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM inintIdUsuario AS INT.

     CREATE BITACORA_ACCESO.
     ASSIGN BITACORA_ACCESO.ID_BIT_ACCESO = NEXT-VALUE(SEC_BITACORA_ACCESO)
            BITACORA_ACCESO.FECHA = TODAY
            BITACORA_ACCESO.HORA = STRING(TIME,"HH:MM:SS")
            BITACORA_ACCESO.ID_USUARIO = inintIdUsuario.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuscaUsuario Dialog-Frame-Login 
PROCEDURE pBuscaUsuario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM incharUsuario AS CHAR.
DEF INPUT PARAM incharCadEncript AS CHAR.

FIND Usuario WHERE Usuario.contrasenia = incharCadEncript AND Usuario.usuario = incharUsuario NO-LOCK NO-ERROR.
         FIND Empleado WHERE Empleado.ID_USUARIO = Usuario.ID_USUARIO NO-LOCK NO-ERROR.
            FIND ROL WHERE Rol.ID_ROL = Empleado.ID_ROL NO-LOCK NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          restaurante      PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dlg_MenuUsuarios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dlg_MenuUsuarios 
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
{productos.i}
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dlg_MenuUsuarios
&Scoped-define BROWSE-NAME Bws_Usuarios

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES USUARIO

/* Definitions for BROWSE Bws_Usuarios                                  */
&Scoped-define FIELDS-IN-QUERY-Bws_Usuarios USUARIO.ID_USUARIO ~
USUARIO.USUARIO USUARIO.CONTRASENIA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Bws_Usuarios 
&Scoped-define QUERY-STRING-Bws_Usuarios FOR EACH USUARIO NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Bws_Usuarios OPEN QUERY Bws_Usuarios FOR EACH USUARIO NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Bws_Usuarios USUARIO
&Scoped-define FIRST-TABLE-IN-QUERY-Bws_Usuarios USUARIO


/* Definitions for DIALOG-BOX Dlg_MenuUsuarios                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dlg_MenuUsuarios ~
    ~{&OPEN-QUERY-Bws_Usuarios}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Bws_Usuarios Btn_Agregar Btn_Delete ~
Btn_Salir 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Agregar 
     LABEL "Nuevo" 
     SIZE 20 BY 2.62
     BGCOLOR 8 .

DEFINE BUTTON Btn_Delete 
     LABEL "Eliminar" 
     SIZE 20 BY 2.62
     BGCOLOR 8 .

DEFINE BUTTON Btn_Salir 
     LABEL "Salir" 
     SIZE 20 BY 2.62
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Bws_Usuarios FOR 
      USUARIO SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Bws_Usuarios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Bws_Usuarios Dlg_MenuUsuarios _STRUCTURED
  QUERY Bws_Usuarios NO-LOCK DISPLAY
      USUARIO.ID_USUARIO FORMAT "->,>>>,>>9":U WIDTH 15.2
      USUARIO.USUARIO FORMAT "X(50)":U WIDTH 33.2
      USUARIO.CONTRASENIA COLUMN-LABEL "CONTRASEÑA" FORMAT "X(50)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 35 BY 13.33 ROW-HEIGHT-CHARS .76 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dlg_MenuUsuarios
     Bws_Usuarios AT ROW 2.91 COL 27
     Btn_Agregar AT ROW 3.86 COL 71
     Btn_Delete AT ROW 7.67 COL 71
     Btn_Salir AT ROW 11.48 COL 71
     SPACE(10.39) SKIP(4.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Usuarios".


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
/* SETTINGS FOR DIALOG-BOX Dlg_MenuUsuarios
                                                                        */
/* BROWSE-TAB Bws_Usuarios 1 Dlg_MenuUsuarios */
ASSIGN 
       FRAME Dlg_MenuUsuarios:SCROLLABLE       = FALSE
       FRAME Dlg_MenuUsuarios:HIDDEN           = TRUE.

ASSIGN 
       USUARIO.ID_USUARIO:VISIBLE IN BROWSE Bws_Usuarios = FALSE
       USUARIO.CONTRASENIA:VISIBLE IN BROWSE Bws_Usuarios = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Bws_Usuarios
/* Query rebuild information for BROWSE Bws_Usuarios
     _TblList          = "Restaurante.USUARIO"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Restaurante.USUARIO.ID_USUARIO
"ID_USUARIO" ? ? "integer" ? ? ? ? ? ? no ? no no "15.2" no no no "U" "" ""
     _FldNameList[2]   > Restaurante.USUARIO.USUARIO
"USUARIO" ? ? "character" ? ? ? ? ? ? no ? no no "33.2" yes no no "U" "" ""
     _FldNameList[3]   > Restaurante.USUARIO.CONTRASENIA
"CONTRASENIA" "CONTRASEÑA" ? "character" ? ? ? ? ? ? no ? no no ? no no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE Bws_Usuarios */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dlg_MenuUsuarios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dlg_MenuUsuarios Dlg_MenuUsuarios
ON WINDOW-CLOSE OF FRAME Dlg_MenuUsuarios /* Usuarios */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Agregar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Agregar Dlg_MenuUsuarios
ON CHOOSE OF Btn_Agregar IN FRAME Dlg_MenuUsuarios /* Nuevo */
DO:
  RUN ActualizarUsuarios.w(1, ?).
    {&OPEN-QUERY-Bws_Usuarios}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Delete Dlg_MenuUsuarios
ON CHOOSE OF Btn_Delete IN FRAME Dlg_MenuUsuarios /* Eliminar */
DO:
  RUN ActualizarUsuarios.w(1, ?).
    {&OPEN-QUERY-Bws_Usuarios}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir Dlg_MenuUsuarios
ON CHOOSE OF Btn_Salir IN FRAME Dlg_MenuUsuarios /* Salir */
DO:
    APPLY "WINDOW-CLOSE" TO FRAME Dlg_MenuUsuarios.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Bws_Usuarios
&Scoped-define SELF-NAME Bws_Usuarios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bws_Usuarios Dlg_MenuUsuarios
ON MOUSE-SELECT-DBLCLICK OF Bws_Usuarios IN FRAME Dlg_MenuUsuarios
DO:
  RUN selectedItem.
  Bws_Usuarios:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bws_Usuarios Dlg_MenuUsuarios
ON RETURN OF Bws_Usuarios IN FRAME Dlg_MenuUsuarios
DO:
  RUN selectedItem.
  Bws_Usuarios:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bws_Usuarios Dlg_MenuUsuarios
ON START-SEARCH OF Bws_Usuarios IN FRAME Dlg_MenuUsuarios
DO:
    DEFINE VARIABLE vcharOrden AS CHARACTER.
    DEFINE VARIABLE vhandQuery AS HANDLE.
    
    vhandQuery = (QUERY Bws_Usuarios:HANDLE).
    
    IF STRING(Bws_Usuarios:CURRENT-COLUMN:NAME) <> ? THEN
        vcharOrden = "FOR EACH USUARIO BY " + STRING(Bws_Usuarios:CURRENT-COLUMN:NAME).
    ELSE
        vcharOrden = "FOR EACH USUARIOS BY " + "ID_USUARIO".

    vhandQuery:QUERY-PREPARE(vcharOrden).
    vhandQuery:QUERY-OPEN().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dlg_MenuUsuarios 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dlg_MenuUsuarios  _DEFAULT-DISABLE
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
  HIDE FRAME Dlg_MenuUsuarios.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dlg_MenuUsuarios  _DEFAULT-ENABLE
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
  ENABLE Bws_Usuarios Btn_Agregar Btn_Delete Btn_Salir 
      WITH FRAME Dlg_MenuUsuarios.
  VIEW FRAME Dlg_MenuUsuarios.
  {&OPEN-BROWSERS-IN-QUERY-Dlg_MenuUsuarios}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectedItem Dlg_MenuUsuarios 
PROCEDURE SelectedItem :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vrowID AS ROWID.

    vrowID = ROWID(USUARIO).
    RUN ActualizarUsuarios.w(2, vrowID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


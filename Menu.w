&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEF INPUT PARAM inintIdRol AS INT.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Bttn-MSalir 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Bttn-MSalir 
     LABEL "Salir" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Bttn-Alta 
     LABEL "Alta" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-Baja 
     LABEL "Baja" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-Editar 
     LABEL "Editar" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-Menu 
     LABEL "Menú" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-Productos 
     LABEL "Productos" 
     SIZE 16 BY 1.14
     BGCOLOR 16 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 8  NO-FILL 
     SIZE 79 BY 13.24
     BGCOLOR 6 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19.4 BY 8.33.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19.4 BY 8.33.

DEFINE BUTTON Bttn-Alta-3 
     LABEL "Alta" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-Baja-3 
     LABEL "Baja" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-Editar-3 
     LABEL "Editar" 
     SIZE 16 BY 1.14.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 8  NO-FILL 
     SIZE 79.2 BY 13.24.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19.4 BY 8.33.

DEFINE BUTTON Bttn-Alta-4 
     LABEL "Alta" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-Baja-4 
     LABEL "Baja" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-Editar-4 
     LABEL "Editar" 
     SIZE 16 BY 1.14.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19.4 BY 8.33.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 8  NO-FILL 
     SIZE 79.4 BY 13.24.

DEFINE BUTTON Bttn-Alta-2 
     LABEL "Alta" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-Baja-2 
     LABEL "Baja" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-Editar-2 
     LABEL "Editar" 
     SIZE 16 BY 1.14.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 8  NO-FILL 
     SIZE 79 BY 13.24.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19.4 BY 8.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME MENUPPAL-FRAME
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 215.4 BY 33.62.

DEFINE FRAME Frame-General
     "Text 3" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.48 COL 41
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS THREE-D 
         AT COL 1 ROW 4.81
         SIZE 170 BY 28.57
         BGCOLOR 16 .

DEFINE FRAME Frame-Admin
     Bttn-Menu AT ROW 3.86 COL 6.4
     Bttn-Alta AT ROW 3.86 COL 32.8
     Bttn-Productos AT ROW 5.76 COL 6.4
     Bttn-Baja AT ROW 5.81 COL 32.8
     Bttn-Editar AT ROW 7.71 COL 32.8
     RECT-2 AT ROW 1 COL 1
     RECT-6 AT ROW 2.48 COL 4.6
     RECT-7 AT ROW 2.43 COL 31
     "      CATÁLOGO" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 2.57 COL 5
          BGCOLOR 15 FGCOLOR 1 
     "       USUARIOS" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 2.43 COL 31.2
          BGCOLOR 15 FGCOLOR 1 
     "                                                  ADMINISTRADOR" VIEW-AS TEXT
          SIZE 78.6 BY .81 AT ROW 1.1 COL 1.4
          BGCOLOR 15 FGCOLOR 1 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4.4 ROW 1.57
         SIZE 79.6 BY 13.33
         BGCOLOR 16 .

DEFINE FRAME Frame-Ventas
     Bttn-Alta-2 AT ROW 4.33 COL 12.8
     Bttn-Baja-2 AT ROW 6.29 COL 12.8
     Bttn-Editar-2 AT ROW 8.19 COL 12.8
     RECT-3 AT ROW 1 COL 1
     RECT-8 AT ROW 2.91 COL 11
     "       USUARIOS" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 2.91 COL 11.2
          BGCOLOR 15 FGCOLOR 1 
     "                                                         VENTAS" VIEW-AS TEXT
          SIZE 78.6 BY .81 AT ROW 1.1 COL 1.4
          BGCOLOR 15 FGCOLOR 1 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 88.2 ROW 1.57
         SIZE 79.8 BY 13.33
         BGCOLOR 16 .

DEFINE FRAME Frame-Inventario
     Bttn-Alta-3 AT ROW 4.57 COL 12.8
     Bttn-Baja-3 AT ROW 6.52 COL 12.8
     Bttn-Editar-3 AT ROW 8.43 COL 12.8
     RECT-4 AT ROW 1 COL 1
     RECT-9 AT ROW 3.14 COL 11
     "       USUARIOS" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 3.14 COL 11.2
          BGCOLOR 15 FGCOLOR 1 
     "                                                     INVENTARIO" VIEW-AS TEXT
          SIZE 78.6 BY .81 AT ROW 1.1 COL 1.4
          BGCOLOR 15 FGCOLOR 1 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4.4 ROW 15.52
         SIZE 79.6 BY 13.48
         BGCOLOR 16 .

DEFINE FRAME Frame-Reportes
     Bttn-Alta-4 AT ROW 4.33 COL 12.8
     Bttn-Baja-4 AT ROW 6.29 COL 12.8
     Bttn-Editar-4 AT ROW 8.19 COL 12.8
     RECT-10 AT ROW 2.91 COL 11
     RECT-5 AT ROW 1 COL 1
     "       USUARIOS" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 2.91 COL 11.2
          BGCOLOR 15 FGCOLOR 1 
     "                                                         REPORTES" VIEW-AS TEXT
          SIZE 78.6 BY .81 AT ROW 1.1 COL 1.4
          BGCOLOR 15 FGCOLOR 1 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 88.2 ROW 15.52
         SIZE 79.8 BY 13.57
         BGCOLOR 16 .

DEFINE FRAME FRAME-A
     Bttn-MSalir AT ROW 2.38 COL 147
     "MENU PRINCIPAL" VIEW-AS TEXT
          SIZE 47 BY 1.43 AT ROW 1.95 COL 67
          FGCOLOR 1 FONT 70
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 170 BY 3.81
         BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<Principal>"
         HEIGHT             = 32.48
         WIDTH              = 173.6
         MAX-HEIGHT         = 33.62
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 33.62
         VIRTUAL-WIDTH      = 273.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-A:FRAME = FRAME MENUPPAL-FRAME:HANDLE
       FRAME Frame-Admin:FRAME = FRAME Frame-General:HANDLE
       FRAME Frame-General:FRAME = FRAME MENUPPAL-FRAME:HANDLE
       FRAME Frame-Inventario:FRAME = FRAME Frame-General:HANDLE
       FRAME Frame-Reportes:FRAME = FRAME Frame-General:HANDLE
       FRAME Frame-Ventas:FRAME = FRAME Frame-General:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
                                                                        */
ASSIGN 
       Bttn-MSalir:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FRAME Frame-Admin
                                                                        */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME Frame-Admin
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME Frame-General
   UNDERLINE                                                            */
/* SETTINGS FOR FRAME Frame-Inventario
                                                                        */
/* SETTINGS FOR FRAME Frame-Reportes
                                                                        */
ASSIGN 
       RECT-5:HIDDEN IN FRAME Frame-Reportes           = TRUE
       RECT-5:SELECTABLE IN FRAME Frame-Reportes       = TRUE.

/* SETTINGS FOR FRAME Frame-Ventas
                                                                        */
/* SETTINGS FOR FRAME MENUPPAL-FRAME
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-A:MOVE-BEFORE-TAB-ITEM (FRAME Frame-General:HANDLE)
/* END-ASSIGN-TABS */.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Principal> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Principal> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Bttn-MSalir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bttn-MSalir C-Win
ON CHOOSE OF Bttn-MSalir IN FRAME FRAME-A /* Salir */
DO:
  DEF VAR vlogOK AS LOG.

  MESSAGE "¿REALMENTE DESEA SALIR?" VIEW-AS ALERT-BOX BUTTONS YES-NO SET vlogOK.
 
  IF vlogOK = YES THEN DO: 
    HIDE ALL.
    RUN login.w.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frame-Admin
&Scoped-define SELF-NAME Bttn-Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bttn-Productos C-Win
ON CHOOSE OF Bttn-Productos IN FRAME Frame-Admin /* Productos */
DO:
  RUN menuProductos.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-A
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  CASE inintIdRol:

      WHEN 1 THEN DO:
        MESSAGE "Bienvenido" VIEW-AS ALERT-BOX.
      END.

      WHEN 2 THEN DO:

           Bttn-Menu:SENSITIVE = NO.
           Bttn-Productos:SENSITIVE = NO.
           Bttn-Alta:SENSITIVE = NO.
           Bttn-Baja:SENSITIVE = NO.
           Bttn-Editar:SENSITIVE = NO.

      END.

  END CASE.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  ENABLE Bttn-MSalir 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME MENUPPAL-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-MENUPPAL-FRAME}
  ENABLE Bttn-Menu Bttn-Alta Bttn-Productos Bttn-Baja Bttn-Editar RECT-6 RECT-7 
      WITH FRAME Frame-Admin IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frame-Admin}
  ENABLE Bttn-Alta-2 Bttn-Baja-2 Bttn-Editar-2 RECT-3 RECT-8 
      WITH FRAME Frame-Ventas IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frame-Ventas}
  VIEW FRAME Frame-General IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frame-General}
  ENABLE Bttn-Alta-3 Bttn-Baja-3 Bttn-Editar-3 RECT-4 RECT-9 
      WITH FRAME Frame-Inventario IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frame-Inventario}
  ENABLE Bttn-Alta-4 Bttn-Baja-4 Bttn-Editar-4 RECT-10 RECT-5 
      WITH FRAME Frame-Reportes IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frame-Reportes}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


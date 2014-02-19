&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame-Menu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame-Menu 
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

/* Parameters Definitions ---                                           */

DEF INPUT PARAM inintIdRol AS INT.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame-Menu

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Bttn-MSalir Bttn-Alta-2 Btn-Menu Btn-Usuario ~
Bttn-Baja-2 Btn-Productos Bttn-Editar-2 Bttn-Alta-4 Bttn-Alta-3 Bttn-Baja-4 ~
Bttn-Baja-3 Bttn-Editar-4 Bttn-Editar-3 RECT-10 RECT-18 RECT-2 RECT-3 ~
RECT-4 RECT-5 RECT-6 RECT-7 RECT-8 RECT-9 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Menu 
     LABEL "Menú" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Btn-Productos 
     LABEL "Productos" 
     SIZE 16 BY 1.14
     BGCOLOR 16 .

DEFINE BUTTON Btn-Usuario 
     LABEL "Menu Usuario" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-Alta-2 
     LABEL "Alta" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-Alta-3 
     LABEL "Alta" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-Alta-4 
     LABEL "Alta" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-Baja-2 
     LABEL "Baja" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-Baja-3 
     LABEL "Baja" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-Baja-4 
     LABEL "Baja" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-Editar-2 
     LABEL "Editar" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-Editar-3 
     LABEL "Editar" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-Editar-4 
     LABEL "Editar" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Bttn-MSalir 
     LABEL "Salir" 
     SIZE 15 BY 1.14.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19.4 BY 7.1.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 8  
     SIZE 162 BY 3.33
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 8  
     SIZE 80.2 BY 13.24
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 8  
     SIZE 79 BY 13.24
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 8  
     SIZE 80.4 BY 13.24
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 8  
     SIZE 79.4 BY 13.24
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19.4 BY 5.33.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19.4 BY 3.14.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19.4 BY 8.33.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19.4 BY 7.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame-Menu
     Bttn-MSalir AT ROW 2.52 COL 141.4
     Bttn-Alta-2 AT ROW 8.81 COL 100.2
     Btn-Menu AT ROW 9.19 COL 12.2
     Btn-Usuario AT ROW 9.24 COL 36.6
     Bttn-Baja-2 AT ROW 10.76 COL 100.2
     Btn-Productos AT ROW 11.1 COL 12.2
     Bttn-Editar-2 AT ROW 12.67 COL 100.2
     Bttn-Alta-4 AT ROW 22.71 COL 100.4
     Bttn-Alta-3 AT ROW 22.95 COL 16.4
     Bttn-Baja-4 AT ROW 24.67 COL 100.4
     Bttn-Baja-3 AT ROW 24.91 COL 16.4
     Bttn-Editar-4 AT ROW 26.57 COL 100.4
     Bttn-Editar-3 AT ROW 26.81 COL 16.4
     RECT-10 AT ROW 21.29 COL 98.8
     RECT-18 AT ROW 1.43 COL 5
     RECT-2 AT ROW 5.48 COL 4.8
     RECT-3 AT ROW 5.48 COL 88.4
     RECT-4 AT ROW 19.14 COL 4.6
     RECT-5 AT ROW 19.19 COL 88.6
     RECT-6 AT ROW 7.81 COL 10.4
     RECT-7 AT ROW 7.86 COL 34.8
     RECT-8 AT ROW 7.38 COL 98.4
     RECT-9 AT ROW 21.52 COL 14.6
     "MENU PRINCIPAL" VIEW-AS TEXT
          SIZE 47 BY 1.91 AT ROW 2.14 COL 66.8
          BGCOLOR 8 FGCOLOR 15 FONT 70
     "                                                      REPORTES" VIEW-AS TEXT
          SIZE 76.8 BY .81 AT ROW 19.57 COL 90.2
          BGCOLOR 15 FGCOLOR 1 
     "     USUARIOS" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 7.48 COL 98.8
          BGCOLOR 15 FGCOLOR 1 
     "                                                         VENTAS" VIEW-AS TEXT
          SIZE 76 BY .81 AT ROW 5.86 COL 90
          BGCOLOR 15 FGCOLOR 1 
     "                                                  ADMINISTRADOR" VIEW-AS TEXT
          SIZE 78 BY .81 AT ROW 5.86 COL 6
          BGCOLOR 15 FGCOLOR 1 
     "       CATÁLOGO" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 7.91 COL 10.8
          BGCOLOR 15 FGCOLOR 1 
     "       USUARIOS" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 7.95 COL 35
          BGCOLOR 15 FGCOLOR 1 
     "       USUARIOS" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 21.62 COL 15
          BGCOLOR 15 FGCOLOR 1 
     "                                                     INVENTARIO" VIEW-AS TEXT
          SIZE 78 BY .81 AT ROW 19.52 COL 6
          BGCOLOR 15 FGCOLOR 1 
     "       USUARIOS" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 21.38 COL 99.2
          BGCOLOR 15 FGCOLOR 1 
     SPACE(53.39) SKIP(10.90)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE BGCOLOR 8 "<Principal>".


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
/* SETTINGS FOR DIALOG-BOX Dialog-Frame-Menu
   UNDERLINE                                                            */
ASSIGN 
       FRAME Dialog-Frame-Menu:SCROLLABLE       = FALSE
       FRAME Dialog-Frame-Menu:HIDDEN           = TRUE.

ASSIGN 
       Bttn-MSalir:HIDDEN IN FRAME Dialog-Frame-Menu           = TRUE.

ASSIGN 
       RECT-5:HIDDEN IN FRAME Dialog-Frame-Menu           = TRUE
       RECT-5:SELECTABLE IN FRAME Dialog-Frame-Menu       = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame-Menu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame-Menu Dialog-Frame-Menu
ON WINDOW-CLOSE OF FRAME Dialog-Frame-Menu /* <Principal> */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Menu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Menu Dialog-Frame-Menu
ON CHOOSE OF Btn-Menu IN FRAME Dialog-Frame-Menu /* Menú */
DO:
  RUN MenuPlatillos.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Productos Dialog-Frame-Menu
ON CHOOSE OF Btn-Productos IN FRAME Dialog-Frame-Menu /* Productos */
DO:
  RUN menuProductos.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Usuario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Usuario Dialog-Frame-Menu
ON CHOOSE OF Btn-Usuario IN FRAME Dialog-Frame-Menu /* Menu Usuario */
DO:
  RUN menuUsuarios.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Bttn-MSalir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bttn-MSalir Dialog-Frame-Menu
ON CHOOSE OF Bttn-MSalir IN FRAME Dialog-Frame-Menu /* Salir */
DO:
  DEF VAR vlogOK AS LOG.

  MESSAGE "¿REALMENTE DESEA SALIR DEL SISTEMA?" VIEW-AS ALERT-BOX BUTTONS YES-NO SET vlogOK.
 
  IF vlogOK = YES THEN DO:
    HIDE ALL.
    RUN login.w.
    APPLY "WINDOW-CLOSE" TO FRAME Dialog-Frame-Menu.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame-Menu 


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

  CASE inintIdRol:

      WHEN 1 THEN DO:
        
      END.

      WHEN 2 THEN DO:
           Btn-Menu:SENSITIVE = NO.
           Btn-Productos:SENSITIVE = NO.
           Btn-Usuario:SENSITIVE = NO.
      END.

  END CASE.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame-Menu  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame-Menu.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame-Menu  _DEFAULT-ENABLE
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
  ENABLE Bttn-MSalir Bttn-Alta-2 Btn-Menu Btn-Usuario Bttn-Baja-2 Btn-Productos 
         Bttn-Editar-2 Bttn-Alta-4 Bttn-Alta-3 Bttn-Baja-4 Bttn-Baja-3 
         Bttn-Editar-4 Bttn-Editar-3 RECT-10 RECT-18 RECT-2 RECT-3 RECT-4 
         RECT-5 RECT-6 RECT-7 RECT-8 RECT-9 
      WITH FRAME Dialog-Frame-Menu.
  VIEW FRAME Dialog-Frame-Menu.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame-Menu}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


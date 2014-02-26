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
DEF INPUT PARAM inintIdUsuario AS INT.

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
&Scoped-Define ENABLED-OBJECTS Bttn-MSalir Btn-Ventas Btn-Menu Btn-Usuario ~
Btn-Clientes Btn-Facturas Btn-Productos Btn-Inventario Btn-Reporte ~
Btn-Desperdicio RECT-10 RECT-18 RECT-2 RECT-3 RECT-4 RECT-5 RECT-6 RECT-7 ~
RECT-8 RECT-9 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Clientes 
     LABEL "&Clientes" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Btn-Desperdicio 
     LABEL "Merm&a" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Btn-Facturas 
     LABEL "&Facturas" 
     SIZE 16.6 BY 1.14.

DEFINE BUTTON Btn-Inventario 
     LABEL "&Inventario" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Btn-Menu 
     LABEL "&Menú" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Btn-Productos 
     LABEL "&Productos" 
     SIZE 16 BY 1.14
     BGCOLOR 16 .

DEFINE BUTTON Btn-Reporte 
     LABEL "&Reportes" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Btn-Usuario 
     LABEL "&Usuarios" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Btn-Ventas 
     LABEL "&Ventas" 
     SIZE 16.2 BY 1.14.

DEFINE BUTTON Bttn-MSalir 
     LABEL "Salir" 
     SIZE 15.6 BY 1.57.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19.4 BY 5.33.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 8  NO-FILL 
     SIZE 132 BY 3.33
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 8  NO-FILL 
     SIZE 54.6 BY 9.52
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 8  NO-FILL 
     SIZE 55 BY 9.52
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 8  NO-FILL 
     SIZE 54.4 BY 9.48
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 8  NO-FILL 
     SIZE 54.4 BY 9.48
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19.4 BY 5.33.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19.4 BY 5.29.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19.4 BY 5.62.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19.4 BY 5.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame-Menu
     Bttn-MSalir AT ROW 2.29 COL 118
     Btn-Ventas AT ROW 9.33 COL 95.6
     Btn-Menu AT ROW 9.52 COL 19.2
     Btn-Usuario AT ROW 9.52 COL 42.2
     Btn-Clientes AT ROW 11.38 COL 42.2
     Btn-Facturas AT ROW 11.38 COL 95.8
     Btn-Productos AT ROW 11.43 COL 19.2
     Btn-Inventario AT ROW 21.38 COL 30.8
     Btn-Reporte AT ROW 22.24 COL 96
     Btn-Desperdicio AT ROW 23.14 COL 31
     RECT-10 AT ROW 19.71 COL 94.2
     RECT-18 AT ROW 1.43 COL 5
     RECT-2 AT ROW 5.76 COL 76.4
     RECT-3 AT ROW 5.76 COL 11
     RECT-4 AT ROW 17.24 COL 11.2
     RECT-5 AT ROW 17.24 COL 76.4
     RECT-6 AT ROW 8.14 COL 17.4
     RECT-7 AT ROW 8.14 COL 40.4
     RECT-8 AT ROW 7.76 COL 94
     RECT-9 AT ROW 19.71 COL 29
     "MENU PRINCIPAL" VIEW-AS TEXT
          SIZE 39.6 BY 1.91 AT ROW 2.19 COL 51.4
          BGCOLOR 8 FGCOLOR 15 FONT 70
     "                                 INVENTARIO" VIEW-AS TEXT
          SIZE 52 BY .81 AT ROW 17.57 COL 12.6
          BGCOLOR 15 FGCOLOR 1 
     "           MENU" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 7.86 COL 94.4
          BGCOLOR 15 FGCOLOR 1 
     "           MENU" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 19.81 COL 94.6
          BGCOLOR 15 FGCOLOR 1 
     "                                   REPORTES" VIEW-AS TEXT
          SIZE 52 BY .81 AT ROW 17.57 COL 77.8
          BGCOLOR 15 FGCOLOR 1 
     "                                     VENTAS" VIEW-AS TEXT
          SIZE 52.2 BY .81 AT ROW 5.95 COL 77.8
          BGCOLOR 15 FGCOLOR 1 
     "                              ADMINISTRADOR" VIEW-AS TEXT
          SIZE 52 BY .81 AT ROW 5.95 COL 12.6
          BGCOLOR 15 FGCOLOR 1 
     "       CATÁLOGO" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 8.24 COL 17.8
          BGCOLOR 15 FGCOLOR 1 
     "       PERSONAS" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 8.24 COL 40.6
          BGCOLOR 15 FGCOLOR 1 
     "           MENU" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 19.81 COL 29.4
          BGCOLOR 15 FGCOLOR 1 
     SPACE(92.99) SKIP(7.61)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE BGCOLOR 8 "Menu Principal".


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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame-Menu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame-Menu Dialog-Frame-Menu
ON 0 OF FRAME Dialog-Frame-Menu /* Menu Principal */
OR "m","p","u","c","v","f","i","a","r" OF FRAME {&FRAME-NAME} ANYWHERE DO:
   
    CASE CHR(LASTKEY):
        
        WHEN "m" THEN APPLY "CHOOSE" TO Btn-Menu.
        WHEN "p" THEN APPLY "CHOOSE" TO Btn-Productos.
        WHEN "u" THEN APPLY "CHOOSE" TO Btn-Usuario.
        WHEN "c" THEN APPLY "CHOOSE" TO Btn-Clientes.
        WHEN "v" THEN APPLY "CHOOSE" TO Btn-Ventas.
        WHEN "f" THEN APPLY "CHOOSE" TO Btn-Facturas.
        WHEN "i" THEN APPLY "CHOOSE" TO Btn-Inventario.
        WHEN "a" THEN APPLY "CHOOSE" TO Btn-Desperdicio.
        WHEN "r" THEN APPLY "CHOOSE" TO Btn-Reporte.

    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame-Menu Dialog-Frame-Menu
ON WINDOW-CLOSE OF FRAME Dialog-Frame-Menu /* Menu Principal */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Clientes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Clientes Dialog-Frame-Menu
ON CHOOSE OF Btn-Clientes IN FRAME Dialog-Frame-Menu /* Clientes */
DO:
  RUN MenuClientes.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Desperdicio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Desperdicio Dialog-Frame-Menu
ON CHOOSE OF Btn-Desperdicio IN FRAME Dialog-Frame-Menu /* Merma */
DO:
  RUN Perdida_Diaria.w(inintIdUsuario).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Facturas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Facturas Dialog-Frame-Menu
ON CHOOSE OF Btn-Facturas IN FRAME Dialog-Frame-Menu /* Facturas */
DO:
  RUN MenuFactura.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Inventario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Inventario Dialog-Frame-Menu
ON CHOOSE OF Btn-Inventario IN FRAME Dialog-Frame-Menu /* Inventario */
DO:
  RUN Inventario.w(inintIdUsuario).
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


&Scoped-define SELF-NAME Btn-Reporte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Reporte Dialog-Frame-Menu
ON CHOOSE OF Btn-Reporte IN FRAME Dialog-Frame-Menu /* Reportes */
DO:
  RUN Reportes.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Usuario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Usuario Dialog-Frame-Menu
ON CHOOSE OF Btn-Usuario IN FRAME Dialog-Frame-Menu /* Usuarios */
DO:
  RUN menuUsuarios.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Ventas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Ventas Dialog-Frame-Menu
ON CHOOSE OF Btn-Ventas IN FRAME Dialog-Frame-Menu /* Ventas */
DO:
  RUN RegistroVentas.w(inintIdUsuario).
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
    RUN login.w NO-ERROR.
    APPLY "WINDOW-CLOSE" TO FRAME Dialog-Frame-Menu.
    APPLY "END-ERROR":U TO SELF.
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
          MESSAGE "BIENVENIDO ADMINISTRADOR" VIEW-AS ALERT-BOX.
      END.

      WHEN 2 THEN DO:
           Btn-Menu:SENSITIVE = NO.
           Btn-Productos:SENSITIVE = NO.
           Btn-Usuario:SENSITIVE = NO.
           Btn-Clientes:SENSITIVE = NO.
           Btn-Inventario:SENSITIVE = NO.
           Btn-Desperdicio:SENSITIVE = NO.
           Btn-Ventas:SENSITIVE = NO.
      END.

      WHEN 3 THEN DO:
           Btn-Menu:SENSITIVE = NO.
           Btn-Productos:SENSITIVE = NO.
           Btn-Usuario:SENSITIVE = NO.
           Btn-Clientes:SENSITIVE = NO.
      END.

      WHEN 4 THEN DO:
           Btn-Menu:SENSITIVE = NO.
           Btn-Productos:SENSITIVE = NO.
           Btn-Usuario:SENSITIVE = NO.
           Btn-Clientes:SENSITIVE = NO.
           Btn-Ventas:SENSITIVE = NO.
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
  ENABLE Bttn-MSalir Btn-Ventas Btn-Menu Btn-Usuario Btn-Clientes Btn-Facturas 
         Btn-Productos Btn-Inventario Btn-Reporte Btn-Desperdicio RECT-10 
         RECT-18 RECT-2 RECT-3 RECT-4 RECT-5 RECT-6 RECT-7 RECT-8 RECT-9 
      WITH FRAME Dialog-Frame-Menu.
  VIEW FRAME Dialog-Frame-Menu.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame-Menu}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          restaurante      PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Perdida-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Perdida-Frame 
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

/* Local Variable Definitions ---                                       */
DEF INPUT PARAM viniduser AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Perdida-Frame
&Scoped-define BROWSE-NAME BROWSE-15

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES STOCK PRODUCTO

/* Definitions for BROWSE BROWSE-15                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-15 PRODUCTO.DESCRIPCION ~
STOCK.CANTIDAD STOCK.F_CADUCIDAD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-15 
&Scoped-define QUERY-STRING-BROWSE-15 FOR EACH STOCK ~
      WHERE STOCK.F_CADUCIDAD > TODAY ~
 AND STOCK.CANTIDAD > 0 NO-LOCK, ~
      EACH PRODUCTO WHERE PRODUCTO.ID_PRODUCTO = STOCK.ID_PRODUCTO NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-15 OPEN QUERY BROWSE-15 FOR EACH STOCK ~
      WHERE STOCK.F_CADUCIDAD > TODAY ~
 AND STOCK.CANTIDAD > 0 NO-LOCK, ~
      EACH PRODUCTO WHERE PRODUCTO.ID_PRODUCTO = STOCK.ID_PRODUCTO NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-15 STOCK PRODUCTO
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-15 STOCK
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-15 PRODUCTO


/* Definitions for DIALOG-BOX Perdida-Frame                             */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Perdida-Frame ~
    ~{&OPEN-QUERY-BROWSE-15}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-30 BROWSE-15 BUTTON-29 RECT-18 ~
RECT-26 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-29 
     LABEL "Quitar" 
     SIZE 15 BY 2.38.

DEFINE BUTTON BUTTON-30 
     LABEL "Salir" 
     SIZE 13 BY .95.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 8  
     SIZE 118.8 BY 2.1
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 16.4 BY 2.95.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-15 FOR 
      STOCK, 
      PRODUCTO SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-15 Perdida-Frame _STRUCTURED
  QUERY BROWSE-15 NO-LOCK DISPLAY
      PRODUCTO.DESCRIPCION FORMAT "X(150)":U WIDTH 31.2
      STOCK.CANTIDAD FORMAT "->,>>>,>>9":U
      STOCK.F_CADUCIDAD COLUMN-LABEL "FECHA DE CADUCIDAD" FORMAT "99/99/99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 80 BY 8.81
         BGCOLOR 15  EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Perdida-Frame
     BUTTON-30 AT ROW 1.57 COL 104.8
     BROWSE-15 AT ROW 3.91 COL 12
     BUTTON-29 AT ROW 6.95 COL 99
     RECT-18 AT ROW 1 COL 1.2
     RECT-26 AT ROW 6.67 COL 98.2
     "PERDIDA DIARIA" VIEW-AS TEXT
          SIZE 19.2 BY .62 AT ROW 1.71 COL 48
          FGCOLOR 15 
     SPACE(52.99) SKIP(11.23)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Perdidas".


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
/* SETTINGS FOR DIALOG-BOX Perdida-Frame
                                                                        */
/* BROWSE-TAB BROWSE-15 BUTTON-30 Perdida-Frame */
ASSIGN 
       FRAME Perdida-Frame:SCROLLABLE       = FALSE
       FRAME Perdida-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-15
/* Query rebuild information for BROWSE BROWSE-15
     _TblList          = "Restaurante.STOCK,Restaurante.PRODUCTO WHERE Restaurante.STOCK ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "STOCK.F_CADUCIDAD > TODAY
 AND STOCK.CANTIDAD > 0"
     _JoinCode[2]      = "PRODUCTO.ID_PRODUCTO = STOCK.ID_PRODUCTO"
     _FldNameList[1]   > Restaurante.PRODUCTO.DESCRIPCION
"PRODUCTO.DESCRIPCION" ? ? "character" ? ? ? ? ? ? no ? no no "31.2" yes no no "U" "" ""
     _FldNameList[2]   = Restaurante.STOCK.CANTIDAD
     _FldNameList[3]   > Restaurante.STOCK.F_CADUCIDAD
"STOCK.F_CADUCIDAD" "FECHA DE CADUCIDAD" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-15 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Perdida-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Perdida-Frame Perdida-Frame
ON RETURN OF FRAME Perdida-Frame /* Perdidas */
ANYWHERE DO:
  APPLY "CHOOSE" TO BUTTON-29.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Perdida-Frame Perdida-Frame
ON WINDOW-CLOSE OF FRAME Perdida-Frame /* Perdidas */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-29
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-29 Perdida-Frame
ON CHOOSE OF BUTTON-29 IN FRAME Perdida-Frame /* Quitar */
DO:
  RUN Eliminar_Desperdicio.w(ROWID(producto),ROWID(stock),viniduser).
  {&OPEN-query-BROWSE-15}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-30
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-30 Perdida-Frame
ON CHOOSE OF BUTTON-30 IN FRAME Perdida-Frame /* Salir */
DO:
  APPLY "window-close" TO FRAME Perdida-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-15
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Perdida-Frame 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Perdida-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Perdida-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Perdida-Frame  _DEFAULT-ENABLE
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
  ENABLE BUTTON-30 BROWSE-15 BUTTON-29 RECT-18 RECT-26 
      WITH FRAME Perdida-Frame.
  VIEW FRAME Perdida-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Perdida-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


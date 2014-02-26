&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          restaurante      PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Product-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Product-Frame 
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
&Scoped-define FRAME-NAME Product-Frame
&Scoped-define BROWSE-NAME BROWSE-9

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PRODUCTO

/* Definitions for BROWSE BROWSE-9                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-9 PRODUCTO.CODIGO ~
PRODUCTO.DESCRIPCION PRODUCTO.CANT_MINIMA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-9 
&Scoped-define QUERY-STRING-BROWSE-9 FOR EACH PRODUCTO NO-LOCK ~
    BY PRODUCTO.DESCRIPCION INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-9 OPEN QUERY BROWSE-9 FOR EACH PRODUCTO NO-LOCK ~
    BY PRODUCTO.DESCRIPCION INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-9 PRODUCTO
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-9 PRODUCTO


/* Definitions for DIALOG-BOX Product-Frame                             */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Product-Frame ~
    ~{&OPEN-QUERY-BROWSE-9}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-24 FILL-IN-1 BROWSE-9 BUTTON-33 ~
RECT-18 RECT-26 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-24 
     LABEL "Salir" 
     SIZE 13 BY .91.

DEFINE BUTTON BUTTON-33 
     LABEL "Agregar" 
     SIZE 15 BY 2.62.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Busqueda" 
     VIEW-AS FILL-IN 
     SIZE 54 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 8  
     SIZE 123.8 BY 2.1
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 16.4 BY 3.19.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-9 FOR 
      PRODUCTO SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-9 Product-Frame _STRUCTURED
  QUERY BROWSE-9 NO-LOCK DISPLAY
      PRODUCTO.CODIGO FORMAT "X(10)":U
      PRODUCTO.DESCRIPCION FORMAT "X(150)":U WIDTH 38.2
      PRODUCTO.CANT_MINIMA FORMAT "->,>>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 90 BY 16.33
         BGCOLOR 15  ROW-HEIGHT-CHARS .67 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Product-Frame
     BUTTON-24 AT ROW 1.62 COL 110
     FILL-IN-1 AT ROW 4.33 COL 11 COLON-ALIGNED
     BROWSE-9 AT ROW 6.24 COL 11
     BUTTON-33 AT ROW 9.67 COL 106.2
     RECT-18 AT ROW 1 COL 1
     RECT-26 AT ROW 9.38 COL 105.4
     "NUEVO PRODUCTO" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 1.71 COL 51
          FGCOLOR 15 
     SPACE(48.80) SKIP(21.09)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Nuevo Producto".


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
/* SETTINGS FOR DIALOG-BOX Product-Frame
                                                                        */
/* BROWSE-TAB BROWSE-9 FILL-IN-1 Product-Frame */
ASSIGN 
       FRAME Product-Frame:SCROLLABLE       = FALSE
       FRAME Product-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-9
/* Query rebuild information for BROWSE BROWSE-9
     _TblList          = "Restaurante.PRODUCTO"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Restaurante.PRODUCTO.DESCRIPCION|yes"
     _FldNameList[1]   = Restaurante.PRODUCTO.CODIGO
     _FldNameList[2]   > Restaurante.PRODUCTO.DESCRIPCION
"PRODUCTO.DESCRIPCION" ? ? "character" ? ? ? ? ? ? no ? no no "38.2" yes no no "U" "" ""
     _FldNameList[3]   = Restaurante.PRODUCTO.CANT_MINIMA
     _Query            is OPENED
*/  /* BROWSE BROWSE-9 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Product-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Product-Frame Product-Frame
ON WINDOW-CLOSE OF FRAME Product-Frame /* Nuevo Producto */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-24
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-24 Product-Frame
ON CHOOSE OF BUTTON-24 IN FRAME Product-Frame /* Salir */
DO:
  APPLY "window-close" TO FRAME Product-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-33
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-33 Product-Frame
ON CHOOSE OF BUTTON-33 IN FRAME Product-Frame /* Agregar */
DO:
    RUN Insertar_Stock.w(ROWID(Producto),viniduser).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-1 Product-Frame
ON VALUE-CHANGED OF FILL-IN-1 IN FRAME Product-Frame /* Busqueda */
DO:
    DEFINE VARIABLE vcharConsulta AS CHARACTER.
    DEFINE VARIABLE vhandSearch AS HANDLE.

    vhandSearch = (QUERY BROWSE-9:HANDLE).
    vcharConsulta = "FOR EACH PRODUCTO WHERE PRODUCTO.DESCRIPCION BEGINS '" + TRIM(Fill-in-1:SCREEN-VALUE) + "'".
    vhandSearch:QUERY-PREPARE(vcharConsulta).
    vhandSearch:QUERY-OPEN().

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-9
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Product-Frame 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Product-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Product-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Product-Frame  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-1 
      WITH FRAME Product-Frame.
  ENABLE BUTTON-24 FILL-IN-1 BROWSE-9 BUTTON-33 RECT-18 RECT-26 
      WITH FRAME Product-Frame.
  VIEW FRAME Product-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Product-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


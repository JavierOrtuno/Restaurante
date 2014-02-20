&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          restaurante      PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Inventario-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Inventario-Frame 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Inventario-Frame
&Scoped-define BROWSE-NAME BROWSE-8

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PRODUCTO STOCK UNIDAD_MEDIDA

/* Definitions for BROWSE BROWSE-8                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-8 PRODUCTO.CODIGO ~
PRODUCTO.DESCRIPCION STOCK.CANTIDAD UNIDAD_MEDIDA.DESCRIPCION ~
STOCK.F_CADUCIDAD STOCK.F_INGRESO STOCK.LOTE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-8 
&Scoped-define QUERY-STRING-BROWSE-8 FOR EACH PRODUCTO SHARE-LOCK, ~
      EACH STOCK WHERE STOCK.ID_PRODUCTO = PRODUCTO.ID_PRODUCTO SHARE-LOCK, ~
      EACH UNIDAD_MEDIDA WHERE UNIDAD_MEDIDA.ID_UNIDAD = PRODUCTO.ID_UNIDAD SHARE-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-8 OPEN QUERY BROWSE-8 FOR EACH PRODUCTO SHARE-LOCK, ~
      EACH STOCK WHERE STOCK.ID_PRODUCTO = PRODUCTO.ID_PRODUCTO SHARE-LOCK, ~
      EACH UNIDAD_MEDIDA WHERE UNIDAD_MEDIDA.ID_UNIDAD = PRODUCTO.ID_UNIDAD SHARE-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-8 PRODUCTO STOCK UNIDAD_MEDIDA
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-8 PRODUCTO
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-8 STOCK
&Scoped-define THIRD-TABLE-IN-QUERY-BROWSE-8 UNIDAD_MEDIDA


/* Definitions for DIALOG-BOX Inventario-Frame                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Inventario-Frame ~
    ~{&OPEN-QUERY-BROWSE-8}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-25 BROWSE-8 BUTTON-1 BUTTON-22 ~
BUTTON-20 RECT-1 RECT-18 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Agregar Producto" 
     SIZE 20 BY 2.86.

DEFINE BUTTON BUTTON-20 
     LABEL "Borrar" 
     SIZE 20 BY 2.86.

DEFINE BUTTON BUTTON-22 
     LABEL "Modificar" 
     SIZE 20 BY 2.86.

DEFINE BUTTON BUTTON-25 
     LABEL "Salir" 
     SIZE 15 BY 1.19.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 23.8 BY 11.95.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 8  
     SIZE 210 BY 2.33
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-8 FOR 
      PRODUCTO, 
      STOCK, 
      UNIDAD_MEDIDA SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-8 Inventario-Frame _STRUCTURED
  QUERY BROWSE-8 SHARE-LOCK NO-WAIT DISPLAY
      PRODUCTO.CODIGO FORMAT "X(10)":U
      PRODUCTO.DESCRIPCION FORMAT "X(150)":U WIDTH 40.4
      STOCK.CANTIDAD FORMAT "->,>>>,>>9":U
      UNIDAD_MEDIDA.DESCRIPCION COLUMN-LABEL "MEDIDA" FORMAT "X(50)":U
            WIDTH 22.4
      STOCK.F_CADUCIDAD COLUMN-LABEL "FECHA DE CADUCIDAD" FORMAT "99/99/99":U
            WIDTH 25.2
      STOCK.F_INGRESO COLUMN-LABEL "FECHA DE INGRESO" FORMAT "99/99/99":U
            WIDTH 23.2
      STOCK.LOTE FORMAT "X(12)":U WIDTH 28.4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 166 BY 22.14
         BGCOLOR 15  ROW-HEIGHT-CHARS .62 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Inventario-Frame
     BUTTON-25 AT ROW 1.62 COL 193.6
     BROWSE-8 AT ROW 4.14 COL 11
     BUTTON-1 AT ROW 9.1 COL 184.4
     BUTTON-22 AT ROW 12.95 COL 184.4
     BUTTON-20 AT ROW 16.95 COL 184.6
     RECT-1 AT ROW 8.43 COL 182.6
     RECT-18 AT ROW 1 COL 1
     "INVENTARIO" VIEW-AS TEXT
          SIZE 15.2 BY 1.19 AT ROW 1.62 COL 92.4
          BGCOLOR 8 FGCOLOR 15 
     SPACE(103.39) SKIP(24.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Inventario".


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
/* SETTINGS FOR DIALOG-BOX Inventario-Frame
                                                                        */
/* BROWSE-TAB BROWSE-8 BUTTON-25 Inventario-Frame */
ASSIGN 
       FRAME Inventario-Frame:SCROLLABLE       = FALSE
       FRAME Inventario-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-8
/* Query rebuild information for BROWSE BROWSE-8
     _TblList          = "Restaurante.PRODUCTO,Restaurante.STOCK WHERE Restaurante.PRODUCTO ...,Restaurante.UNIDAD_MEDIDA WHERE Restaurante.PRODUCTO ..."
     _Options          = "SHARE-LOCK INDEXED-REPOSITION"
     _JoinCode[2]      = "STOCK.ID_PRODUCTO = PRODUCTO.ID_PRODUCTO"
     _JoinCode[3]      = "UNIDAD_MEDIDA.ID_UNIDAD = PRODUCTO.ID_UNIDAD"
     _FldNameList[1]   = Restaurante.PRODUCTO.CODIGO
     _FldNameList[2]   > Restaurante.PRODUCTO.DESCRIPCION
"PRODUCTO.DESCRIPCION" ? ? "character" ? ? ? ? ? ? no ? no no "40.4" yes no no "U" "" ""
     _FldNameList[3]   = Restaurante.STOCK.CANTIDAD
     _FldNameList[4]   > Restaurante.UNIDAD_MEDIDA.DESCRIPCION
"UNIDAD_MEDIDA.DESCRIPCION" "MEDIDA" ? "character" ? ? ? ? ? ? no ? no no "22.4" yes no no "U" "" ""
     _FldNameList[5]   > Restaurante.STOCK.F_CADUCIDAD
"STOCK.F_CADUCIDAD" "FECHA DE CADUCIDAD" ? "date" ? ? ? ? ? ? no ? no no "25.2" yes no no "U" "" ""
     _FldNameList[6]   > Restaurante.STOCK.F_INGRESO
"STOCK.F_INGRESO" "FECHA DE INGRESO" ? "date" ? ? ? ? ? ? no ? no no "23.2" yes no no "U" "" ""
     _FldNameList[7]   > Restaurante.STOCK.LOTE
"STOCK.LOTE" ? ? "character" ? ? ? ? ? ? no ? no no "24.4" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-8 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Inventario-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Inventario-Frame Inventario-Frame
ON WINDOW-CLOSE OF FRAME Inventario-Frame /* Inventario */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 Inventario-Frame
ON CHOOSE OF BUTTON-1 IN FRAME Inventario-Frame /* Agregar Producto */
DO:
    RUN Nuevo_Producto.w.
    {&OPEN-query-BROWSE-8}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-20 Inventario-Frame
ON CHOOSE OF BUTTON-20 IN FRAME Inventario-Frame /* Borrar */
DO:
   DEF VAR vlogborrar AS LOG.
   MESSAGE "Esta seguro de borrar este registro" VIEW-AS ALERT-BOX BUTTONS YES-NO SET vlogborrar.
   IF vlogborrar = TRUE THEN DO:
       ROWID(stock).
       DELETE stock.
       {&OPEN-query-BROWSE-8}
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-22
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-22 Inventario-Frame
ON CHOOSE OF BUTTON-22 IN FRAME Inventario-Frame /* Modificar */
DO:
    DEF VAR vdteactual AS DATE.
 
    vdteactual = TODAY.
    ROWID(stock).
    IF stock.f_ingreso = vdteactual THEN DO:
        RUN modificacion.w(ROWID(stock)).
        {&OPEN-query-BROWSE-8} 
    END.
        ELSE DO:
            MESSAGE "No puedes modificar registros que no sean del dia actual" VIEW-AS ALERT-BOX.
        END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-25
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-25 Inventario-Frame
ON CHOOSE OF BUTTON-25 IN FRAME Inventario-Frame /* Salir */
DO:
  APPLY "window-close" TO FRAME Inventario-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-8
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Inventario-Frame 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Inventario-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Inventario-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Inventario-Frame  _DEFAULT-ENABLE
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
  ENABLE BUTTON-25 BROWSE-8 BUTTON-1 BUTTON-22 BUTTON-20 RECT-1 RECT-18 
      WITH FRAME Inventario-Frame.
  VIEW FRAME Inventario-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Inventario-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


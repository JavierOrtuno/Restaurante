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
&Scoped-Define ENABLED-OBJECTS BROWSE-8 BUTTON-1 BUTTON-22 BUTTON-20 ~
BUTTON-25 RECT-1 

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
     SIZE 20 BY 2.86.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 30 BY 16.43.

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
      PRODUCTO.DESCRIPCION FORMAT "X(150)":U WIDTH 55.4
      STOCK.CANTIDAD FORMAT "->,>>>,>>9":U
      UNIDAD_MEDIDA.DESCRIPCION FORMAT "X(50)":U WIDTH 33.4
      STOCK.F_CADUCIDAD FORMAT "99/99/99":U
      STOCK.F_INGRESO FORMAT "99/99/99":U
      STOCK.LOTE FORMAT "X(11)":U WIDTH 17.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 164 BY 24.05 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Inventario-Frame
     BROWSE-8 AT ROW 1.95 COL 11
     BUTTON-1 AT ROW 6.71 COL 186.4
     BUTTON-22 AT ROW 10.29 COL 186.4
     BUTTON-20 AT ROW 14.33 COL 186.4
     BUTTON-25 AT ROW 18.14 COL 186.4
     RECT-1 AT ROW 5.76 COL 181.2
     SPACE(9.99) SKIP(4.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>".


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
/* BROWSE-TAB BROWSE-8 1 Inventario-Frame */
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
"PRODUCTO.DESCRIPCION" ? ? "character" ? ? ? ? ? ? no ? no no "55.4" yes no no "U" "" ""
     _FldNameList[3]   = Restaurante.STOCK.CANTIDAD
     _FldNameList[4]   > Restaurante.UNIDAD_MEDIDA.DESCRIPCION
"UNIDAD_MEDIDA.DESCRIPCION" ? ? "character" ? ? ? ? ? ? no ? no no "33.4" yes no no "U" "" ""
     _FldNameList[5]   = Restaurante.STOCK.F_CADUCIDAD
     _FldNameList[6]   = Restaurante.STOCK.F_INGRESO
     _FldNameList[7]   > Restaurante.STOCK.LOTE
"STOCK.LOTE" ? "X(11)" "character" ? ? ? ? ? ? no ? no no "17.6" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-8 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Inventario-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Inventario-Frame Inventario-Frame
ON WINDOW-CLOSE OF FRAME Inventario-Frame /* <insert dialog title> */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-8
&Scoped-define SELF-NAME BROWSE-8
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
   ROWID(stock).
   DELETE stock.
   {&OPEN-query-BROWSE-8}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-22
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-22 Inventario-Frame
ON CHOOSE OF BUTTON-22 IN FRAME Inventario-Frame /* Modificar */
DO:
    DEF VAR vdteactual AS DATE.

    vdteactual = TODAY.
    MESSAGE browse-8:VALUE VIEW-AS ALERT-BOX.
    RUN modificacion.w(ROWID(stock)).
    {&OPEN-query-BROWSE-8}
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
  ENABLE BROWSE-8 BUTTON-1 BUTTON-22 BUTTON-20 BUTTON-25 RECT-1 
      WITH FRAME Inventario-Frame.
  VIEW FRAME Inventario-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Inventario-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


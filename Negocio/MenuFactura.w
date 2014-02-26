&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          restaurante      PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Factura-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Factura-Frame 
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

DEF VAR crowid AS ROWID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Factura-Frame
&Scoped-define BROWSE-NAME BROWSE-21

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES FACTURA COMANDA FORMA_PAGO ESTATUS

/* Definitions for BROWSE BROWSE-21                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-21 FACTURA.FOLIO FACTURA.FECHA ~
FACTURA.SUBTOTAL FACTURA.IVA COMANDA.PROPINA FACTURA.TOTAL ~
FORMA_PAGO.DESCRIPCION ESTATUS.DESCRIPCION COMANDA.HORA_LLEGADA ~
COMANDA.HORA_SALIDA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-21 
&Scoped-define QUERY-STRING-BROWSE-21 FOR EACH FACTURA NO-LOCK, ~
      EACH COMANDA WHERE COMANDA.ID_COMANDA = FACTURA.ID_COMANDA NO-LOCK, ~
      EACH FORMA_PAGO WHERE FORMA_PAGO.ID_PAGO = FACTURA.ID_PAGO NO-LOCK, ~
      EACH ESTATUS WHERE ESTATUS.ID_ESTATUS = FACTURA.ID_ESTATUS NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-21 OPEN QUERY BROWSE-21 FOR EACH FACTURA NO-LOCK, ~
      EACH COMANDA WHERE COMANDA.ID_COMANDA = FACTURA.ID_COMANDA NO-LOCK, ~
      EACH FORMA_PAGO WHERE FORMA_PAGO.ID_PAGO = FACTURA.ID_PAGO NO-LOCK, ~
      EACH ESTATUS WHERE ESTATUS.ID_ESTATUS = FACTURA.ID_ESTATUS NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-21 FACTURA COMANDA FORMA_PAGO ESTATUS
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-21 FACTURA
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-21 COMANDA
&Scoped-define THIRD-TABLE-IN-QUERY-BROWSE-21 FORMA_PAGO
&Scoped-define FOURTH-TABLE-IN-QUERY-BROWSE-21 ESTATUS


/* Definitions for DIALOG-BOX Factura-Frame                             */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Factura-Frame ~
    ~{&OPEN-QUERY-BROWSE-21}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-21 BUTTON-2 BtnCancel RECT-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 15 BY 1.81
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-2 
     LABEL "Actualizar" 
     SIZE 15 BY 1.91.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 8  
     SIZE 170 BY 2.38
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-21 FOR 
      FACTURA, 
      COMANDA, 
      FORMA_PAGO, 
      ESTATUS SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-21 Factura-Frame _STRUCTURED
  QUERY BROWSE-21 NO-LOCK DISPLAY
      FACTURA.FOLIO FORMAT "X(14)":U WIDTH 16.8
      FACTURA.FECHA FORMAT "99/99/99":U WIDTH 15.2
      FACTURA.SUBTOTAL FORMAT "->>,>>9.99":U
      FACTURA.IVA FORMAT "->>,>>9.99":U
      COMANDA.PROPINA FORMAT "->>,>>9.99":U
      FACTURA.TOTAL FORMAT "->>,>>9.99":U
      FORMA_PAGO.DESCRIPCION FORMAT "X(14)":U
      ESTATUS.DESCRIPCION FORMAT "X(14)":U
      COMANDA.HORA_LLEGADA FORMAT "X(5)":U
      COMANDA.HORA_SALIDA FORMAT "X(5)":U WIDTH 17.4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 149.6 BY 4.52
         BGCOLOR 15  EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Factura-Frame
     BROWSE-21 AT ROW 4.52 COL 11.4
     BUTTON-2 AT ROW 10.05 COL 14
     BtnCancel AT ROW 10.14 COL 31
     RECT-1 AT ROW 1 COL 1
     "ACTUALIZAR PRODUCTOS" VIEW-AS TEXT
          SIZE 28 BY 1.19 AT ROW 1.62 COL 32
          BGCOLOR 8 FGCOLOR 15 FONT 12
     SPACE(110.99) SKIP(10.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Facturas"
         CANCEL-BUTTON BtnCancel.


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
/* SETTINGS FOR DIALOG-BOX Factura-Frame
                                                                        */
/* BROWSE-TAB BROWSE-21 1 Factura-Frame */
ASSIGN 
       FRAME Factura-Frame:SCROLLABLE       = FALSE
       FRAME Factura-Frame:HIDDEN           = TRUE.

ASSIGN 
       BUTTON-2:HIDDEN IN FRAME Factura-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-21
/* Query rebuild information for BROWSE BROWSE-21
     _TblList          = "Restaurante.FACTURA,Restaurante.COMANDA WHERE Restaurante.FACTURA ...,Restaurante.FORMA_PAGO WHERE Restaurante.FACTURA ...,Restaurante.ESTATUS WHERE Restaurante.FACTURA ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _JoinCode[2]      = "COMANDA.ID_COMANDA = FACTURA.ID_COMANDA"
     _JoinCode[3]      = "FORMA_PAGO.ID_PAGO = FACTURA.ID_PAGO"
     _JoinCode[4]      = "ESTATUS.ID_ESTATUS = FACTURA.ID_ESTATUS"
     _FldNameList[1]   > Restaurante.FACTURA.FOLIO
"FACTURA.FOLIO" ? "X(14)" "character" ? ? ? ? ? ? no ? no no "16.8" yes no no "U" "" ""
     _FldNameList[2]   > Restaurante.FACTURA.FECHA
"FACTURA.FECHA" ? ? "date" ? ? ? ? ? ? no ? no no "15.2" yes no no "U" "" ""
     _FldNameList[3]   = Restaurante.FACTURA.SUBTOTAL
     _FldNameList[4]   = Restaurante.FACTURA.IVA
     _FldNameList[5]   = Restaurante.COMANDA.PROPINA
     _FldNameList[6]   = Restaurante.FACTURA.TOTAL
     _FldNameList[7]   > Restaurante.FORMA_PAGO.DESCRIPCION
"FORMA_PAGO.DESCRIPCION" ? "X(14)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Restaurante.ESTATUS.DESCRIPCION
"ESTATUS.DESCRIPCION" ? "X(14)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   = Restaurante.COMANDA.HORA_LLEGADA
     _FldNameList[10]   > Restaurante.COMANDA.HORA_SALIDA
"COMANDA.HORA_SALIDA" ? ? "character" ? ? ? ? ? ? no ? no no "17.4" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-21 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Factura-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Factura-Frame Factura-Frame
ON WINDOW-CLOSE OF FRAME Factura-Frame /* Facturas */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 Factura-Frame
ON CHOOSE OF BUTTON-2 IN FRAME Factura-Frame /* Actualizar */
DO:
  FIND CURRENT Factura.
  crowid = ROWID(Factura).
  RUN ActualizaFactura.w(crowid).
  {&OPEN-query-BROWSE-21}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-21
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Factura-Frame 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Factura-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Factura-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Factura-Frame  _DEFAULT-ENABLE
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
  ENABLE BROWSE-21 BUTTON-2 BtnCancel RECT-1 
      WITH FRAME Factura-Frame.
  VIEW FRAME Factura-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Factura-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


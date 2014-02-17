&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          restaurante      PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-20

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES EMPLEADO PERSONA COMANDA FACTURA ESTATUS ~
FORMA_PAGO

/* Definitions for BROWSE BROWSE-20                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-20 FACTURA.FOLIO COMANDA.F_ATENCION ~
COMANDA.HORA_LLEGADA COMANDA.HORA_SALIDA ESTATUS.DESCRIPCION ~
PERSONA.A_PATERNO PERSONA.NOMBRES FACTURA.SUBTOTAL FACTURA.IVA ~
FACTURA.TOTAL FORMA_PAGO.DESCRIPCION COMANDA.PROPINA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-20 
&Scoped-define QUERY-STRING-BROWSE-20 FOR EACH EMPLEADO NO-LOCK, ~
      EACH PERSONA WHERE PERSONA.ID_PERSONA = EMPLEADO.ID_PERSONA NO-LOCK, ~
      EACH COMANDA WHERE COMANDA.ID_EMPLEADO = EMPLEADO.ID_EMPLEADO NO-LOCK, ~
      EACH FACTURA WHERE FACTURA.ID_COMANDA = COMANDA.ID_COMANDA NO-LOCK, ~
      EACH ESTATUS WHERE ESTATUS.ID_ESTATUS = FACTURA.ID_ESTATUS NO-LOCK, ~
      EACH FORMA_PAGO WHERE FORMA_PAGO.ID_PAGO = FACTURA.ID_PAGO NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-20 OPEN QUERY BROWSE-20 FOR EACH EMPLEADO NO-LOCK, ~
      EACH PERSONA WHERE PERSONA.ID_PERSONA = EMPLEADO.ID_PERSONA NO-LOCK, ~
      EACH COMANDA WHERE COMANDA.ID_EMPLEADO = EMPLEADO.ID_EMPLEADO NO-LOCK, ~
      EACH FACTURA WHERE FACTURA.ID_COMANDA = COMANDA.ID_COMANDA NO-LOCK, ~
      EACH ESTATUS WHERE ESTATUS.ID_ESTATUS = FACTURA.ID_ESTATUS NO-LOCK, ~
      EACH FORMA_PAGO WHERE FORMA_PAGO.ID_PAGO = FACTURA.ID_PAGO NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-20 EMPLEADO PERSONA COMANDA FACTURA ~
ESTATUS FORMA_PAGO
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-20 EMPLEADO
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-20 PERSONA
&Scoped-define THIRD-TABLE-IN-QUERY-BROWSE-20 COMANDA
&Scoped-define FOURTH-TABLE-IN-QUERY-BROWSE-20 FACTURA
&Scoped-define FIFTH-TABLE-IN-QUERY-BROWSE-20 ESTATUS
&Scoped-define SIXTH-TABLE-IN-QUERY-BROWSE-20 FORMA_PAGO


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-20}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-20 BUTTON-30 Btn_Cancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Salir" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-30 
     LABEL "Registrar" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-20 FOR 
      EMPLEADO, 
      PERSONA, 
      COMANDA, 
      FACTURA, 
      ESTATUS, 
      FORMA_PAGO SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-20 Dialog-Frame _STRUCTURED
  QUERY BROWSE-20 NO-LOCK DISPLAY
      FACTURA.FOLIO FORMAT "X(8)":U
      COMANDA.F_ATENCION COLUMN-LABEL "Fecha" FORMAT "99/99/99":U
      COMANDA.HORA_LLEGADA COLUMN-LABEL "Entrada" FORMAT "X(5)":U
      COMANDA.HORA_SALIDA COLUMN-LABEL "Salida" FORMAT "X(5)":U
      ESTATUS.DESCRIPCION COLUMN-LABEL "Estatus" FORMAT "X(15)":U
      PERSONA.A_PATERNO COLUMN-LABEL "" FORMAT "X(25)":U
      PERSONA.NOMBRES COLUMN-LABEL "Mesero" FORMAT "X(25)":U
      FACTURA.SUBTOTAL COLUMN-LABEL "Subtotal" FORMAT "->>,>>9.99":U
      FACTURA.IVA FORMAT "->>,>>9.99":U
      FACTURA.TOTAL COLUMN-LABEL "Total" FORMAT "->>,>>9.99":U
      FORMA_PAGO.DESCRIPCION COLUMN-LABEL "Forma de Pago" FORMAT "X(15)":U
      COMANDA.PROPINA COLUMN-LABEL "Propina" FORMAT "->>,>>9.99":U
            WIDTH 18.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 173 BY 7.38 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-20 AT ROW 1.71 COL 5
     BUTTON-30 AT ROW 10.52 COL 11
     Btn_Cancel AT ROW 10.52 COL 31
     SPACE(138.59) SKIP(1.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>"
         CANCEL-BUTTON Btn_Cancel.


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
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
/* BROWSE-TAB BROWSE-20 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-20
/* Query rebuild information for BROWSE BROWSE-20
     _TblList          = "Restaurante.EMPLEADO,Restaurante.PERSONA WHERE Restaurante.EMPLEADO ...,Restaurante.COMANDA WHERE Restaurante.EMPLEADO ...,Restaurante.FACTURA WHERE Restaurante.COMANDA ...,Restaurante.ESTATUS WHERE Restaurante.FACTURA ...,Restaurante.FORMA_PAGO WHERE Restaurante.FACTURA ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _JoinCode[2]      = "PERSONA.ID_PERSONA = EMPLEADO.ID_PERSONA"
     _JoinCode[3]      = "COMANDA.ID_EMPLEADO = EMPLEADO.ID_EMPLEADO"
     _JoinCode[4]      = "FACTURA.ID_COMANDA = COMANDA.ID_COMANDA"
     _JoinCode[5]      = "ESTATUS.ID_ESTATUS = FACTURA.ID_ESTATUS"
     _JoinCode[6]      = "FORMA_PAGO.ID_PAGO = FACTURA.ID_PAGO"
     _FldNameList[1]   > Restaurante.FACTURA.FOLIO
"FACTURA.FOLIO" ? "X(8)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Restaurante.COMANDA.F_ATENCION
"COMANDA.F_ATENCION" "Fecha" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Restaurante.COMANDA.HORA_LLEGADA
"COMANDA.HORA_LLEGADA" "Entrada" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Restaurante.COMANDA.HORA_SALIDA
"COMANDA.HORA_SALIDA" "Salida" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Restaurante.ESTATUS.DESCRIPCION
"ESTATUS.DESCRIPCION" "Estatus" "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Restaurante.PERSONA.A_PATERNO
"PERSONA.A_PATERNO" "" "X(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Restaurante.PERSONA.NOMBRES
"PERSONA.NOMBRES" "Mesero" "X(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Restaurante.FACTURA.SUBTOTAL
"FACTURA.SUBTOTAL" "Subtotal" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   = Restaurante.FACTURA.IVA
     _FldNameList[10]   > Restaurante.FACTURA.TOTAL
"FACTURA.TOTAL" "Total" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[11]   > Restaurante.FORMA_PAGO.DESCRIPCION
"FORMA_PAGO.DESCRIPCION" "Forma de Pago" "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[12]   > Restaurante.COMANDA.PROPINA
"COMANDA.PROPINA" "Propina" ? "decimal" ? ? ? ? ? ? no ? no no "18.6" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-20 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-30
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-30 Dialog-Frame
ON CHOOSE OF BUTTON-30 IN FRAME Dialog-Frame /* Registrar */
DO:
  RUN RegistroVentas.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-20
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  ENABLE BROWSE-20 BUTTON-30 Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


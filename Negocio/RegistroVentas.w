&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          restaurante      PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME RegVenta-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS RegVenta-Frame 
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

DEF VAR vintProducto AS INT.
DEF VAR vintCantidad AS INT.
DEF VAR vintUno AS INT.
DEF VAR vdecSubtotal AS DEC.
DEF VAR vdecIVA AS DEC.
DEF VAR vdecTotal AS DEC.
DEF VAR vchrFolio AS CHAR.
DEF VAR vdteFecha AS DATE.
DEF VAR vchrEntrada AS CHAR.
DEF VAR vchrSalida AS CHAR.
DEF VAR vdecPropina AS DEC.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME RegVenta-Frame
&Scoped-define BROWSE-NAME BROWSE-11

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ESTATUS FORMA_PAGO PERSONA EMPLEADO ROL MENU ~
MESA

/* Definitions for BROWSE BROWSE-11                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-11 ESTATUS.DESCRIPCION 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-11 
&Scoped-define QUERY-STRING-BROWSE-11 FOR EACH ESTATUS NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-11 OPEN QUERY BROWSE-11 FOR EACH ESTATUS NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-11 ESTATUS
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-11 ESTATUS


/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 FORMA_PAGO.DESCRIPCION 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH FORMA_PAGO NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH FORMA_PAGO NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 FORMA_PAGO
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 FORMA_PAGO


/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 PERSONA.NOMBRES 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3 
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH PERSONA NO-LOCK, ~
      EACH EMPLEADO WHERE EMPLEADO.ID_PERSONA = PERSONA.ID_PERSONA NO-LOCK, ~
      EACH ROL WHERE ROL.ID_ROL = EMPLEADO.ID_ROL ~
  AND ROL.ID_ROL = 2 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY BROWSE-3 FOR EACH PERSONA NO-LOCK, ~
      EACH EMPLEADO WHERE EMPLEADO.ID_PERSONA = PERSONA.ID_PERSONA NO-LOCK, ~
      EACH ROL WHERE ROL.ID_ROL = EMPLEADO.ID_ROL ~
  AND ROL.ID_ROL = 2 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 PERSONA EMPLEADO ROL
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 PERSONA
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-3 EMPLEADO
&Scoped-define THIRD-TABLE-IN-QUERY-BROWSE-3 ROL


/* Definitions for BROWSE BROWSE-8                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-8 MENU.DESCRIPCION MENU.PRECIO 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-8 
&Scoped-define QUERY-STRING-BROWSE-8 FOR EACH MENU NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-8 OPEN QUERY BROWSE-8 FOR EACH MENU NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-8 MENU
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-8 MENU


/* Definitions for BROWSE BROWSE-9                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-9 MESA.DESCRIPCION 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-9 
&Scoped-define QUERY-STRING-BROWSE-9 FOR EACH MESA NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-9 OPEN QUERY BROWSE-9 FOR EACH MESA NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-9 MESA
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-9 MESA


/* Definitions for DIALOG-BOX RegVenta-Frame                            */
&Scoped-define OPEN-BROWSERS-IN-QUERY-RegVenta-Frame ~
    ~{&OPEN-QUERY-BROWSE-11}~
    ~{&OPEN-QUERY-BROWSE-2}~
    ~{&OPEN-QUERY-BROWSE-3}~
    ~{&OPEN-QUERY-BROWSE-8}~
    ~{&OPEN-QUERY-BROWSE-9}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Folio HoraEntrada HoraSalida BROWSE-8 ~
BROWSE-9 BROWSE-2 BROWSE-11 Subtotal BROWSE-3 IVA Cantidad BUTTON-1 Total ~
Propina BtnOK BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS Fecha Folio HoraEntrada HoraSalida ~
Subtotal IVA Cantidad Total Propina 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FechaActual RegVenta-Frame 
FUNCTION FechaActual RETURNS DATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     LABEL "Agregar" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE Cantidad AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cantidad" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Fecha AS DATE FORMAT "99/99/99":U 
     LABEL "Fecha" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Folio AS CHARACTER FORMAT "X(256)":U 
     LABEL "No. de Folio" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE HoraEntrada AS CHARACTER FORMAT "99:99":U 
     LABEL "Hora Entrada" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE HoraSalida AS CHARACTER FORMAT "99:99":U 
     LABEL "Hora Salida" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE IVA AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "IVA" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Propina AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Propina" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Subtotal AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Subtotal" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Total AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Total" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-11 FOR 
      ESTATUS SCROLLING.

DEFINE QUERY BROWSE-2 FOR 
      FORMA_PAGO SCROLLING.

DEFINE QUERY BROWSE-3 FOR 
      PERSONA, 
      EMPLEADO, 
      ROL SCROLLING.

DEFINE QUERY BROWSE-8 FOR 
      MENU SCROLLING.

DEFINE QUERY BROWSE-9 FOR 
      MESA SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-11 RegVenta-Frame _STRUCTURED
  QUERY BROWSE-11 NO-LOCK DISPLAY
      ESTATUS.DESCRIPCION COLUMN-LABEL "Estatus" FORMAT "X(10)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-SCROLLBAR-VERTICAL SIZE 24 BY 4.05 ROW-HEIGHT-CHARS .76 EXPANDABLE.

DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 RegVenta-Frame _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      FORMA_PAGO.DESCRIPCION COLUMN-LABEL "Forma de Pago" FORMAT "X(15)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-SCROLLBAR-VERTICAL SIZE 22 BY 3.57 ROW-HEIGHT-CHARS .67 EXPANDABLE.

DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 RegVenta-Frame _STRUCTURED
  QUERY BROWSE-3 NO-LOCK DISPLAY
      PERSONA.NOMBRES COLUMN-LABEL "Meseros" FORMAT "X(25)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-SCROLLBAR-VERTICAL SIZE 30 BY 2.62 EXPANDABLE.

DEFINE BROWSE BROWSE-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-8 RegVenta-Frame _STRUCTURED
  QUERY BROWSE-8 NO-LOCK DISPLAY
      MENU.DESCRIPCION COLUMN-LABEL "Producto" FORMAT "X(30)":U
            WIDTH 46.2
      MENU.PRECIO COLUMN-LABEL "Precio" FORMAT "->>,>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-SCROLLBAR-VERTICAL SIZE 69 BY 6.43 EXPANDABLE.

DEFINE BROWSE BROWSE-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-9 RegVenta-Frame _STRUCTURED
  QUERY BROWSE-9 NO-LOCK DISPLAY
      MESA.DESCRIPCION COLUMN-LABEL "Mesa" FORMAT "X(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 23 BY 4.52 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME RegVenta-Frame
     Fecha AT ROW 1.95 COL 9 COLON-ALIGNED
     Folio AT ROW 1.95 COL 40 COLON-ALIGNED
     HoraEntrada AT ROW 1.95 COL 74 COLON-ALIGNED
     HoraSalida AT ROW 1.95 COL 104 COLON-ALIGNED
     BROWSE-8 AT ROW 3.62 COL 111
     BROWSE-9 AT ROW 3.86 COL 10
     BROWSE-2 AT ROW 3.86 COL 44
     BROWSE-11 AT ROW 3.86 COL 77
     Subtotal AT ROW 9.33 COL 51 COLON-ALIGNED
     BROWSE-3 AT ROW 9.57 COL 9
     IVA AT ROW 9.57 COL 76 COLON-ALIGNED
     Cantidad AT ROW 10.52 COL 145 COLON-ALIGNED
     BUTTON-1 AT ROW 10.52 COL 164
     Total AT ROW 11.48 COL 50 COLON-ALIGNED
     Propina AT ROW 11.48 COL 76 COLON-ALIGNED
     BtnOK AT ROW 14.1 COL 142
     BtnCancel AT ROW 14.1 COL 161
     SPACE(15.99) SKIP(0.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Registro de Venta"
         CANCEL-BUTTON BtnCancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB RegVenta-Frame 
/* ************************* Included-Libraries *********************** */

{ventas.i}
{inventario.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX RegVenta-Frame
                                                                        */
/* BROWSE-TAB BROWSE-8 HoraSalida RegVenta-Frame */
/* BROWSE-TAB BROWSE-9 BROWSE-8 RegVenta-Frame */
/* BROWSE-TAB BROWSE-2 BROWSE-9 RegVenta-Frame */
/* BROWSE-TAB BROWSE-11 BROWSE-2 RegVenta-Frame */
/* BROWSE-TAB BROWSE-3 Subtotal RegVenta-Frame */
ASSIGN 
       FRAME RegVenta-Frame:SCROLLABLE       = FALSE
       FRAME RegVenta-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Fecha IN FRAME RegVenta-Frame
   NO-ENABLE                                                            */
ASSIGN 
       Fecha:READ-ONLY IN FRAME RegVenta-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-11
/* Query rebuild information for BROWSE BROWSE-11
     _TblList          = "Restaurante.ESTATUS"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Restaurante.ESTATUS.DESCRIPCION
"ESTATUS.DESCRIPCION" "Estatus" "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-11 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "Restaurante.FORMA_PAGO"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Restaurante.FORMA_PAGO.DESCRIPCION
"FORMA_PAGO.DESCRIPCION" "Forma de Pago" "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _TblList          = "Restaurante.PERSONA,Restaurante.EMPLEADO WHERE Restaurante.PERSONA ... ...,Restaurante.ROL WHERE Restaurante.EMPLEADO ... ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _JoinCode[2]      = "EMPLEADO.ID_PERSONA = PERSONA.ID_PERSONA"
     _JoinCode[3]      = "ROL.ID_ROL = EMPLEADO.ID_ROL
  AND ROL.ID_ROL = 2"
     _FldNameList[1]   > Restaurante.PERSONA.NOMBRES
"PERSONA.NOMBRES" "Meseros" "X(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-8
/* Query rebuild information for BROWSE BROWSE-8
     _TblList          = "Restaurante.MENU"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Restaurante.MENU.DESCRIPCION
"MENU.DESCRIPCION" "Producto" "X(30)" "character" ? ? ? ? ? ? no ? no no "46.2" yes no no "U" "" ""
     _FldNameList[2]   > Restaurante.MENU.PRECIO
"MENU.PRECIO" "Precio" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-8 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-9
/* Query rebuild information for BROWSE BROWSE-9
     _TblList          = "Restaurante.MESA"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Restaurante.MESA.DESCRIPCION
"MESA.DESCRIPCION" "Mesa" "X(8)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-9 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME RegVenta-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RegVenta-Frame RegVenta-Frame
ON WINDOW-CLOSE OF FRAME RegVenta-Frame /* Registro de Venta */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK RegVenta-Frame
ON CHOOSE OF BtnOK IN FRAME RegVenta-Frame /* OK */
DO:
  
  vchrFolio = Folio:SCREEN-VALUE.
  vdteFecha = DATE(Fecha:SCREEN-VALUE).
  vchrEntrada = HoraEntrada:SCREEN-VALUE.
  vchrSalida = HoraSalida:SCREEN-VALUE.
  vdecPropina = DEC(Propina:SCREEN-VALUE).

  RUN LlenarComanda(vdteFecha,vchrEntrada,vchrSalida,vdecPropina).
  RUN LlenarFactura(vchrFolio,vdecSubtotal,vdecIVA,vdecTotal).
  RUN LlenarConsumo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 RegVenta-Frame
ON CHOOSE OF BUTTON-1 IN FRAME RegVenta-Frame /* Agregar */
DO:
  vintCantidad = INT(Cantidad:SCREEN-VALUE).

  vdecSubTotal = CalcularSubtotal(vintCantidad).
  ASSIGN Subtotal:SCREEN-VALUE = STRING(vdecSubtotal).
  
  vdecIVA = CalcularIVA(vdecSubtotal).
  ASSIGN IVA:SCREEN-VALUE = STRING(vdecIVA).

  vdecTotal = CalcularTotal(vdecSubtotal,vdecIVA).
  ASSIGN TOTAL:SCREEN-VALUE = String(vdecTotal).

  RUN DescontarInventario(vintCantidad,OUTPUT vintProducto,OUTPUT vintCantidad,OUTPUT vintUno).

  DescontarExistencia(vintProducto,vintCantidad,vintUno).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-11
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK RegVenta-Frame 


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
  ASSIGN Fecha:SCREEN-VALUE = string(FechaActual()).
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI RegVenta-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME RegVenta-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI RegVenta-Frame  _DEFAULT-ENABLE
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
  DISPLAY Fecha Folio HoraEntrada HoraSalida Subtotal IVA Cantidad Total Propina 
      WITH FRAME RegVenta-Frame.
  ENABLE Folio HoraEntrada HoraSalida BROWSE-8 BROWSE-9 BROWSE-2 BROWSE-11 
         Subtotal BROWSE-3 IVA Cantidad BUTTON-1 Total Propina BtnOK BtnCancel 
      WITH FRAME RegVenta-Frame.
  VIEW FRAME RegVenta-Frame.
  {&OPEN-BROWSERS-IN-QUERY-RegVenta-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FechaActual RegVenta-Frame 
FUNCTION FechaActual RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Obtener la fecha actual  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR vchrFecha AS DATE.

  vchrFecha = TODAY.  

  RETURN vchrFecha.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


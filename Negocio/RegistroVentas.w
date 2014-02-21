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
DEF VAR vchrFolio AS CHAR.
DEF VAR vdteFecha AS DATE.
DEF VAR vchrEntrada AS CHAR.
DEF VAR vchrSalida AS CHAR.
DEF VAR vdecPropina AS DEC.
DEF VAR vdecSubtotal AS DEC.
DEF VAR vdecIVA AS DEC.
DEF VAR vdecTotal AS DEC.
DEF VAR vdecCuenta AS DEC.
DEF VAR vintStock AS INT.
DEF VAR vlogBandera AS LOG.
DEF VAR vintTotal AS INT.
DEF VAR vlogSemaforo AS LOG.

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
&Scoped-define INTERNAL-TABLES ESTATUS FORMA_PAGO MENU PERSONA EMPLEADO ROL ~
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


/* Definitions for BROWSE BROWSE-20                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-20 MENU.DESCRIPCION MENU.PRECIO 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-20 
&Scoped-define QUERY-STRING-BROWSE-20 FOR EACH MENU NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-20 OPEN QUERY BROWSE-20 FOR EACH MENU NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-20 MENU
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-20 MENU


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
    ~{&OPEN-QUERY-BROWSE-20}~
    ~{&OPEN-QUERY-BROWSE-3}~
    ~{&OPEN-QUERY-BROWSE-9}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS HoraEntrada HoraSalida BROWSE-20 BROWSE-9 ~
BROWSE-2 BROWSE-11 Cantidad BUTTON-1 Subtotal BROWSE-3 IVA SELECT-1 Total ~
Propina BtnOK BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS Fecha HoraEntrada HoraSalida Cantidad ~
Subtotal IVA SELECT-1 Total Propina 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidarSalida RegVenta-Frame 
FUNCTION ValidarSalida RETURNS LOGICAL
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

DEFINE VARIABLE HoraEntrada AS CHARACTER FORMAT "x(5)":U 
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

DEFINE VARIABLE SELECT-1 AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 63 BY 4.52 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-11 FOR 
      ESTATUS SCROLLING.

DEFINE QUERY BROWSE-2 FOR 
      FORMA_PAGO SCROLLING.

DEFINE QUERY BROWSE-20 FOR 
      MENU SCROLLING.

DEFINE QUERY BROWSE-3 FOR 
      PERSONA, 
      EMPLEADO, 
      ROL SCROLLING.

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

DEFINE BROWSE BROWSE-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-20 RegVenta-Frame _STRUCTURED
  QUERY BROWSE-20 NO-LOCK DISPLAY
      MENU.DESCRIPCION FORMAT "X(25)":U WIDTH 31.2
      MENU.PRECIO FORMAT "->>,>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 62 BY 5 EXPANDABLE.

DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 RegVenta-Frame _STRUCTURED
  QUERY BROWSE-3 NO-LOCK DISPLAY
      PERSONA.NOMBRES COLUMN-LABEL "Meseros" FORMAT "X(25)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-SCROLLBAR-VERTICAL SIZE 30 BY 2.62 EXPANDABLE.

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
     HoraEntrada AT ROW 1.95 COL 43.2 COLON-ALIGNED
     HoraSalida AT ROW 1.95 COL 73 COLON-ALIGNED
     BROWSE-20 AT ROW 1.95 COL 121
     BROWSE-9 AT ROW 3.86 COL 10
     BROWSE-2 AT ROW 3.86 COL 44
     BROWSE-11 AT ROW 3.86 COL 77
     Cantidad AT ROW 7.67 COL 140 COLON-ALIGNED
     BUTTON-1 AT ROW 7.67 COL 165
     Subtotal AT ROW 9.33 COL 51 COLON-ALIGNED
     BROWSE-3 AT ROW 9.57 COL 9
     IVA AT ROW 9.57 COL 76 COLON-ALIGNED
     SELECT-1 AT ROW 9.57 COL 121 NO-LABEL
     Total AT ROW 11.48 COL 50 COLON-ALIGNED
     Propina AT ROW 11.48 COL 76 COLON-ALIGNED
     BtnOK AT ROW 14.1 COL 9
     BtnCancel AT ROW 14.1 COL 25
     SPACE(151.99) SKIP(1.04)
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
/* BROWSE-TAB BROWSE-20 HoraSalida RegVenta-Frame */
/* BROWSE-TAB BROWSE-9 BROWSE-20 RegVenta-Frame */
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

ASSIGN 
       HoraEntrada:READ-ONLY IN FRAME RegVenta-Frame        = TRUE.

ASSIGN 
       IVA:READ-ONLY IN FRAME RegVenta-Frame        = TRUE.

ASSIGN 
       Subtotal:READ-ONLY IN FRAME RegVenta-Frame        = TRUE.

ASSIGN 
       Total:READ-ONLY IN FRAME RegVenta-Frame        = TRUE.

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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-20
/* Query rebuild information for BROWSE BROWSE-20
     _TblList          = "Restaurante.MENU"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Restaurante.MENU.DESCRIPCION
"MENU.DESCRIPCION" ? "X(25)" "character" ? ? ? ? ? ? no ? no no "31.2" yes no no "U" "" ""
     _FldNameList[2]   = Restaurante.MENU.PRECIO
     _Query            is OPENED
*/  /* BROWSE BROWSE-20 */
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
  vdteFecha = DATE(Fecha:SCREEN-VALUE).
  vchrEntrada = HoraEntrada:SCREEN-VALUE.
  vchrSalida = HoraSalida:SCREEN-VALUE.
  vdecPropina = DEC(Propina:SCREEN-VALUE).

  RUN LlenarComanda(vdteFecha,vchrEntrada,vchrSalida,vdecPropina).
  RUN LlenarFactura(vdecSubtotal,vdecIVA,vdecTotal).
  RUN LlenarConsumo(vintCantidad).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 RegVenta-Frame
ON CHOOSE OF BUTTON-1 IN FRAME RegVenta-Frame /* Agregar */
DO:
  IF ValidarSalida() = TRUE 
      THEN vlogSemaforo = ValidarFrame().
  ELSE 
      MESSAGE "La hora de salida debe estar en formato de 24 horas" VIEW-AS ALERT-BOX.

  IF vlogSemaforo = TRUE THEN DO:
  ASSIGN vintCantidad = INT(Cantidad:SCREEN-VALUE).
  vdecSubtotal = vdecSubtotal + CalcularSubtotal(vintCantidad).
  vdecIVA = CalcularIVA(vdecSubtotal).
  vdecTotal = CalcularTotal(vdecSubtotal,vdecIVA) + DEC(Propina:SCREEN-VALUE).

  vlogBandera = DescontarInventario(vintCantidad).
  IF vlogBandera = FALSE THEN DO:
      ASSIGN Subtotal:SCREEN-VALUE = STRING(vdecSubtotal)
             IVA:SCREEN-VALUE = STRING(vdecIVA)
              TOTAL:SCREEN-VALUE = STRING(vdecTotal).
      RUN Platillos(vintCantidad).
  END.
  ELSE
      MESSAGE "No te puedo vender esa cantidad" VIEW-AS ALERT-BOX.
  END.
  ELSE
      MESSAGE "Venta no registrada" VIEW-AS ALERT-BOX.
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
  vchrEntrada = HoraEntrada().
  ASSIGN HoraEntrada:SCREEN-VALUE = vchrEntrada.
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
  DISPLAY Fecha HoraEntrada HoraSalida Cantidad Subtotal IVA SELECT-1 Total 
          Propina 
      WITH FRAME RegVenta-Frame.
  ENABLE HoraEntrada HoraSalida BROWSE-20 BROWSE-9 BROWSE-2 BROWSE-11 Cantidad 
         BUTTON-1 Subtotal BROWSE-3 IVA SELECT-1 Total Propina BtnOK BtnCancel 
      WITH FRAME RegVenta-Frame.
  VIEW FRAME RegVenta-Frame.
  {&OPEN-BROWSERS-IN-QUERY-RegVenta-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Platillos RegVenta-Frame 
PROCEDURE Platillos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAMETER inintCantidad AS INT.
  DEF VAR vchrList1 AS CHAR.
  DEF VAR vchrList2 AS CHAR.

  IF inintCantidad <> 0 
      THEN DO:
            ASSIGN vchrList1 = STRING(MENU.Descripcion).
            SELECT-1:LIST-ITEM-PAIRS IN FRAME RegVenta-Frame = vchrList1 + ",Menu.ID".
            SELECT-1:SCREEN-VALUE.
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidarSalida RegVenta-Frame 
FUNCTION ValidarSalida RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR vlogPase AS LOG.

  ASSIGN vchrSalida = (HoraSalida:SCREEN-VALUE IN FRAME RegVenta-Frame).


  IF INT(TRIM(SUBSTR(vchrSalida,1,2),":")) < 24 AND INT(TRIM(SUBSTR(vchrSalida,4,2),":")) < 60
  THEN DO:
      vlogPase = TRUE.
  END.

  RETURN vlogPase.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


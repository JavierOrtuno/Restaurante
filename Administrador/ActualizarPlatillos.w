&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dlg_CreacionP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dlg_CreacionP 
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
{productos.i}
{platillos.i}
{string_library.i}
/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER pinIntAction AS INTEGER.
DEFINE INPUT PARAMETER pinRowId AS ROWID.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE vcharIngredientes AS CHARACTER INITIAL "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dlg_CreacionP

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Salir Fill_Search Sel_Productos ~
Fill_Descripcion Fill_Precio List_Clasificacion Sel_Ingredientes ~
Btn_Agregar Btn_Producto RECT-18 RECT-24 
&Scoped-Define DISPLAYED-OBJECTS Fill_Search Sel_Productos Fill_Descripcion ~
Fill_Precio List_Clasificacion Sel_Ingredientes 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD validatePlatillo Dlg_CreacionP 
FUNCTION validatePlatillo RETURNS LOGICAL
    ( vcharDesc AS CHARACTER, 
      vdecPrecio AS DECIMAL, 
      vintClas AS INTEGER, 
      vintIng AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Agregar 
     LABEL "Agregar" 
     SIZE 20 BY 2.52.

DEFINE BUTTON Btn_Producto 
     LABEL "Nuevo Producto" 
     SIZE 20 BY 2.52.

DEFINE BUTTON Btn_Salir 
     LABEL "Salir" 
     SIZE 15.2 BY 1.14.

DEFINE VARIABLE List_Clasificacion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Clasificación" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 35.4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_Descripcion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Platillo" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_Precio AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Precio" 
     VIEW-AS FILL-IN 
     SIZE 19.4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_Search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Buscar" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 8  
     SIZE 134 BY 2.38
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60.2 BY 10.71.

DEFINE VARIABLE Sel_Ingredientes AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "0","0" 
     SIZE 38 BY 4.76
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Sel_Productos AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "0","0" 
     SIZE 46.6 BY 18.71
     BGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dlg_CreacionP
     Btn_Salir AT ROW 1.67 COL 117.2
     Fill_Search AT ROW 4.57 COL 17 COLON-ALIGNED
     Sel_Productos AT ROW 6.76 COL 11.2 NO-LABEL
     Fill_Descripcion AT ROW 7.81 COL 80.2 COLON-ALIGNED
     Fill_Precio AT ROW 9.1 COL 80.2 COLON-ALIGNED
     List_Clasificacion AT ROW 10.38 COL 80.2 COLON-ALIGNED
     Sel_Ingredientes AT ROW 11.71 COL 82.2 NO-LABEL
     Btn_Agregar AT ROW 19.33 COL 86.8
     Btn_Producto AT ROW 25.91 COL 11.4
     RECT-18 AT ROW 1 COL 1
     RECT-24 AT ROW 6.76 COL 65.4
     "Ingredientes:" VIEW-AS TEXT
          SIZE 12.4 BY .62 AT ROW 13.71 COL 69.6
     "AGREGAR PLATILLOS" VIEW-AS TEXT
          SIZE 23.6 BY .62 AT ROW 1.91 COL 63.8
          FGCOLOR 15 
     "Campos Requeridos:" VIEW-AS TEXT
          SIZE 40 BY .62 AT ROW 6.1 COL 65.6
     "Seleccionar Ingrediente:" VIEW-AS TEXT
          SIZE 40 BY .62 AT ROW 5.95 COL 11.2
     SPACE(83.80) SKIP(22.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Agregar".


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
/* SETTINGS FOR DIALOG-BOX Dlg_CreacionP
                                                                        */
ASSIGN 
       FRAME Dlg_CreacionP:SCROLLABLE       = FALSE
       FRAME Dlg_CreacionP:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dlg_CreacionP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dlg_CreacionP Dlg_CreacionP
ON WINDOW-CLOSE OF FRAME Dlg_CreacionP /* Agregar */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Agregar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Agregar Dlg_CreacionP
ON CHOOSE OF Btn_Agregar IN FRAME Dlg_CreacionP /* Agregar */
DO:    
    DEFINE VARIABLE vcharIng AS CHARACTER INITIAL "".

    ASSIGN Fill_Descripcion Fill_Precio List_Clasificacion Sel_Ingredientes.
    IF validatePlatillo(Fill_Descripcion, DECIMAL(Fill_Precio), INTEGER(List_Clasificacion), Sel_Ingredientes:NUM-ITEMS) THEN DO:
        RUN getIngredientes(OUTPUT vcharIng).     
        
        CASE pinIntAction:
            WHEN 1 THEN DO:
                RUN addPlatillo(
                    STRING(Fill_Descripcion), 
                    DECIMAL(Fill_Precio),
                    INTEGER(List_Clasificacion),
                    STRING(TRIM(vcharIng))).
                APPLY "WINDOW-CLOSE" TO FRAME Dlg_CreacionP.
            END.
            WHEN 2 THEN DO:
                RUN updatePlatillo(
                    pinRowId,
                    STRING(Fill_Descripcion), 
                    DECIMAL(Fill_Precio),
                    INTEGER(List_Clasificacion),
                    STRING(TRIM(vcharIng))).
                APPLY "WINDOW-CLOSE" TO FRAME Dlg_CreacionP.
            END.
        END CASE.
    END.
    ELSE DO:
        MESSAGE "REGISTRO INCOMPLETO" VIEW-AS ALERT-BOX.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Producto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Producto Dlg_CreacionP
ON CHOOSE OF Btn_Producto IN FRAME Dlg_CreacionP /* Nuevo Producto */
DO:
    RUN menuProductos.w.
    ASSIGN Sel_Productos:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = getCatProducto("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir Dlg_CreacionP
ON CHOOSE OF Btn_Salir IN FRAME Dlg_CreacionP /* Salir */
DO:
    APPLY "WINDOW-CLOSE" TO FRAME Dlg_CreacionP.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Fill_Search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fill_Search Dlg_CreacionP
ON VALUE-CHANGED OF Fill_Search IN FRAME Dlg_CreacionP /* Buscar */
DO:
    DEFINE VARIABLE vcharCatalogo AS CHARACTER.

    vcharCatalogo = getCatProducto(Fill_Search:SCREEN-VALUE).
    IF TRIM(vcharCatalogo) = "" THEN
        vcharCatalogo = ",".
    ASSIGN Sel_Productos:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = vcharCatalogo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Sel_Ingredientes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Sel_Ingredientes Dlg_CreacionP
ON DELETE-CHARACTER OF Sel_Ingredientes IN FRAME Dlg_CreacionP
DO:
    IF Sel_Ingredientes:SCREEN-VALUE <> "" AND 
       Sel_Ingredientes:SCREEN-VALUE <> "0" AND 
       Sel_Ingredientes:SCREEN-VALUE <> ? THEN
            Sel_Ingredientes:DELETE ( Sel_Ingredientes:SCREEN-VALUE ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Sel_Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Sel_Productos Dlg_CreacionP
ON MOUSE-SELECT-DBLCLICK OF Sel_Productos IN FRAME Dlg_CreacionP
DO:
    DEFINE VARIABLE vcharId AS CHARACTER.

    vcharId = Sel_Productos:SCREEN-VALUE IN FRAME Dlg_CreacionP.
    IF vcharId <> ? AND vcharId <> "" THEN
        RUN validarAgregado.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Sel_Productos Dlg_CreacionP
ON RETURN OF Sel_Productos IN FRAME Dlg_CreacionP
DO:
    RUN validarAgregado.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dlg_CreacionP 


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
  RUN setInitial.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addIngredienteList Dlg_CreacionP 
PROCEDURE addIngredienteList :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pinCharId AS CHARACTER.    
    DEFINE INPUT PARAMETER pinCharDesc AS CHARACTER.    
        
    Sel_Ingredientes:ADD-LAST(pinCharDesc, pinCharId) IN FRAME Dlg_CreacionP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dlg_CreacionP  _DEFAULT-DISABLE
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
  HIDE FRAME Dlg_CreacionP.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dlg_CreacionP  _DEFAULT-ENABLE
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
  DISPLAY Fill_Search Sel_Productos Fill_Descripcion Fill_Precio 
          List_Clasificacion Sel_Ingredientes 
      WITH FRAME Dlg_CreacionP.
  ENABLE Btn_Salir Fill_Search Sel_Productos Fill_Descripcion Fill_Precio 
         List_Clasificacion Sel_Ingredientes Btn_Agregar Btn_Producto RECT-18 
         RECT-24 
      WITH FRAME Dlg_CreacionP.
  VIEW FRAME Dlg_CreacionP.
  {&OPEN-BROWSERS-IN-QUERY-Dlg_CreacionP}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getIngredientes Dlg_CreacionP 
PROCEDURE getIngredientes :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER poutCharIngredientes AS CHARACTER.
    DEFINE VARIABLE vintCount AS INTEGER.
    DEFINE VARIABLE vcharLista AS CHARACTER.
    
    vcharLista = Sel_Ingredientes:LIST-ITEM-PAIRS IN FRAME Dlg_CreacionP.    
    vcharLista = TRIM(vcharLista, ",").
    DO vintCount = 1 TO NUM-ENTRIES(vcharLista):        
        IF isNumber(TRIM(ENTRY(vintCount, vcharLista))) = TRUE THEN DO:
            poutCharIngredientes = poutCharIngredientes + 
                TRIM(ENTRY(vintCount, vcharLista)) + "|".
        END.
        ELSE DO:
            IF LENGTH(TRIM(ENTRY(vintCount, vcharLista))) > 1 THEN DO:
                poutCharIngredientes = poutCharIngredientes +
                    TRIM(ENTRY(2, ENTRY(1, TRIM(ENTRY(vintCount, vcharLista)), "-"), "~/")) + ",".
            END.
        END.
    END.
    
    poutCharIngredientes = TRIM(poutCharIngredientes, ",").
    poutCharIngredientes = TRIM(poutCharIngredientes, "|").
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setInitial Dlg_CreacionP 
PROCEDURE setInitial :
/*------------------------------------------------------------------------------
        Purpose: 
        Parameters: <none> 
        Notes: 
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcharCatProductos AS CHARACTER.
    DEFINE VARIABLE vcharCatClasif AS CHARACTER.

    vcharCatProductos = getCatProducto("").
    ASSIGN Sel_Productos:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = vcharCatProductos.   
    
    ASSIGN Sel_Ingredientes:LIST-ITEM-PAIRS = ",".   
    
    vcharCatClasif = getCatClasificacion().
    ASSIGN List_Clasificacion:LIST-ITEM-PAIRS = vcharCatClasif.
        
    IF pinRowId <> ? THEN DO:
        FIND FIRST MENU WHERE ROWID(MENU) = pinRowId.
        ASSIGN 
            Fill_Descripcion:SCREEN-VALUE = MENU.DESCRIPCION
            Fill_Precio:SCREEN-VALUE = STRING(MENU.PRECIO)
            List_Clasificacion:SCREEN-VALUE = STRING(MENU.ID_CLASIFICACION).
        FOR EACH INGREDIENTE WHERE INGREDIENTE.ID_MENU = MENU.ID_MENU:
            FIND FIRST PRODUCTO WHERE PRODUCTO.ID_PRODUCTO = INGREDIENTE.ID_PRODUCTO.
            FIND FIRST UNIDAD_MEDIDA WHERE UNIDAD_MEDIDA.ID_UNIDAD = PRODUCTO.ID_UNIDAD.
            RUN addIngredienteList(PRODUCTO.ID_PRODUCTO, PRODUCTO.DESCRIPCION + "/" + STRING(INGREDIENTE.CANTIDAD) + "-" + UNIDAD_MEDIDA.DESCRIPCION).
        END.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validarAgregado Dlg_CreacionP 
PROCEDURE validarAgregado :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vintCantidad AS INTEGER.
    DEFINE VARIABLE vcharId AS CHARACTER.    
    DEFINE VARIABLE vcharDesc AS CHARACTER.
    DEFINE VARIABLE vcharUnidad AS CHARACTER.

    vcharId = Sel_Productos:SCREEN-VALUE IN FRAME Dlg_CreacionP.
    vcharDesc = getDescProducto(INTEGER(vcharId)).
    
    IF LOOKUP (vcharId, Sel_Ingredientes:LIST-ITEM-PAIRS) = 0 THEN DO:
        RUN agregarCantidad.w(INTEGER(vcharId), OUTPUT vintCantidad, OUTPUT vcharUnidad).
        IF vintCantidad <> 0 THEN
            RUN addIngredienteList(vcharId, vcharDesc + "/" + STRING(vintCantidad) + "-" + vcharUnidad).
        ELSE
            MESSAGE "LA CANTIDAD DEBE SER MAYOR A CERO" VIEW-AS ALERT-BOX.
    END.
    ELSE DO:
        MESSAGE "YA HA AGREGADO ESE INGREDIENTE" VIEW-AS ALERT-BOX.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION validatePlatillo Dlg_CreacionP 
FUNCTION validatePlatillo RETURNS LOGICAL
    ( vcharDesc AS CHARACTER, 
      vdecPrecio AS DECIMAL, 
      vintClas AS INTEGER, 
      vintIng AS INTEGER ) :
    /*------------------------------------------------------------------------------
        Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/        
        
    IF vintClas = 13 THEN DO:    
        IF TRIM(vcharDesc) = "" OR 
           vdecPrecio < 0 OR 
           vintClas = 0 OR 
           vintIng <= 1 THEN DO:
            RETURN FALSE.
        END.
    END.
    ELSE DO:
        IF TRIM(vcharDesc) = "" OR 
           vdecPrecio <= 0 OR 
           vintClas = 0 OR 
           vintIng <= 1 THEN DO:
            RETURN FALSE.
        END.
    END.

    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


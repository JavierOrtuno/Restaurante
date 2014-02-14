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
&Scoped-Define ENABLED-OBJECTS Sel_Productos Fill_Descripcion Fill_Precio ~
List_Clasificacion Sel_Ingredientes Btn_Agregar Btn_Salir 
&Scoped-Define DISPLAYED-OBJECTS Sel_Productos Fill_Descripcion Fill_Precio ~
List_Clasificacion Sel_Ingredientes 

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
     SIZE 20 BY 2.5.

DEFINE BUTTON Btn_Salir 
     LABEL "Cancelar" 
     SIZE 20 BY 2.5.

DEFINE VARIABLE List_Clasificacion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Clasificación" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "",""
     DROP-DOWN-LIST
     SIZE 35.4 BY 1 NO-UNDO.

DEFINE VARIABLE Fill_Descripcion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Platillo" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE Fill_Precio AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Precio" 
     VIEW-AS FILL-IN 
     SIZE 19.4 BY 1 NO-UNDO.

DEFINE VARIABLE Sel_Ingredientes AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "","" 
     SIZE 38 BY 4.76 NO-UNDO.

DEFINE VARIABLE Sel_Productos AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 42 BY 20.24 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dlg_CreacionP
     Sel_Productos AT ROW 2.67 COL 7 NO-LABEL
     Fill_Descripcion AT ROW 4.91 COL 63.6 COLON-ALIGNED
     Fill_Precio AT ROW 6.19 COL 63.6 COLON-ALIGNED
     List_Clasificacion AT ROW 7.48 COL 63.6 COLON-ALIGNED
     Sel_Ingredientes AT ROW 8.81 COL 65.6 NO-LABEL
     Btn_Agregar AT ROW 15.29 COL 72.4
     Btn_Salir AT ROW 18.38 COL 72.4
     "Ingredientes:" VIEW-AS TEXT
          SIZE 12.4 BY .62 AT ROW 10.81 COL 53
     "Seleccionar Ingrediente:" VIEW-AS TEXT
          SIZE 40 BY .62 AT ROW 1.86 COL 7
     SPACE(62.39) SKIP(21.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Agregar Platillos".


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
ON WINDOW-CLOSE OF FRAME Dlg_CreacionP /* Agregar Platillos */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Agregar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Agregar Dlg_CreacionP
ON CHOOSE OF Btn_Agregar IN FRAME Dlg_CreacionP /* Agregar */
DO:
    ASSIGN 
        Fill_Descripcion
        Fill_Precio
        List_Clasificacion
        Sel_Ingredientes.
    IF validatePlatillo(Fill_Descripcion, DECIMAL(Fill_Precio), INTEGER(List_Clasificacion), Sel_Ingredientes:NUM-ITEMS) THEN DO:
            
    END.
    ELSE DO:
        MESSAGE "REGISTRO INCOMPLETO" VIEW-AS ALERT-BOX.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir Dlg_CreacionP
ON CHOOSE OF Btn_Salir IN FRAME Dlg_CreacionP /* Cancelar */
DO:
    APPLY "WINDOW-CLOSE" TO FRAME Dlg_CreacionP.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Sel_Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Sel_Productos Dlg_CreacionP
ON MOUSE-SELECT-DBLCLICK OF Sel_Productos IN FRAME Dlg_CreacionP
DO:
    DEFINE VARIABLE vcharId AS CHARACTER.
    DEFINE VARIABLE vcharDesc AS CHARACTER.

    vcharId = Sel_Productos:SCREEN-VALUE.
    vcharDesc = getDescProducto(INTEGER(vcharId)).
    RUN addIngrediente(vcharId, vcharDesc).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addIngrediente Dlg_CreacionP 
PROCEDURE addIngrediente :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pinCharIdProducto AS CHARACTER.
    DEFINE INPUT PARAMETER pinCharDesc AS CHARACTER.
    
    Sel_Ingredientes:ADD-LAST(pinCharDesc, pinCharIdProducto) IN FRAME Dlg_CreacionP.
    
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
  DISPLAY Sel_Productos Fill_Descripcion Fill_Precio List_Clasificacion 
          Sel_Ingredientes 
      WITH FRAME Dlg_CreacionP.
  ENABLE Sel_Productos Fill_Descripcion Fill_Precio List_Clasificacion 
         Sel_Ingredientes Btn_Agregar Btn_Salir 
      WITH FRAME Dlg_CreacionP.
  VIEW FRAME Dlg_CreacionP.
  {&OPEN-BROWSERS-IN-QUERY-Dlg_CreacionP}
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

    vcharCatProductos = getCatProducto().
    ASSIGN Sel_Productos:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = vcharCatProductos.
    
    vcharCatClasif = getCatClasificacion().
    ASSIGN List_Clasificacion:LIST-ITEM-PAIRS = vcharCatClasif.
    
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
    IF TRIM(vcharDesc) = "" OR 
       vdecPrecio <= 0 OR 
       vintClas = 0 OR 
       vintIng <= 1 THEN DO:
        RETURN FALSE.
    END.

    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


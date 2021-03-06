&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dlg_UpdateProd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dlg_UpdateProd 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 
    I.S.C. Fco. Javier Ortu�o Colchado  
  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{productos.i}
/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER pinIntEvento AS INTEGER.
DEFINE INPUT PARAMETER pinRowId AS ROWID.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE vintIdProducto AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dlg_UpdateProd

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Cancelar Fill_Descripcion Fill_Cantidad ~
List_Unidad Btn_Aceptar RECT-1 RECT-22 
&Scoped-Define DISPLAYED-OBJECTS Fill_Codigo Fill_Descripcion Fill_Cantidad ~
List_Unidad 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD validarRegistroProd Dlg_UpdateProd 
FUNCTION validarRegistroProd RETURNS LOGICAL
    ( vcharDesc AS CHARACTER, vintCant AS CHARACTER, vintUnidad AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Aceptar 
     LABEL "Aceptar" 
     SIZE 20 BY 2.52.

DEFINE BUTTON Btn_Cancelar 
     LABEL "Salir" 
     SIZE 14 BY 1.19.

DEFINE VARIABLE List_Unidad AS CHARACTER FORMAT "X(256)":U 
     LABEL "Unidad" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Seleccionar"," 0"
     DROP-DOWN-LIST
     SIZE 21 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_Cantidad AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cant. M�nima" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_Codigo AS CHARACTER FORMAT "X(10)":U 
     LABEL "C�digo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_Descripcion AS CHARACTER FORMAT "X(100)":U 
     LABEL "Descripci�n" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 8  
     SIZE 90 BY 2.38
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 51 BY 9.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dlg_UpdateProd
     Btn_Cancelar AT ROW 1.62 COL 74.6
     Fill_Codigo AT ROW 6.52 COL 35 COLON-ALIGNED
     Fill_Descripcion AT ROW 8.81 COL 35 COLON-ALIGNED
     Fill_Cantidad AT ROW 11.24 COL 35 COLON-ALIGNED
     List_Unidad AT ROW 13.48 COL 35 COLON-ALIGNED
     Btn_Aceptar AT ROW 16.71 COL 36
     RECT-1 AT ROW 1 COL 1
     RECT-22 AT ROW 5.76 COL 20.6
     "ACTUALIZAR PRODUCTOS" VIEW-AS TEXT
          SIZE 28 BY 1.19 AT ROW 1.62 COL 32
          BGCOLOR 8 FGCOLOR 15 FONT 12
     "Campos Requeridos:" VIEW-AS TEXT
          SIZE 40 BY .62 AT ROW 5 COL 20.8
     SPACE(30.39) SKIP(14.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Actualizar".


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
/* SETTINGS FOR DIALOG-BOX Dlg_UpdateProd
                                                                        */
ASSIGN 
       FRAME Dlg_UpdateProd:SCROLLABLE       = FALSE
       FRAME Dlg_UpdateProd:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Fill_Codigo IN FRAME Dlg_UpdateProd
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dlg_UpdateProd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dlg_UpdateProd Dlg_UpdateProd
ON WINDOW-CLOSE OF FRAME Dlg_UpdateProd /* Actualizar */
DO:
    APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Aceptar Dlg_UpdateProd
ON CHOOSE OF Btn_Aceptar IN FRAME Dlg_UpdateProd /* Aceptar */
DO:    
    DEFINE VARIABLE vintUnidad AS INTEGER.
    DEFINE VARIABLE vcharDescripcion AS CHARACTER.
    DEFINE VARIABLE vintCantidad AS CHARACTER.

    vintCantidad = Fill_Cantidad:SCREEN-VALUE.
    vintUnidad = INTEGER(List_Unidad:SCREEN-VALUE).
    vcharDescripcion = Fill_Descripcion:SCREEN-VALUE.
        
    IF validarRegistroProd(vcharDescripcion, vintCantidad, vintUnidad) = FALSE THEN DO:
        MESSAGE "Todos los Campos son Requeridos" VIEW-AS ALERT-BOX.
        LEAVE.
    END.
    ASSIGN 
        Fill_Codigo Fill_Descripcion Fill_Cantidad List_Unidad.
    CASE pinIntEvento:
        WHEN 1 THEN DO:
            RUN addProducto(vintIdProducto, Fill_Codigo, Fill_Descripcion, INTEGER(Fill_Cantidad), INTEGER(List_Unidad)).
            APPLY "WINDOW-CLOSE" TO FRAME Dlg_UpdateProd.    
        END.
        WHEN 2 THEN DO:
            RUN updateProducto(pinRowId, Fill_Codigo, Fill_Descripcion, INTEGER(Fill_Cantidad), INTEGER(List_Unidad)).
            APPLY "WINDOW-CLOSE" TO FRAME Dlg_UpdateProd.    
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar Dlg_UpdateProd
ON CHOOSE OF Btn_Cancelar IN FRAME Dlg_UpdateProd /* Salir */
DO:
    APPLY "WINDOW-CLOSE" TO FRAME Dlg_UpdateProd.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dlg_UpdateProd 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asignarValores Dlg_UpdateProd 
PROCEDURE asignarValores :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    FIND FIRST PRODUCTO WHERE ROWID(PRODUCTO) = pinRowId.

    Fill_Codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = UPPER(PRODUCTO.CODIGO).
    Fill_Descripcion:SCREEN-VALUE = UPPER(PRODUCTO.DESCRIPCION).
    Fill_Cantidad:SCREEN-VALUE = STRING(INTEGER(PRODUCTO.CANT_MINIMA)).
    List_Unidad:SCREEN-VALUE = STRING(INTEGER(PRODUCTO.ID_UNIDAD)).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dlg_UpdateProd  _DEFAULT-DISABLE
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
  HIDE FRAME Dlg_UpdateProd.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dlg_UpdateProd  _DEFAULT-ENABLE
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
  DISPLAY Fill_Codigo Fill_Descripcion Fill_Cantidad List_Unidad 
      WITH FRAME Dlg_UpdateProd.
  ENABLE Btn_Cancelar Fill_Descripcion Fill_Cantidad List_Unidad Btn_Aceptar 
         RECT-1 RECT-22 
      WITH FRAME Dlg_UpdateProd.
  VIEW FRAME Dlg_UpdateProd.
  {&OPEN-BROWSERS-IN-QUERY-Dlg_UpdateProd}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setInitial Dlg_UpdateProd 
PROCEDURE setInitial :
/*------------------------------------------------------------------------------
        Purpose: Inicializaci�n del Dialogo de Registro de Productos    
        Parameters: <none>
        Author: I.S.C. Fco. Javier Ortu�o Colchado
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcharCatUnidad AS CHARACTER.

    vcharCatUnidad = getCatUnidad().        
    ASSIGN List_Unidad:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = vcharCatUnidad.
    CASE pinIntEvento:
        WHEN 1 THEN DO:
            vintIdProducto = NEXT-VALUE(SEC_PRODUCTO).
            Fill_Codigo:SCREEN-VALUE = getCodProducto(vintIdProducto).
        END.
        WHEN 2 THEN DO:
            RUN asignarValores.
        END.
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION validarRegistroProd Dlg_UpdateProd 
FUNCTION validarRegistroProd RETURNS LOGICAL
    ( vcharDesc AS CHARACTER, vintCant AS CHARACTER, vintUnidad AS INTEGER ) :
    /*------------------------------------------------------------------------------
        Purpose: Validar Registro de Producto  
        Notes: Retorna TRUE si la validaci�n es correcta
        Author: I.S.C. Fco. Javier Ortu�o Colchado
    ------------------------------------------------------------------------------*/
    IF LENGTH(TRIM(vcharDesc)) = 0 OR vintCant = "0" OR vintUnidad = 0 THEN
        RETURN FALSE.
    
    RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


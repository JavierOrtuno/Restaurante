&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          restaurante      PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dlg_MenuProd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dlg_MenuProd 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 
    I.S.C. Fco. Javier Ortuño Colchado
  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{productos.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dlg_MenuProd
&Scoped-define BROWSE-NAME Bws_Productos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PRODUCTO

/* Definitions for BROWSE Bws_Productos                                 */
&Scoped-define FIELDS-IN-QUERY-Bws_Productos PRODUCTO.CODIGO ~
PRODUCTO.DESCRIPCION PRODUCTO.CANT_MINIMA ~
getUnidadMedida(PRODUCTO.ID_UNIDAD) 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Bws_Productos 
&Scoped-define QUERY-STRING-Bws_Productos FOR EACH PRODUCTO NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Bws_Productos OPEN QUERY Bws_Productos FOR EACH PRODUCTO NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Bws_Productos PRODUCTO
&Scoped-define FIRST-TABLE-IN-QUERY-Bws_Productos PRODUCTO


/* Definitions for DIALOG-BOX Dlg_MenuProd                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dlg_MenuProd ~
    ~{&OPEN-QUERY-Bws_Productos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Salir Fill_Search Bws_Productos ~
Btn_Agregar RECT-18 RECT-21 
&Scoped-Define DISPLAYED-OBJECTS Fill_Search 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Agregar 
     LABEL "Nuevo" 
     SIZE 18 BY 2.14.

DEFINE BUTTON Btn_Salir 
     LABEL "Salir" 
     SIZE 13.6 BY 1.1.

DEFINE VARIABLE Fill_Search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Buscar" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 8  
     SIZE 164 BY 2.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 20 BY 2.86.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Bws_Productos FOR 
      PRODUCTO SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Bws_Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Bws_Productos Dlg_MenuProd _STRUCTURED
  QUERY Bws_Productos NO-LOCK DISPLAY
      PRODUCTO.CODIGO FORMAT "X(10)":U WIDTH 14.2
      PRODUCTO.DESCRIPCION FORMAT "X(150)":U WIDTH 58.2
      PRODUCTO.CANT_MINIMA COLUMN-LABEL "CANT.  MÍNIMA" FORMAT "->,>>>,>>9":U
            WIDTH 24.2
      getUnidadMedida(PRODUCTO.ID_UNIDAD) COLUMN-LABEL "UNIDAD DE MEDIDA" FORMAT "X(50)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-BOX NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS SIZE 126.4 BY 13.33
         BGCOLOR 15  EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dlg_MenuProd
     Btn_Salir AT ROW 1.57 COL 145.8
     Fill_Search AT ROW 4.1 COL 17 COLON-ALIGNED
     Bws_Productos AT ROW 5.81 COL 11
     Btn_Agregar AT ROW 9 COL 142.2
     RECT-18 AT ROW 1 COL 1
     RECT-21 AT ROW 8.62 COL 141.2
     "MENU PRODUCTOS" VIEW-AS TEXT
          SIZE 22.8 BY .62 AT ROW 1.71 COL 71
          FGCOLOR 15 
     SPACE(71.20) SKIP(18.33)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Menu".


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
/* SETTINGS FOR DIALOG-BOX Dlg_MenuProd
                                                                        */
/* BROWSE-TAB Bws_Productos Fill_Search Dlg_MenuProd */
ASSIGN 
       FRAME Dlg_MenuProd:SCROLLABLE       = FALSE
       FRAME Dlg_MenuProd:HIDDEN           = TRUE.

ASSIGN 
       Bws_Productos:ALLOW-COLUMN-SEARCHING IN FRAME Dlg_MenuProd = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Bws_Productos
/* Query rebuild information for BROWSE Bws_Productos
     _TblList          = "Restaurante.PRODUCTO"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Restaurante.PRODUCTO.CODIGO
"PRODUCTO.CODIGO" ? ? "character" ? ? ? ? ? ? no ? no no "14.2" yes no no "U" "" ""
     _FldNameList[2]   > Restaurante.PRODUCTO.DESCRIPCION
"PRODUCTO.DESCRIPCION" ? ? "character" ? ? ? ? ? ? no ? no no "58.2" yes no no "U" "" ""
     _FldNameList[3]   > Restaurante.PRODUCTO.CANT_MINIMA
"PRODUCTO.CANT_MINIMA" "CANT.  MÍNIMA" ? "integer" ? ? ? ? ? ? no ? no no "24.2" yes no no "U" "" ""
     _FldNameList[4]   > "_<CALC>"
"getUnidadMedida(PRODUCTO.ID_UNIDAD)" "UNIDAD DE MEDIDA" "X(50)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE Bws_Productos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dlg_MenuProd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dlg_MenuProd Dlg_MenuProd
ON WINDOW-CLOSE OF FRAME Dlg_MenuProd /* Menu */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Agregar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Agregar Dlg_MenuProd
ON CHOOSE OF Btn_Agregar IN FRAME Dlg_MenuProd /* Nuevo */
DO:
    RUN ActualizarProductos.w(1, ?).
    {&OPEN-QUERY-Bws_Productos}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir Dlg_MenuProd
ON CHOOSE OF Btn_Salir IN FRAME Dlg_MenuProd /* Salir */
DO:
    APPLY "WINDOW-CLOSE" TO FRAME Dlg_MenuProd.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Bws_Productos
&Scoped-define SELF-NAME Bws_Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bws_Productos Dlg_MenuProd
ON DELETE-CHARACTER OF Bws_Productos IN FRAME Dlg_MenuProd
DO:
    DEFINE VARIABLE vrowID AS ROWID.       
    DEFINE VARIABLE vlogAceptar AS LOGICAl.

    vrowID = ROWID(PRODUCTO).
          
    IF canDeleteProducto(vrowID) THEN DO:
        MESSAGE "¿REALMENTE DESEA ELIMINAR EL REGISTRO?" VIEW-AS ALERT-BOX BUTTONS YES-NO SET vlogAceptar.
        IF vlogAceptar = TRUE THEN DO:                    
            FIND FIRST PRODUCTO WHERE ROWID(PRODUCTO) = vrowID.
            DELETE PRODUCTO.
            Bws_Productos:DELETE-SELECTED-ROW(1).
        END.
    END.
    ELSE 
        MESSAGE "¡IMPOSIBLE ELIMINAR PLATILLO, EL REGISTRO YA TIENE RELACIONES!" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bws_Productos Dlg_MenuProd
ON MOUSE-SELECT-DBLCLICK OF Bws_Productos IN FRAME Dlg_MenuProd
DO:    
    RUN selectedItem.
    Bws_Productos:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bws_Productos Dlg_MenuProd
ON RETURN OF Bws_Productos IN FRAME Dlg_MenuProd
DO:
    RUN selectedItem.
    Bws_Productos:REFRESH().  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bws_Productos Dlg_MenuProd
ON START-SEARCH OF Bws_Productos IN FRAME Dlg_MenuProd
DO:
    DEFINE VARIAblE vcharOrden AS CHARACTER.
    DEFINE VARIABLE vhandQuery AS HANDLE.
    
    vhandQuery = (QUERY Bws_Productos:HANDLE).
    
    IF STRING(Bws_Productos:CURRENT-COLUMN:NAME) <> ? THEN
        vcharOrden = "FOR EACH PRODUCTO BY " + STRING(Bws_Productos:CURRENT-COLUMN:NAME).
    ELSE
        vcharOrden = "FOR EACH PRODUCTO BY " + "ID_UNIDAD".

    vhandQuery:QUERY-PREPARE(vcharOrden).
    vhandQuery:QUERY-OPEN().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Fill_Search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fill_Search Dlg_MenuProd
ON VALUE-CHANGED OF Fill_Search IN FRAME Dlg_MenuProd /* Buscar */
DO:
    DEFINE VARIABLE vcharConsulta AS CHARACTER.
    DEFINE VARIABLE vhandSearch AS HANDLE.

    vhandSearch = (QUERY Bws_Productos:HANDLE).
    vcharConsulta = "FOR EACH PRODUCTO WHERE PRODUCTO.DESCRIPCION BEGINS '" + TRIM(Fill_Search:SCREEN-VALUE) + "'".
    vhandSearch:QUERY-PREPARE(vcharConsulta).
    vhandSearch:QUERY-OPEN().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dlg_MenuProd 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dlg_MenuProd  _DEFAULT-DISABLE
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
  HIDE FRAME Dlg_MenuProd.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dlg_MenuProd  _DEFAULT-ENABLE
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
  DISPLAY Fill_Search 
      WITH FRAME Dlg_MenuProd.
  ENABLE Btn_Salir Fill_Search Bws_Productos Btn_Agregar RECT-18 RECT-21 
      WITH FRAME Dlg_MenuProd.
  VIEW FRAME Dlg_MenuProd.
  {&OPEN-BROWSERS-IN-QUERY-Dlg_MenuProd}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectedItem Dlg_MenuProd 
PROCEDURE selectedItem :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vrowID AS ROWID.

    vrowID = ROWID(PRODUCTO).
    RUN ActualizarProductos.w(2, vrowID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


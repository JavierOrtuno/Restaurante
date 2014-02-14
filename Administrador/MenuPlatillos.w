&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          restaurante      PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dlg_Platillos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dlg_Platillos 
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
{Productos.i}
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dlg_Platillos
&Scoped-define BROWSE-NAME Bws_Platillos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES MENU

/* Definitions for BROWSE Bws_Platillos                                 */
&Scoped-define FIELDS-IN-QUERY-Bws_Platillos MENU.CODIGO MENU.DESCRIPCION ~
MENU.PRECIO STRING(MENU.ID_CLASIFICACION) 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Bws_Platillos 
&Scoped-define QUERY-STRING-Bws_Platillos FOR EACH MENU NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Bws_Platillos OPEN QUERY Bws_Platillos FOR EACH MENU NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Bws_Platillos MENU
&Scoped-define FIRST-TABLE-IN-QUERY-Bws_Platillos MENU


/* Definitions for DIALOG-BOX Dlg_Platillos                             */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dlg_Platillos ~
    ~{&OPEN-QUERY-Bws_Platillos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Bws_Platillos Btn_Agregar Btn_Salir 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Agregar 
     LABEL "Agregar" 
     SIZE 20 BY 2.52.

DEFINE BUTTON Btn_Salir 
     LABEL "Salir" 
     SIZE 20 BY 2.52.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Bws_Platillos FOR 
      MENU SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Bws_Platillos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Bws_Platillos Dlg_Platillos _STRUCTURED
  QUERY Bws_Platillos NO-LOCK DISPLAY
      MENU.CODIGO FORMAT "X(10)":U WIDTH 11.2
      MENU.DESCRIPCION FORMAT "X(100)":U WIDTH 42.4
      MENU.PRECIO FORMAT "->>,>>9.99":U WIDTH 17.2
      STRING(MENU.ID_CLASIFICACION) COLUMN-LABEL "CLASIFICACIÓN" FORMAT "X(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 104 BY 16.43 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dlg_Platillos
     Bws_Platillos AT ROW 2.52 COL 7
     Btn_Agregar AT ROW 4.81 COL 117
     Btn_Salir AT ROW 8.62 COL 117
     SPACE(5.59) SKIP(9.71)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Platillos".


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
/* SETTINGS FOR DIALOG-BOX Dlg_Platillos
                                                                        */
/* BROWSE-TAB Bws_Platillos 1 Dlg_Platillos */
ASSIGN 
       FRAME Dlg_Platillos:SCROLLABLE       = FALSE
       FRAME Dlg_Platillos:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Bws_Platillos
/* Query rebuild information for BROWSE Bws_Platillos
     _TblList          = "Restaurante.MENU"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Restaurante.MENU.CODIGO
"MENU.CODIGO" ? ? "character" ? ? ? ? ? ? no ? no no "11.2" yes no no "U" "" ""
     _FldNameList[2]   > Restaurante.MENU.DESCRIPCION
"MENU.DESCRIPCION" ? ? "character" ? ? ? ? ? ? no ? no no "42.4" yes no no "U" "" ""
     _FldNameList[3]   > Restaurante.MENU.PRECIO
"MENU.PRECIO" ? ? "decimal" ? ? ? ? ? ? no ? no no "17.2" yes no no "U" "" ""
     _FldNameList[4]   > "_<CALC>"
"STRING(MENU.ID_CLASIFICACION)" "CLASIFICACIÓN" "X(20)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE Bws_Platillos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dlg_Platillos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dlg_Platillos Dlg_Platillos
ON WINDOW-CLOSE OF FRAME Dlg_Platillos /* Platillos */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Agregar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Agregar Dlg_Platillos
ON CHOOSE OF Btn_Agregar IN FRAME Dlg_Platillos /* Agregar */
DO:
    RUN ActualizarPlatillos.w(1, ?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir Dlg_Platillos
ON CHOOSE OF Btn_Salir IN FRAME Dlg_Platillos /* Salir */
DO:
    APPLY "WINDOW-CLOSE" TO FRAME Dlg_Platillos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Bws_Platillos
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dlg_Platillos 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dlg_Platillos  _DEFAULT-DISABLE
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
  HIDE FRAME Dlg_Platillos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dlg_Platillos  _DEFAULT-ENABLE
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
  ENABLE Bws_Platillos Btn_Agregar Btn_Salir 
      WITH FRAME Dlg_Platillos.
  VIEW FRAME Dlg_Platillos.
  {&OPEN-BROWSERS-IN-QUERY-Dlg_Platillos}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


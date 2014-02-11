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
&Scoped-define BROWSE-NAME Bws_Productos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PRODUCTO

/* Definitions for BROWSE Bws_Productos                                 */
&Scoped-define FIELDS-IN-QUERY-Bws_Productos PRODUCTO.CODIGO ~
PRODUCTO.DESCRIPCION PRODUCTO.CANT_MINIMA PRODUCTO.ID_UNIDAD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Bws_Productos 
&Scoped-define QUERY-STRING-Bws_Productos FOR EACH PRODUCTO NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Bws_Productos OPEN QUERY Bws_Productos FOR EACH PRODUCTO NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Bws_Productos PRODUCTO
&Scoped-define FIRST-TABLE-IN-QUERY-Bws_Productos PRODUCTO


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-Bws_Productos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Bws_Productos Btn_Agregar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Agregar 
     LABEL "Agregar" 
     SIZE 21 BY 2.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Bws_Productos FOR 
      PRODUCTO SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Bws_Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Bws_Productos Dialog-Frame _STRUCTURED
  QUERY Bws_Productos NO-LOCK DISPLAY
      PRODUCTO.CODIGO FORMAT "X(10)":U WIDTH 14.2
      PRODUCTO.DESCRIPCION FORMAT "X(150)":U WIDTH 61.2
      PRODUCTO.CANT_MINIMA COLUMN-LABEL "CANT.  MÍNIMA" FORMAT "->,>>>,>>9":U
            WIDTH 20.2
      PRODUCTO.ID_UNIDAD COLUMN-LABEL "UNIDAD MEDIDA" WIDTH 23.8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-SCROLLBAR-VERTICAL SIZE 126.4 BY 6.43 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Bws_Productos AT ROW 3.38 COL 18
     Btn_Agregar AT ROW 11 COL 26.2
     SPACE(115.79) SKIP(2.75)
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{librerias/productos.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
/* BROWSE-TAB Bws_Productos 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Bws_Productos
/* Query rebuild information for BROWSE Bws_Productos
     _TblList          = "Restaurante.PRODUCTO"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Restaurante.PRODUCTO.CODIGO
"CODIGO" ? ? "character" ? ? ? ? ? ? no ? no no "14.2" yes no no "U" "" ""
     _FldNameList[2]   > Restaurante.PRODUCTO.DESCRIPCION
"DESCRIPCION" ? ? "character" ? ? ? ? ? ? no ? no no "61.2" yes no no "U" "" ""
     _FldNameList[3]   > Restaurante.PRODUCTO.CANT_MINIMA
"CANT_MINIMA" "CANT.  MÍNIMA" ? "integer" ? ? ? ? ? ? no ? no no "20.2" yes no no "U" "" ""
     _FldNameList[4]   > "_<CALC>"
"PRODUCTO.ID_UNIDAD" "UNIDAD MEDIDA" ? ? ? ? ? ? ? ? no ? no no "23.8" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE Bws_Productos */
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


&Scoped-define BROWSE-NAME Bws_Productos
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
  ENABLE Bws_Productos Btn_Agregar 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


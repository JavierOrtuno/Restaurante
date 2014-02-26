&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Modificar-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Modificar-Frame 
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
DEF INPUT PARAM inrowReg AS ROWID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Modificar-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-26 FILL-IN-31 FILL-IN-32 BUTTON-27 ~
RECT-18 RECT-25 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 FILL-IN-2 FILL-IN-31 FILL-IN-32 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-26 
     LABEL "Salir" 
     SIZE 11 BY .95.

DEFINE BUTTON BUTTON-27 AUTO-GO 
     LABEL "Modificar" 
     SIZE 20 BY 1.91.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Codigo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Descripcion" 
     VIEW-AS FILL-IN 
     SIZE 24.4 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE FILL-IN-31 AS DATE FORMAT "99/99/99":U 
     LABEL "Fecha de Caducidad" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-32 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cantidad" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 8  
     SIZE 80 BY 2.1
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60.4 BY 9.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Modificar-Frame
     BUTTON-26 AT ROW 1.62 COL 67.8
     FILL-IN-1 AT ROW 5.76 COL 37.2 COLON-ALIGNED
     FILL-IN-2 AT ROW 7.91 COL 37.2 COLON-ALIGNED
     FILL-IN-31 AT ROW 10.1 COL 37.2 COLON-ALIGNED
     FILL-IN-32 AT ROW 12.38 COL 37.2 COLON-ALIGNED
     BUTTON-27 AT ROW 15.29 COL 31
     RECT-18 AT ROW 1 COL 1.2
     RECT-25 AT ROW 4.67 COL 11.2
     "MODIFICAR PRODUCTO" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 1.76 COL 28.8
          FGCOLOR 15 
     "Campos Requeridos:" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 3.86 COL 11
     SPACE(47.39) SKIP(13.61)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Modificar"
         DEFAULT-BUTTON BUTTON-27.


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
/* SETTINGS FOR DIALOG-BOX Modificar-Frame
                                                                        */
ASSIGN 
       FRAME Modificar-Frame:SCROLLABLE       = FALSE
       FRAME Modificar-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME Modificar-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-1:READ-ONLY IN FRAME Modificar-Frame        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-2 IN FRAME Modificar-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-2:READ-ONLY IN FRAME Modificar-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Modificar-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Modificar-Frame Modificar-Frame
ON WINDOW-CLOSE OF FRAME Modificar-Frame /* Modificar */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-26
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-26 Modificar-Frame
ON CHOOSE OF BUTTON-26 IN FRAME Modificar-Frame /* Salir */
DO:
  APPLY "window-close" TO FRAME Modificar-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-27
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-27 Modificar-Frame
ON CHOOSE OF BUTTON-27 IN FRAME Modificar-Frame /* Modificar */
DO:
    ROWID(stock).
    ASSIGN stock.f_caducidad = INPUT fill-in-31
              stock.cantidad = INPUT FILL-in-32.
    MESSAGE "Actualizado Correctamente" VIEW-AS ALERT-BOX.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Modificar-Frame 


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
  RUN Modificar_Stock(inrowReg).
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Modificar-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Modificar-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Modificar-Frame  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-1 FILL-IN-2 FILL-IN-31 FILL-IN-32 
      WITH FRAME Modificar-Frame.
  ENABLE BUTTON-26 FILL-IN-31 FILL-IN-32 BUTTON-27 RECT-18 RECT-25 
      WITH FRAME Modificar-Frame.
  VIEW FRAME Modificar-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Modificar-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Modificar_Stock Modificar-Frame 
PROCEDURE Modificar_Stock :
/*------------------------------------------------------------------------------
  Purpose: Mostrar stock para modificacion    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR vintregpro AS INT.
DEF INPUT PARAM inrowRec AS ROWID.

 FIND Stock WHERE ROWID(Stock) = inrowRec.
  FILL-in-31:SCREEN-VALUE IN FRAME Modificar-Frame = string(stock.f_caducidad).
  FILL-in-32:SCREEN-VALUE = string(stock.cantidad).
  vintregpro = stock.id_producto.
 FIND FIRST Producto WHERE producto.id_producto = vintregpro.
  FILL-in-1:SCREEN-VALUE = Producto.codigo.
  FILL-in-2:SCREEN-VALUE = Producto.descripcion.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


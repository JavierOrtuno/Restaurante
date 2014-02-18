&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Delete-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Delete-Frame 
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

DEF INPUT PARAM inrowReg  AS ROWID.
DEF INPUT PARAM inrowReg2 AS ROWID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Delete-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-31 BUTTON-31 FILL-IN-32 FILL-IN-33 ~
BUTTON-32 FILL-IN-34 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-31 FILL-IN-32 FILL-IN-33 ~
FILL-IN-34 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-31 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-32 
     LABEL "Salir" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FILL-IN-31 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Descripción" 
     VIEW-AS FILL-IN 
     SIZE 23.8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-32 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cantidad" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-33 AS DATE FORMAT "99/99/99":U INITIAL ? 
     LABEL "Caducidad" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-34 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cantidad a Eliminar" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Delete-Frame
     FILL-IN-31 AT ROW 1.95 COL 15.2 COLON-ALIGNED
     BUTTON-31 AT ROW 2.91 COL 51
     FILL-IN-32 AT ROW 3.86 COL 15.2 COLON-ALIGNED
     FILL-IN-33 AT ROW 5.76 COL 15.2 COLON-ALIGNED
     BUTTON-32 AT ROW 5.76 COL 51
     FILL-IN-34 AT ROW 8.62 COL 25 COLON-ALIGNED
     SPACE(32.99) SKIP(1.94)
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Delete-Frame
                                                                        */
ASSIGN 
       FRAME Delete-Frame:SCROLLABLE       = FALSE
       FRAME Delete-Frame:HIDDEN           = TRUE.

ASSIGN 
       FILL-IN-31:READ-ONLY IN FRAME Delete-Frame        = TRUE.

ASSIGN 
       FILL-IN-32:READ-ONLY IN FRAME Delete-Frame        = TRUE.

ASSIGN 
       FILL-IN-33:READ-ONLY IN FRAME Delete-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Delete-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Delete-Frame Delete-Frame
ON WINDOW-CLOSE OF FRAME Delete-Frame /* <insert dialog title> */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-31
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-31 Delete-Frame
ON CHOOSE OF BUTTON-31 IN FRAME Delete-Frame /* OK */
DO:
DEF VAR vintCantidadActual AS INT.
DEF VAR vintCantidadElimin AS INT.
DEF VAR vintCantidad       AS INT.

vintCantidadActual = INT(fill-in-32:SCREEN-VALUE).
vintCantidadElimin = INT(fill-in-34:SCREEN-VALUE).
  IF vintCantidadActual < vintCantidadElimin THEN DO:
      MESSAGE "La cantidad eliminada es mayor a la del stock" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
  END.
  ELSE DO:
   vintCantidad = vintCantidadActual - vintCantidadElimin.
   ROWID(stock).
   ASSIGN stock.cantidad = vintCantidad.
   MESSAGE "Eliminado Correctamente" VIEW-AS ALERT-BOX.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-32
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-32 Delete-Frame
ON CHOOSE OF BUTTON-32 IN FRAME Delete-Frame /* Salir */
DO:
  APPLY "window-close" TO FRAME Delete-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Delete-Frame 


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
  RUN Eliminar_Desperdicio(inrowReg,inrowReg2).
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Delete-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Delete-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Eliminar_Desperdicio Delete-Frame 
PROCEDURE Eliminar_Desperdicio :
/*------------------------------------------------------------------------------
  Purpose: Eliminar el sobrante del dia   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM inrowRec  AS ROWID.
DEF INPUT PARAM inrowRec2 AS ROWID.

 FIND Producto WHERE ROWID(producto) = inrowRec.
  FILL-in-31:SCREEN-VALUE IN FRAME Delete-Frame = Producto.descripcion.
 FIND stock WHERE ROWID(stock) = inrowRec2.
  fill-in-32:SCREEN-VALUE = string(stock.cantidad).
  fill-in-33:SCREEN-VALUE = string(stock.f_caducidad).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Delete-Frame  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-31 FILL-IN-32 FILL-IN-33 FILL-IN-34 
      WITH FRAME Delete-Frame.
  ENABLE FILL-IN-31 BUTTON-31 FILL-IN-32 FILL-IN-33 BUTTON-32 FILL-IN-34 
      WITH FRAME Delete-Frame.
  VIEW FRAME Delete-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Delete-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


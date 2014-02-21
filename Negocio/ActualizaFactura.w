&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Actualiza-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Actualiza-Frame 
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

DEF INPUT PARAMETER crowid AS ROWID.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Actualiza-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-14 FILL-IN-15 FILL-IN-17 FILL-IN-18 ~
FILL-IN-19 COMBO-BOX-2 COMBO-BOX-1 Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-14 FILL-IN-15 FILL-IN-17 ~
FILL-IN-18 FILL-IN-19 COMBO-BOX-2 COMBO-BOX-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE COMBO-BOX-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estatus" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Generada","1",
                     "Cancelada","2",
                     "Pagada","3"
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Forma de Pago" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Efectivo","1",
                     "Tarjeta","2",
                     "Vales","3"
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-14 AS CHARACTER FORMAT "X(8)":U 
     LABEL "Folio" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-15 AS CHARACTER FORMAT "X(8)":U 
     LABEL "Fecha" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-17 AS CHARACTER FORMAT "X(8)":U 
     LABEL "Subtotal" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-18 AS CHARACTER FORMAT "X(8)":U 
     LABEL "IVA" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-19 AS CHARACTER FORMAT "X(8)":U 
     LABEL "Total" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Actualiza-Frame
     FILL-IN-14 AT ROW 1.95 COL 23 COLON-ALIGNED
     FILL-IN-15 AT ROW 3.38 COL 23 COLON-ALIGNED
     FILL-IN-17 AT ROW 4.81 COL 23 COLON-ALIGNED
     FILL-IN-18 AT ROW 6.24 COL 23 COLON-ALIGNED
     FILL-IN-19 AT ROW 7.91 COL 23 COLON-ALIGNED
     COMBO-BOX-2 AT ROW 9.57 COL 23 COLON-ALIGNED
     COMBO-BOX-1 AT ROW 11 COL 23 COLON-ALIGNED
     Btn_OK AT ROW 13.14 COL 12
     Btn_Cancel AT ROW 13.14 COL 33
     SPACE(8.39) SKIP(0.95)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Actualiza Factura"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
/* SETTINGS FOR DIALOG-BOX Actualiza-Frame
                                                                        */
ASSIGN 
       FRAME Actualiza-Frame:SCROLLABLE       = FALSE
       FRAME Actualiza-Frame:HIDDEN           = TRUE.

ASSIGN 
       FILL-IN-14:READ-ONLY IN FRAME Actualiza-Frame        = TRUE.

ASSIGN 
       FILL-IN-15:READ-ONLY IN FRAME Actualiza-Frame        = TRUE.

ASSIGN 
       FILL-IN-17:READ-ONLY IN FRAME Actualiza-Frame        = TRUE.

ASSIGN 
       FILL-IN-18:READ-ONLY IN FRAME Actualiza-Frame        = TRUE.

ASSIGN 
       FILL-IN-19:READ-ONLY IN FRAME Actualiza-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Actualiza-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Actualiza-Frame Actualiza-Frame
ON WINDOW-CLOSE OF FRAME Actualiza-Frame /* Actualiza Factura */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Actualiza-Frame
ON CHOOSE OF Btn_OK IN FRAME Actualiza-Frame /* OK */
DO:
  FIND Factura WHERE ROWID(Factura) = crowid.
    ASSIGN 
        Factura.ID_Estatus = INT(COMBO-BOX-1:SCREEN-VALUE).
        Factura.ID_Pago = INT(COMBO-BOX-2:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Actualiza-Frame 


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
  RUN Desplegar.
  WAIT-FOR GO OF FRAME Actualiza-Frame.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Desplegar Actualiza-Frame 
PROCEDURE Desplegar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND Factura WHERE ROWID(Factura) = crowid.
  ASSIGN
           FILL-IN-14:SCREEN-VALUE IN FRAME Actualiza-Frame = Factura.Folio 
           FILL-IN-15:SCREEN-VALUE IN FRAME Actualiza-Frame = string(Factura.Fecha)   
           FILL-IN-17:SCREEN-VALUE IN FRAME Actualiza-Frame = STRING(Factura.Subtotal) 
           FILL-IN-18:SCREEN-VALUE IN FRAME Actualiza-Frame = STRING(Factura.IVA)  
           FILL-IN-19:SCREEN-VALUE IN FRAME Actualiza-Frame = STRING(Factura.TOTAL).   
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Actualiza-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Actualiza-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Actualiza-Frame  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-14 FILL-IN-15 FILL-IN-17 FILL-IN-18 FILL-IN-19 COMBO-BOX-2 
          COMBO-BOX-1 
      WITH FRAME Actualiza-Frame.
  ENABLE FILL-IN-14 FILL-IN-15 FILL-IN-17 FILL-IN-18 FILL-IN-19 COMBO-BOX-2 
         COMBO-BOX-1 Btn_OK Btn_Cancel 
      WITH FRAME Actualiza-Frame.
  VIEW FRAME Actualiza-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Actualiza-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


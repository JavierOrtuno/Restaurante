&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          layouts          PROGRESS
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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Call_Click

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame Call_Click.curp Call_Click.name ~
Call_Click.lastname1 Call_Click.lastname2 Call_Click.desstreet ~
Call_Click.cod_pol Call_Click.gender 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame Call_Click.curp ~
Call_Click.name Call_Click.lastname1 Call_Click.lastname2 ~
Call_Click.desstreet Call_Click.cod_pol Call_Click.gender 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame Call_Click
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame Call_Click
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH Call_Click SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH Call_Click SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame Call_Click
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame Call_Click


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Call_Click.curp Call_Click.name ~
Call_Click.lastname1 Call_Click.lastname2 Call_Click.desstreet ~
Call_Click.cod_pol Call_Click.gender 
&Scoped-define ENABLED-TABLES Call_Click
&Scoped-define FIRST-ENABLED-TABLE Call_Click
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel BUTTON-8 BUTTON-7 BUTTON-6 ~
BUTTON-5 BUTTON-2 BUTTON-1 
&Scoped-Define DISPLAYED-FIELDS Call_Click.curp Call_Click.name ~
Call_Click.lastname1 Call_Click.lastname2 Call_Click.desstreet ~
Call_Click.cod_pol Call_Click.gender 
&Scoped-define DISPLAYED-TABLES Call_Click
&Scoped-define FIRST-DISPLAYED-TABLE Call_Click


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

DEFINE BUTTON BUTTON-1 
     LABEL "siguiente" 
     SIZE 16 BY 1.67.

DEFINE BUTTON BUTTON-2 
     LABEL "Previo" 
     SIZE 16 BY 1.67.

DEFINE BUTTON BUTTON-5 
     LABEL "Primero" 
     SIZE 18 BY 1.67.

DEFINE BUTTON BUTTON-6 
     LABEL "Ultimo" 
     SIZE 17 BY 1.67.

DEFINE BUTTON BUTTON-7 
     LABEL "eliminar" 
     SIZE 15 BY 1.19.

DEFINE BUTTON BUTTON-8 
     LABEL "agregar" 
     SIZE 16 BY 1.19.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      Call_Click SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Call_Click.curp AT ROW 6.95 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     Btn_OK AT ROW 7.67 COL 82
     Call_Click.name AT ROW 8.62 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Btn_Cancel AT ROW 10.19 COL 82
     Call_Click.lastname1 AT ROW 10.29 COL 11 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Call_Click.lastname2 AT ROW 12.43 COL 11 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     BUTTON-8 AT ROW 12.43 COL 81
     BUTTON-7 AT ROW 14.33 COL 82
     Call_Click.desstreet AT ROW 14.57 COL 9 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     Call_Click.cod_pol AT ROW 16.24 COL 49 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     Call_Click.gender AT ROW 17.19 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     BUTTON-6 AT ROW 20.05 COL 74
     BUTTON-5 AT ROW 20.29 COL 9
     BUTTON-2 AT ROW 20.29 COL 33
     BUTTON-1 AT ROW 20.29 COL 54
     "ALTAS, BAJAS, CAMBIOS" VIEW-AS TEXT
          SIZE 36 BY 2.14 AT ROW 3.14 COL 35
          FONT 3
     SPACE(27.99) SKIP(20.57)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "ABC"
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
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "Layouts.Call_Click"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* ABC */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
  DEF VAR vlogk AS LOG.
  MESSAGE "desea salir" VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE vlogk.
  IF NOT vlogk THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   ASSIGN CALL_click.curp CALL_click.NAME CALL_click.lastname1 CALL_click.lastname2 CALL_click.desstreet CALL_click.cod_pol CALL_click.gender.
   IF curp:SCREEN-VALUE = "" THEN
   DO:
    MESSAGE "Minimo ingresar curp" VIEW-AS ALERT-BOX.
    RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 Dialog-Frame
ON CHOOSE OF BUTTON-1 IN FRAME Dialog-Frame /* siguiente */
DO:
  GET NEXT Dialog-Frame.
        IF AVAILABLE Call_Click THEN 
    DISPLAY Call_Click.curp Call_Click.name Call_Click.lastname1 
          Call_Click.lastname2 Call_Click.cod_pol Call_Click.desstreet 
          Call_Click.gender 
      WITH FRAME Dialog-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 Dialog-Frame
ON CHOOSE OF BUTTON-2 IN FRAME Dialog-Frame /* Previo */
DO:
    GET PREV Dialog-Frame.
        IF AVAILABLE Call_Click THEN 
    DISPLAY Call_Click.curp Call_Click.name Call_Click.lastname1 
          Call_Click.lastname2 Call_Click.cod_pol Call_Click.desstreet 
          Call_Click.gender 
      WITH FRAME Dialog-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 Dialog-Frame
ON CHOOSE OF BUTTON-5 IN FRAME Dialog-Frame /* Primero */
DO:
  GET FIRST Dialog-Frame.
        IF AVAILABLE Call_Click THEN 
    DISPLAY Call_Click.curp Call_Click.name Call_Click.lastname1 
          Call_Click.lastname2 Call_Click.cod_pol Call_Click.desstreet 
          Call_Click.gender 
      WITH FRAME Dialog-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 Dialog-Frame
ON CHOOSE OF BUTTON-6 IN FRAME Dialog-Frame /* Ultimo */
DO:
  GET LAST Dialog-Frame.
        IF AVAILABLE Call_Click THEN 
    DISPLAY Call_Click.curp Call_Click.name Call_Click.lastname1 
          Call_Click.lastname2 Call_Click.cod_pol Call_Click.desstreet 
          Call_Click.gender 
      WITH FRAME Dialog-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 Dialog-Frame
ON CHOOSE OF BUTTON-7 IN FRAME Dialog-Frame /* eliminar */
DO:
  DELETE CALL_click.
  GET NEXT Dialog-Frame.
        IF AVAILABLE Call_Click THEN 
    DISPLAY Call_Click.curp Call_Click.name Call_Click.lastname1 
          Call_Click.lastname2 Call_Click.cod_pol Call_Click.desstreet 
          Call_Click.gender 
      WITH FRAME Dialog-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 Dialog-Frame
ON CHOOSE OF BUTTON-8 IN FRAME Dialog-Frame /* agregar */
DO:
  CREATE CALL_click.
  DISPLAY Call_Click.curp Call_Click.name Call_Click.lastname1 
          Call_Click.lastname2 Call_Click.cod_pol Call_Click.desstreet 
          Call_Click.gender 
      WITH FRAME Dialog-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  IF AVAILABLE Call_Click THEN 
    DISPLAY Call_Click.curp Call_Click.name Call_Click.lastname1 
          Call_Click.lastname2 Call_Click.desstreet Call_Click.cod_pol 
          Call_Click.gender 
      WITH FRAME Dialog-Frame.
  ENABLE Call_Click.curp Btn_OK Call_Click.name Btn_Cancel Call_Click.lastname1 
         Call_Click.lastname2 BUTTON-8 BUTTON-7 Call_Click.desstreet 
         Call_Click.cod_pol Call_Click.gender BUTTON-6 BUTTON-5 BUTTON-2 
         BUTTON-1 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


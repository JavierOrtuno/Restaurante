&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
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
DEF VAR vchrTest AS CHAR.

DEF FRAME Dummy vchrTest.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK BUTTON-10 Btn_Cancel FILL-IN-4 ~
FILL-IN-3 FILL-IN-5 FILL-IN-6 FILL-IN-8 RECT-2 RECT-3 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-4 FILL-IN-3 FILL-IN-5 FILL-IN-6 ~
FILL-IN-8 

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

DEFINE BUTTON Btn_OK 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-10 
     LABEL "Archivo" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FILL-IN-3 AS DECIMAL FORMAT "->>,>>9.9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-4 AS DECIMAL FORMAT "->>,>>9.9":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS DECIMAL FORMAT "->>,>>9.9":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-6 AS DECIMAL FORMAT "->>,>>9.9":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 20 BY 2.62.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 20 BY 2.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 1.52 COL 49
     BUTTON-10 AT ROW 1.95 COL 11
     Btn_Cancel AT ROW 2.76 COL 49
     FILL-IN-4 AT ROW 6.71 COL 21 COLON-ALIGNED
     FILL-IN-3 AT ROW 6.76 COL 11 COLON-ALIGNED NO-LABEL
     FILL-IN-5 AT ROW 6.81 COL 37 COLON-ALIGNED NO-LABEL
     FILL-IN-6 AT ROW 6.81 COL 47 COLON-ALIGNED
     FILL-IN-8 AT ROW 10.86 COL 24 COLON-ALIGNED NO-TAB-STOP 
     RECT-2 AT ROW 5.91 COL 11
     RECT-3 AT ROW 5.95 COL 37
     "," VIEW-AS TEXT
          SIZE 2.8 BY 1.91 AT ROW 6.29 COL 21.8 RIGHT-ALIGNED
          FONT 8
     "Punto 1" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 4.81 COL 17
     "," VIEW-AS TEXT
          SIZE 2.8 BY 1.91 AT ROW 6.33 COL 47.8 RIGHT-ALIGNED
          FONT 8
     "Punto 2" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 4.86 COL 43.4
     "Resultado" VIEW-AS TEXT
          SIZE 12.4 BY 1.43 AT ROW 9.19 COL 27.6
     SPACE(25.19) SKIP(2.04)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Distancia"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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

{c:/users/userpc/downloads/mathlibrary.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       FILL-IN-8:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR TEXT-LITERAL ","
          SIZE 2.8 BY 1.91 AT ROW 6.29 COL 21.8 RIGHT-ALIGNED           */

/* SETTINGS FOR TEXT-LITERAL ","
          SIZE 2.8 BY 1.91 AT ROW 6.33 COL 47.8 RIGHT-ALIGNED           */

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Distancia */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR vdecResult AS DEC.
  DEF VAR vdecx1 AS DEC.
  DEF VAR vdecy1 AS DEC.
  DEF VAR vdecx2 AS DEC.
  DEF VAR vdecy2 AS DEC.
  
  vdecx1 = DEC(FILL-IN-3:SCREEN-VALUE).
  vdecy1 = DEC(FILL-IN-4:SCREEN-VALUE).
  vdecx2 = DEC(FILL-IN-5:SCREEN-VALUE).
  vdecy2 = DEC(FILL-IN-6:SCREEN-VALUE).
  vdecResult = getDistance(vdecx1,vdecy1,vdecx2,vdecy2).

  FILL-IN-8:SCREEN-VALUE = STRING(vdecResult).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-10 Dialog-Frame
ON CHOOSE OF BUTTON-10 IN FRAME Dialog-Frame /* Archivo */
DO:
   DEF VAR vdecprueba AS DEC EXTENT 4.
   OS-COMMAND SILENT (notepad.exe "C:\Curso_Progress\Datos\Prueba.txt" ).
   
   INPUT FROM VALUE("C:\Curso_Progress\Datos\Prueba.txt").
   
   REPEAT:
    IMPORT DELIMITER "," vdecprueba.
  END.
  FILL-IN-3:SCREEN-VALUE = STRING (vdecprueba[1]).
  FILL-IN-4:SCREEN-VALUE = STRING (vdecprueba[2]).
  FILL-IN-5:SCREEN-VALUE = STRING (vdecprueba[3]).
  FILL-IN-6:SCREEN-VALUE = STRING (vdecprueba[4]).
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
  DISPLAY FILL-IN-4 FILL-IN-3 FILL-IN-5 FILL-IN-6 FILL-IN-8 
      WITH FRAME Dialog-Frame.
  ENABLE Btn_OK BUTTON-10 Btn_Cancel FILL-IN-4 FILL-IN-3 FILL-IN-5 FILL-IN-6 
         FILL-IN-8 RECT-2 RECT-3 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


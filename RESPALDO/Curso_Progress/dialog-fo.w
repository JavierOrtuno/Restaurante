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
DEF OUTPUT PARAM vchrforeach AS CHAR.
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS COMBO-BOX-1 FILL-IN-26 COMBO-BOX-2 ~
FILL-IN-27 COMBO-BOX-3 FILL-IN-28 COMBO-BOX-4 FILL-IN-29 COMBO-BOX-5 ~
FILL-IN-30 Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-1 FILL-IN-26 COMBO-BOX-2 ~
FILL-IN-27 COMBO-BOX-3 FILL-IN-28 COMBO-BOX-4 FILL-IN-29 COMBO-BOX-5 ~
FILL-IN-30 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Add-List Dialog-Frame 
FUNCTION Add-List RETURNS CHARACTER
  (Lst as char,   /* Mdo => 1 = Prefijo con repeticion */
  Txt as char,   /*        2 = Sufijo  con repeticion */
  Sep as char,   /*        3 = Prefijo sin repeticion */
  Mdo as int) FORWARD.

/* _UIB-CODE-BLOCK-END */
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
     LABEL "CURP" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEM-PAIRS "Igual a","=",
                     "Menor que","<",
                     "Mayor que",">",
                     "Diferente de","<>",
                     "Comienza","Begin",
                     "Contiene","Matches"
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nombre" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEM-PAIRS "Igual a","=",
                     "Menor que","<",
                     "Mayor que",">",
                     "Diferente de","<>",
                     "Comienza","Begin",
                     "Contiene","Matches"
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Apellido Paterno" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEM-PAIRS "Igual a","=",
                     "Menor que","<",
                     "Mayor que",">",
                     "Diferente de","<>",
                     "Comienza","Begin",
                     "Contiene","Matches"
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-4 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Apellido Materno" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEM-PAIRS "Igual a","=",
                     "Menor que","<",
                     "Mayor que",">",
                     "Diferente de","<>",
                     "Comienza","Begin",
                     "Contiene","Matches"
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-5 AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Telefono" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEM-PAIRS "Igual a","=",
                     "Menor que","<",
                     "Mayor que",">",
                     "Diferente de","<>",
                     "Comienza","Begin",
                     "Contiene","Matches"
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-26 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 26" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-27 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 27" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-28 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 28" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-29 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 29" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-30 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Fill 30" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     COMBO-BOX-1 AT ROW 1.95 COL 16 COLON-ALIGNED
     FILL-IN-26 AT ROW 1.95 COL 59 COLON-ALIGNED
     COMBO-BOX-2 AT ROW 3.86 COL 16 COLON-ALIGNED
     FILL-IN-27 AT ROW 3.86 COL 59 COLON-ALIGNED
     COMBO-BOX-3 AT ROW 5.76 COL 16 COLON-ALIGNED
     FILL-IN-28 AT ROW 5.76 COL 59 COLON-ALIGNED
     COMBO-BOX-4 AT ROW 7.67 COL 16 COLON-ALIGNED
     FILL-IN-29 AT ROW 7.67 COL 59 COLON-ALIGNED
     COMBO-BOX-5 AT ROW 9.57 COL 16 COLON-ALIGNED
     FILL-IN-30 AT ROW 9.57 COL 59 COLON-ALIGNED
     Btn_OK AT ROW 14.33 COL 21
     Btn_Cancel AT ROW 14.33 COL 51
     SPACE(24.39) SKIP(0.85)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>"
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    DEF VAR Lista AS CHAR.
    DEF VAR Lista1 AS CHAR.
    DEF VAR Lista2 AS CHAR.
    DEF VAR Lista3 AS CHAR.
    DEF VAR Lista4 AS CHAR.
    DEF VAR Lista5 AS CHAR.
    ASSIGN COMBO-BOX-1.
    ASSIGN COMBO-BOX-2.
    ASSIGN COMBO-BOX-3.
    ASSIGN COMBO-BOX-4.
    ASSIGN COMBO-BOX-5.
    ASSIGN fill-in-30.

    IF FILL-IN-26:SCREEN-VALUE <> "" THEN DO:
        Lista1 = "curp" + COMBO-BOX-1 + "'" + FILL-IN-26:SCREEN-VALUE + "'".
        Lista = Add-List(Lista,Lista1,' and ',2).
    END.
    IF FILL-IN-27:SCREEN-VALUE <> "" THEN DO:
        Lista2 = "name" + COMBO-BOX-2 + "'" + FILL-IN-27:SCREEN-VALUE + "'".
        Lista = ADD-list(Lista,Lista2,' and ',2).
    END.
    IF FILL-IN-28:SCREEN-VALUE <> "" THEN DO:
        Lista3 = "lastname1" + COMBO-BOX-3 + "'" + FILL-IN-28:SCREEN-VALUE + "'".
        Lista = ADD-list(Lista,Lista3,' and ',2).
    END.
    IF FILL-IN-29:SCREEN-VALUE <> "" THEN DO:
        Lista4  = "lastname2" + COMBO-BOX-4 + "'" + FILL-IN-29:SCREEN-VALUE + "'".
        Lista = ADD-list(Lista,Lista4,' and ',2).
    END.
    IF fill-in-30 <> 0 THEN DO:
        Lista5 = "telephone" + COMBO-BOX-5 + FILL-IN-30:SCREEN-VALUE.
        Lista = ADD-list(Lista,Lista5,' and ',2).
    END.
    
    vchrforeach = "for each call_click where " + Lista .
    MESSAGE vchrforeach VIEW-AS ALERT-BOX.
    RETURN vchrforeach.
   
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
  DISPLAY COMBO-BOX-1 FILL-IN-26 COMBO-BOX-2 FILL-IN-27 COMBO-BOX-3 FILL-IN-28 
          COMBO-BOX-4 FILL-IN-29 COMBO-BOX-5 FILL-IN-30 
      WITH FRAME Dialog-Frame.
  ENABLE COMBO-BOX-1 FILL-IN-26 COMBO-BOX-2 FILL-IN-27 COMBO-BOX-3 FILL-IN-28 
         COMBO-BOX-4 FILL-IN-29 COMBO-BOX-5 FILL-IN-30 Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Add-List Dialog-Frame 
FUNCTION Add-List RETURNS CHARACTER
  (Lst as char,   /* Mdo => 1 = Prefijo con repeticion */
  Txt as char,   /*        2 = Sufijo  con repeticion */
  Sep as char,   /*        3 = Prefijo sin repeticion */
  Mdo as int):   /*        4 = Sufijo  sin repeticion */ 

 Def var PreF as log.
 Def var Step  as int.

 Assign PreF = (Mdo = 1 or Mdo = 3).

 Do Step = 1 to 5:
  Case Step:
   when 1 then if Mdo le 2                 then Step = 3.
   when 2 then if Txt = ""                 then Step = 3.
   when 3 then if Lookup(Txt,Lst,Sep) ne 0 then Step = 5.
   when 4 then if Lst = ""                 then Sep = "".
   when 5 then Case PreF:
                when yes then Lst = Txt + Sep + Lst.
                when no  then Lst = Lst + Sep + Txt.
               end.
  end.
 end.
 
 Return Lst.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


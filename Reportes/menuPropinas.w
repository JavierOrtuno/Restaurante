&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dlg_Propinas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dlg_Propinas 
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
DEFINE OUTPUT PARAMETER poutCharFechas AS CHARACTER INITIAL ?.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dlg_Propinas

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Fill_Desde Fill_Hasta Btn_Aceptar RECT-18 ~
RECT-29 
&Scoped-Define DISPLAYED-OBJECTS Fill_Desde Fill_Hasta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Aceptar 
     LABEL "Aceptar" 
     SIZE 20 BY 2.52.

DEFINE VARIABLE Fill_Desde AS DATE FORMAT "99/99/99":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fill_Hasta AS DATE FORMAT "99/99/99":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 8  
     SIZE 60.4 BY 2.19
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 41 BY 4.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dlg_Propinas
     Fill_Desde AT ROW 5.76 COL 25 COLON-ALIGNED
     Fill_Hasta AT ROW 7.67 COL 25 COLON-ALIGNED
     Btn_Aceptar AT ROW 10.81 COL 21
     RECT-18 AT ROW 1 COL 1
     RECT-29 AT ROW 4.81 COL 11
     "PROPINAS" VIEW-AS TEXT
          SIZE 19.8 BY 1.19 AT ROW 1.52 COL 25.6
          BGCOLOR 8 FGCOLOR 15 FONT 68
     "Campos Requeridos:" VIEW-AS TEXT
          SIZE 40 BY .62 AT ROW 4.1 COL 11.2
     SPACE(10.19) SKIP(9.70)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Propinas".


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
/* SETTINGS FOR DIALOG-BOX Dlg_Propinas
                                                                        */
ASSIGN 
       FRAME Dlg_Propinas:SCROLLABLE       = FALSE
       FRAME Dlg_Propinas:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dlg_Propinas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dlg_Propinas Dlg_Propinas
ON WINDOW-CLOSE OF FRAME Dlg_Propinas /* Propinas */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Aceptar Dlg_Propinas
ON CHOOSE OF Btn_Aceptar IN FRAME Dlg_Propinas /* Aceptar */
DO:
    DEFINE VARIABLE vcharDesde AS CHARACTER.
    DEFINE VARIABLE vcharHasta AS CHARACTER.

    ASSIGN
        Fill_Desde
        Fill_Hasta.    

    vcharDesde = STRING(Fill_Desde).
    vcharHasta = STRING(Fill_Hasta).

    IF vcharDesde <> ? AND vcharHasta <> ? AND Fill_Desde > Fill_Hasta THEN DO:
        MESSAGE "La Fecha Hasta no Puede ser Mayor a la Fecha Desde" VIEW-AS ALERT-BOX.
    END.
    ELSE DO:                
        IF vcharDesde = ? THEN
            vcharDesde = "".
        IF vcharHasta = ? THEN
            vcharHasta = "".

        poutCharFechas = vcharDesde + "#" + vcharHasta.        
        APPLY "WINDOW-CLOSE" TO FRAME Dlg_Propinas.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dlg_Propinas 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dlg_Propinas  _DEFAULT-DISABLE
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
  HIDE FRAME Dlg_Propinas.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dlg_Propinas  _DEFAULT-ENABLE
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
  DISPLAY Fill_Desde Fill_Hasta 
      WITH FRAME Dlg_Propinas.
  ENABLE Fill_Desde Fill_Hasta Btn_Aceptar RECT-18 RECT-29 
      WITH FRAME Dlg_Propinas.
  VIEW FRAME Dlg_Propinas.
  {&OPEN-BROWSERS-IN-QUERY-Dlg_Propinas}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


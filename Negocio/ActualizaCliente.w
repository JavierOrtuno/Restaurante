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
&Scoped-Define ENABLED-OBJECTS Btn_Cancel FILL-IN-21 FILL-IN-22 FILL-IN-23 ~
FILL-IN-24 FILL-IN-25 FILL-IN-26 Btn_OK RECT-18 RECT-25 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-21 FILL-IN-22 FILL-IN-23 ~
FILL-IN-24 FILL-IN-25 FILL-IN-26 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Actualizar Actualiza-Frame 
FUNCTION Actualizar RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Salir" 
     SIZE 13 BY .95
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Actualizar" 
     SIZE 20 BY 1.91
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-21 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-22 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Apellido Psterno" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-23 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Apellido Materno" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-24 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Domicilio" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-25 AS DATE FORMAT "99/99/99":U 
     LABEL "Fecha Nacimiento" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-26 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Correo Electrónico" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 8  
     SIZE 80 BY 2.1
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70 BY 9.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Actualiza-Frame
     Btn_Cancel AT ROW 1.62 COL 65.8
     FILL-IN-21 AT ROW 5.43 COL 28.2 COLON-ALIGNED
     FILL-IN-22 AT ROW 6.86 COL 28.2 COLON-ALIGNED
     FILL-IN-23 AT ROW 8.29 COL 28.2 COLON-ALIGNED
     FILL-IN-24 AT ROW 9.71 COL 28.2 COLON-ALIGNED
     FILL-IN-25 AT ROW 11.14 COL 28.2 COLON-ALIGNED
     FILL-IN-26 AT ROW 12.57 COL 28.2 COLON-ALIGNED
     Btn_OK AT ROW 15.29 COL 31
     RECT-18 AT ROW 1 COL 1
     RECT-25 AT ROW 4.67 COL 6
     "ACTUALIZAR CLIENTE" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 1.76 COL 28.8
          FGCOLOR 15 
     "Campos Requeridos:" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 3.86 COL 6.4
     SPACE(51.59) SKIP(13.70)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Actualizar"
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Actualiza-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Actualiza-Frame Actualiza-Frame
ON WINDOW-CLOSE OF FRAME Actualiza-Frame /* Actualizar */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Actualiza-Frame
ON CHOOSE OF Btn_OK IN FRAME Actualiza-Frame /* Actualizar */
DO:
    Actualizar().
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
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
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
  FIND Persona WHERE ROWID(Persona) = crowid.
  ASSIGN
           FILL-IN-21:SCREEN-VALUE IN FRAME Actualiza-Frame = Persona.Nombres 
           FILL-IN-22:SCREEN-VALUE  = Persona.A_Paterno   
           FILL-IN-23:SCREEN-VALUE  = Persona.A_Materno
           FILL-IN-24:SCREEN-VALUE  = Persona.Domicilio
           FILL-IN-25:SCREEN-VALUE  = STRING(Persona.F_Nacimiento)   
           FILL-IN-26:SCREEN-VALUE  = Persona.Correo.   
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
  DISPLAY FILL-IN-21 FILL-IN-22 FILL-IN-23 FILL-IN-24 FILL-IN-25 FILL-IN-26 
      WITH FRAME Actualiza-Frame.
  ENABLE Btn_Cancel FILL-IN-21 FILL-IN-22 FILL-IN-23 FILL-IN-24 FILL-IN-25 
         FILL-IN-26 Btn_OK RECT-18 RECT-25 
      WITH FRAME Actualiza-Frame.
  VIEW FRAME Actualiza-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Actualiza-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Actualizar Actualiza-Frame 
FUNCTION Actualizar RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    FIND Persona WHERE ROWID(Persona) = crowid.
    ASSIGN 
           Persona.Nombre = FILL-IN-21:SCREEN-VALUE IN FRAME Actualiza-Frame  
           Persona.A_Paterno = FILL-IN-22:SCREEN-VALUE
           Persona.A_Materno = FILL-IN-23:SCREEN-VALUE
           Persona.Domicilio = FILL-IN-24:SCREEN-VALUE
           Persona.F_Nacimiento = DATE(FILL-IN-25:SCREEN-VALUE)  
           Persona.Correo = FILL-IN-26:SCREEN-VALUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


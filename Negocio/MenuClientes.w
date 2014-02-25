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
DEF VAR vlogOK AS LOG.
DEF VAR crowid AS ROWID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PERSONA CLIENTE

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 PERSONA.NOMBRES PERSONA.A_PATERNO ~
PERSONA.A_MATERNO PERSONA.DOMICILIO PERSONA.F_NACIMIENTO PERSONA.CORREO 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH PERSONA SHARE-LOCK, ~
      EACH CLIENTE WHERE CLIENTE.ID_PERSONA = PERSONA.ID_PERSONA SHARE-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH PERSONA SHARE-LOCK, ~
      EACH CLIENTE WHERE CLIENTE.ID_PERSONA = PERSONA.ID_PERSONA SHARE-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 PERSONA CLIENTE
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 PERSONA
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-2 CLIENTE


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-4 BROWSE-2 BUTTON-1 BUTTON-3 BUTTON-2 ~
RECT-18 RECT-28 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Agregar" 
     SIZE 16 BY 1.86.

DEFINE BUTTON BUTTON-2 
     LABEL "Eliminar" 
     SIZE 16 BY 1.91.

DEFINE BUTTON BUTTON-3 
     LABEL "Actualizar" 
     SIZE 16 BY 1.86.

DEFINE BUTTON BUTTON-4 
     LABEL "Salir" 
     SIZE 15 BY .95.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 8  
     SIZE 220 BY 2.1
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 20 BY 7.86.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      PERSONA, 
      CLIENTE SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 Dialog-Frame _STRUCTURED
  QUERY BROWSE-2 SHARE-LOCK NO-WAIT DISPLAY
      PERSONA.NOMBRES FORMAT "X(50)":U
      PERSONA.A_PATERNO FORMAT "X(50)":U
      PERSONA.A_MATERNO FORMAT "X(50)":U
      PERSONA.DOMICILIO FORMAT "X(50)":U
      PERSONA.F_NACIMIENTO FORMAT "99/99/99":U
      PERSONA.CORREO FORMAT "X(50)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 183.2 BY 10.81
         BGCOLOR 15  ROW-HEIGHT-CHARS .62 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BUTTON-4 AT ROW 1.62 COL 203.8
     BROWSE-2 AT ROW 4.24 COL 10.8
     BUTTON-1 AT ROW 5.14 COL 200.4
     BUTTON-3 AT ROW 7.57 COL 200.4
     BUTTON-2 AT ROW 10 COL 200.6
     RECT-18 AT ROW 1 COL 1
     RECT-28 AT ROW 4.57 COL 198.2
     "MENU CLIENTES PREFERENTES" VIEW-AS TEXT
          SIZE 35 BY .62 AT ROW 1.76 COL 104
          FGCOLOR 15 
     SPACE(82.00) SKIP(13.56)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Menu".


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
/* BROWSE-TAB BROWSE-2 BUTTON-4 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "Restaurante.PERSONA,Restaurante.CLIENTE WHERE Restaurante.PERSONA ..."
     _Options          = "SHARE-LOCK INDEXED-REPOSITION"
     _JoinCode[2]      = "CLIENTE.ID_PERSONA = PERSONA.ID_PERSONA"
     _FldNameList[1]   = Restaurante.PERSONA.NOMBRES
     _FldNameList[2]   = Restaurante.PERSONA.A_PATERNO
     _FldNameList[3]   = Restaurante.PERSONA.A_MATERNO
     _FldNameList[4]   = Restaurante.PERSONA.DOMICILIO
     _FldNameList[5]   = Restaurante.PERSONA.F_NACIMIENTO
     _FldNameList[6]   = Restaurante.PERSONA.CORREO
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Menu */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 Dialog-Frame
ON CHOOSE OF BUTTON-1 IN FRAME Dialog-Frame /* Agregar */
DO:

  RUN InsertarClientes.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 Dialog-Frame
ON CHOOSE OF BUTTON-2 IN FRAME Dialog-Frame /* Eliminar */
DO:
    FIND CURRENT Persona.
    IF Cliente.ID_Persona = Persona.ID_Persona
    THEN DO TRANSACTION:
            DELETE Cliente.
            DELETE Persona.
         END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 Dialog-Frame
ON CHOOSE OF BUTTON-3 IN FRAME Dialog-Frame /* Actualizar */
DO:
  FIND CURRENT Persona.
  crowid = ROWID(Persona).
  RUN ActualizaCliente.w(crowid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 Dialog-Frame
ON CHOOSE OF BUTTON-4 IN FRAME Dialog-Frame /* Salir */
DO:
  MESSAGE "¿Desea Salir?" VIEW-AS ALERT-BOX BUTTONS YES-NO
  UPDATE vlogOK.
  IF vlogOK THEN APPLY "window-close" TO CURRENT-WINDOW.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
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
  ENABLE BUTTON-4 BROWSE-2 BUTTON-1 BUTTON-3 BUTTON-2 RECT-18 RECT-28 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


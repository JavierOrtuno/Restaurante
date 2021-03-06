&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Insertar-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Insertar-Frame 
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

DEF VAR vID AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Insertar-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Cancel Name LastName1 LastName2 Address ~
Birthdate e-mail BUTTON-28 RECT-18 RECT-25 
&Scoped-Define DISPLAYED-OBJECTS Name LastName1 LastName2 Address Birthdate ~
e-mail 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Insertar Insertar-Frame 
FUNCTION Insertar RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Limpiar Insertar-Frame 
FUNCTION Limpiar RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Salir" 
     SIZE 15 BY .95
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-28 
     LABEL "Agregar" 
     SIZE 20 BY 1.91.

DEFINE VARIABLE Address AS CHARACTER FORMAT "X(70)":U 
     LABEL "Domicilio" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1.19
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Birthdate AS DATE FORMAT "99/99/99":U 
     LABEL "Fecha Nacimiento" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE e-mail AS CHARACTER FORMAT "X(35)":U 
     LABEL "Correo Electrónico" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE LastName1 AS CHARACTER FORMAT "X(25)":U 
     LABEL "Apellido Paterno" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE LastName2 AS CHARACTER FORMAT "X(25)":U 
     LABEL "Apellido Materno" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Name AS CHARACTER FORMAT "X(25)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 8  
     SIZE 80 BY 2.1
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70 BY 9.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Insertar-Frame
     Btn_Cancel AT ROW 1.62 COL 63.6
     Name AT ROW 5.14 COL 25 COLON-ALIGNED
     LastName1 AT ROW 6.52 COL 25 COLON-ALIGNED
     LastName2 AT ROW 8 COL 25 COLON-ALIGNED
     Address AT ROW 9.48 COL 25 COLON-ALIGNED
     Birthdate AT ROW 11.19 COL 25 COLON-ALIGNED
     e-mail AT ROW 12.76 COL 25 COLON-ALIGNED
     BUTTON-28 AT ROW 15.29 COL 31
     RECT-18 AT ROW 1 COL 1
     RECT-25 AT ROW 4.67 COL 6
     "AGREGAR CLIENTE" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 1.76 COL 29.6
          FGCOLOR 15 
     "Campos Requeridos:" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 3.86 COL 6.4
     SPACE(51.60) SKIP(13.61)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Agregar"
         CANCEL-BUTTON Btn_Cancel.


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
/* SETTINGS FOR DIALOG-BOX Insertar-Frame
                                                                        */
ASSIGN 
       FRAME Insertar-Frame:SCROLLABLE       = FALSE
       FRAME Insertar-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Insertar-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Insertar-Frame Insertar-Frame
ON WINDOW-CLOSE OF FRAME Insertar-Frame /* Agregar */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-28
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-28 Insertar-Frame
ON CHOOSE OF BUTTON-28 IN FRAME Insertar-Frame /* Agregar */
DO:
   Insertar().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Insertar-Frame 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Insertar-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Insertar-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Insertar-Frame  _DEFAULT-ENABLE
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
  DISPLAY Name LastName1 LastName2 Address Birthdate e-mail 
      WITH FRAME Insertar-Frame.
  ENABLE Btn_Cancel Name LastName1 LastName2 Address Birthdate e-mail BUTTON-28 
         RECT-18 RECT-25 
      WITH FRAME Insertar-Frame.
  VIEW FRAME Insertar-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Insertar-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Insertar Insertar-Frame 
FUNCTION Insertar RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /*Comprueba que los campos Nombre, Domicilio y Correo no esten vacios*/
  IF NAME:SCREEN-VALUE IN FRAME Insertar-Frame = "" OR address:SCREEN-VALUE = "" OR e-mail:SCREEN-VALUE = ""
    THEN DO:
      MESSAGE "Nombre, Domicilio y Correo son obligatorios" VIEW-AS ALERT-BOX. 
      RETURN NO-APPLY.
  END.
         
        
  /*Registra un nuevo cliente en la base de datos*/ 
  ELSE DO TRANSACTION:      
      CREATE Persona.
      ASSIGN
       Persona.Nombre = NAME:SCREEN-VALUE 
       Persona.A_Paterno = lastname1:SCREEN-VALUE  
       Persona.A_Materno = lastname2:SCREEN-VALUE
       Persona.Domicilio = Address:SCREEN-VALUE
       Persona.F_Nacimiento = DATE(birthdate:SCREEN-VALUE)  
       Persona.Correo = e-mail:SCREEN-VALUE.
  
       Persona.ID_Persona = NEXT-VALUE(SEC_PERSONA).
       CREATE Cliente.
       Cliente.ID_Cliente = NEXT-VALUE(SEC_CLIENTE).
       Cliente.ID_Persona = Persona.ID_Persona.
  END.
      Limpiar().
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Limpiar Insertar-Frame 
FUNCTION Limpiar RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
       DO.
            ASSIGN
                Name:SCREEN-VALUE IN FRAME Insertar-Frame = " "   
                lastname1:SCREEN-VALUE = " "
                lastname2:screen-value = " "
                address:screen-value =  " "
                birthdate:SCREEN-VALUE = ?
                e-mail:screen-value = " ".
        END.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


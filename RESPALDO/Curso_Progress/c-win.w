&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          layouts          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR iintAccion AS INT.
DEF VAR irowRec AS ROWID.
DEF VAR outlogOK AS LOG.
DEF VAR vchrforeach AS CHAR.
DEF VAR vlogOK AS LOG.
DEF VAR vhndQuery AS HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME Browse-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Call_Click

/* Definitions for BROWSE Browse-1                                      */
&Scoped-define FIELDS-IN-QUERY-Browse-1 Call_Click.curp Call_Click.name ~
Call_Click.lastname1 Call_Click.lastname2 Call_Click.birthdate ~
Call_Click.gender Call_Click.idmarital_status Call_Click.telephone ~
Call_Click.tperson Call_Click.desstreet Call_Click.desext_num ~
Call_Click.desint_num Call_Click.descolony Call_Click.zipcode ~
Call_Click.iddelegation Call_Click.idstate Call_Click.idcity ~
Call_Click.idcountry Call_Click.numpolicy Call_Click.cod_pol 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browse-1 
&Scoped-define QUERY-STRING-Browse-1 FOR EACH Call_Click NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Browse-1 OPEN QUERY Browse-1 FOR EACH Call_Click NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Browse-1 Call_Click
&Scoped-define FIRST-TABLE-IN-QUERY-Browse-1 Call_Click


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-Browse-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browse-1 BUTTON-9 BUTTON-18 BUTTON-13 ~
BUTTON-16 BUTTON-11 BUTTON-19 BUTTON-14 BUTTON-17 BUTTON-12 BUTTON-15 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-11 
     LABEL "Modificar" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-12 
     LABEL "Eliminar" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-13 
     LABEL "Ultimo" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-14 
     LABEL "Primero" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-15 AUTO-END-KEY 
     LABEL "Salir" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-16 
     LABEL "Importar" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-17 
     LABEL "Exportar" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-18 
     LABEL "Filtrar" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-19 
     LABEL "Ordenar" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-9 
     LABEL "Nuevo" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browse-1 FOR 
      Call_Click SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browse-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browse-1 C-Win _STRUCTURED
  QUERY Browse-1 NO-LOCK DISPLAY
      Call_Click.curp COLUMN-LABEL "CURP" FORMAT "X(20)":U
      Call_Click.name COLUMN-LABEL "Nombre" FORMAT "X(30)":U
      Call_Click.lastname1 COLUMN-LABEL "Apellido Paterno" FORMAT "X(30)":U
      Call_Click.lastname2 COLUMN-LABEL "Apellido Materno" FORMAT "X(30)":U
      Call_Click.birthdate COLUMN-LABEL "Fecha de Nacimiento" FORMAT "99/99/99":U
            WIDTH 21
      Call_Click.gender COLUMN-LABEL "Genero" FORMAT "X":U WIDTH 9.2
      Call_Click.idmarital_status COLUMN-LABEL "Estado Marital" FORMAT "->,>>>,>>9":U
      Call_Click.telephone COLUMN-LABEL "Telefono" FORMAT "->,>>>,>>9":U
      Call_Click.tperson COLUMN-LABEL "Tipo de Persona" FORMAT "X(2)":U
            WIDTH 21
      Call_Click.desstreet COLUMN-LABEL "Calle" FORMAT "X(50)":U
            WIDTH 26
      Call_Click.desext_num COLUMN-LABEL "Numero Exterior" FORMAT "X(5)":U
            WIDTH 18.4
      Call_Click.desint_num COLUMN-LABEL "Numero Interior" FORMAT "X(5)":U
            WIDTH 18.2
      Call_Click.descolony COLUMN-LABEL "Colonia" FORMAT "X(25)":U
            WIDTH 18.6
      Call_Click.zipcode COLUMN-LABEL "Codigo Postal" FORMAT "->,>>>,>>99999":U
      Call_Click.iddelegation COLUMN-LABEL "Numero de Delegacion" FORMAT "->,>>>,>>99":U
            WIDTH 23
      Call_Click.idstate COLUMN-LABEL "Numero de Estado" FORMAT "->,>>>,>>99":U
            WIDTH 17.4
      Call_Click.idcity COLUMN-LABEL "Numero de Ciudad" FORMAT "->,>>>,>>99":U
            WIDTH 19.2
      Call_Click.idcountry COLUMN-LABEL "Numero de Pais" FORMAT "->,>>>,>>99":U
      Call_Click.numpolicy COLUMN-LABEL "Num. de Poliza" FORMAT "X(12)":U
            WIDTH 14.6
      Call_Click.cod_pol COLUMN-LABEL "Codigo Poliza" FORMAT "X(4)":U
            WIDTH 13.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 264 BY 15.48 ROW-HEIGHT-CHARS .52 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Browse-1 AT ROW 1 COL 3
     BUTTON-9 AT ROW 18.14 COL 95.4
     BUTTON-18 AT ROW 18.14 COL 203
     BUTTON-13 AT ROW 19.57 COL 12
     BUTTON-16 AT ROW 19.57 COL 153
     BUTTON-11 AT ROW 21 COL 95.6
     BUTTON-19 AT ROW 21 COL 203
     BUTTON-14 AT ROW 22.43 COL 12
     BUTTON-17 AT ROW 22.67 COL 154
     BUTTON-12 AT ROW 23.86 COL 96.2
     BUTTON-15 AT ROW 23.86 COL 246
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 266.4 BY 25.14
         CANCEL-BUTTON BUTTON-15.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 25.14
         WIDTH              = 266.4
         MAX-HEIGHT         = 31.62
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 31.62
         VIRTUAL-WIDTH      = 273.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB Browse-1 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browse-1
/* Query rebuild information for BROWSE Browse-1
     _TblList          = "Layouts.Call_Click"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Layouts.Call_Click.curp
"Call_Click.curp" "CURP" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Layouts.Call_Click.name
"Call_Click.name" "Nombre" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Layouts.Call_Click.lastname1
"Call_Click.lastname1" "Apellido Paterno" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Layouts.Call_Click.lastname2
"Call_Click.lastname2" "Apellido Materno" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Layouts.Call_Click.birthdate
"Call_Click.birthdate" "Fecha de Nacimiento" ? "date" ? ? ? ? ? ? no ? no no "21" yes no no "U" "" ""
     _FldNameList[6]   > Layouts.Call_Click.gender
"Call_Click.gender" "Genero" ? "character" ? ? ? ? ? ? no ? no no "9.2" yes no no "U" "" ""
     _FldNameList[7]   > Layouts.Call_Click.idmarital_status
"Call_Click.idmarital_status" "Estado Marital" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Layouts.Call_Click.telephone
"Call_Click.telephone" "Telefono" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > Layouts.Call_Click.tperson
"Call_Click.tperson" "Tipo de Persona" ? "character" ? ? ? ? ? ? no ? no no "21" yes no no "U" "" ""
     _FldNameList[10]   > Layouts.Call_Click.desstreet
"Call_Click.desstreet" "Calle" ? "character" ? ? ? ? ? ? no ? no no "26" yes no no "U" "" ""
     _FldNameList[11]   > Layouts.Call_Click.desext_num
"Call_Click.desext_num" "Numero Exterior" ? "character" ? ? ? ? ? ? no ? no no "18.4" yes no no "U" "" ""
     _FldNameList[12]   > Layouts.Call_Click.desint_num
"Call_Click.desint_num" "Numero Interior" ? "character" ? ? ? ? ? ? no ? no no "18.2" yes no no "U" "" ""
     _FldNameList[13]   > Layouts.Call_Click.descolony
"Call_Click.descolony" "Colonia" ? "character" ? ? ? ? ? ? no ? no no "18.6" yes no no "U" "" ""
     _FldNameList[14]   > Layouts.Call_Click.zipcode
"Call_Click.zipcode" "Codigo Postal" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[15]   > Layouts.Call_Click.iddelegation
"Call_Click.iddelegation" "Numero de Delegacion" ? "integer" ? ? ? ? ? ? no ? no no "23" yes no no "U" "" ""
     _FldNameList[16]   > Layouts.Call_Click.idstate
"Call_Click.idstate" "Numero de Estado" ? "integer" ? ? ? ? ? ? no ? no no "17.4" yes no no "U" "" ""
     _FldNameList[17]   > Layouts.Call_Click.idcity
"Call_Click.idcity" "Numero de Ciudad" ? "integer" ? ? ? ? ? ? no ? no no "19.2" yes no no "U" "" ""
     _FldNameList[18]   > Layouts.Call_Click.idcountry
"Call_Click.idcountry" "Numero de Pais" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[19]   > Layouts.Call_Click.numpolicy
"Call_Click.numpolicy" "Num. de Poliza" ? "character" ? ? ? ? ? ? no ? no no "14.6" yes no no "U" "" ""
     _FldNameList[20]   > Layouts.Call_Click.cod_pol
"Call_Click.cod_pol" "Codigo Poliza" ? "character" ? ? ? ? ? ? no ? no no "13.2" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE Browse-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-11 C-Win
ON CHOOSE OF BUTTON-11 IN FRAME DEFAULT-FRAME /* Modificar */
DO:
    RUN Dialog-ABC.w(2,ROWID(CALL_click),OUTPUT outlogOK).

    {&OPEN-query-BROWSE-1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-12 C-Win
ON CHOOSE OF BUTTON-12 IN FRAME DEFAULT-FRAME /* Eliminar */
DO:
    RUN Dialog-ABC.w(3,ROWID(CALL_click),OUTPUT outlogOK).

    {&OPEN-query-BROWSE-1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-13 C-Win
ON CHOOSE OF BUTTON-13 IN FRAME DEFAULT-FRAME /* Ultimo */
DO:
  FIND LAST CALL_click NO-LOCK.
  REPOSITION Browse-1 TO ROWID ROWID(CALL_click).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-14 C-Win
ON CHOOSE OF BUTTON-14 IN FRAME DEFAULT-FRAME /* Primero */
DO:
    FIND FIRST CALL_click NO-LOCK.
    REPOSITION Browse-1 TO ROWID ROWID(CALL_click).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-15 C-Win
ON CHOOSE OF BUTTON-15 IN FRAME DEFAULT-FRAME /* Salir */
DO:
  APPLY "window-close" TO CURRENT-WINDOW.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-16 C-Win
ON CHOOSE OF BUTTON-16 IN FRAME DEFAULT-FRAME /* Importar */
DO:
    DELETE FROM CALL_click.
    INPUT FROM VALUE("C:\Curso_Progress\Datos\call_click.csv").
    REPEAT :
        CREATE CALL_click.
        IMPORT DELIMITER "," CALL_click.
    END.
    MESSAGE "Importo Correctamente!" VIEW-AS ALERT-BOX.
    {&OPEN-query-BROWSE-1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-17 C-Win
ON CHOOSE OF BUTTON-17 IN FRAME DEFAULT-FRAME /* Exportar */
DO:
    DEF VAR archivo AS CHAR.
    archivo = "C:\Curso.Progress\Datos\call_click.csv".
    OUTPUT TO VALUE (archivo).
    FOR EACH CALL_click NO-LOCK.
        EXPORT DELIMITER "," CALL_click.
    END.
    MESSAGE "Exporto correctamente!" VIEW-AS ALERT-BOX.
    {&OPEN-query-BROWSE-1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-18 C-Win
ON CHOOSE OF BUTTON-18 IN FRAME DEFAULT-FRAME /* Filtrar */
DO:
  RUN Dialog-fo.w(OUTPUT vchrforeach).
  vhndQuery = (QUERY Browse-1:HANDLE).
  vhndQuery:QUERY-PREPARE (vchrforeach).
  vhndQuery:QUERY-OPEN().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-19 C-Win
ON CHOOSE OF BUTTON-19 IN FRAME DEFAULT-FRAME /* Ordenar */
DO:
  RUN Dialog-ord.w(OUTPUT vchrforeach).
  vhndQuery = (QUERY Browse-1:HANDLE).
  vhndQuery:QUERY-PREPARE (vchrforeach).
  vhndQuery:QUERY-OPEN().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 C-Win
ON CHOOSE OF BUTTON-9 IN FRAME DEFAULT-FRAME /* Nuevo */
DO:
    RUN Dialog-ABC.w(1,?,OUTPUT outlogOK).

    {&OPEN-query-BROWSE-1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Browse-1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  ENABLE Browse-1 BUTTON-9 BUTTON-18 BUTTON-13 BUTTON-16 BUTTON-11 BUTTON-19 
         BUTTON-14 BUTTON-17 BUTTON-12 BUTTON-15 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


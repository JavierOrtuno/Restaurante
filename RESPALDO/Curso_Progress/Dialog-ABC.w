&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-ABC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-ABC 
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
DEF INPUT PARAM inintAccion AS INT.
DEF INPUT PARAM inrowRec AS ROWID.
DEF OUTPUT PARAM outlogOK AS LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-ABC

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-5 FILL-IN-15 Btn_OK FILL-IN-6 ~
FILL-IN-16 FILL-IN-7 FILL-IN-17 Btn_Cancel FILL-IN-8 FILL-IN-18 FILL-IN-9 ~
FILL-IN-19 FILL-IN-10 FILL-IN-20 FILL-IN-11 FILL-IN-22 FILL-IN-12 ~
FILL-IN-23 FILL-IN-13 FILL-IN-24 FILL-IN-14 FILL-IN-25 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-5 FILL-IN-15 FILL-IN-6 FILL-IN-16 ~
FILL-IN-7 FILL-IN-17 FILL-IN-8 FILL-IN-18 FILL-IN-9 FILL-IN-19 FILL-IN-10 ~
FILL-IN-20 FILL-IN-11 FILL-IN-22 FILL-IN-12 FILL-IN-23 FILL-IN-13 ~
FILL-IN-24 FILL-IN-14 FILL-IN-25 

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

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Genero" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-11 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Estado Marital" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-12 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Telefono" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-13 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipo de Persona" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-14 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Calle" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-15 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Numero Exterior" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-16 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Numero Interior" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-17 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Colonia" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-18 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Codigo Postal" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-19 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Delegacion" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-20 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Estado" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-22 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Ciudad" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-23 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Pais" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-24 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Numero de Poliza" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-25 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Codigo de Poliza" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS CHARACTER FORMAT "X(256)":U 
     LABEL "CURP" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-6 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Apellido Paterno" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Apellido Materno" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS DATE FORMAT "99/99/99":U INITIAL ? 
     LABEL "Fecha de Nacimiento" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-ABC
     FILL-IN-5 AT ROW 1.95 COL 25 COLON-ALIGNED
     FILL-IN-15 AT ROW 1.95 COL 95 COLON-ALIGNED
     Btn_OK AT ROW 1.95 COL 131
     FILL-IN-6 AT ROW 3.86 COL 25 COLON-ALIGNED
     FILL-IN-16 AT ROW 3.86 COL 95 COLON-ALIGNED
     FILL-IN-7 AT ROW 5.76 COL 25 COLON-ALIGNED
     FILL-IN-17 AT ROW 5.76 COL 95 COLON-ALIGNED
     Btn_Cancel AT ROW 5.76 COL 131
     FILL-IN-8 AT ROW 7.67 COL 25 COLON-ALIGNED
     FILL-IN-18 AT ROW 7.67 COL 95 COLON-ALIGNED
     FILL-IN-9 AT ROW 9.57 COL 25 COLON-ALIGNED
     FILL-IN-19 AT ROW 9.57 COL 95 COLON-ALIGNED
     FILL-IN-10 AT ROW 11.48 COL 25 COLON-ALIGNED
     FILL-IN-20 AT ROW 11.48 COL 95 COLON-ALIGNED
     FILL-IN-11 AT ROW 13.38 COL 25 COLON-ALIGNED
     FILL-IN-22 AT ROW 13.38 COL 95 COLON-ALIGNED
     FILL-IN-12 AT ROW 15.29 COL 25 COLON-ALIGNED
     FILL-IN-23 AT ROW 15.29 COL 95 COLON-ALIGNED
     FILL-IN-13 AT ROW 17.19 COL 25 COLON-ALIGNED
     FILL-IN-24 AT ROW 17.19 COL 95 COLON-ALIGNED
     FILL-IN-14 AT ROW 18.86 COL 25 COLON-ALIGNED
     FILL-IN-25 AT ROW 19.1 COL 95 COLON-ALIGNED
     SPACE(49.59) SKIP(0.70)
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
/* SETTINGS FOR DIALOG-BOX Dialog-ABC
                                                                        */
ASSIGN 
       FRAME Dialog-ABC:SCROLLABLE       = FALSE
       FRAME Dialog-ABC:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-ABC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-ABC Dialog-ABC
ON WINDOW-CLOSE OF FRAME Dialog-ABC /* <insert dialog title> */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-ABC
ON CHOOSE OF Btn_OK IN FRAME Dialog-ABC /* OK */
DO:
    CASE inintAccion :
        WHEN 1 THEN
        DO:
            IF FILL-IN-5:SCREEN-VALUE = "" THEN
                DO:
                   MESSAGE "Minimo ingresar CURP" VIEW-AS ALERT-BOX.
                   RETURN NO-APPLY.
                END.
            ELSE
                FIND FIRST CALL_click WHERE curp = FILL-IN-5:SCREEN-VALUE NO-ERROR.
                IF AVAILABLE CALL_click THEN
                DO:
                    MESSAGE "CURP repetido" VIEW-AS ALERT-BOX.
                    RETURN NO-APPLY.
                END.
                ELSE
                    DO:
                        CREATE CALL_click.
                        ASSIGN CALL_click.curp = INPUT fill-in-5
                        CALL_click.NAME = INPUT FILL-in-6
                        CALL_click.lastname1 = INPUT FILL-in-7
                        CALL_click.lastname2 = INPUT FILL-in-8
                        CALL_click.birthdate = INPUT FILL-in-9
                        CALL_click.gender = INPUT FILL-in-10
                        CALL_click.idmarital_status = INPUT FILL-in-11
                        CALL_click.telephone = INPUT FILL-in-12
                        CALL_click.tperson = INPUT FILL-in-13
                        CALL_click.desstreet = INPUT FILL-in-14
                        CALL_click.desext_num = INPUT FILL-in-15
                        CALL_click.desint_num = INPUT FILL-in-16
                        CALL_click.descolony = INPUT FILL-in-17
                        CALL_click.zipcode = INPUT FILL-in-18
                        CALL_click.iddelegation = INPUT FILL-in-19
                        CALL_click.idstate = INPUT FILL-in-20
                        CALL_click.idcity = INPUT FILL-in-22
                        CALL_click.idcountry = INPUT FILL-in-23
                        CALL_click.numpolicy = INPUT FILL-in-24
                        CALL_click.cod_pol = INPUT FILL-in-25.
                        MESSAGE "Registro insertado correctamente" VIEW-AS ALERT-BOX.
                   END.
        END.
        WHEN 2 THEN
        DO:
            ASSIGN CALL_click.curp = INPUT fill-in-5
                        CALL_click.NAME = INPUT FILL-in-6
                        CALL_click.lastname1 = INPUT FILL-in-7
                        CALL_click.lastname2 = INPUT FILL-in-8
                        CALL_click.birthdate = INPUT FILL-in-9
                        CALL_click.gender = INPUT FILL-in-10
                        CALL_click.idmarital_status = INPUT FILL-in-11
                        CALL_click.telephone = INPUT FILL-in-12
                        CALL_click.tperson = INPUT FILL-in-13
                        CALL_click.desstreet = INPUT FILL-in-14
                        CALL_click.desext_num = INPUT FILL-in-15
                        CALL_click.desint_num = INPUT FILL-in-16
                        CALL_click.descolony = INPUT FILL-in-17
                        CALL_click.zipcode = INPUT FILL-in-18
                        CALL_click.iddelegation = INPUT FILL-in-19
                        CALL_click.idstate = INPUT FILL-in-20
                        CALL_click.idcity = INPUT FILL-in-22
                        CALL_click.idcountry = INPUT FILL-in-23
                        CALL_click.numpolicy = INPUT FILL-in-24
                        CALL_click.cod_pol = INPUT FILL-in-25.
        END.
        WHEN 3 THEN
            DELETE CALL_click.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-ABC 


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
  CASE inintAccion :
      WHEN 1 THEN
          MESSAGE "nuevo" VIEW-AS ALERT-BOX.
      WHEN 2 THEN
      DO:
          MESSAGE "modificacion" VIEW-AS ALERT-BOX.
          FIND CALL_click WHERE ROWID(CALL_click) = inrowRec.
          FILL-in-5:SCREEN-VALUE = CALL_click.curp.
          FILL-in-6:SCREEN-VALUE = CALL_click.NAME.
          FILL-in-7:SCREEN-VALUE = CALL_click.lastname1.
          FILL-in-8:SCREEN-VALUE = CALL_click.lastname2.
          FILL-in-9:SCREEN-VALUE = string(CALL_click.birthdate).
          FILL-in-10:SCREEN-VALUE = string(CALL_click.gender).
          FILL-in-11:SCREEN-VALUE = string(CALL_click.idmarital_status).
          FILL-in-12:SCREEN-VALUE = string(CALL_click.telephone).
          FILL-in-13:SCREEN-VALUE = CALL_click.tperson.
          FILL-in-14:SCREEN-VALUE = CALL_click.desstreet.
          FILL-in-15:SCREEN-VALUE = CALL_click.desext_num.
          FILL-in-16:SCREEN-VALUE = CALL_click.desint_num.
          FILL-in-17:SCREEN-VALUE = CALL_click.descolony.
          FILL-in-18:SCREEN-VALUE = string(CALL_click.zipcode).
          FILL-in-19:SCREEN-VALUE = string(CALL_click.iddelegation).
          FILL-in-20:SCREEN-VALUE = string(CALL_click.idstate).
          FILL-in-22:SCREEN-VALUE = string(CALL_click.idcity).
          FILL-in-23:SCREEN-VALUE = string(CALL_click.idcountry).
          FILL-in-24:SCREEN-VALUE = CALL_click.numpolicy.
          FILL-in-25:SCREEN-VALUE = CALL_click.cod_pol.
      END.
      WHEN 3 THEN
      DO:
          MESSAGE "eliminar" VIEW-AS ALERT-BOX.
          FIND CALL_click WHERE ROWID(CALL_click) = inrowRec.
          FILL-in-5:SCREEN-VALUE = CALL_click.curp.
          FILL-in-6:SCREEN-VALUE = CALL_click.NAME.
          FILL-in-7:SCREEN-VALUE = CALL_click.lastname1.
          FILL-in-8:SCREEN-VALUE = CALL_click.lastname2.
          FILL-in-9:SCREEN-VALUE = string(CALL_click.birthdate).
          FILL-in-10:SCREEN-VALUE = string(CALL_click.gender).
          FILL-in-11:SCREEN-VALUE = string(CALL_click.idmarital_status).
          FILL-in-12:SCREEN-VALUE = string(CALL_click.telephone).
          FILL-in-13:SCREEN-VALUE = CALL_click.tperson.
          FILL-in-14:SCREEN-VALUE = CALL_click.desstreet.
          FILL-in-15:SCREEN-VALUE = CALL_click.desext_num.
          FILL-in-16:SCREEN-VALUE = CALL_click.desint_num.
          FILL-in-17:SCREEN-VALUE = CALL_click.descolony.
          FILL-in-18:SCREEN-VALUE = string(CALL_click.zipcode).
          FILL-in-19:SCREEN-VALUE = string(CALL_click.iddelegation).
          FILL-in-20:SCREEN-VALUE = string(CALL_click.idstate).
          FILL-in-22:SCREEN-VALUE = string(CALL_click.idcity).
          FILL-in-23:SCREEN-VALUE = string(CALL_click.idcountry).
          FILL-in-24:SCREEN-VALUE = CALL_click.numpolicy.
          FILL-in-25:SCREEN-VALUE = CALL_click.cod_pol.
          FILL-in-5:SENSITIVE = NO.
          FILL-in-6:SENSITIVE = NO.
          FILL-in-7:SENSITIVE = NO.
          FILL-in-8:SENSITIVE = NO.
          FILL-in-9:SENSITIVE = NO.
          FILL-in-10:SENSITIVE = NO.
          FILL-in-11:SENSITIVE = NO.
          FILL-in-12:SENSITIVE = NO.
          FILL-in-13:SENSITIVE = NO.
          FILL-in-14:SENSITIVE = NO.
          FILL-in-15:SENSITIVE = NO.
          FILL-in-16:SENSITIVE = NO.
          FILL-in-17:SENSITIVE = NO.
          FILL-in-18:SENSITIVE = NO.
          FILL-in-19:SENSITIVE = NO.
          FILL-in-20:SENSITIVE = NO.
          FILL-in-22:SENSITIVE = NO.
          FILL-in-23:SENSITIVE = NO.
          FILL-in-24:SENSITIVE = NO.
          FILL-in-25:SENSITIVE = NO.
      END.
  END CASE.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-ABC  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-ABC.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-ABC  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-5 FILL-IN-15 FILL-IN-6 FILL-IN-16 FILL-IN-7 FILL-IN-17 
          FILL-IN-8 FILL-IN-18 FILL-IN-9 FILL-IN-19 FILL-IN-10 FILL-IN-20 
          FILL-IN-11 FILL-IN-22 FILL-IN-12 FILL-IN-23 FILL-IN-13 FILL-IN-24 
          FILL-IN-14 FILL-IN-25 
      WITH FRAME Dialog-ABC.
  ENABLE FILL-IN-5 FILL-IN-15 Btn_OK FILL-IN-6 FILL-IN-16 FILL-IN-7 FILL-IN-17 
         Btn_Cancel FILL-IN-8 FILL-IN-18 FILL-IN-9 FILL-IN-19 FILL-IN-10 
         FILL-IN-20 FILL-IN-11 FILL-IN-22 FILL-IN-12 FILL-IN-23 FILL-IN-13 
         FILL-IN-24 FILL-IN-14 FILL-IN-25 
      WITH FRAME Dialog-ABC.
  VIEW FRAME Dialog-ABC.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-ABC}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


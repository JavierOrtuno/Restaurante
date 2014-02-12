&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dlg_UpdateProd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dlg_UpdateProd 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 
    I.S.C. Fco. Javier Ortuño Colchado  
  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{productos.i}
/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER pinIntEvento AS INTEGER.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dlg_UpdateProd

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Fill_Descripcion Fill_Cantidad List_Unidad ~
Btn_Aceptar Btn_Cancelar 
&Scoped-Define DISPLAYED-OBJECTS Fill_Codigo Fill_Descripcion Fill_Cantidad ~
List_Unidad 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Aceptar 
     LABEL "Aceptar" 
     SIZE 20 BY 2.5.

DEFINE BUTTON Btn_Cancelar 
     LABEL "Cancelar" 
     SIZE 20 BY 2.5.

DEFINE VARIABLE List_Unidad AS CHARACTER FORMAT "X(256)":U 
     LABEL "Unidad" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Seleccionar"," 0"
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE Fill_Cantidad AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cant. Mínima" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Fill_Codigo AS CHARACTER FORMAT "X(10)":U 
     LABEL "Código" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Fill_Descripcion AS CHARACTER FORMAT "X(100)":U 
     LABEL "Descripción" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dlg_UpdateProd
     Fill_Codigo AT ROW 4.24 COL 19 COLON-ALIGNED
     Fill_Descripcion AT ROW 6.14 COL 19 COLON-ALIGNED
     Fill_Cantidad AT ROW 8.05 COL 19 COLON-ALIGNED
     List_Unidad AT ROW 9.95 COL 19 COLON-ALIGNED
     Btn_Aceptar AT ROW 12.43 COL 16.8
     Btn_Cancelar AT ROW 12.43 COL 46.8
     SPACE(14.19) SKIP(1.16)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Actualizar Productos".


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
/* SETTINGS FOR DIALOG-BOX Dlg_UpdateProd
                                                                        */
ASSIGN 
       FRAME Dlg_UpdateProd:SCROLLABLE       = FALSE
       FRAME Dlg_UpdateProd:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Fill_Codigo IN FRAME Dlg_UpdateProd
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dlg_UpdateProd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dlg_UpdateProd Dlg_UpdateProd
ON WINDOW-CLOSE OF FRAME Dlg_UpdateProd /* Actualizar Productos */
DO:
    APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Aceptar Dlg_UpdateProd
ON CHOOSE OF Btn_Aceptar IN FRAME Dlg_UpdateProd /* Aceptar */
DO:
    CASE pinIntEvento:
        WHEN 1 THEN DO:
            MESSAGE "NUEVO" VIEW-AS ALERT-BOX.
        END.
        WHEN 2 THEN DO:
            MESSAGE "ACTUALIZACION" VIEW-AS ALERT-BOX.
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dlg_UpdateProd 


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
  RUN setInitial.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dlg_UpdateProd  _DEFAULT-DISABLE
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
  HIDE FRAME Dlg_UpdateProd.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dlg_UpdateProd  _DEFAULT-ENABLE
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
  DISPLAY Fill_Codigo Fill_Descripcion Fill_Cantidad List_Unidad 
      WITH FRAME Dlg_UpdateProd.
  ENABLE Fill_Descripcion Fill_Cantidad List_Unidad Btn_Aceptar Btn_Cancelar 
      WITH FRAME Dlg_UpdateProd.
  VIEW FRAME Dlg_UpdateProd.
  {&OPEN-BROWSERS-IN-QUERY-Dlg_UpdateProd}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setInitial Dlg_UpdateProd 
PROCEDURE setInitial :
/*------------------------------------------------------------------------------
      Purpose: Inicialización del Dialogo de Registro de Productos    
      Parameters: <none>
      Author:
        I.S.C. Fco. Javier Ortuño Colchado
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcharCatUnidad AS CHARACTER.

    vcharCatUnidad = getCatUnidad().        
    ASSIGN List_Unidad:LIST-ITEM-PAIRS IN FRAME Dlg_UpdateProd = vcharCatUnidad.
    CASE pinIntEvento:
        WHEN 1 THEN DO:
            Fill_Codigo:SCREEN-VALUE = getCodProducto().
        END.
        WHEN 2 THEN DO:
            
        END.
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


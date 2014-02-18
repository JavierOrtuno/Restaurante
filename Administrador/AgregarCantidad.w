&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dlg_Cantidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dlg_Cantidad 
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
DEFINE INPUT PARAMETER pinIntId AS INTEGER.
DEFINE OUTPUT PARAMETER poutIntCantidad AS INTEGER.
DEFINE OUTPUT PARAMETER poutCharUnidad AS CHARACTER.
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dlg_Cantidad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Fill_Cantidad Btn_Agregar 
&Scoped-Define DISPLAYED-OBJECTS Fill_Cantidad Fill_Unidad 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Agregar 
     LABEL "Agregar" 
     SIZE 20 BY 2.52.

DEFINE VARIABLE Fill_Cantidad AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cantidad" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Fill_Unidad AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12.8 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dlg_Cantidad
     Fill_Cantidad AT ROW 2.81 COL 10 COLON-ALIGNED
     Fill_Unidad AT ROW 2.81 COL 24.2 COLON-ALIGNED NO-LABEL
     Btn_Agregar AT ROW 5.43 COL 11
     SPACE(8.99) SKIP(0.52)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Cantidad".


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
/* SETTINGS FOR DIALOG-BOX Dlg_Cantidad
                                                                        */
ASSIGN 
       FRAME Dlg_Cantidad:SCROLLABLE       = FALSE
       FRAME Dlg_Cantidad:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Fill_Unidad IN FRAME Dlg_Cantidad
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dlg_Cantidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dlg_Cantidad Dlg_Cantidad
ON WINDOW-CLOSE OF FRAME Dlg_Cantidad /* Cantidad */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Agregar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Agregar Dlg_Cantidad
ON CHOOSE OF Btn_Agregar IN FRAME Dlg_Cantidad /* Agregar */
DO:
    poutIntCantidad  = INTEGER(Fill_Cantidad:SCREEN-VALUE).
    APPLY "WINDOW-CLOSE" TO FRAME Dlg_Cantidad.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dlg_Cantidad 


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
  RUN setUnidad.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dlg_Cantidad  _DEFAULT-DISABLE
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
  HIDE FRAME Dlg_Cantidad.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dlg_Cantidad  _DEFAULT-ENABLE
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
  DISPLAY Fill_Cantidad Fill_Unidad 
      WITH FRAME Dlg_Cantidad.
  ENABLE Fill_Cantidad Btn_Agregar 
      WITH FRAME Dlg_Cantidad.
  VIEW FRAME Dlg_Cantidad.
  {&OPEN-BROWSERS-IN-QUERY-Dlg_Cantidad}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setUnidad Dlg_Cantidad 
PROCEDURE setUnidad :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    FIND FIRST PRODUCTO WHERE PRODUCTO.ID_PRODUCTO = pinIntId.
    FIND FIRST UNIDAD_MEDIDA WHERE UNIDAD_MEDIDA.ID_UNIDAD = PRODUCTO.ID_UNIDAD.
    Fill_Unidad:SCREEN-VALUE IN FRAME {&FRAME-NAME} = UNIDAD_MEDIDA.DESCRIPCION.
    poutCharUnidad = UNIDAD_MEDIDA.DESCRIPCION.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dlg_Reportes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dlg_Reportes 
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
{reportes.i}
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE STREAM outFile.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dlg_Reportes

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Menu Btn_Propinas Btn_Inventario ~
Btn_Ventas Btn_Facturas 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Facturas 
     LABEL "Facturas" 
     SIZE 20 BY 2.52.

DEFINE BUTTON Btn_Inventario 
     LABEL "Inventario" 
     SIZE 20 BY 2.52.

DEFINE BUTTON Btn_Menu 
     LABEL "Menú" 
     SIZE 20 BY 2.52.

DEFINE BUTTON Btn_Propinas 
     LABEL "Propinas" 
     SIZE 20 BY 2.52.

DEFINE BUTTON Btn_Ventas 
     LABEL "Ventas" 
     SIZE 20 BY 2.5.

DEFINE VARIABLE Edit_File AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 11 BY 2.38 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dlg_Reportes
     Edit_File AT ROW 2.19 COL 5 NO-LABEL
     Btn_Menu AT ROW 4.81 COL 5
     Btn_Propinas AT ROW 4.81 COL 31
     Btn_Inventario AT ROW 7.67 COL 5
     Btn_Ventas AT ROW 7.67 COL 31
     Btn_Facturas AT ROW 10.52 COL 5
     SPACE(30.79) SKIP(0.81)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Reportes".


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
/* SETTINGS FOR DIALOG-BOX Dlg_Reportes
                                                                        */
ASSIGN 
       FRAME Dlg_Reportes:SCROLLABLE       = FALSE
       FRAME Dlg_Reportes:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR Edit_File IN FRAME Dlg_Reportes
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Edit_File:HIDDEN IN FRAME Dlg_Reportes           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dlg_Reportes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dlg_Reportes Dlg_Reportes
ON WINDOW-CLOSE OF FRAME Dlg_Reportes /* Reportes */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Facturas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Facturas Dlg_Reportes
ON CHOOSE OF Btn_Facturas IN FRAME Dlg_Reportes /* Facturas */
DO:
    RUN menuFacturas.w.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Inventario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Inventario Dlg_Reportes
ON CHOOSE OF Btn_Inventario IN FRAME Dlg_Reportes /* Inventario */
DO:
    DEFINE VARIABLE vcharFileName AS CHARACTER.
    Edit_File:SCREEN-VALUE = "".

    SYSTEM-DIALOG 
        GET-FILE vcharFileName 
        TITLE "Guardar Archivo ..." 
        FILTERS "Archivos (*.html)"   "*.html" 
        SAVE-AS.

    RUN generarInventario.
    Edit_File:SAVE-FILE ( vcharFileName + ".html" ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Menu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Menu Dlg_Reportes
ON CHOOSE OF Btn_Menu IN FRAME Dlg_Reportes /* Menú */
DO:    
    DEFINE VARIABLE vcharFileName AS CHARACTER.

    Edit_File:SCREEN-VALUE = "".
    SYSTEM-DIALOG 
        GET-FILE vcharFileName 
        TITLE "Guardar Archivo ..." 
        FILTERS "Archivos (*.html)"   "*.html" 
        SAVE-AS.

    RUN generarMenu.
    Edit_File:SAVE-FILE ( vcharFileName + ".html" ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Propinas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Propinas Dlg_Reportes
ON CHOOSE OF Btn_Propinas IN FRAME Dlg_Reportes /* Propinas */
DO:
    DEFINE VARIABLE vcharFechas AS CHARACTER INITIAL ?.
    DEFINE VARIABLE vcharFileName AS CHARACTER.

    RUN menuPropinas.w(OUTPUT vcharFechas).
    IF vcharFechas <> ? THEN DO:
        Edit_File:SCREEN-VALUE = "".

        SYSTEM-DIALOG 
            GET-FILE vcharFileName 
            TITLE "Guardar Archivo ..." 
            FILTERS "Archivos (*.html)"   "*.html" 
            SAVE-AS.

        RUN generarPropinas(vcharFechas).
        Edit_File:SAVE-FILE ( vcharFileName + ".html" ).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ventas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ventas Dlg_Reportes
ON CHOOSE OF Btn_Ventas IN FRAME Dlg_Reportes /* Ventas */
DO:
    DEFINE VARIABLE vcharFechas AS CHARACTER INITIAL ?.
    DEFINE VARIABLE vcharFileName AS CHARACTER.

    RUN menuVentas.w(OUTPUT vcharFechas).
    IF vcharFechas <> ? THEN DO:
        Edit_File:SCREEN-VALUE = "".

        SYSTEM-DIALOG 
            GET-FILE vcharFileName 
            TITLE "Guardar Archivo ..." 
            FILTERS "Archivos (*.html)"   "*.html" 
            SAVE-AS.

        RUN generarVentas(vcharFechas).
        Edit_File:SAVE-FILE ( vcharFileName + ".html" ).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dlg_Reportes 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dlg_Reportes  _DEFAULT-DISABLE
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
  HIDE FRAME Dlg_Reportes.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dlg_Reportes  _DEFAULT-ENABLE
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
  ENABLE Btn_Menu Btn_Propinas Btn_Inventario Btn_Ventas Btn_Facturas 
      WITH FRAME Dlg_Reportes.
  VIEW FRAME Dlg_Reportes.
  {&OPEN-BROWSERS-IN-QUERY-Dlg_Reportes}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generarInventario Dlg_Reportes 
PROCEDURE generarInventario :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vintCursor AS INTEGER.
    DEFINE VARIABLE vcharFile AS CHARACTER.
    DEFINE VARIABLE vcharTexto AS CHARACTER.    
    DEFINE VARIABLE vcharOut AS CHARACTER.       

    vcharFile = search("inventario.html").
    Edit_File:INSERT-FILE(vcharFile) IN FRAME Dlg_Reportes.
    vcharTexto = Edit_File:SCREEN-VALUE.
        
    DO vintCursor = 1 TO NUM-ENTRIES(vcharTexto, "~n"):        
        IF LENGTH(TRIM(ENTRY(vintCursor, vcharTexto, "~n"))) > 0 THEN DO:
            vcharOut = vcharOut + ENTRY(vintCursor, vcharTexto, "~n") + "~n".
        END.
        ELSE DO:
            vcharOut = vcharOut + getReporteInventario() + "~n".
        END.
    END.
    ASSIGN Edit_File:SCREEN-VALUE = vcharOut.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generarMenu Dlg_Reportes 
PROCEDURE generarMenu :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/    
    DEFINE VARIABLE vintCursor AS INTEGER.
    DEFINE VARIABLE vcharFile AS CHARACTER.
    DEFINE VARIABLE vcharTexto AS CHARACTER.    
    DEFINE VARIABLE vcharOut AS CHARACTER.       

    vcharFile = search("menu.html").
    Edit_File:INSERT-FILE(vcharFile) IN FRAME Dlg_Reportes.
    vcharTexto = Edit_File:SCREEN-VALUE.
        
    DO vintCursor = 1 TO NUM-ENTRIES(vcharTexto, "~n"):        
        IF LENGTH(TRIM(ENTRY(vintCursor, vcharTexto, "~n"))) > 0 THEN DO:
            vcharOut = vcharOut + ENTRY(vintCursor, vcharTexto, "~n") + "~n".
        END.
        ELSE DO:
            vcharOut = vcharOut + getReporteMenu() + "~n".
        END.
    END.
    ASSIGN Edit_File:SCREEN-VALUE = vcharOut.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generarPedido Dlg_Reportes 
PROCEDURE generarPedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generarPropinas Dlg_Reportes 
PROCEDURE generarPropinas :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pinCharFechas AS CHARACTER.
    DEFINE VARIABLE vintCursor AS INTEGER.
    DEFINE VARIABLE vcharFile AS CHARACTER.
    DEFINE VARIABLE vcharTexto AS CHARACTER.    
    DEFINE VARIABLE vcharOut AS CHARACTER.       

    vcharFile = search("propinas.html").
    Edit_File:INSERT-FILE(vcharFile) IN FRAME Dlg_Reportes.
    vcharTexto = Edit_File:SCREEN-VALUE.
        
    DO vintCursor = 1 TO NUM-ENTRIES(vcharTexto, "~n"):
        IF LENGTH(TRIM(ENTRY(vintCursor, vcharTexto, "~n"))) > 0 THEN DO:
            vcharOut = vcharOut + ENTRY(vintCursor, vcharTexto, "~n") + "~n".
        END.
        ELSE DO:
            vcharOut = vcharOut + getReportePropinas(pinCharFechas) + "~n".
        END.
    END.
    ASSIGN Edit_File:SCREEN-VALUE = vcharOut.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generarVentas Dlg_Reportes 
PROCEDURE generarVentas :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pinCharFechas AS CHARACTER.
    DEFINE VARIABLE vintCursor AS INTEGER.
    DEFINE VARIABLE vcharFile AS CHARACTER.
    DEFINE VARIABLE vcharTexto AS CHARACTER.    
    DEFINE VARIABLE vcharOut AS CHARACTER.       

    vcharFile = search("ventas.html").
    Edit_File:INSERT-FILE(vcharFile) IN FRAME Dlg_Reportes.
    vcharTexto = Edit_File:SCREEN-VALUE.
        
    DO vintCursor = 1 TO NUM-ENTRIES(vcharTexto, "~n"):
        IF LENGTH(TRIM(ENTRY(vintCursor, vcharTexto, "~n"))) > 0 THEN DO:
            vcharOut = vcharOut + ENTRY(vintCursor, vcharTexto, "~n") + "~n".
        END.
        ELSE DO:
            vcharOut = vcharOut + getReporteVentas(pinCharFechas) + "~n".
        END.
    END.
    ASSIGN Edit_File:SCREEN-VALUE = vcharOut.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


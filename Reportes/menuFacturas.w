&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          restaurante      PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dlg_Facturas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dlg_Facturas 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dlg_Facturas
&Scoped-define BROWSE-NAME Bws_Facturas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES FACTURA

/* Definitions for BROWSE Bws_Facturas                                  */
&Scoped-define FIELDS-IN-QUERY-Bws_Facturas FACTURA.FOLIO FACTURA.FECHA ~
FACTURA.TOTAL FACTURA.ID_ESTATUS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Bws_Facturas 
&Scoped-define QUERY-STRING-Bws_Facturas FOR EACH FACTURA NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Bws_Facturas OPEN QUERY Bws_Facturas FOR EACH FACTURA NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Bws_Facturas FACTURA
&Scoped-define FIRST-TABLE-IN-QUERY-Bws_Facturas FACTURA


/* Definitions for DIALOG-BOX Dlg_Facturas                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dlg_Facturas ~
    ~{&OPEN-QUERY-Bws_Facturas}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Bws_Facturas 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE VARIABLE Edit_Factura AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 10 BY 1.91 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Bws_Facturas FOR 
      FACTURA SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Bws_Facturas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Bws_Facturas Dlg_Facturas _STRUCTURED
  QUERY Bws_Facturas NO-LOCK DISPLAY
      FACTURA.FOLIO FORMAT "X(50)":U WIDTH 26.2
      FACTURA.FECHA FORMAT "99/99/99":U WIDTH 33.2
      FACTURA.TOTAL FORMAT "->>,>>9.99":U WIDTH 33.2
      FACTURA.ID_ESTATUS FORMAT "->,>>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 121 BY 10.71 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dlg_Facturas
     Bws_Facturas AT ROW 4.05 COL 7
     Edit_Factura AT ROW 14.81 COL 7 NO-LABEL
     SPACE(116.99) SKIP(0.22)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Facturas".


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
/* SETTINGS FOR DIALOG-BOX Dlg_Facturas
                                                                        */
/* BROWSE-TAB Bws_Facturas 1 Dlg_Facturas */
ASSIGN 
       FRAME Dlg_Facturas:SCROLLABLE       = FALSE
       FRAME Dlg_Facturas:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR Edit_Factura IN FRAME Dlg_Facturas
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Edit_Factura:HIDDEN IN FRAME Dlg_Facturas           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Bws_Facturas
/* Query rebuild information for BROWSE Bws_Facturas
     _TblList          = "Restaurante.FACTURA"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Restaurante.FACTURA.FOLIO
"FACTURA.FOLIO" ? ? "character" ? ? ? ? ? ? no ? no no "26.2" yes no no "U" "" ""
     _FldNameList[2]   > Restaurante.FACTURA.FECHA
"FACTURA.FECHA" ? ? "date" ? ? ? ? ? ? no ? no no "33.2" yes no no "U" "" ""
     _FldNameList[3]   > Restaurante.FACTURA.TOTAL
"FACTURA.TOTAL" ? ? "decimal" ? ? ? ? ? ? no ? no no "33.2" yes no no "U" "" ""
     _FldNameList[4]   = Restaurante.FACTURA.ID_ESTATUS
     _Query            is OPENED
*/  /* BROWSE Bws_Facturas */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dlg_Facturas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dlg_Facturas Dlg_Facturas
ON WINDOW-CLOSE OF FRAME Dlg_Facturas /* Facturas */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Bws_Facturas
&Scoped-define SELF-NAME Bws_Facturas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bws_Facturas Dlg_Facturas
ON MOUSE-SELECT-DBLCLICK OF Bws_Facturas IN FRAME Dlg_Facturas
DO:
    DEFINE VARIABLE vcharFileName AS CHARACTER.

    Edit_Factura:SCREEN-VALUE = "".
    SYSTEM-DIALOG 
        GET-FILE vcharFileName 
        TITLE "Guardar Archivo ..." 
        FILTERS "Archivos (*.html)"   "*.html" 
        SAVE-AS.

    RUN generarFactura(ROWID(FACTURA)).
    Edit_Factura:SAVE-FILE ( vcharFileName + ".html" ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dlg_Facturas 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dlg_Facturas  _DEFAULT-DISABLE
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
  HIDE FRAME Dlg_Facturas.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dlg_Facturas  _DEFAULT-ENABLE
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
  ENABLE Bws_Facturas 
      WITH FRAME Dlg_Facturas.
  VIEW FRAME Dlg_Facturas.
  {&OPEN-BROWSERS-IN-QUERY-Dlg_Facturas}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generarFactura Dlg_Facturas 
PROCEDURE generarFactura :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pinIdFactura AS ROWID.
    DEFINE VARIABLE vintIdFactura AS INTEGER.
    DEFINE VARIABLE vintCursor AS INTEGER.
    DEFINE VARIABLE vcharFile AS CHARACTER.
    DEFINE VARIABLE vcharTexto AS CHARACTER.
    DEFINE VARIABLE vcharOut AS CHARACTER.    

    vcharFile = search("factura.html").
    Edit_Factura:INSERT-FILE(vcharFile) IN FRAME Dlg_Facturas.
    vcharTexto = Edit_Factura:SCREEN-VALUE.

    FIND FIRST FACTURA WHERE ROWID(FACTURA) = pinIdFactura.
    vintIdFactura = FACTURA.ID_FACTURA.
        
    DO vintCursor = 1 TO NUM-ENTRIES(vcharTexto, "~n"):
        IF LENGTH(TRIM(ENTRY(vintCursor, vcharTexto, "~n"))) > 0 THEN DO:
            vcharOut = vcharOut + ENTRY(vintCursor, vcharTexto, "~n") + "~n".
        END.
        ELSE DO:
            vcharOut = vcharOut + getReporteFactura(vintIdFactura) + "~n".
        END.
    END.
    ASSIGN Edit_Factura:SCREEN-VALUE = vcharOut.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


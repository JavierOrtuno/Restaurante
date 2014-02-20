&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Insert-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Insert-Frame 
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
  DEF INPUT PARAM inrowReg AS ROWID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Insert-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-23 FILL-IN-26 FILL-IN-27 FILL-IN-28 ~
FILL-IN-29 FILL-IN-30 Btn_OK RECT-18 RECT-25 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-26 FILL-IN-27 FILL-IN-28 ~
FILL-IN-29 FILL-IN-30 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Agregar" 
     SIZE 18 BY 2.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-23 
     LABEL "Salir" 
     SIZE 15 BY .95.

DEFINE VARIABLE FILL-IN-26 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Codigo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-27 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Descripcion" 
     VIEW-AS FILL-IN 
     SIZE 30.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-28 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cantidad Minima" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-29 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cantidad" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-30 AS DATE FORMAT "99/99/99":U 
     LABEL "Fecha de Caducidad" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 8  
     SIZE 80 BY 2.1
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60.4 BY 9.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Insert-Frame
     BUTTON-23 AT ROW 1.62 COL 63.8
     FILL-IN-26 AT ROW 5.38 COL 34.4 COLON-ALIGNED
     FILL-IN-27 AT ROW 7.1 COL 34.4 COLON-ALIGNED
     FILL-IN-28 AT ROW 8.95 COL 34.4 COLON-ALIGNED
     FILL-IN-29 AT ROW 10.76 COL 34.4 COLON-ALIGNED
     FILL-IN-30 AT ROW 12.62 COL 34.4 COLON-ALIGNED
     Btn_OK AT ROW 15.1 COL 32.2
     RECT-18 AT ROW 1 COL 1.2
     RECT-25 AT ROW 4.67 COL 11.2
     "AGREGAR PRODUCTO" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 1.76 COL 28.8
          FGCOLOR 15 
     "Campos Requeridos:" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 3.86 COL 11
     SPACE(47.20) SKIP(13.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Agregar"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Insert-Frame 
/* ************************* Included-Libraries *********************** */

{inventario.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Insert-Frame
                                                                        */
ASSIGN 
       FRAME Insert-Frame:SCROLLABLE       = FALSE
       FRAME Insert-Frame:HIDDEN           = TRUE.

ASSIGN 
       FILL-IN-26:READ-ONLY IN FRAME Insert-Frame        = TRUE.

ASSIGN 
       FILL-IN-27:READ-ONLY IN FRAME Insert-Frame        = TRUE.

ASSIGN 
       FILL-IN-28:READ-ONLY IN FRAME Insert-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Insert-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Insert-Frame Insert-Frame
ON WINDOW-CLOSE OF FRAME Insert-Frame /* Agregar */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Insert-Frame
ON CHOOSE OF Btn_OK IN FRAME Insert-Frame /* Agregar */
DO:
    DEF VAR vdteactual AS DATE.
    DEF VAR vchrlote AS CHAR.

    vdteactual = TODAY.
    IF int(FILL-IN-29:SCREEN-VALUE) <= 0 THEN DO:
         MESSAGE "Ingresar Cantidad minimo 1" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
    END.
    ELSE DO:
      IF DATE(FILL-IN-30:SCREEN-VALUE) <= vdteactual OR date(FILL-IN-30:SCREEN-VALUE) = ?  THEN DO:
          MESSAGE "La Fecha de Caducidad no puede ser nula o menorigual a la actual" VIEW-AS ALERT-BOX.
          RETURN NO-APPLY.
      END.
      ELSE DO:
          vchrlote = Genera_Lote(vdteactual).
          FIND Producto WHERE ROWID(Producto) = inrowReg.
          CREATE Stock.
          ASSIGN stock.id_stock = NEXT-VALUE(sec_stock)
                 stock.lote = vchrlote
                 stock.cantidad = INPUT FILL-in-29
                 stock.f_ingreso = vdteactual
                 stock.f_caducidad = INPUT FILL-in-30
                 stock.id_producto = producto.id_producto.
          MESSAGE "Registro insertado correctamente" VIEW-AS ALERT-BOX.
      END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-23
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-23 Insert-Frame
ON CHOOSE OF BUTTON-23 IN FRAME Insert-Frame /* Salir */
DO:
   APPLY "window-close" TO FRAME Insert-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Insert-Frame 


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
  RUN Nuevo_Stock(inrowReg).
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Insert-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Insert-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Insert-Frame  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-26 FILL-IN-27 FILL-IN-28 FILL-IN-29 FILL-IN-30 
      WITH FRAME Insert-Frame.
  ENABLE BUTTON-23 FILL-IN-26 FILL-IN-27 FILL-IN-28 FILL-IN-29 FILL-IN-30 
         Btn_OK RECT-18 RECT-25 
      WITH FRAME Insert-Frame.
  VIEW FRAME Insert-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Insert-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Nuevo_Stock Insert-Frame 
PROCEDURE Nuevo_Stock :
/*------------------------------------------------------------------------------
  Purpose: Mostrar datos para nuevo stock    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM inrowRec AS ROWID.
 FIND Producto WHERE ROWID(Producto) = inrowRec.
  FILL-in-26:SCREEN-VALUE IN FRAME Insert-Frame = Producto.codigo.
  FILL-in-27:SCREEN-VALUE = Producto.descripcion.
  FILL-in-28:SCREEN-VALUE = string(producto.cant_minima).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


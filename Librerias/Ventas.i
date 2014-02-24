&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AsignarFolio Include 
FUNCTION AsignarFolio RETURNS CHARACTER
    ( INPUT vintIdFactura AS INTEGER,  
      INPUT vintElementos AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CalcularIVA Include 
FUNCTION CalcularIVA RETURNS DEC
  ( vdecSubtotal AS DEC /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CalcularSubtotal Include 
FUNCTION CalcularSubtotal RETURNS DECIMAL
  ( vintCantidad AS INT /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CalcularTotal Include 
FUNCTION CalcularTotal RETURNS DECIMAL
  ( vdecSubtotal AS DEC,
    vdecIVA AS DEC /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DescontarInventario Include 
FUNCTION DescontarInventario RETURNS LOGICAL
  ( vintCantidad AS INT /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD HoraEntrada Include 
FUNCTION HoraEntrada RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ListarMenu Include 
FUNCTION ListarMenu RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidarFrame Include 
FUNCTION ValidarFrame RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LlenarComanda Include 
PROCEDURE LlenarComanda :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER indteFecha AS DATE.
DEF INPUT PARAMETER inchrEntrada AS CHAR.
DEF INPUT PARAMETER inchrSalida AS CHAR.
DEF INPUT PARAMETER indecPropina AS DEC.

DO TRANSACTION:
    CREATE Comanda.

    Comanda.F_Atencion = indteFecha.
    Comanda.Hora_Llegada = inchrEntrada.
    Comanda.Hora_Salida = inchrSalida.
    Comanda.Propina = indecPropina.
    Comanda.ID_Comanda = NEXT-VALUE(SEC_COMANDA).

    FIND CURRENT Mesa.
    Comanda.ID_Mesa = Mesa.ID_mesa.

    FIND CURRENT Persona.
    Comanda.ID_Empleado = Empleado.ID_Empleado.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LlenarConsumo Include 
PROCEDURE LlenarConsumo :
/*------------------------------------------------------------------------------
  Purpose: Actualizar la tabla de consumo cuando se registra una venta     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER inintCantidad AS INT.
    
DO TRANSACTION:
    CREATE Consumo.

    Consumo.ID_Consumo = NEXT-VALUE(SEC_CONSUMO).

    Consumo.Cantidad = inintCantidad.
    
    FIND CURRENT MENU.
    Consumo.ID_Menu = MENU.ID_Menu.

    FIND CURRENT Comanda.
    Consumo.ID_Comanda = Comanda.ID_Comanda.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LlenarFactura Include 
PROCEDURE LlenarFactura :
/*------------------------------------------------------------------------------
  Purpose: Actualizar la tabla de factura cuando se registra una venta     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER indecSubtotal AS DEC.
DEF INPUT PARAMETER indecIVA AS DEC.
DEF INPUT PARAMETER indecTotal AS DEC.

DEF VAR vintIDFactura AS INT.

DO TRANSACTION: 
    CREATE Factura.
    
    Factura.Subtotal = indecSubtotal.
    Factura.IVA = indecIVA.
    Factura.TOTAL = indecTotal.
    vintIDFactura = NEXT-VALUE(SEC_FACTURA).
    Factura.ID_Factura = vintIDFactura.
    Factura.Folio = AsignarFolio(vintIDFactura,4).

    FIND CURRENT Comanda.
    Factura.Fecha = Comanda.F_Atencion.
    Factura.ID_Comanda = Comanda.ID_Comanda.

    FIND CURRENT FORMA_Pago.
    Factura.ID_Pago = FORMA_Pago.ID_Pago.

    FIND CURRENT Estatus.
    Factura.ID_Estatus = Estatus.ID_Estatus.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AsignarFolio Include 
FUNCTION AsignarFolio RETURNS CHARACTER
    ( INPUT vintIdFactura AS INTEGER,  
      INPUT vintElementos AS INTEGER ) :
    /*------------------------------------------------------------------------------
        Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vintCount AS INTEGER.
    DEFINE VARIABLE vcharCodigo AS CHARACTER INITIAL "FT-".    

    DO vintCount = LENGTH(STRING(vintIdFactura)) TO vintElementos:
        vcharCodigo = vcharCodigo + "0".
    END.
    vcharCodigo = vcharCodigo + STRING(vintIdFactura).

    RETURN vcharCodigo.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CalcularIVA Include 
FUNCTION CalcularIVA RETURNS DEC
  ( vdecSubtotal AS DEC /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Calcular el IVA que se cobrará al cliente por el total de su consumo 
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR vdecIVA AS DEC.  

  vdecIVA = vdecSubTotal * 0.15.

  RETURN vdecIVA.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CalcularSubtotal Include 
FUNCTION CalcularSubtotal RETURNS DECIMAL
  ( vintCantidad AS INT /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR vdecCuenta AS DEC.

  FIND CURRENT MENU.

  vdecCuenta = vdecCuenta + (vintCantidad * Menu.Precio).

  RETURN vdecCuenta.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CalcularTotal Include 
FUNCTION CalcularTotal RETURNS DECIMAL
  ( vdecSubtotal AS DEC,
    vdecIVA AS DEC /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR vdecTotal AS DEC.

  vdecTotal = vdecSubtotal + vdecIVA.

  RETURN vdecTotal.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DescontarInventario Include 
FUNCTION DescontarInventario RETURNS LOGICAL
  ( vintCantidad AS INT /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEF VAR vintNumero AS INT.
DEF VAR vlogBandera AS LOG.
DEF VAR vlogLinterna AS LOG.

FIND CURRENT MENU.

FOR EACH Ingrediente WHERE Ingrediente.ID_Menu = MENU.ID_Menu:
    vintNumero = Ingrediente.Cantidad * vintCantidad.
    vlogLinterna = FALSE.
    FOR EACH Stock WHERE Stock.ID_Producto = Ingrediente.ID_Producto AND Stock.Cantidad > 0 AND Stock.F_Caducidad > TODAY.
    IF AVAILABLE Stock 
        THEN DO:
                    IF  Stock.Cantidad >= vintNumero THEN DO:
                        vlogLinterna = TRUE.
                        LEAVE.
                    END.
                    ELSE DO:
                        vintNumero = vintNumero - Stock.Cantidad.
                         END.
             END.
     ELSE DO:
         vintNumero = 1.
         LEAVE.
     END.
    END.
    IF vlogLinterna = FALSE THEN DO:
        IF vintNumero > 0 THEN DO:
            vlogBandera = TRUE.
            LEAVE.   
        END.
    END.
END.

  RETURN vlogBandera.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION HoraEntrada Include 
FUNCTION HoraEntrada RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hour AS INTEGER.
  DEFINE VARIABLE minute AS INTEGER.
  DEFINE VARIABLE sec AS INTEGER.
  DEFINE VARIABLE timeleft AS INTEGER.
  DEFINE VARIABLE vchrHora AS CHAR.
      
      timeleft = (24 * 60 * 60) - TIME.  
  
  /* seconds till next midnight */
  sec = timeleft MOD 60.
      timeleft = (timeleft - sec) / 60.  
  
  /* minutes till next midnight */
  minute = timeleft MOD 60.

      
  /* hours till next midnight */
      hour = (timeleft - minute) / 60. 

  IF (59 - minute) < 10 
      THEN vchrHora = STRING(23 - hour) + ":" + "0" + STRING(59 - minute).
  ELSE vchrHora = STRING(23 - hour) + ":" + STRING(59 - minute).

  RETURN vchrHora.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ListarMenu Include 
FUNCTION ListarMenu RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR vchrLista AS CHAR.
  DEF VAR vintCantidad AS INT.

  FOR EACH MENU:
      FOR EACH Ingrediente WHERE Ingrediente.ID_Menu = MENU.ID_Menu:
          vintCantidad = Ingrediente.Cantidad.
          FOR EACH Producto:
              FOR EACH Stock WHERE Stock.ID_Producto = Producto.ID_Producto.
                  vchrLista = vchrLista + string(MENU.Descripcion) + " " + STRING(MENU.Precio).
              END.
          END.
      END.
  END.

  RETURN vchrLista.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidarFrame Include 
FUNCTION ValidarFrame RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR vlogSemaforo AS LOG.

    FIND CURRENT Mesa NO-ERROR.
    IF  NOT AVAILABLE Mesa THEN MESSAGE "Selecciona una mesa" VIEW-AS ALERT-BOX.
    ELSE DO:
        FIND CURRENT Persona NO-ERROR.
        IF  NOT AVAILABLE Persona THEN MESSAGE "Selecciona un mesero" VIEW-AS ALERT-BOX.
        ELSE DO:
            FIND CURRENT FORMA_pago NO-ERROR.
            IF  NOT AVAILABLE Forma_Pago THEN MESSAGE "Selecciona una forma de pago" VIEW-AS ALERT-BOX.
            ELSE DO:
                FIND CURRENT Estatus NO-ERROR.
                IF  NOT AVAILABLE Estatus THEN MESSAGE "Selecciona un estatus" VIEW-AS ALERT-BOX.
                ELSE DO:
                    vlogSemaforo = TRUE.
                END.
            END.
        END.
    END.

  RETURN vlogSemaforo.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


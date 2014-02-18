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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Sumatoria Include 
FUNCTION Sumatoria RETURNS INTEGER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DescontarInventario Include 
PROCEDURE DescontarInventario :
/*------------------------------------------------------------------------------
  Purpose: Enviar al inventario el producto y la cantidad que se descontarán del stock
           por la venta de un platillo/bebida.    
  Parameters:  1.- outintProducto es el producto que se dará de baja del stock
               2.- outintCantidad es la cantidad del producto que se descontará del stock
               3.- outintUno es el valor que le indica a inventario que la baja de producto
                   es por consumo y no por desperdicio
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER inintCantidad AS INT.
DEF OUTPUT PARAMETER outintProducto AS INT. 
DEF OUTPUT PARAMETER outintCantidad AS INT.
DEF OUTPUT PARAMETER outintUno AS INT.
DEF VAR vintStock AS INT.

FIND CURRENT MENU.

FOR EACH Ingrediente WHERE Ingrediente.ID_Menu = MENU.ID_Menu.
    FIND Stock WHERE Stock.ID_Producto = Ingrediente.ID_Producto AND Stock.F_Caducidad > TODAY.
    IF (inintCantidad * Ingrediente.Cantidad) <= Sumatoria() 
        THEN DO:
            outintProducto = Stock.ID_Producto.
            outintCantidad = inintCantidad.
            outintUno = 1.
             END.
    ELSE
        MESSAGE "No hay stock para competar venta" VIEW-AS ALERT-BOX.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
DO TRANSACTION:
    CREATE Consumo.
    
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
DEF INPUT PARAMETER inchrFolio AS CHAR.
DEF INPUT PARAMETER indecSubtotal AS DEC.
DEF INPUT PARAMETER indecIVA AS DEC.
DEF INPUT PARAMETER indecTotal AS DEC.

DO TRANSACTION: 
    CREATE Factura.

    Factura.Folio = inchrFolio.
    Factura.Subtotal = indecSubtotal.
    Factura.IVA = indecIVA.
    Factura.TOTAL = indecTotal.
    Factura.ID_Factura = NEXT-VALUE(SEC_FACTURA).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Sumatoria Include 
FUNCTION Sumatoria RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR vintStock AS INT.

  vintStock = vintStock + Stock.Cantidad.

  RETURN vintStock.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


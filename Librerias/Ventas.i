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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DescontarInventario Include 
FUNCTION DescontarInventario RETURNS LOGICAL
  ( vintCantidad AS INT /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ListarMenu Include 
FUNCTION ListarMenu RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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
  Purpose: Calcular el IVA que se cobrar� al cliente por el total de su consumo 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Sumatoria Include 
FUNCTION Sumatoria RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR vintStock AS INT.

  FIND CURRENT MENU.

        vintStock = vintStock + Stock.Cantidad.

  RETURN vintStock.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


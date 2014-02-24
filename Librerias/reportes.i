&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*------------------------------------------------------------------------
    Library     : 
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

&IF DEFINED(EXCLUDE-getDescClasificacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescClasificacion Method-Library 
FUNCTION getDescClasificacion RETURNS CHARACTER
  ( INPUT vintIdClasificacion AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getReporteFactura) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReporteFactura Method-Library 
FUNCTION getReporteFactura RETURNS CHARACTER
    ( INPUT vintFactura AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getReporteInventario) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReporteInventario Method-Library 
FUNCTION getReporteInventario RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getReporteMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReporteMenu Method-Library 
FUNCTION getReporteMenu RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getReportePropinas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReportePropinas Method-Library 
FUNCTION getReportePropinas RETURNS CHARACTER
    ( INPUT vcharFechas AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getReporteVentas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReporteVentas Method-Library 
FUNCTION getReporteVentas RETURNS CHARACTER
    ( INPUT vcharFechas AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Method-Library 
/* ************************* Included-Libraries *********************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-getProporciones) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getProporciones Method-Library 
PROCEDURE getProporciones :
/*------------------------------------------------------------------------------
        Purpose:     
        Parameters:  <none>
        Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pinCharFechas AS CHARACTER.
    DEFINE OUTPUT PARAMETER poutDecTotal AS DECIMAL.    
    DEFINE OUTPUT PARAMETER poutDecMe AS DECIMAL.
    DEFINE OUTPUT PARAMETER poutDecCo AS DECIMAL.
    DEFINE OUTPUT PARAMETER poutDecAd AS DECIMAL.    
    DEFINE VARIABLE vintNumMe AS INTEGER.
    DEFINE VARIABLE vintNumCo AS INTEGER.
    DEFINE VARIABLE vintNumAd AS INTEGER.
    DEFINE VARIABLE vdecMeserosT AS DECIMAL.
    DEFINE VARIABLE vdecCocinerosT AS DECIMAL.
    DEFINE VARIABLE vdecAdminT AS DECIMAL.
    
    IF TRIM(pinCharFechas) = "#" THEN DO:
        FOR EACH FACTURA WHERE FACTURA.ID_COMANDA > 0:
            FIND FIRST COMANDA WHERE COMANDA.ID_COMANDA = FACTURA.ID_COMANDA.
            poutDecTotal = poutDecTotal + COMANDA.PROPINA.
        END.
    END.
    ELSE DO:
        IF TRIM(ENTRY(1, pinCharFechas, "#")) = "" THEN DO:
            FOR EACH FACTURA WHERE FACTURA.ID_COMANDA > 0 AND
                FACTURA.FECHA <= DATE(ENTRY(2, pinCharFechas, "#")):       
                    FIND FIRST COMANDA WHERE COMANDA.ID_COMANDA = FACTURA.ID_COMANDA.
                    poutDecTotal = poutDecTotal + COMANDA.PROPINA.
            END.
        END.
        ELSE DO:
            IF TRIM(ENTRY(2, pinCharFechas, "#")) = "" THEN DO:
                FOR EACH FACTURA WHERE FACTURA.ID_COMANDA > 0 AND
                    FACTURA.FECHA >= DATE(ENTRY(1, pinCharFechas, "#")):
                        FIND FIRST COMANDA WHERE COMANDA.ID_COMANDA = FACTURA.ID_COMANDA.
                        poutDecTotal = poutDecTotal + COMANDA.PROPINA.
                END.
            END.
            ELSE DO:
                FOR EACH FACTURA WHERE FACTURA.ID_COMANDA > 0 AND
                    FACTURA.FECHA >= DATE(ENTRY(1, pinCharFechas, "#")) AND
                    FACTURA.FECHA <= DATE(ENTRY(2, pinCharFechas, "#")):       
                        FIND FIRST COMANDA WHERE COMANDA.ID_COMANDA = FACTURA.ID_COMANDA.
                        poutDecTotal = poutDecTotal + COMANDA.PROPINA.
                END.
            END.
        END.        
    END.

    vdecMeserosT = poutDecTotal * 0.8.
    vdecCocinerosT = poutDecTotal * 0.15.
    vdecAdminT = poutDecTotal * 0.05.

    FOR EACH EMPLEADO:
        CASE EMPLEADO.ID_ROL:
            WHEN 1 THEN
                vintNumAd = vintNumAd + 1.
            WHEN 2 THEN
                vintNumMe = vintNumMe + 1.
            WHEN 4 THEN
                vintNumCo = vintNumCo + 1.
        END CASE.
    END.

    poutDecMe = vdecMeserosT / vintNumMe.
    poutDecCo = vdecCocinerosT / vintNumCo.
    poutDecAd = vdecAdminT / vintNumAd.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getDescClasificacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescClasificacion Method-Library 
FUNCTION getDescClasificacion RETURNS CHARACTER
  ( INPUT vintIdClasificacion AS INTEGER ) :
    /*------------------------------------------------------------------------------
        Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    FIND FIRST CLASIFICACION WHERE CLASIFICACION.ID_CLASIFICACION = vintIdClasificacion.

    RETURN CLASIFICACION.DESCRIPCION.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getReporteFactura) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getReporteFactura Method-Library 
FUNCTION getReporteFactura RETURNS CHARACTER
    ( INPUT vintFactura AS INTEGER ) :
    /*------------------------------------------------------------------------------
        Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcharReporte AS CHARACTER.

    FIND FIRST FACTURA WHERE FACTURA.ID_FACTURA = vintFactura.
    FIND FIRST FORMA_PAGO WHERE FORMA_PAGO.ID_PAGO = FACTURA.ID_PAGO.
    FIND FIRST COMANDA WHERE COMANDA.ID_COMANDA = FACTURA.ID_COMANDA.
    FIND FIRST MESA WHERE MESA.ID_MESA = COMANDA.ID_MESA.
    FIND FIRST EMPLEADO WHERE EMPLEADO.ID_EMPLEADO = COMANDA.ID_EMPLEADO.
    FIND FIRST PERSONA WHERE PERSONA.ID_PERSONA = EMPLEADO.ID_PERSONA.

    vcharReporte = vcharReporte + "<thead>~n<tr>~n".
    vcharReporte = vcharReporte + "<th colspan='2'>Folio: " + FACTURA.FOLIO + "</th>~n".
    vcharReporte = vcharReporte + "<th>" + STRING(FACTURA.FECHA) + " " + STRING(COMANDA.HORA_SALIDA) + "</th>~n</tr>~n<tr>~n".
    vcharReporte = vcharReporte + "<th colspan='2'>Mesa: " + MESA.DESCRIPCION + "</th>~n<th></th>~n</tr>~n<tr>~n".
    vcharReporte = vcharReporte + "<th colspan='2'>Mesero: " + PERSONA.NOMBRE + " " + PERSONA.A_PATERNO + " " + PERSONA.A_MATERNO + "</th>~n<th></th>~n</tr>~n<tr>~n".
    vcharReporte = vcharReporte + "<th colspan='3'>&nbsp;</th>~n</tr>~n</thead>~n".
    
    vcharReporte = vcharReporte + "<tbody>~n".
    FOR EACH CONSUMO WHERE CONSUMO.ID_COMANDA = COMANDA.ID_COMANDA:
        FIND FIRST MENU WHERE MENU.ID_MENU = CONSUMO.ID_MENU.
        vcharReporte = vcharReporte + "<tr>~n".
        vcharReporte = vcharReporte + "<td style='width: 5%'>" + STRING(CONSUMO.CANTIDAD) + "</td>~n".
        vcharReporte = vcharReporte + "<td style='width: 65%'>" + MENU.DESCRIPCION + "</td>~n".
        vcharReporte = vcharReporte + "<td style='width: 30%'>" + STRING(CONSUMO.CANTIDAD * MENU.PRECIO) + "</td>~n".
        vcharReporte = vcharReporte + "</tr>~n".
    END.

    vcharReporte = vcharReporte + "<tr>~n<td></td>~n".
    vcharReporte = vcharReporte + "<td class='right'>SUBTOTAL</td>~n".
    vcharReporte = vcharReporte + "<td>" + STRING(FACTURA.SUBTOTAL) + "</td>~n</tr>~n<tr>~n<td></td>~n".
    vcharReporte = vcharReporte + "<td class='right'>IVA</td>~n".
    vcharReporte = vcharReporte + "<td>" + STRING(FACTURA.IVA) + "</td>~n".
    vcharReporte = vcharReporte + "</tr>~n<tr>~n<td></td>~n".
    vcharReporte = vcharReporte + "<td class='right'>PROPINA</td>~n".
    vcharReporte = vcharReporte + "<td>" + STRING(COMANDA.PROPINA) + "</td>~n</tr>~n<tr>~n<td></td>~n".
    vcharReporte = vcharReporte + "<td class='right'>TOTAL</td>~n".
    vcharReporte = vcharReporte + "<td>" + STRING(FACTURA.TOTAL) + "</td>~n</tr>~n".
    vcharReporte = vcharReporte + "<tr>~n".
    vcharReporte = vcharReporte + "<td colspan='3' class='center'>" + "Forma de Pago: " + FORMA_PAGO.DESCRIPCION + "</td>~n".
    vcharReporte = vcharReporte + "</tr>~n<tr>~n".
    vcharReporte = vcharReporte + "<td colspan='3' class='center'><br><span>¡Gracias por su Preferencia!</span><br></td>~n".
    vcharReporte = vcharReporte + "</tr>~n".    
    vcharReporte = vcharReporte + "</tbody>".

    RETURN vcharReporte.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getReporteInventario) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getReporteInventario Method-Library 
FUNCTION getReporteInventario RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
        Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcharReporte AS CHARACTER.

    FOR EACH STOCK BY STOCK.CANTIDAD DESC:
        FIND FIRST PRODUCTO WHERE PRODUCTO.ID_PRODUCTO = STOCK.ID_PRODUCTO.
        FIND FIRST UNIDAD_MEDIDA WHERE UNIDAD_MEDIDA.ID_UNIDAD = PRODUCTO.ID_UNIDAD.
        IF STOCK.CANTIDAD <= 0 OR STOCK.F_CADUCIDAD <= TODAY THEN
            vcharReporte = vcharReporte + "<tr class='alta'>~n".
        ELSE
            IF PRODUCTO.CANT_MINIMA >= STOCK.CANTIDAD THEN
                vcharReporte = vcharReporte + "<tr class='media'>~n".
            ELSE
                vcharReporte = vcharReporte + "<tr class='normal'>~n".
        vcharReporte = vcharReporte + "<td>" + STOCK.LOTE + "</td>~n".
        vcharReporte = vcharReporte + "<td>" + PRODUCTO.DESCRIPCION + "</td>~n".
        vcharReporte = vcharReporte + "<td>" + STRING(STOCK.CANTIDAD) + " " + UNIDAD_MEDIDA.DESCRIPCION + "</td>~n".
        vcharReporte = vcharReporte + "<td>" + STRING(STOCK.F_CADUCIDAD) + "</td>~n</tr>~n".
    END.

    RETURN vcharReporte.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getReporteMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getReporteMenu Method-Library 
FUNCTION getReporteMenu RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
        Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcharReporte AS CHARACTER.
    DEFINE VARIABLE vintBandera AS INTEGER.
    DEFINE VARIABLE vlogPrev AS LOGICAL.
    
    FOR EACH MENU BY MENU.ID_CLASIFICACION:
        IF vintBandera <> MENU.ID_CLASIFICACION THEN DO:
            IF vlogPrev = TRUE THEN DO:
                vcharReporte = vcharReporte + "</ul>~n</div>~n".
                vlogPrev = FALSE.
            END.
            vcharReporte = vcharReporte + "<div class='clasificacion shadow'>~n".
            vcharReporte = vcharReporte + "<div class='titulo text-shadow'>" + getDescClasificacion(MENU.ID_CLASIFICACION) + "</div>~n".
            vcharReporte = vcharReporte + "<ul>~n".
            vcharReporte = vcharReporte + "<li>" + MENU.DESCRIPCION + "<div>$" + STRING(MENU.PRECIO) + "</div></li>~n".
            vlogPrev = TRUE.
        END.
        ELSE
            vcharReporte = vcharReporte + "<li>" + MENU.DESCRIPCION + "<div>$" + STRING(MENU.PRECIO) + "</div></li>~n".
        vintBandera = MENU.ID_CLASIFICACION.
    END.
    IF vlogPrev = TRUE THEN
        vcharReporte = vcharReporte + "</ul>~n</div>~n".
    RETURN vcharReporte.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getReportePropinas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getReportePropinas Method-Library 
FUNCTION getReportePropinas RETURNS CHARACTER
    ( INPUT vcharFechas AS CHARACTER ) :
    /*------------------------------------------------------------------------------
        Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vdecTotal AS DECIMAL.    
    DEFINE VARIABLE vdecSubMe AS DECIMAL.
    DEFINE VARIABLE vdecSubCo AS DECIMAL.
    DEFINE VARIABLE vdecSubAd AS DECIMAL.    
    DEFINE VARIABLE vcharReporte AS CHARACTER.

    RUN getProporciones(vcharFechas, OUTPUT vdecTotal,
        OUTPUT vdecSubMe, OUTPUT vdecSubCo, OUTPUT vdecSubAd ).
    
    vcharReporte = vcharReporte + "<tr class='sub-th'>~n".
    vcharReporte = vcharReporte + "<td colspan='2' class='left'>MESEROS</td>~n</tr>~n".

    FOR EACH EMPLEADO WHERE EMPLEADO.ID_ROL = 2:
        FIND FIRST PERSONA WHERE PERSONA.ID_PERSONA = EMPLEADO.ID_PERSONA.
        vcharReporte = vcharReporte + "<tr>~n<td class='right'>" + PERSONA.NOMBRE + " " + PERSONA.A_PATERNO + " " + PERSONA.A_MATERNO + "</td>~n".
        vcharReporte = vcharReporte + "<td>$" + STRING(vdecSubMe) + "</td>~n</tr>~n".
    END.

    vcharReporte = vcharReporte + "<tr class='sub-th'>~n".
    vcharReporte = vcharReporte + "<td colspan='2' class='left'>COCINEROS</td>~n</tr>~n".

    FOR EACH EMPLEADO WHERE EMPLEADO.ID_ROL = 4:
        FIND FIRST PERSONA WHERE PERSONA.ID_PERSONA = EMPLEADO.ID_PERSONA.
        vcharReporte = vcharReporte + "<tr>~n<td class='right'>" + PERSONA.NOMBRE + " " + PERSONA.A_PATERNO + " " + PERSONA.A_MATERNO + "</td>~n".
        vcharReporte = vcharReporte + "<td>$" + STRING(vdecSubCo) + "</td>~n</tr>~n".
    END.

    vcharReporte = vcharReporte + "<tr class='sub-th'>~n".
    vcharReporte = vcharReporte + "<td colspan='2' class='left'>ADMIN</td>~n</tr>~n".

    FOR EACH EMPLEADO WHERE EMPLEADO.ID_ROL = 1:
        FIND FIRST PERSONA WHERE PERSONA.ID_PERSONA = EMPLEADO.ID_PERSONA.
        vcharReporte = vcharReporte + "<tr>~n<td class='right'>" + PERSONA.NOMBRE + " " + PERSONA.A_PATERNO + " " + PERSONA.A_MATERNO + "</td>~n".
        vcharReporte = vcharReporte + "<td>$" + STRING(vdecSubAd) + "</td>~n</tr>~n".
    END.

    vcharReporte = vcharReporte + "<tr class='sub-th'>~n".
    vcharReporte = vcharReporte + "<td colspan='2' class='left'>TOTAL</td>~n</tr>~n".
    vcharReporte = vcharReporte + "<tr>~n<td></td>~n<td>$" + STRING(vdecTotal) + "</td>~n</tr>~n".
    RETURN vcharReporte.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getReporteVentas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getReporteVentas Method-Library 
FUNCTION getReporteVentas RETURNS CHARACTER
    ( INPUT vcharFechas AS CHARACTER ) :
    /*------------------------------------------------------------------------------
        Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcharReporte AS CHARACTER.

    IF TRIM(vcharFechas) = "#" THEN DO:
        FOR EACH FACTURA WHERE FACTURA.ID_COMANDA > 0:
            FIND FIRST COMANDA WHERE COMANDA.ID_COMANDA = FACTURA.ID_COMANDA.
            FIND FIRST ESTATUS WHERE ESTATUS.ID_ESTATUS = FACTURA.ID_ESTATUS.
            FIND FIRST MESA WHERE MESA.ID_MESA = COMANDA.ID_MESA.
            FIND FIRST EMPLEADO WHERE EMPLEADO.ID_EMPLEADO = COMANDA.ID_EMPLEADO.
            FIND FIRST PERSONA WHERE PERSONA.ID_PERSONA = EMPLEADO.ID_PERSONA.
                vcharReporte = vcharReporte + "<tr>~n".
                vcharReporte = vcharReporte + "<td>" + FACTURA.FOLIO + "</td>~n".
                vcharReporte = vcharReporte + "<td>" + MESA.DESCRIPCION + "</td>~n".
                vcharReporte = vcharReporte + "<td>" + PERSONA.NOMBRE + " " + PERSONA.A_PATERNO + " " + PERSONA.A_MATERNO + "</td>~n".
                vcharReporte = vcharReporte + "<td>" + STRING(FACTURA.FECHA) + "</td>~n".
                vcharReporte = vcharReporte + "<td>" + ESTATUS.DESCRIPCION + "</td>~n".
                vcharReporte = vcharReporte + "<td>" + STRING(FACTURA.TOTAL) + "</td>~n".
                vcharReporte = vcharReporte + "</tr>~n".    
        END.
    END.
    ELSE DO:
        IF TRIM(ENTRY(1, vcharFechas, "#")) = "" THEN DO:
            FOR EACH FACTURA WHERE FACTURA.ID_COMANDA > 0 AND
                FACTURA.FECHA <= DATE(ENTRY(2, vcharFechas, "#")):       
                    FIND FIRST COMANDA WHERE COMANDA.ID_COMANDA = FACTURA.ID_COMANDA.
                    FIND FIRST ESTATUS WHERE ESTATUS.ID_ESTATUS = FACTURA.ID_ESTATUS.
                    FIND FIRST MESA WHERE MESA.ID_MESA = COMANDA.ID_MESA.
                    FIND FIRST EMPLEADO WHERE EMPLEADO.ID_EMPLEADO = COMANDA.ID_EMPLEADO.
                    FIND FIRST PERSONA WHERE PERSONA.ID_PERSONA = EMPLEADO.ID_PERSONA.
                        vcharReporte = vcharReporte + "<tr>~n".
                        vcharReporte = vcharReporte + "<td>" + FACTURA.FOLIO + "</td>~n".
                        vcharReporte = vcharReporte + "<td>" + MESA.DESCRIPCION + "</td>~n".
                        vcharReporte = vcharReporte + "<td>" + PERSONA.NOMBRE + " " + PERSONA.A_PATERNO + " " + PERSONA.A_MATERNO + "</td>~n".
                        vcharReporte = vcharReporte + "<td>" + STRING(FACTURA.FECHA) + "</td>~n".
                        vcharReporte = vcharReporte + "<td>" + ESTATUS.DESCRIPCION + "</td>~n".
                        vcharReporte = vcharReporte + "<td>" + STRING(FACTURA.TOTAL) + "</td>~n".
                        vcharReporte = vcharReporte + "</tr>~n".
            END.
        END.
        ELSE DO:
            IF TRIM(ENTRY(2, vcharFechas, "#")) = "" THEN DO:
                FOR EACH FACTURA WHERE FACTURA.ID_COMANDA > 0 AND
                    FACTURA.FECHA >= DATE(ENTRY(1, vcharFechas, "#")):
                        FIND FIRST COMANDA WHERE COMANDA.ID_COMANDA = FACTURA.ID_COMANDA.
                        FIND FIRST ESTATUS WHERE ESTATUS.ID_ESTATUS = FACTURA.ID_ESTATUS.
                        FIND FIRST MESA WHERE MESA.ID_MESA = COMANDA.ID_MESA.
                        FIND FIRST EMPLEADO WHERE EMPLEADO.ID_EMPLEADO = COMANDA.ID_EMPLEADO.
                        FIND FIRST PERSONA WHERE PERSONA.ID_PERSONA = EMPLEADO.ID_PERSONA.
                            vcharReporte = vcharReporte + "<tr>~n".
                            vcharReporte = vcharReporte + "<td>" + FACTURA.FOLIO + "</td>~n".
                            vcharReporte = vcharReporte + "<td>" + MESA.DESCRIPCION + "</td>~n".
                            vcharReporte = vcharReporte + "<td>" + PERSONA.NOMBRE + " " + PERSONA.A_PATERNO + " " + PERSONA.A_MATERNO + "</td>~n".
                            vcharReporte = vcharReporte + "<td>" + STRING(FACTURA.FECHA) + "</td>~n".
                            vcharReporte = vcharReporte + "<td>" + ESTATUS.DESCRIPCION + "</td>~n".
                            vcharReporte = vcharReporte + "<td>" + STRING(FACTURA.TOTAL) + "</td>~n".
                            vcharReporte = vcharReporte + "</tr>~n".
                END.
            END.
            ELSE DO:
                FOR EACH FACTURA WHERE FACTURA.ID_COMANDA > 0 AND
                    FACTURA.FECHA >= DATE(ENTRY(1, vcharFechas, "#")) AND
                    FACTURA.FECHA <= DATE(ENTRY(2, vcharFechas, "#")):       
                        FIND FIRST COMANDA WHERE COMANDA.ID_COMANDA = FACTURA.ID_COMANDA.
                        FIND FIRST ESTATUS WHERE ESTATUS.ID_ESTATUS = FACTURA.ID_ESTATUS.
                        FIND FIRST MESA WHERE MESA.ID_MESA = COMANDA.ID_MESA.
                        FIND FIRST EMPLEADO WHERE EMPLEADO.ID_EMPLEADO = COMANDA.ID_EMPLEADO.
                        FIND FIRST PERSONA WHERE PERSONA.ID_PERSONA = EMPLEADO.ID_PERSONA.
                            vcharReporte = vcharReporte + "<tr>~n".
                            vcharReporte = vcharReporte + "<td>" + FACTURA.FOLIO + "</td>~n".
                            vcharReporte = vcharReporte + "<td>" + MESA.DESCRIPCION + "</td>~n".
                            vcharReporte = vcharReporte + "<td>" + PERSONA.NOMBRE + " " + PERSONA.A_PATERNO + " " + PERSONA.A_MATERNO + "</td>~n".
                            vcharReporte = vcharReporte + "<td>" + STRING(FACTURA.FECHA) + "</td>~n".
                            vcharReporte = vcharReporte + "<td>" + ESTATUS.DESCRIPCION + "</td>~n".
                            vcharReporte = vcharReporte + "<td>" + STRING(FACTURA.TOTAL) + "</td>~n".
                            vcharReporte = vcharReporte + "</tr>~n".
                END.
            END.
        END.        
    END.

    RETURN vcharReporte.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


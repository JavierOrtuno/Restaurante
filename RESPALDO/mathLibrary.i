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
&GLOBAL-DEFINE PI 3.1415926535897932384626433832795

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getCos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCos Method-Library 
FUNCTION getCos RETURNS DECIMAL
    ( INPUT vdecReg AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurveArea) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurveArea Method-Library 
FUNCTION getCurveArea RETURNS DECIMAL
  ( INPUT vintA AS INTEGER, INPUT vintB AS INTEGER, INPUT vintC AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurveVolume) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurveVolume Method-Library 
FUNCTION getCurveVolume RETURNS DECIMAL
    ( INPUT vintA AS INTEGER, INPUT vintB AS INTEGER, INPUT vintC AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDegToRad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDegToRad Method-Library 
FUNCTION getDegToRad RETURNS DECIMAL
    ( INPUT vdecDeg AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDistance) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDistance Method-Library 
FUNCTION getDistance RETURNS DECIMAL
    ( INPUT vdecX1 AS DECIMAL, INPUT vdecY1 AS DECIMAL,
      INPUT vdecX2 AS DECIMAL, INPUT vdecY2 AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFactorial) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFactorial Method-Library 
FUNCTION getFactorial RETURNS INTEGER
    ( INPUT vintValue AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getHypotenuse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getHypotenuse Method-Library 
FUNCTION getHypotenuse RETURNS DECIMAL
    ( INPUT vdecA AS DECIMAL, INPUT vdecB AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getInverseFactorial) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getInverseFactorial Method-Library 
FUNCTION getInverseFactorial RETURNS DECIMAL
  ( INPUT vintValue AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPerimeter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPerimeter Method-Library 
FUNCTION getPerimeter RETURNS DECIMAL
    ( INPUT vintLista AS CHARACTER, INPUT vcharSeparator AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPoligonArea) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPoligonArea Method-Library 
FUNCTION getPoligonArea RETURNS DECIMAL
    ( INPUT vcharListPoints AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getQuadratic) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQuadratic Method-Library 
FUNCTION getQuadratic RETURNS CHARACTER
  ( INPUT vintA AS INTEGER, INPUT vintB AS INTEGER, INPUT vintC AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRadius) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRadius Method-Library 
FUNCTION getRadius RETURNS DECIMAL
  ( INPUT vintA AS INTEGER, INPUT vintB AS INTEGER, INPUT vintC AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRadToDeg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRadToDeg Method-Library 
FUNCTION getRadToDeg RETURNS DECIMAL
    ( INPUT vdecRad AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSemiPerimeter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSemiPerimeter Method-Library 
FUNCTION getSemiPerimeter RETURNS DECIMAL
    ( INPUT vintLista AS CHARACTER, INPUT vcharSeparator AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSin Method-Library 
FUNCTION getSin RETURNS DECIMAL
    ( INPUT vdecDeg AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTan) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTan Method-Library 
FUNCTION getTan RETURNS DECIMAL
    ( INPUT vdecDeg AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTriangleArea) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTriangleArea Method-Library 
FUNCTION getTriangleArea RETURNS DECIMAL
    ( INPUT vcharPointA AS CHARACTER,
      INPUT vcharPointB AS CHARACTER,
      INPUT vcharPointC AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getVariance) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getVariance Method-Library 
FUNCTION getVariance RETURNS DECIMAL
    ( INPUT vcharList AS CHARACTER, INPUT vcharSeparator AS CHARACTER )  FORWARD.

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


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getCos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCos Method-Library 
FUNCTION getCos RETURNS DECIMAL
    ( INPUT vdecReg AS DECIMAL ) :
    /*------------------------------------------------------------------------------
        Purpose: 
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vdecCos AS DECIMAL.
    
    vdecCos = SQRT( 1 - EXP( getSin(vdecReg), 2) ).

    RETURN vdecCos.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurveArea) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurveArea Method-Library 
FUNCTION getCurveArea RETURNS DECIMAL
  ( INPUT vintA AS INTEGER, INPUT vintB AS INTEGER, INPUT vintC AS INTEGER ) :
    /*------------------------------------------------------------------------------
        Purpose: Obtener el Area Bajo la Curva de una Ecuación Cuadrática
        Notes: F(x) = 1/3 ax3 + 1/2 bx2 + cx
        Parameters:
            vintA: Elemento A de la Función Cuadrática
            vintB: Elemento B de la Función Cuadrática
            vintC: Elemento C de la Función Cuadrática
        Author: I.S.C. Fco Javier Ortuño Colchado
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcharX AS CHARACTER.
    DEFINE VARIABLE vdecX1 AS DECIMAL.
    DEFINE VARIABLE vdecX2 AS DECIMAL.
    DEFINE VARIABLE vdecArea AS DECIMAL.
    vcharX = getQuadratic(vintA, vintB, vintC).
    IF vcharX <> '?' THEN DO:
        vdecX1 = DECIMAL( ENTRY( 1, vcharX, "|" ) ).
        vdecX2 = DECIMAL( ENTRY( 2, vcharX, "|" ) ).

        vdecArea = ( ( ( vintA * EXP(vdecX2, 3) ) / 3 ) + ( ( vintB * EXP(vdecX2, 2) ) / 2 ) + ( vintC * vdecX2 ) ) - 
                   ( ( ( vintA * EXP(vdecX1, 3) ) / 3 ) + ( ( vintB * EXP(vdecX1, 2) ) / 2 ) + ( vintC * vdecX1 ) ).
        RETURN vdecArea.
    END.
    ELSE DO:
        RETURN ?.
    END.   
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurveVolume) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurveVolume Method-Library 
FUNCTION getCurveVolume RETURNS DECIMAL
    ( INPUT vintA AS INTEGER, INPUT vintB AS INTEGER, INPUT vintC AS INTEGER ) :
    /*------------------------------------------------------------------------------
        Purpose: Calculo de Volumen bajo una Curva dada por una Ecuación Cuadrática
        Notes: V = Pi * R2
        Parameters:
            vintA: Elemento A de la Cuadrática
            vintB: Elemento B de la Cuadrática
            vintC: Elemento C de la Cuadrática
        Author: I.S.C. Fco Javier Ortuño Colchado
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vdecArea AS DECIMAL.
    DEFINE VARIABLE vdecRadius AS DECIMAL.
    DEFINE VARIABLE vdecVolume AS DECIMAL.
    
    vdecArea = getCurveArea( vintA, vintB, vintC ).
    vdecRadius = getRadius( vintA, vintB, vintC ).
    vdecVolume = ( vdecArea ) * {&PI} * EXP(vdecRadius, 2).

    RETURN vdecVolume.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDegToRad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDegToRad Method-Library 
FUNCTION getDegToRad RETURNS DECIMAL
    ( INPUT vdecDeg AS DECIMAL ) :
    /*------------------------------------------------------------------------------
        Purpose: Convertir Grados a Radianes  
        Notes:  
        Parameters:
            vdecDeg: Grados
        Author: I.S.C. Fco. Javier Ortuño Colchado
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vdecRad AS DECIMAL.

    vdecRad = {&PI} * ( vdecDeg  / 180 ).
    RETURN vdecRad.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDistance) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDistance Method-Library 
FUNCTION getDistance RETURNS DECIMAL
    ( INPUT vdecX1 AS DECIMAL, INPUT vdecY1 AS DECIMAL,
      INPUT vdecX2 AS DECIMAL, INPUT vdecY2 AS DECIMAL ) :
    /*------------------------------------------------------------------------------
        Purpose: Calcular la Distancia entre dos puntos (X1, Y1)(X2, Y2)
        Notes:  
        Parameters:
            vdecX1: Valor X del Primer punto
            vdecY1: Valor Y del Primer punto
            vdecX2: Valor X del Segundo punto
            vdecY2: Valor Y del Segundo punto
        Author: 
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vdecimalDistance AS DECIMAL.
    vdecimalDistance = SQRT( EXP( vdecX2 - vdecX1, 2 ) + EXP( vdecY2 - vdecY1, 2 ) ).
    
    RETURN vdecimalDistance.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFactorial) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFactorial Method-Library 
FUNCTION getFactorial RETURNS INTEGER
    ( INPUT vintValue AS INTEGER ) :
    /*------------------------------------------------------------------------------
      Purpose: Obtener el Factorial de un Número.
        Notes:  
        Parameters:
            vintValue: Valor del Número que requiere el factorial.
        Author: I.S.C. Fco Javier Ortuño Colchado.
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vintCount AS INTEGER.
    DEFINE VARIABLE vintFactorial AS INTEGER INITIAL 1.
     
    DO vintCount = 1 TO vintValue:
        vintFactorial = vintFactorial * vintCount.
    END.

    RETURN vintFactorial.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getHypotenuse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getHypotenuse Method-Library 
FUNCTION getHypotenuse RETURNS DECIMAL
    ( INPUT vdecA AS DECIMAL, INPUT vdecB AS DECIMAL ) :
    /*------------------------------------------------------------------------------
        Purpose: Cálculo de la Hipotenusa en base al teorema de pitágoras
        Notes: 
        Parameters:
            vdecA: Cateto Opuesto
            vdecB: Cateto Adyacente
        Author: I.S.C. Fco Javier Ortuño Colchado
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vdecHypotenuse AS DECIMAL.
    vdecHypotenuse = SQRT ( EXP( vdecA, 2 ) + EXP( vdecB, 2 ) ).

    RETURN vdecHypotenuse.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getInverseFactorial) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getInverseFactorial Method-Library 
FUNCTION getInverseFactorial RETURNS DECIMAL
  ( INPUT vintValue AS INTEGER ) :
    /*------------------------------------------------------------------------------
      Purpose: Obtener el Factorial de un Número.
        Notes:  
        Parameters:
            vintValue: Valor del Número que requiere el factorial.
        Author: I.S.C. Fco Javier Ortuño Colchado.
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vintCount AS INTEGER.
    DEFINE VARIABLE vintFactorial AS DECIMAL INITIAL 1.
     
    DO vintCount = 1 TO vintValue:
        vintFactorial = vintFactorial * (1 / vintCount).
    END.

    RETURN vintFactorial.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPerimeter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPerimeter Method-Library 
FUNCTION getPerimeter RETURNS DECIMAL
    ( INPUT vintLista AS CHARACTER, INPUT vcharSeparator AS CHARACTER ) :
    /*------------------------------------------------------------------------------        
        Purpose: Calcular el Perímetro de una lista de Puntos
        Parámeters: vintLista(Lista de Valores), vcharSeparator(Separador de la Lista)
        Author: I.S.C. Fco. Javier Ortuño Colchado
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vintCount AS INTEGER.
    DEFINE VARIABLE vdecPerimeter AS DECIMAL.
    DO vintCount = 1 TO NUM-ENTRIES(vintLista, vcharSeparator):
        vdecPerimeter = vdecPerimeter + DECIMAL(ENTRY(vintCount, vintLista, vcharSeparator)).
    END.

    RETURN vdecPerimeter.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPoligonArea) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPoligonArea Method-Library 
FUNCTION getPoligonArea RETURNS DECIMAL
    ( INPUT vcharListPoints AS CHARACTER ) :
    /*------------------------------------------------------------------------------
        Purpose: Calcular el Área de un Polígono en Base a sus Coordenadas en el Plano.
        Notes:  
        Parameters:
            vcharListPoints: Lista de Puntos en el Plano ("1.0,10|34.09,20")
        Author: I.S.C. Fco. Javier Ortuño Colchado
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vintCount AS INTEGER.  
    DEFINE VARIABLE vintNumEntries AS INTEGER.
    DEFINE VARIABLE vdecTotalArea AS DECIMAL INITIAL 0.

    vintNumEntries = NUM-ENTRIES(vcharListPoints, '|') - 1.
    IF NUM-ENTRIES(vcharListPoints, '|') > 2 THEN DO:
        DO vintCount = 2 TO (vintNumEntries) :
            vdecTotalArea = vdecTotalArea + 
                            getTriangleArea( ENTRY(1, vcharListPoints, '|'),
                                             ENTRY(vintCount, vcharListPoints, '|'),
                                             ENTRY(vintCount + 1, vcharListPoints, '|') ).
        END.
    END.
    
    RETURN vdecTotalArea.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getQuadratic) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQuadratic Method-Library 
FUNCTION getQuadratic RETURNS CHARACTER
  ( INPUT vintA AS INTEGER, INPUT vintB AS INTEGER, INPUT vintC AS INTEGER ) :
    /*------------------------------------------------------------------------------
        Purpose: Calcular X1 y X2 en Base a una Ecuación Cuadrática
        Notes: AX2 + BX + C.
        Parameters: 
            vintA: Integer
            vintB: Integer
            vintC: Integer
        Author: I.S.C. Fco Javier Ortuño Colchado
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vdecRaiz AS DECIMAL INITIAL 0.
    DEFINE VARIABLE vdecX1 AS DECIMAL INITIAL 0.
    DEFINE VARIABLE vdecX2 AS DECIMAL INITIAL 0.

    vdecRaiz = SQRT( EXP( vintB, 2 ) - (4 * vintA * vintC) ).
    IF vdecRaiz = ? THEN DO:
        RETURN "?".
    END.
    ELSE DO:
        vdecX1 = ( - vintB + vdecRaiz ) / (2 * vintA).
        vdecX2 = ( - vintB - vdecRaiz ) / (2 * vintA).
        RETURN STRING( vdecX1 ) + "|" + STRING( vdecX2 ).
    END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRadius) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRadius Method-Library 
FUNCTION getRadius RETURNS DECIMAL
  ( INPUT vintA AS INTEGER, INPUT vintB AS INTEGER, INPUT vintC AS INTEGER ) :
    /*------------------------------------------------------------------------------
        Purpose: Obtener el Radio de una Curva dada por una Ecuación Cuadrática
        Notes: R =  - b / 2a
        Parameters:
            vintA: Elemento A de la Función Cuadrática
            vintB: Elemento B de la Función Cuadrática
        Author: I.S.C. Fco Javier Ortuño Colchado
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vdecXm AS DECIMAL.
    DEFINE VARIABLE vdecRadius AS DECIMAL.

    vdecXm = ( - vintB ) / ( 2 * vintA ).
    vdecRadius = ( vintA * EXP(vdecXm, 2) ) + ( vintB * vdecXm ) + ( vintC ).
    RETURN vdecRadius.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRadToDeg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRadToDeg Method-Library 
FUNCTION getRadToDeg RETURNS DECIMAL
    ( INPUT vdecRad AS DECIMAL ) :
    /*------------------------------------------------------------------------------
        Purpose: Convertir Radianes a Grados
        Notes:  
        Parameters:
            vdecRad: Radianes
        Author: I.S.C. Fco. Javier Ortuño Colchado
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vdecDeg AS DECIMAL.

    vdecDeg = ( vdecRad * 180 ) / ( {&PI} ).
    RETURN vdecDeg.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSemiPerimeter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSemiPerimeter Method-Library 
FUNCTION getSemiPerimeter RETURNS DECIMAL
    ( INPUT vintLista AS CHARACTER, INPUT vcharSeparator AS CHARACTER ) :
    /*------------------------------------------------------------------------------
        Purpose: Calcular el Semi-Perímetro de una lista de Puntos
        Parámeters: vintLista(Lista de Valores), vcharSeparator(Separador de la Lista)
        Author: I.S.C. Fco. Javier Ortuño Colchado
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vdecPerimeter AS DECIMAL.
    vdecPerimeter = getPerimeter(vintLista, vcharSeparator).

    RETURN vdecPerimeter / 2.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSin Method-Library 
FUNCTION getSin RETURNS DECIMAL
    ( INPUT vdecDeg AS DECIMAL ) :
    /*------------------------------------------------------------------------------
        Purpose: Cálculo del Seno de un Angulo en Grados
        Notes: 
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vintCount AS INTEGER.
    DEFINE VARIABLE vintIterator AS INTEGER INITIAL 20.
    DEFINE VARIABLE vdecRad AS DECIMAL.
    DEFINE VARIABLE vdecDegToRad AS DECIMAL.
    DEFINE VARIABLE vintPosition AS INTEGER.
    
    vdecDegToRad = getDegToRad(DECIMAL(vdecDeg MOD 360)).    

    DO vintCount = 0 TO vintIterator:
        vintPosition = 2 * vintCount + 1.
        vdecRad = vdecRad + 
            (
                EXP(-1, vintCount) * 
                (
                    EXP(vdecDegToRad, vintPosition) * getInverseFactorial(vintPosition)
                )
            ).
    END.

    RETURN vdecRad.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTan) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTan Method-Library 
FUNCTION getTan RETURNS DECIMAL
    ( INPUT vdecDeg AS DECIMAL ) :
    /*------------------------------------------------------------------------------
        Purpose: Cálculo de la Tangente de un Aángulo  
        Notes:  
        Parameters:
            vdecDeg: Grados
        Author: I.S.C. Fco Javier Ortuño
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vdecTan AS DECIMAL.
    DEFINE VARIABLE vdecSin AS DECIMAL.

    vdecSin = getSin(vdecDeg).
    vdecTan = vdecSin / ( SQRT( 1 - vdecSin ) ).
    RETURN vdecTan.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTriangleArea) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTriangleArea Method-Library 
FUNCTION getTriangleArea RETURNS DECIMAL
    ( INPUT vcharPointA AS CHARACTER,
      INPUT vcharPointB AS CHARACTER,
      INPUT vcharPointC AS CHARACTER ) :
    /*------------------------------------------------------------------------------
        Purpose: Calcular el Area de un Triángulo con la fórmula de Herón.
        Notes:          
        Parameters: 
            vcharPointA: Punto A (X,Y)            
            vcharPointB: Punto B (X,Y)
            vcharPointC: Punto C (X,Y)
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vdecArea AS DECIMAL.
    DEFINE VARIABLE vdecSemiPerimeter AS DECIMAL.
    DEFINE VARIABLE vdecDistanceA AS DECIMAL.
    DEFINE VARIABLE vdecDistanceB AS DECIMAL.
    DEFINE VARIABLE vdecDistanceC AS DECIMAL.

    vdecDistanceA = getDistance( DECIMAL( ENTRY( 1, vcharPointA, ',' ) ), 
                                 DECIMAL( ENTRY( 2, vcharPointA, ',' ) ),
                                 DECIMAL( ENTRY( 1, vcharPointB, ',' ) ),
                                 DECIMAL( ENTRY( 2, vcharPointB, ',' ) ) ).

    vdecDistanceB = getDistance( DECIMAL( ENTRY( 1, vcharPointB, ',' ) ),
                                 DECIMAL( ENTRY( 2, vcharPointB, ',' ) ),
                                 DECIMAL( ENTRY( 1, vcharPointC, ',' ) ),
                                 DECIMAL( ENTRY( 2, vcharPointC, ',' ) ) ).

    vdecDistanceC = getDistance( DECIMAL( ENTRY( 1, vcharPointC, ',' ) ),
                                 DECIMAL( ENTRY( 2, vcharPointC, ',' ) ),
                                 DECIMAL( ENTRY( 1, vcharPointA, ',' ) ),
                                 DECIMAL( ENTRY( 2, vcharPointA, ',' ) ) ).

    vdecSemiPerimeter = getSemiPerimeter( STRING(vdecDistanceA) + "," + 
                          STRING(vdecDistanceB) + "," + 
                          STRING(vdecDistanceC), "," ).
    
    vdecArea = SQRT( ABS ( vdecSemiPerimeter * 
                   ( vdecSemiPerimeter - vdecDistanceA ) * 
                   ( vdecSemiPerimeter - vdecDistanceB ) * 
                   ( vdecSemiPerimeter - vdecDistanceC ) ) ).

    RETURN vdecArea.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getVariance) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getVariance Method-Library 
FUNCTION getVariance RETURNS DECIMAL
    ( INPUT vcharList AS CHARACTER, INPUT vcharSeparator AS CHARACTER ) :
    /*------------------------------------------------------------------------------
        Purpose:  
        Notes:  
        Parameters:
            vcharList: Lista de valores numéricos
            vcharSeparator: Caracter Separador
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vintCount AS INTEGER.
    DEFINE VARIABLE vintSize AS INTEGER.
    DEFINE VARIABLE vdecAverage AS DECIMAL.
    DEFINE VARIABLE vdecVariance AS DECIMAL.
    DEFINE VARIABLE vdecValue AS DECIMAL.    

    vintSize = NUM-ENTRIES(vcharList, vcharSeparator).
    DO vintCount = 1 TO vintSize :
        vdecValue = DECIMAL( ENTRY(vintCount, vcharList, vcharSeparator) ).
        vdecAverage = vdecAverage + vdecValue.
        vdecVariance = vdecVariance + EXP(vdecValue, 2).
    END.
    vdecAverage = vdecAverage / vintSize.
    vdecVariance = vdecVariance / vintSize.

    RETURN SQRT( vdecVariance - vdecAverage ).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


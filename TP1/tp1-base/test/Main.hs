module Main (main) where

import App
import Expr
import Expr.Parser
import GHC.Stack (HasCallStack)
import Generador
import Histograma
import Test.HUnit
import Util
import System.Random (genByteString)
import GHC.Read (list)

main :: IO ()
main = runTestTTAndExit allTests

-- | Función auxiliar para marcar tests como pendientes a completar
completar :: (HasCallStack) => Test
completar = TestCase (assertFailure "COMPLETAR")

totalEnCasilleros :: Histograma -> Int
totalEnCasilleros = sum . map casCantidad . casilleros

sumaDePorcentajes :: [Casillero] -> Float
sumaDePorcentajes = sum . map casPorcentaje

unCasilleroConTodo :: [Casillero] -> Bool
unCasilleroConTodo cs = length (filter (\c -> casPorcentaje c == 100.0) cs) == 1

listaDeNumerosEnExpresion :: Expr -> [Float]
listaDeNumerosEnExpresion = foldExpr (\x -> [x]) (\_ _ -> []) (++) (++) (++) (++)

allTests :: Test
allTests =
  test
    [ "Ej 1 - Util.alinearDerecha" ~: testsAlinearDerecha,
      "Ej 2 - Util.actualizarElem" ~: testsActualizarElem,
      "Ej 3 - Histograma.vacio" ~: testsVacio,
      "Ej 4 - Histograma.agregar" ~: testsAgregar,
      "Ej 5 - Histograma.histograma" ~: testsHistograma,
      "Ej 6 - Histograma.casilleros" ~: testsCasilleros,
      "Ej 7 - Expr.recrExpr" ~: testsRecr,
      "Ej 7 - Expr.foldExpr" ~: testsFold,
      "Ej 8 - Expr.eval" ~: testsEval,
      "Ej 9 - Expr.armarHistograma" ~: testsArmarHistograma,
      "Ej 10 - Expr.evalHistograma" ~: testsEvalHistograma,
      "Ej 11 - Expr.mostrar" ~: testsMostrar,
      "Expr.Parser.parse" ~: testsParse,
      "App.mostrarFloat" ~: testsMostrarFloat,
      "App.mostrarHistograma" ~: testsMostrarHistograma
    ]

testsAlinearDerecha :: Test
testsAlinearDerecha =
  test
    [ alinearDerecha 6 "hola" ~?= "  hola",
      alinearDerecha 10 "incierticalc" ~?= "incierticalc",
      alinearDerecha 0 "estrellaCulona" ~?= "estrellaCulona"
    ]

testsActualizarElem :: Test
testsActualizarElem =
  test
    [ actualizarElem 0 (+ 10) [1, 2, 3] ~?= [11, 2, 3],
      actualizarElem 1 (+ 10) [1, 2, 3] ~?= [1, 12, 3],
      actualizarElem 4 (+ 10) [1,2,3,4,5,6,7,8] ~?= [1,2,3,4, 15, 6, 7 ,8]
    ]

testsVacio :: Test
testsVacio =
  test
    [ casilleros (vacio 1 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2 0 0,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ],
      casilleros (vacio 4 (0,16))
      ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 4 0 0,
              Casillero 4 8 0 0,
              Casillero 8 12 0 0,
              Casillero 12 16 0 0,
              Casillero 16 infinitoPositivo 0 0
            ]

    ]

testsAgregar :: Test
testsAgregar =
  let h0 = vacio 3 (0, 6)
   in test
        [ casilleros (agregar 0 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100, -- El 100% de los valores están acá
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 2 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 100, -- El 100% de los valores están acá
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar (-1) h0)
            ~?= [ Casillero infinitoNegativo 0 1 100, -- El 100% de los valores están acá
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar (-1) (agregar 2 h0))
           ~?= [ Casillero infinitoNegativo 0 1 50,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 50,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ]
        ]

testsHistograma :: Test
testsHistograma =
  test
    [ histograma 4 (1, 5) [1, 2, 3] ~?= agregar 3 (agregar 2 (agregar 1 (vacio 4 (1, 5)))),
      histograma 4 (0, 7) [4, 6, 8] ~?= agregar 8 (agregar 6 (agregar 4 (vacio 4 (0, 7))))
    ]

testsCasilleros :: Test
testsCasilleros =
  test
    [ casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (agregar 2 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 1 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (agregar 4 (agregar 2 (vacio 3 (0, 6))))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 1 50.0,
              Casillero 4.0 6.0 1 50.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ]

    ]

testsRecr :: Test
testsRecr =
  let sumaUnoA = Suma (Const 1.0)
      restaUnoA = Resta (Const 1.0)
      mult2y3 = Mult (Const 2.0) (Const 3.0)
      div6y3 = Div (Const 6.0) (Const 3.0)

      fRangoNula = (\_ _ -> 0)
      fSuma = (\_ _ x y -> x + y)
      fResta = (\_ _ x y -> x - y)
      fMulti = (\_ _ x y -> x * y)
      fDiv = (\_ _ x y -> x / y)

      fSumaConRec ex ey x y = fst (eval ex (genNormalConSemilla 0)) + fst (eval ey (genNormalConSemilla 0)) + x + y
      fRestaConRec ex ey x y = fst (eval ex (genNormalConSemilla 0)) - fst (eval ey (genNormalConSemilla 0)) + x - y
      fMultiConRec ex ey x y = fst (eval ex (genNormalConSemilla 0)) * fst (eval ey (genNormalConSemilla 0)) + x * y
      fDivConRec ex ey x y = fst (eval ex (genNormalConSemilla 0)) / fst (eval ey (genNormalConSemilla 0)) + x / y


   in
  test
    [
    recrExpr id fRangoNula fSuma fResta fMulti fDiv (sumaUnoA mult2y3) ~?= 7.0,
    recrExpr id fRangoNula fSuma fResta fMulti fDiv (restaUnoA mult2y3) ~?= -5.0,
    recrExpr id fRangoNula fSuma fResta fMulti fDiv mult2y3 ~?= 6.0,
    recrExpr id fRangoNula fSuma fResta fMulti fDiv div6y3 ~?= 2.0,

    --Rango
    recrExpr id (\x _ -> x) fSuma fResta fMulti fDiv (Mult (Rango 2 6) (Const 6.0)) ~?= 12.0,
    recrExpr id (\_ y -> y) fSuma fResta fMulti fDiv (Mult (Rango 2 6) (Const 6.0)) ~?= 36.0,
    recrExpr id (\x y -> (x + y) / 2) fSuma fResta fMulti fDiv (Mult (Rango 2 6) (Const 6.0)) ~?= 24.0,

    -- Invertidos
    recrExpr id (\_ _ -> 0) fResta fSuma fMulti fDiv (sumaUnoA mult2y3) ~?= -5.0,
    recrExpr id (\_ _ -> 0) fResta fSuma fMulti fDiv (restaUnoA mult2y3) ~?= 7.0,
    recrExpr id (\_ _ -> 0) fSuma fResta fDiv fMulti mult2y3 ~?= 0.6666667,
    recrExpr id (\_ _ -> 0) fSuma fResta fDiv fMulti div6y3 ~?= 18.0,

    -- Usando el cuerpo de la recursión para obtener la suma de las cuentas
    recrExpr id (\_ _ -> 0) fSumaConRec fResta fMulti fDiv (sumaUnoA mult2y3) ~?= 14.0,
    recrExpr id (\_ _ -> 0) fSuma fRestaConRec fMulti fDiv (restaUnoA mult2y3) ~?= -10.0,
    recrExpr id (\_ _ -> 0) fSuma fResta fMultiConRec fDiv mult2y3 ~?= 12.0,
    recrExpr id (\_ _ -> 0) fSuma fResta fMulti fDivConRec div6y3 ~?= 4.0
    ]

testsFold :: Test
testsFold =
  let sumaUnoA = Suma (Const 1.0)
      restaUnoA = Resta (Const 1.0)
      mult2y3 = Mult (Const 2.0) (Const 3.0)
      div6y3 = Div (Const 6.0) (Const 3.0)



   in
  test
    [
    foldExpr id (\_ _ -> 0) (+) (-) (*) (/) (sumaUnoA mult2y3) ~?= 7.0,
    foldExpr id (\_ _ -> 0) (+) (-) (*) (/) (restaUnoA mult2y3) ~?= -5.0,
    foldExpr id (\_ _ -> 0) (+) (-) (*) (/) mult2y3 ~?= 6.0,
    foldExpr id (\_ _ -> 0) (+) (-) (*) (/) div6y3 ~?= 2.0,

    --Rango
    foldExpr id (\x _ -> x) (+) (-) (*) (/) (Mult (Rango 2 6) (Const 6.0)) ~?= 12.0,
    foldExpr id (\_ y -> y) (+) (-) (*) (/) (Mult (Rango 2 6) (Const 6.0)) ~?= 36.0,
    foldExpr id (\x y -> (x + y) / 2) (+) (-) (*) (/) (Mult (Rango 2 6) (Const 6.0)) ~?= 24.0,

    -- Invertidos
    foldExpr id (\_ _ -> 0) (-) (+) (*) (/) (sumaUnoA mult2y3) ~?= -5.0,
    foldExpr id (\_ _ -> 0) (-) (+) (*) (/) (restaUnoA mult2y3) ~?= 7.0,
    foldExpr id (\_ _ -> 0) (+) (-) (/) (*) mult2y3 ~?= 0.6666667,
    foldExpr id (\_ _ -> 0) (+) (-) (/) (*) div6y3 ~?= 18.0,

    listaDeNumerosEnExpresion (restaUnoA div6y3) ~?= [1.0,6.0,3.0],
    listaDeNumerosEnExpresion (sumaUnoA (Suma (Const 2) (Const 3))) ~?= [1.0,2.0,3.0]

    ]

testsEval :: Test
testsEval =
  test
    [ fst (eval (Suma (Rango 1 5) (Const 1)) genFijo) ~?= 4.0,
      fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 3.7980492,
      -- el primer rango evalua a 2.7980492 y el segundo a 3.1250308
      fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 5.92308
    ]

testsArmarHistograma :: Test
testsArmarHistograma =
  let  histograma_grande_semilla10 = fst (armarHistograma 10 100 (dameUno (1, 5)) (genNormalConSemilla 10))
       histograma_grande_fijo = fst (armarHistograma 10 100 (dameUno (1, 5)) genFijo)

       histograma_chico_fijo = fst (armarHistograma 3 100 (dameUno (1, 5)) genFijo)
       histograma_chico_semilla10 = fst (armarHistograma 3 100 (dameUno (1, 5)) (genNormalConSemilla 10))
       histograma_chico_semilla20 = fst (armarHistograma 3 100 (dameUno (1, 5)) (genNormalConSemilla 20))
   in
  test
    [ length (casilleros histograma_grande_semilla10) ~?= 12,
      totalEnCasilleros histograma_grande_semilla10 ~?= 100,
      sumaDePorcentajes (casilleros histograma_grande_semilla10) ~?= 100.0,
      unCasilleroConTodo (casilleros histograma_grande_semilla10) ~?= False,


      length (casilleros histograma_grande_fijo) ~?= 12,
      totalEnCasilleros histograma_grande_fijo ~?= 100,
      sumaDePorcentajes (casilleros histograma_grande_fijo) ~?= 100.0,
      unCasilleroConTodo (casilleros histograma_grande_fijo) ~?= True,

      length (casilleros histograma_chico_fijo) ~?= 5,
      totalEnCasilleros histograma_chico_fijo ~?= 100,
      sumaDePorcentajes (casilleros histograma_chico_fijo) ~?= 100.0,
      unCasilleroConTodo (casilleros histograma_chico_fijo) ~?= True,

      length (casilleros histograma_chico_semilla10) ~?= 5,
      totalEnCasilleros histograma_chico_semilla10 ~?= 100,
      sumaDePorcentajes (casilleros histograma_chico_semilla10) ~?= 100.0,
      unCasilleroConTodo (casilleros histograma_chico_semilla10) ~?= False,


      length (casilleros histograma_chico_semilla20) ~?= length (casilleros histograma_chico_semilla10),
      totalEnCasilleros histograma_chico_semilla20 ~?= 100,
      sumaDePorcentajes (casilleros histograma_chico_semilla20) ~?= 100.0,
      unCasilleroConTodo (casilleros histograma_chico_semilla20) ~?= False,


      let (h,_) = armarHistograma 3 5 (dameUno (2, 6)) genFijo in
      casilleros h ~?= [ Casillero infinitoNegativo 3.0 0 0.0,
                         Casillero 3.0 3.6666667 0 0.0,
                         Casillero 3.6666667 4.3333335 5 100.0,
                         Casillero 4.3333335 5.0 0 0.0,
                         Casillero 5.0 infinitoPositivo 0 0.0
                       ],

      let (h,_) = armarHistograma 10 100 (dameUno (2, 6)) genFijo in
      casilleros h ~?= [ Casillero infinitoNegativo 3.0 0 0.0,
                         Casillero 3.0 3.2 0 0.0,
                         Casillero 3.2 3.4 0 0.0,
                         Casillero 3.4 3.6000001 0 0.0,
                         Casillero 3.6000001 3.8000002 0 0.0,
                         Casillero 3.8000002 4.0 0 0.0,
                         Casillero 4.0 4.2000003 100 100.0,
                         Casillero 4.2000003 4.4000006 0 0.0,
                         Casillero 4.4000006 4.6000004 0 0.0,
                         Casillero 4.6000004 4.8 0 0.0,
                         Casillero 4.8 5.0000005 0 0.0,
                         Casillero 5.0000005 infinitoPositivo 0 0.0
                       ]
    ]

testsEvalHistograma :: Test
testsEvalHistograma =
  let  eval_sinRangos_semilla10 =  evalHistograma 3 5 (Suma (Const 2) (Const 1)) (genNormalConSemilla 10)
       eval_sinRangos_fijo =  evalHistograma 3 5 (Suma (Const 2) (Const 1)) genFijo

       eval_sumaRangos_semilla10 =  evalHistograma 3 5 (Suma (Rango 1 5) (Rango 2 5)) (genNormalConSemilla 10)
       eval_sumaRangosInvertidos_semilla10 =  evalHistograma 3 5 (Suma (Rango 2 5) (Rango 1 5)) (genNormalConSemilla 10)

       eval_chico_semilla20 = evalHistograma 3 5 (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 20)
       eval_chico_fijo =  evalHistograma 3 5 (Suma (Rango 1 5) (Const 1)) genFijo
   in
  test    [
      length (casilleros (fst eval_sinRangos_semilla10)) ~?= length (casilleros (fst eval_sinRangos_fijo)),
      casilleros (fst eval_sinRangos_semilla10) ~?= casilleros (fst eval_sinRangos_fijo), --Sin rangos no hay elemenots aleatorios

      length (casilleros (fst eval_sumaRangos_semilla10)) ~?= length (casilleros (fst eval_sumaRangosInvertidos_semilla10)), --Misma Longitud
      False ~?= casilleros (fst eval_sumaRangos_semilla10) == casilleros (fst eval_sumaRangosInvertidos_semilla10), -- El orden de los rangos afecta el resultado

      totalEnCasilleros (fst eval_chico_semilla20) ~?= 5,
      totalEnCasilleros (fst eval_chico_fijo) ~?= 5,
      False ~?= casilleros (fst eval_chico_semilla20) == casilleros (fst eval_chico_fijo), -- Distintas semillas, distintos resultados

      unCasilleroConTodo (casilleros (fst eval_chico_fijo)) ~?= True, --Semilla fija no hay variacion
      unCasilleroConTodo (casilleros (fst eval_sinRangos_fijo)) ~?= True,

      let (h,_) = eval_chico_fijo in
      casilleros h ~?= [ Casillero infinitoNegativo 3.0 0 0.0,
                         Casillero 3.0 3.6666667 0 0.0,
                         Casillero 3.6666667 4.3333335 5 100.0,
                         Casillero 4.3333335 5.0 0 0.0,
                         Casillero 5.0 infinitoPositivo 0 0.0
                       ]
  ]

testsParse :: Test
testsParse =
  test
    [ parse "1" ~?= Const 1.0,
      parse "-1.7 ~ -0.5" ~?= Rango (-1.7) (-0.5),
      parse "1+2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2 * 3" ~?= Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0)),
      parse "1 + 2 + 3" ~?= Suma (Suma (Const 1.0) (Const 2.0)) (Const 3.0),
      parse "1 + (2 + 3)" ~?= Suma (Const 1.0) (Suma (Const 2.0) (Const 3.0)),
      parse "1 + 2 ~ 3 + 4" ~?= Suma (Suma (Const 1.0) (Rango 2.0 3.0)) (Const 4.0),
      parse "1 - 2 - 3 - 4" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "(((1 - 2) - 3) - 4)" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "1 " ~?= Const 1.0,
      parse "   1    " ~?= Const 1.0
    ]

testsMostrar :: Test
testsMostrar =
  test
    [ mostrar (Div (Suma (Rango 1 5) (Mult (Const 3) (Rango 100 105))) (Const 2))
        ~?= "(1.0~5.0 + (3.0 * 100.0~105.0)) / 2.0",
      mostrar (Suma (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Const 1) (Suma (Const 2) (Suma (Const 3) (Const 4))))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Suma (Const 1) (Const 2)) (Suma (Const 3) (Const 4)))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Mult (Mult (Mult (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Const 1) (Mult (Const 2) (Mult (Const 3) (Const 4))))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Mult (Const 1) (Const 2)) (Mult (Const 3) (Const 4)))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Resta (Resta (Const 1) (Const 2)) (Resta (Const 3) (Const 4)))
        ~?= "(1.0 - 2.0) - (3.0 - 4.0)",
      mostrar (Resta (Resta (Resta (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 - 2.0) - 3.0) - 4.0",
      mostrar (Suma (Mult (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 + 2.0) * 3.0) + 4.0",
      mostrar (Mult (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "(1.0 + 2.0 + 3.0) * 4.0"
    ]

testsMostrarFloat :: Test
testsMostrarFloat =
  test
    [ mostrarFloat 0.0 ~?= "0.00",
      mostrarFloat 1.0 ~?= "1.00",
      mostrarFloat (-1.0) ~?= "-1.00",
      -- Redondeo
      mostrarFloat 3.14159 ~?= "3.14",
      mostrarFloat 2.71828 ~?= "2.72",
      mostrarFloat 0.000001 ~?= "1.00e-6",
      mostrarFloat 100000 ~?= "100000.00",
      -- Infinitos
      mostrarFloat infinitoPositivo ~?= "+inf",
      mostrarFloat infinitoNegativo ~?= "-inf"
    ]

testsMostrarHistograma :: Test
testsMostrarHistograma =
  let h0 = vacio 3 (0, 6)
      h123 = agregar 1 (agregar 2 (agregar 3 h0))
   in test
        [ lines (mostrarHistograma h123)
            ~?= [ "6.00 - +inf |",
                  "4.00 - 6.00 |",
                  "2.00 - 4.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 66.67%",
                  "0.00 - 2.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒",
                  "-inf - 0.00 |"
                ],
          lines (mostrarHistograma (agregar 1 (vacio 3 (0, 1000))))
            ~?= [ "  1000.00 - +inf |",
                  "666.67 - 1000.00 |",
                  " 333.33 - 666.67 |",
                  "   0.00 - 333.33 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 100.00%",
                  "     -inf - 0.00 |"
                ]
        ]

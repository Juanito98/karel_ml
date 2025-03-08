open! Core
module Runtime = Karel_compiler.Runtime
module World_in = Karel_compiler.World_in

let%expect_test _ =
  let t =
    World_in.load_xml
      {|
<ejecucion>
	<condiciones instruccionesMaximasAEjecutar="10000000" longitudStack="65000"></condiciones>
	<mundos>
		<mundo nombre="mundo_0" ancho="10" alto="10">
			<monton x="1" y="3" zumbadores="1"></monton>
			<pared x1="2" y1="2" y2="3"></pared>
			<pared x1="2" y1="4" x2="3"></pared>
		</mundo>
	</mundos>
	<programas tipoEjecucion="CONTINUA" intruccionesCambioContexto="1" milisegundosParaPasoAutomatico="0">
		<programa nombre="p1" ruta="{$2$}" mundoDeEjecucion="mundo_0" xKarel="1" yKarel="2" direccionKarel="NORTE" mochilaKarel="434">
			<despliega tipo="POSICION"></despliega>
			<despliega tipo="ORIENTACION"></despliega>
			<despliega tipo="MOCHILA"></despliega>
			<despliega tipo="UNIVERSO"></despliega>
			<despliega tipo="AVANZA"></despliega>
			<despliega tipo="GIRA_IZQUIERDA"></despliega>
			<despliega tipo="COGE_ZUMBADOR"></despliega>
			<despliega tipo="DEJA_ZUMBADOR"></despliega>
		</programa>
	</programas>
</ejecucion>
  |}
  in

  print_s [%sexp (t : Runtime.t)];
  [%expect
    {|
    stack limit: 65000
    ((pc 0) (line 0) (function_stack ()) (expression_stack ())
     (world
      ((width 10) (height 10) (position (1 2)) (orientation North) (bag 434)
       (beepers (((1 3) 1))) (walls (((2 3) 4) ((3 3) 1) ((3 4) 2) ((3 5) 8)))))
     (ic ((counter 0) (limit (10000000))))
     (left_counter ((counter 0) (limit ())))
     (forward_counter ((counter 0) (limit ())))
     (pickbuzzer_counter ((counter 0) (limit ())))
     (leavebuzzer_counter ((counter 0) (limit ())))) |}]

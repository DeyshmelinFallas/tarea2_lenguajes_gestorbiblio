(* tipo que representa un libro, con nombre a cada campo *)
type libro = {codigo:string, autor:string, genero:string, fecha:string, copias:int}


(* lee lo que el usuario escribe y quita el salto de linea *)
fun quitarSalto cadena =
  if String.isSuffix "\n" cadena then String.substring(cadena, 0, size cadena - 1)
  else cadena 

fun leerLinea () =
  case TextIO.inputLine TextIO.stdIn of
      SOME cadena => quitarSalto cadena
    | NONE => ""


(* --- convertir una linea de texto en un libro --- *)
(* String.fields separa el texto por comas y devuelve una lista
si la linea no tiene 5 campos o las copias no son un numero,
se descarta devolviendo NONE*)
fun parsear cadena =
  case String.fields (fn cod => cod = #",") (quitarSalto cadena) of
      [cod, autor, gen, fecha, numCopias] =>
        (case Int.fromString numCopias of
             SOME numCopias => SOME {codigo=cod, autor=autor, genero=gen, fecha=fecha, copias=numCopias}
           | NONE => NONE)
    | _ => NONE


(*lee todo el archivo y lo hace una lista de libros*)
fun leerLibros ruta =
  let
    val ins = TextIO.openIn ruta

    (*loop lee linea por linea y las guarda en una lista *)
    fun loop acc =
      case TextIO.inputLine ins of
          NONE => rev acc                 (* NONE = ya no hay mas lineas *)
        | SOME libIndividual => loop (libIndividual :: acc)

    val lineas = loop []
  in
    TextIO.closeIn ins;
    (* tl = quita el primer elemento (el encabezado) *)
    (* List.mapPartial = aplica parsear y se queda solo con los SOME *)
    List.mapPartial parsear (tl lineas)
  end


(* imprime un libro en pantalla con formato legible *)
fun imprimirLibro (libIndividual: libro) =
  print ("Codigo: " ^ #codigo libIndividual ^
         " | Fecha: " ^ #fecha libIndividual ^
         " | Autor: " ^ #autor libIndividual ^
         " | Genero: " ^ #genero libIndividual ^
         " | Copias: " ^ Int.toString (#copias libIndividual) ^ "\n")
         
(*funciones de rango de copias*)

(* deja solo los libros con copias entre copiasMin y copiasMax *)
fun enRango copiasMin copiasMax (libros: libro list) =
  List.filter (fn lib => #copias lib >= copiasMin andalso #copias lib <= copiasMax) libros

(* ordeno de mayor a menor copias con insertion sort, es el mas facil de entender *)
fun insertar (x: libro) [] = [x]
  | insertar x (y :: ys) =
      if #copias x >= #copias y then x :: y :: ys
      else y :: insertar x ys

fun ordenar (libros: libro list) =
  List.foldl (fn (x, acc) => insertar x acc) [] libros

fun opcionA (libros: libro list) =
  let
    val _ = print "Copias minimas: "
    val copiasMin = valOf (Int.fromString (leerLinea ()))
    val _ = print "Copias maximas: "
    val copiasMax = valOf (Int.fromString (leerLinea ()))
    val resultado = ordenar (enRango copiasMin copiasMax libros)
  in
    app imprimirLibro resultado
  end

  
(*funciones de autores con 5 o mas libros*)

(* voy contando cuantas veces se repite una clave (autor, genero, etc) *)
(* devuelve una lista de pares (clave, cantidad)                       *)
fun contar clave [] = [(clave, 1)]
  | contar clave ((datoActual, n) :: resto) =
      if datoActual = clave then (datoActual, n + 1) :: resto
      else (datoActual, n) :: contar clave resto

fun opcionB (libros: libro list) =
  let
    val porAutor = List.foldl (fn (libIndividual, acc) => contar (#autor libIndividual) acc) [] libros
    val conCinco = List.filter (fn (autor, n) => n >= 5) porAutor
  in
    app (fn (autor, n) => print (autor ^ ": " ^ Int.toString n ^ " libros\n")) conCinco
  end


(* funciones de buscar por codigo o autor *)

fun opcionC (libros: libro list) =
  let
    val _ = print "Codigo o autor a buscar: "
    val texto = leerLinea ()
    val resultado =
      List.filter (fn libIndividual => #codigo libIndividual = texto orelse #autor libIndividual = texto) libros
  in
    app imprimirLibro resultado
  end


(*funcionws de consultar la cantidad de libros por genero *)

fun opcionD (libros: libro list) =
  let
    val _ = print "Genero a consultar: "
    val genero = leerLinea ()
    val resultado = List.filter (fn libs => #genero libs = genero) libros
  in
    print ("Cantidad de libros en " ^ genero ^ ": " ^
           Int.toString (length resultado) ^ "\n")
  end


(*funciones de resumen general*)

(* recorre una lista de pares (clave, cantidad) y devuelve el mas alto *)
fun masAlto [] = ("", 0)
  | masAlto [x] = x
  | masAlto ((clave, n) :: resto) =
      let val (clave2, n2) = masAlto resto
      in if n >= n2 then (clave, n) else (clave2, n2) end

fun opcionE (libros: libro list) =
  let
    val porGenero = List.foldl (fn (libIndividual, acc) => contar (#genero libIndividual) acc) [] libros
    val porAutor  = List.foldl (fn (libIndividual, acc) => contar (#autor libIndividual) acc) [] libros
    (* uso los primeros 7 caracteres de la fecha para agrupar por mes/año *)
    val porMes = List.foldl
                   (fn (libIndividual, acc) => contar (String.substring (#fecha libIndividual
                   , 0, 7)) acc)
                   [] libros

    val librosConCopias = map (fn libIndividual => (#codigo libIndividual, #copias libIndividual)) libros
    val libroMasCopias = masAlto librosConCopias
    val (autorTop, _)  = masAlto porAutor
    val (generoTop, _) = masAlto porGenero
    val (mesTop, _)    = masAlto porMes
  in
    print "\n=== RESUMEN GENERAL ===\n";
    print "Libros por genero:\n";
    app (fn (genero, n) => print ("  " ^ genero ^ ": " ^ Int.toString n ^ "\n")) porGenero;
    print ("Libro con mas copias: " ^ #1 libroMasCopias ^
           " (" ^ Int.toString (#2 libroMasCopias) ^ " copias)\n");
    print ("Autor con mas libros: " ^ autorTop ^ "\n");
    print ("Genero con mas libros: " ^ generoTop ^ "\n");
    print ("Mes-año con mas publicaciones: " ^ mesTop ^ "\n")
  end


(*menu*)
fun menu libros =
  (
    print "\n=== ANALIZADOR ===\n";
    print "a) Libros por rango de copias\n";
    print "b) Autores con 5 o mas libros\n";
    print "c) Buscar por codigo o autor\n";
    print "d) Cantidad de libros por genero\n";
    print "e) Resumen general\n";
    print "0) Salir\n";
    print "Opcion: ";
    case leerLinea () of
        "a" => (opcionA libros; menu libros)
      | "b" => (opcionB libros; menu libros)
      | "c" => (opcionC libros; menu libros)
      | "d" => (opcionD libros; menu libros)
      | "e" => (opcionE libros; menu libros)
      | "0" => print "Graciaas por usar nuestro gestor bibliotecario:D\n"
      | _   => (print "Opcion invalida.\n"; menu libros)
  )


(*funcion para empezAr el programa *)
fun main () =
  (
    print "Ruta del archivo csv (ejemplo /tmp/datalibros.csv): ";
    let
      val ruta = leerLinea ()
      val libros = leerLibros ruta
    in
      menu libros
    end
  )
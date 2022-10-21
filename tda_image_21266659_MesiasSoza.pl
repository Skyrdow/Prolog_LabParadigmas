:- use_module(tda_pixel).

% TDA Imagen
% Dominio: Ancho (int), Alto (int), Valor de Compresión (int | lista | string), Pixeles (lista)
% Recorrido: Lista
% Ancho y Alto deben ser positivos, los pixeles ingresados deben ser homogeneos y deben completar la imagen totalmente.

% Predicados:
% image(Width, Height, Pixs, Imagen)
% imageGetPixs(Imagen, Pixs).
% imageGetComp(Imagen, Comp).
% imageSetComp(ImagenEntrada, ValorNuevo, ImagenSalida).
% imageIsBitmap(Imagen)
% imageIsPixmap(Imagen)
% imageIsHexmap(Imagen)
% imageIsCompressed(Imagen)
% imageFlipH(Imagen, ImagenSalida)
% flipPixsH(Ancho, Pixeles, PixelesVolteados)
% imageFlipV(Imagen, ImagenSalida)
% flipPixsV(Alto, Pixeles, PixelesVolteados)
% imageCrop(Imagen, X1, Y1, X2, Y2, ImagenSalida)
% cropPixs(X1, Y1, X2, Y2, Pixeles, PixelesRecortados)
% imageRGBToHex(Imagen, ImagenSalida)
% pixsRGBtoHex(PixelesRGB, PixelesHex)
% imageToHistogram(Imagen, R)
% compoundToHistogram(ClumpedList, Histograma)
% pixsToVal(Pixeles, Valores)
% imageRotate90(Imagen, ImagenSalida)
% pixsSwapXY(Pixeles, PixelesTraspuestos)
% imageCompress(Imagen, ImagenComprimida)
% maxHistogram(Histograma, ValorMaximoTemporal, MaximoValorHistograma)
% erasePixs(Pixeles, ValorFiltrador, PixelesFiltrados)
% imageChangePixel(Imagen, PixelModificado, ImagenModificada)
% changePixs(Pixeles, PixelModificado, PixelesModificados)
% imageInvertColorRGB(PixelEntrada, PixelSalida)
% imageString(Imagen, StringSalida)
% sortImage(Imagen, ImagenOrdenada)
% sortPixs(IteradorI, IteradorJ, Ancho, Alto, Pixeles, PixelesOrdenados)
% findPix(X, Y, Pixeles, PixelEncontrado/NoEncontrado)
% pixbitToString(Ancho, Pixbits, StringPixeles)
% pixrgbToString(Ancho, Pixrgb, StringPixeles)
% pixhexToString(Ancho, Pixhex, StringPixeles)
% imageDepthLayers(Imagen, ListaImagenes)
% imageDepthLayers(Imagen, ListaImagenes)
% imageDepthLayers(Imagen, ListaImagenes)
% pixsToDepths(Pixeles, Profundidades)
% bitDepthLayersGen(Imagen, Profundidades, ListaImagenes)
% bitDepthPixs(Pixeles, Profundidad, PixelesDeProfundidad)
% rgbDepthLayersGen(Imagen, Profundidades, ListaImagenes)
% rgbDepthPixs(Pixeles, Profundidad, PixelesDeProfundidad)
% hexDepthLayersGen(Imagen, Profundidades, ListaImagenes)
% hexDepthPixs(Pixeles, Profundidad, PixelesDeProfundidad)
% imageDecompress(ImagenComprimida, ImagenDescomprimida)
% decompPixs(IteradorI, IteradorJ, Ancho, Alto, ValorCompresión, Pixeles, PixelesDescomprimidos)
% decompPix(PixelComprimido, ValorCompresión, PixelDescomprimido)


%% Clausulas
% Reglas

% Dominio: Ancho (int), Alto (int), Pixeles (lista), Imagen
% Constructor de imagen
image(Width, Height, Pixs, [Width, Height, -1, Pixs]) :-
    Width > 0,
    Height > 0.

% Dominio: Imagen, Pixeles (lista)
% Selector de pixeles de una imagen
imageGetPixs([_, _, _, Pixs], Pixs).
% Dominio: Imagen, ValorDeColor (int | lista | string)
% Selector del valor de compresión de una imagen
imageGetComp([_, _, Comp, _], Comp).
% Dominio: Imagen, ValorDeColor (int | lista | string), Imagen
% Modificador del valor de compresión de una imagen
% Solo se puede modificar el valor de compresión de una imagen no comprimida (-1),
% esto impide comprimir de nuevo una imagen comprimida
imageSetComp([Width, Height, -1, Pixs], Val, [Width, Height, Val, Pixs]).

% Dominio: Imagen
% Revisa si la imagen es un bitmap
imageIsBitmap(I) :-
    image(_, _, [H | _], I),
    pixbit(_, _, _, _, H). % revisa el primer pixel, ya que la entrada tiene pixeles homogeneos

% Dominio: Imagen
% Revisa si la imagen es un pixmap
imageIsPixmap(I) :-
    image(_, _, [H | _], I),
    pixrgb(_, _, _, _, _, _, H). % revisa el primer pixel, ya que la entrada tiene pixeles homogeneos

% Dominio: Imagen
% Revisa si la imagen es un hexmap
imageIsHexmap(I) :-
    image(_, _, [H | _], I),
    pixhex(_, _, _, _, H). % revisa el primer pixel, ya que la entrada tiene pixeles homogeneos

% Dominio: Imagen
% Revisa si la imagen esta comprimida
imageIsCompressed(I) :-
    imageGetComp(I, Comp),
    not(Comp = -1).
    
% Dominio: Imagen, Imagen
% Voltea horizontalmente la imagen modificando la coordenada X de cada pixel
% según la siguiente fórmula NewPixX = abs(pixX - imgW)
imageFlipH(I, I2) :-
    image(W, H, Pixs, I),
    % Se usa W-1, no W, porque las dimensiones de la imagen comienzan desde 1, pero las coordenadas de pixel desde 0
    flipPixsH(W-1, Pixs, FPixs),
    image(W, H, FPixs, I2).

% Dominio: Ancho (int), Pixeles (lista), Pixeles (lista)
% Se usa recursión para cambiar la coordenada X de cada uno de los pixeles
flipPixsH(_, [], []).   % Caso base
flipPixsH(Dim, [H | T], [PixR | ListaR]) :-
    pixel(X1, Y, Val, D, H),
    X2 is abs(X1 - Dim),    % Fórmula
    pixel(X2, Y, Val, D, PixR),
    flipPixsH(Dim, T, ListaR).


% Dominio: Imagen, Imagen
% Voltea horizontalmente la imagen modificando la coordenada Y de cada pixel
% según la siguiente fórmula NewPixY = abs(pixY - imgH)
imageFlipV(I, I2) :-
    image(W, H, Pixs, I),
    % Se usa H-1, no H, porque las dimensiones de la imagen comienzan desde 1, pero las coordenadas de pixel desde 0
    flipPixsV(H-1, Pixs, FPixs),
    image(W, H, FPixs, I2).

% Dominio: Ancho (int), Pixeles (lista), Pixeles (lista)
% Se usa recursión para cambiar la coordenada X de cada uno de los pixeles
flipPixsV(_, [], []).   % Caso base
flipPixsV(Dim, [H | T], [PixR | ListaR]) :-
    pixel(X, Y1, Val, D, H),
    Y2 is abs(Y1 - Dim),    % Fórmula
    pixel(X, Y2, Val, D, PixR),
    flipPixsV(Dim, T, ListaR).


% Dominio: Imagen, X1 (int), Y1 (int), X2 (int), Y2 (int), Imagen
% Elimina los pixeles que no se encuentren dentro del rectángulo determinado por
% los 2 puntos P1(X1,Y1) y P2(X2,Y2), donde P1 corresponde a la esquina superior izquierda
% y P2 corresponde a la esquina inferior derecha de la imagen 
imageCrop(I, X1, Y1, X2, Y2, I2) :-
    image(_, _, Pixs, I),
    cropPixs(X1, Y1, X2, Y2, Pixs, FPixs),  % Filtrar pixeles
    NewW is 1+X2-X1,    % Ajustar dimensiones de la imagen nueva
    NewH is 1+Y2-Y1,    % Ajustar dimensiones de la imagen nueva
    image(NewW, NewH, FPixs, I2).

% Dominio: X1 (int), Y1 (int), X2 (int), Y2 (int), Pixeles (lista), Pixeles (lista)
% Se recorre recursivamente la lista de pixeles filtrando según las coordenadas delimitadoras
cropPixs(_, _, _, _, [], []).   % Caso base
cropPixs(X1, Y1, X2, Y2, [H | T], [PixR | ListaAux]) :-    % Caso 1: El pixel se encuentra dentro del rectángulo
    pixel(X, Y, Val, D, H),
    X1 =< X, X =< X2, Y1 =< Y, Y =< Y2, % revisar posición
    NewX is X - X1, % Calcular nuevas coordenadas
    NewY is Y - Y1, % Calcular nuevas coordenadas
    pixel(NewX, NewY, Val, D, PixR),
    cropPixs(X1, Y1, X2, Y2, T, ListaAux).
cropPixs(X1, Y1, X2, Y2, [_ | T], ListaAux) :-  % Caso 2: El pixel no se encuentra dentro del rectángulo
    cropPixs(X1, Y1, X2, Y2, T, ListaAux).

% Dominio: Imagen, Imagen
% Transforma los pixeles rgb de una imagen a hex
imageRGBToHex(I, I2):-
    image(W, H, Pixs, I),
    imageIsPixmap(I),
    pixsRGBtoHex(Pixs, RPixs),
    image(W, H, RPixs, I2).

% Dominio: Pixeles, Pixeles
% Se recorre recursivamente la lista de pixeles y se transforma
pixsRGBtoHex([], []).   % Caso base
pixsRGBtoHex([H | T], [R | ListaR]) :-
    pixel(X, Y, RGB, D, H),
    hex_bytes(HexStr, RGB), % Se transforma de [R, G, B] a String "RRGGBB"
    string_concat("#", HexStr, NewVal), % Se le añade '#' a la string
    pixel(X, Y, NewVal, D, R),
    pixsRGBtoHex(T, ListaR).

% Dominio: Imagen, Histograma (lista bidimensional)
% Se obtiene una lista de todos los colores de la imagen y la cantidad de veces que se repiten
imageToHistogram(I, R) :-
    image(_, _, _, I),
    imageGetPixs(I, Pixs),
    pixsToVal(Pixs, Vals),  % Obtener la lista de valores de color
    msort(Vals, SortVals),  % Ordenar la lista, sin eliminar duplicados
    clumped(SortVals, Clump),   % Contar las repeticiones
    compoundToHistogram(Clump, R).  % Transformar la lista de formato Color-Numero a [Color, Numero]

% Dominio: Lista Clumped, Histograma (lista bidimensional)
% Se recorre recursivamente la lista clumped y se cambia de formato
compoundToHistogram([], []).    % Caso base
compoundToHistogram([Cl | T], [Histo | ListaR]) :-
    compound_name_arguments(Cl, -, Histo),  % Se separan Color-Numero usando de referencia el '-'
    compoundToHistogram(T, ListaR).


% Dominio: Pixeles (lista), Valores (lista)
% Se aplica el selector de valor de color a cada elemento de la lista de pixeles
pixsToVal([], []).  % Caso base
pixsToVal([Pix | T], [Val | ListaR]) :-
    pixelGetVal(Pix, Val),
    pixsToVal(T, ListaR).

% Dominio: Imagen, Imagen
% Se rota la imagen en 90 grados en dirección horaria, intercambiando las coordenadas X e Y de los pixeles, y volteandolos
imageRotate90(I, I2) :-
    image(W, H, Pixs, I),
    pixsSwapXY(Pixs, PixsSwap), % intercambiar coordenadas X e Y
    flipPixsH(W-1, PixsSwap, PixsR),    % voltear
    image(H, W, PixsR, I2).

% Dominio: Pixeles (lista), Pixeles (lista)
% Se recorre la lista de pixeles recursivamente y se intercambian X e Y
pixsSwapXY([], []).
pixsSwapXY([Pix | T], [PixR | ListaR]) :-
    pixel(X, Y, Val, D, Pix),
    pixel(Y, X, Val, D, PixR),  % intercambio
    pixsSwapXY(T, ListaR).

% Dominio: Imagen, Imagen
% Se comprime la imagen eliminando los pixeles del color más repetido, perdiendo la profundidad de los pixeles eliminados
imageCompress(I, [W, H, CompVal, PixsR]) :-
    imageToHistogram(I, Histo), % Se obtiene el histograma de la imagen
    maxHistogram(Histo, [0, 0], CompVal),   % Se obtiene el valor que más se repite
    image(W, H, Pixs, I),
    erasePixs(Pixs, CompVal, PixsR).    % Se borran los pixeles del color que más se repite

% Dominio: Histograma, MaximoTemporal, ValorMaximo
% Se recorre el histograma recursivamente, se compara el maximo temporal con cada valor
maxHistogram([], [MaxVal, _], MaxVal).  % Caso base, se revisó por completo el histograma
maxHistogram([[Val | Count] | T], [_, MaxCount], MaxHVal) :-    % Caso 1: El color se repite más que el maximo temporal  
    Count > MaxCount,
    maxHistogram(T, [Val | Count], MaxHVal).    % Se cambia el nuevo maximo temporal
maxHistogram([_ | T], Max, MaxHVal) :-  % Caso 2: El maximo temporal se mantiene
    maxHistogram(T, Max, MaxHVal).  % se sigue con la recursión

% Dominio: Pixeles (lista), ValorDeColor (int | lista | string), Pixeles (lista)
% Borra todos los pixeles del color especificado
erasePixs([], _, []).   % Caso base
erasePixs([[_, _, EraseVal, _] | T], EraseVal, ListaR) :-   % Caso 1: Valor = EraseVal
    erasePixs(T, EraseVal, ListaR).
erasePixs([Pix | T], EraseVal, [Pix | ListaR]) :-   % Caso 2: Valor != EraseVal
    erasePixs(T, EraseVal, ListaR).

% Dominio: Imagen, Pixel, Imagen
% Cambia un pixel de la imagen por el entregado en el argumento, en base a la posición de estos
imageChangePixel(I, PMod, I2) :-
    image(W, H, Pixs, I),
    changePixs(Pixs, PMod, PixsR),
    image(W, H, PixsR, I2).

% Dominio: Pixeles (lista), Pixel, Pixeles (lista)
% Se recorre la lista de pixeles en busqueda del pixel que comparte posición X,Y con el pixel modificado
changePixs([], _, []).  % Caso base
changePixs([[X, Y, _, _] | T], [X, Y, NewVal, NewD], [[X, Y, NewVal, NewD] | T]).   % Caso 1: Coordenadas X,Y coinciden
changePixs([Pix | T], PMod, [Pix | ListaR]) :-  % Caso 2: Las coordenadas no coinciden
    changePixs(T, PMod, ListaR).

% Dominio: Pixel, Pixel
% Invierte el color RGB de un pixrgb
imageInvertColorRGB(PE, PR) :-
    pixrgb(X, Y, R, G, B, D, PE),   % Obtener valores
    NewR is 255 - R,    % Invertir colores
    NewG is 255 - G,    % Invertir colores
    NewB is 255 - B,    % Invertir colores
    pixrgb(X, Y, NewR, NewG, NewB, D, PR).

% Dominio: Imagen, String
% Transforma una imagen a una string de sus pixeles
% Se usa el ancho de la imagen disminuido en 1 para detectar cuando un pixel es el último de la fila, y agregar un salto de linea '\n'
imageString(I, Str) :-  % Caso 1: la imagen de entrada esta comprimida
    imageIsCompressed(I),
    imageDecompress(I, I2), % Decomprimirla
    imageString(I2, Str).
imageString(I, Str) :-  % Caso 2: bitmap
    image(W, _, _, I),
    imageIsBitmap(I),
    W1 is W - 1,
    sortImage(I, I2),   % Se ordenan los pixeles de la imagen para simplificar la transformación a string
    imageGetPixs(I2, Pixs),
    pixbitToString(W1, Pixs, Str).
imageString(I, Str) :-  % Caso 3: pixmap
    image(W, _, _, I),
    imageIsPixmap(I),
    W1 is W - 1,
    sortImage(I, I2),   % Se ordenan los pixeles de la imagen para simplificar la transformación a string
    imageGetPixs(I2, Pixs),
    pixrgbToString(W1, Pixs, Str).
imageString(I, Str) :-  % Caso 4: hexmap
    image(W, _, _, I),
    imageIsHexmap(I),
    W1 is W - 1,
    sortImage(I, I2),   % Se ordenan los pixeles de la imagen para simplificar la transformación a string
    imageGetPixs(I2, Pixs),
    pixhexToString(W1, Pixs, Str).

% Dominio: Imagen, Imagen
% Ordena los pixeles de la imagen, partiendo de la coordenada X, y luego la coordenada Y 
sortImage(I, I2) :-
    image(W, H, Pixs, I),
    image(W, H, PixsR, I2),
    W1 is W - 1,    % Se ajusta en -1 por la diferencia entre comenzar a contar las dimensiones de la imagen en 1, y las coordenadas del pixel desde 0
    H1 is H - 1,    % Se ajusta en -1 por la diferencia entre comenzar a contar las dimensiones de la imagen en 1, y las coordenadas del pixel desde 0
    sortPixs(0, 0, W1, H1, Pixs, PixsR).

% Dominio: IteradorI (int), IteradorJ (int), Ancho (int), Alto (int), Pixeles (lista), Pixeles (lista)
% Se recorren todas las coordenadas de la imagen buscando los pixeles que corresponden, añadiendolos a la lista de pixeles
sortPixs(W, H, W, H, Pixs, [PixR]) :-   % Caso base: Se recorrió toda la imagen
    findPix(W, H, Pixs, PixR).

sortPixs(W, J, W, H, Pixs, [PixR | PixT]) :-    % Caso 1: Se llega al fin de una fila
    findPix(W, J, Pixs, PixR),
    NewI is 0,  % Se reinicia IteradorI a 0
    NewJ is J+1,    % Se aumenta en 1 IteradorJ
    sortPixs(NewI, NewJ, W, H, Pixs, PixT).

sortPixs(I, J, W, H, Pixs, [PixR | PixT]) :-    % Caso 2: No hay ningún caso especial
    findPix(I, J, Pixs, PixR),
    NewI is I+1,    % Se aumenta en 1 IteradorI
    sortPixs(NewI, J, W, H, Pixs, PixT).

% Dominio: X (int), Y (int), Pixeles (lista), Pixel
% Se itera en toda la lista de pixeles hasta encontrar el que esta ubicado en la posición X,Y
findPix(X, Y, [], [X, Y, -1, 0]).   % Caso 1: No existe
findPix(X, Y, [[X, Y, Val, D] | _], [X, Y, Val, D]).    % Caso 2: Pixel encontrado
findPix(X, Y, [_ | T], PixR) :- % Caso 3: se sigue con la recursión
    findPix(X, Y, T, PixR).

% Dominio: Ancho (int), Pixeles (lista), String
% Se recorre la lista ordenada de pixeles, y se transforma cada uno a string
% Función especial para pixbit
pixbitToString(_, [], "").  % Caso base
pixbitToString(W, [[W, _, Val, _] | T], StrR) :-    % Caso 1: Si el pixel es el último de la fila
    number_string(Val, StrVal), % Transformación
    pixbitToString(W, T, StrT), % Recursión
    string_concat(StrVal, "\n", StrValNewL),    % Se agrega nueva linea
    string_concat(StrValNewL, StrT, StrR).
pixbitToString(W, [Pix | T], StrR) :-   % Caso 2: El pixel no es un caso especial
    pixelGetVal(Pix, Val),
    number_string(Val, StrVal), % Transformación
    pixbitToString(W, T, StrT), % Recursión
    string_concat(StrVal, "\t", StrValSpace),    % Se agrega un espacio
    string_concat(StrValSpace, StrT, StrR).


% Dominio: Ancho (int), Pixeles (lista), String
% Se recorre la lista ordenada de pixeles, y se transforma cada uno a string
% Función especial para pixrgb
pixrgbToString(_, [], "").  % Caso base
pixrgbToString(W, [[W, _, Val, _] | T], StrR) :-    % Caso 1: Si el pixel es el último de la fila
    rgbToString(Val, StrVal), % Transformación
    pixrgbToString(W, T, StrT), % Recursión
    string_concat(StrVal, "\n", StrValNewL),    % Se agrega nueva linea
    string_concat(StrValNewL, StrT, StrR).
pixrgbToString(W, [Pix | T], StrR) :-   % Caso 2: El pixel no es un caso especial
    pixelGetVal(Pix, Val),
    rgbToString(Val, StrVal), % Transformación
    pixrgbToString(W, T, StrT), % Recursión
    string_concat(StrVal, "\t", StrValSpace),    % Se agrega un espacio
    string_concat(StrValSpace, StrT, StrR).
    
    
% Dominio: Ancho (int), Pixeles (lista), String
% Se recorre la lista ordenada de pixeles, y se transforma cada uno a string
% Función especial para pixhex
pixhexToString(_, [], "").  % Caso base
pixhexToString(W, [[W, _, Val, _] | T], StrR) :-    % Caso 1: Si el pixel es el último de la fila
    pixhexToString(W, T, StrT), % Recursión
    string_concat(Val, "\n", StrValNewL),    % Se agrega nueva linea
    string_concat(StrValNewL, StrT, StrR).
pixhexToString(W, [Pix | T], StrR) :-   % Caso 2: El pixel no es un caso especial
    pixelGetVal(Pix, Val),
    pixhexToString(W, T, StrT), % Recursión
    string_concat(Val, "\t", StrValSpace),    % Se agrega un espacio
    string_concat(StrValSpace, StrT, StrR).

% Dominio: Image, Lista
% Decompone la imagen dada por capas de profundidad, dejando en blanco los pixeles que no pertenecen a cada capa
% Versión de la funcion para bitmaps
imageDepthLayers(I, LI) :-
    imageIsBitmap(I),
    imageGetPixs(I, Pixs),
    pixsToDepths(Pixs, Depths), % Extraer las profundidades presentes en la imagen
    sort(Depths, SortDepths),   % Ordenar de mayor a menor y eliminar duplicados
    bitDepthLayersGen(I, SortDepths, LI).   % Generar DepthLayers

% Dominio: Image, Lista
% Decompone la imagen dada por capas de profundidad, dejando en blanco los pixeles que no pertenecen a cada capa
% Versión de la funcion para pixmaps
imageDepthLayers(I, LI) :-
    imageIsPixmap(I),
    imageGetPixs(I, Pixs),
    pixsToDepths(Pixs, Depths), % Extraer las profundidades presentes en la imagen
    sort(Depths, SortDepths),   % Ordenar de mayor a menor y eliminar duplicados
    rgbDepthLayersGen(I, SortDepths, LI).   % Generar DepthLayers

% Dominio: Image, Lista
% Decompone la imagen dada por capas de profundidad, dejando en blanco los pixeles que no pertenecen a cada capa
% Versión de la funcion para hexmaps
imageDepthLayers(I, LI) :-
    imageIsHexmap(I),
    imageGetPixs(I, Pixs),
    pixsToDepths(Pixs, Depths), % Extraer las profundidades presentes en la imagen
    sort(Depths, SortDepths),   % Ordenar de mayor a menor y eliminar duplicados
    hexDepthLayersGen(I, SortDepths, LI).   % Generar DepthLayers

% Dominio: Pixeles (lista), Lista
% Obtener todas las profundidades de una lista de pixeles
pixsToDepths([], []).   % Caso base
pixsToDepths([Pix | T], [D | ListaR]) :-
    pixelGetDepth(Pix, D),
    pixsToDepths(T, ListaR).

% Dominio: Imagen, Lista, Pixeles (lista)
% Se genera en cada recursión una capa de profundidad, recorriendo la lista de profundidades de una imagen
% Versión para bitmaps
bitDepthLayersGen(_, [], []).   % Caso base: Generadas todas las profundidades
bitDepthLayersGen(I, [IterDep | T], [DImg | ListaR]) :- 
    image(W, H, Pixs, I),
    bitDepthPixs(Pixs, IterDep, PixsDep),   % Obtener pixeles de capa de profundidad
    image(W, H, PixsDep, DImg),
    bitDepthLayersGen(I, T, ListaR).    % Recursión

% Dominio: Pixeles (lista), Profundidad (int), Pixeles (lista)
% Se recorre la lista de pixeles, si el pixel es de la profundidad se mantiene, sino, se cambia por uno blanco
% Versión de pixbit
bitDepthPixs([], _, []).    % Caso base
bitDepthPixs([Pix | T], D, [Pix | ListaR]) :-   % Caso 1: Misma profundidad
    pixelGetDepth(Pix, D),
    bitDepthPixs(T, D, ListaR).
bitDepthPixs([Pix | T], Depth, [P | ListaR]) :- % Caso 2: Otra profundidad
    pixbit(X, Y, _, D, Pix),
    pixbit(X, Y, 1, D, P),
    bitDepthPixs(T, Depth, ListaR).


% Dominio: Imagen, Lista, Pixeles (lista)
% Se genera en cada recursión una capa de profundidad, recorriendo la lista de profundidades de una imagen
% Versión para bitmaps
rgbDepthLayersGen(_, [], []).   % Caso base: Generadas todas las profundidades
rgbDepthLayersGen(I, [IterDep | T], [DImg | ListaR]) :-
    image(W, H, Pixs, I),
    rgbDepthPixs(Pixs, IterDep, PixsDep),   % Obtener pixeles de capa de profundidad
    image(W, H, PixsDep, DImg),
    rgbDepthLayersGen(I, T, ListaR).    % Recursión

% Dominio: Pixeles (lista), Profundidad (int), Pixeles (lista)
% Se recorre la lista de pixeles, si el pixel es de la profundidad se mantiene, sino, se cambia por uno blanco
% Versión de pixbit
rgbDepthPixs([], _, []).    % Caso base
rgbDepthPixs([Pix | T], D, [Pix | ListaR]) :-   % Caso 1: Misma profundidad
    pixelGetDepth(Pix, D),
    rgbDepthPixs(T, D, ListaR).
rgbDepthPixs([Pix | T], Depth, [P | ListaR]) :- % Caso 2: Otra profundidad
    pixrgb(X, Y, _, _, _, D, Pix),
    pixrgb(X, Y, 255, 255, 255, D, P),
    rgbDepthPixs(T, Depth, ListaR).


% Dominio: Imagen, Lista, Pixeles (lista)
% Se genera en cada recursión una capa de profundidad, recorriendo la lista de profundidades de una imagen
% Versión para bitmaps
hexDepthLayersGen(_, [], []).   % Caso base: Generadas todas las profundidades
hexDepthLayersGen(I, [IterDep | T], [DImg | ListaR]) :-
    image(W, H, Pixs, I),
    hexDepthPixs(Pixs, IterDep, PixsDep),   % Obtener pixeles de capa de profundidad
    image(W, H, PixsDep, DImg),
    hexDepthLayersGen(I, T, ListaR).    % Recursión

% Dominio: Pixeles (lista), Profundidad (int), Pixeles (lista)
% Se recorre la lista de pixeles, si el pixel es de la profundidad se mantiene, sino, se cambia por uno blanco
% Versión de pixbit
hexDepthPixs([], _, []).    % Caso base
hexDepthPixs([Pix | T], D, [Pix | ListaR]) :-   % Caso 1: Misma profundidad
    pixelGetDepth(Pix, D),
    hexDepthPixs(T, D, ListaR).
hexDepthPixs([Pix | T], Depth, [P | ListaR]) :- % Caso 2: Otra profundidad
    pixhex(X, Y, _, D, Pix),
    pixhex(X, Y, "#FFFFFF", D, P),
    hexDepthPixs(T, Depth, ListaR).

% Dominio: Imagen, Imagen
% Descomprime una función, rellenando los pixeles faltantes con el valor de compresión guardado,
% se pierde la información de profundidad de los pixeles comprimidos
imageDecompress(I, I) :-
    not(imageIsCompressed(I)).  % Si la imagen no está comprimida, no se hace nada
imageDecompress([W, H, Comp, Pixs], I2) :-
    W1 is W - 1,    % Se ajusta en -1 por la diferencia entre comenzar a contar las dimensiones de la imagen en 1, y las coordenadas del pixel desde 0
    H1 is H - 1,    % Se ajusta en -1 por la diferencia entre comenzar a contar las dimensiones de la imagen en 1, y las coordenadas del pixel desde 0
    decompPixs(0, 0, W1, H1, Comp, Pixs, DecompPixs),   % Descomprimir pixeles
    image(W, H, DecompPixs, I2).

% Dominio: IteradorI (int), IteradorJ (int), Ancho (int), Alto (int), Valor (int | lista | string), Pixeles (lista), Pixeles (lista)
% Se recorre cada coordenada de la imagen, rellenando los pixeles no encontrados con el valor de color especificado
decompPixs(W, H, W, H, CompVal, Pixs, [PixR]) :-    % Caso base: Se recorrió toda la imagen
    findPix(W, H, Pixs, PixF),
    decompPix(PixF, CompVal, PixR).
decompPixs(W, J, W, H, CompVal, Pixs, [PixR | PixT]) :- % Caso 1: Se llega al fin de una fila
    findPix(W, J, Pixs, PixF),
    decompPix(PixF, CompVal, PixR),
    NewI is 0,  % Se reinicia el iteradorI
    NewJ is J+1,    % Se aumenta el iteradorJ
    decompPixs(NewI, NewJ, W, H, CompVal, Pixs, PixT).
decompPixs(I, J, W, H, CompVal, Pixs, [PixR | PixT]) :- % Caso 2: Coordenada sin caso especial
    findPix(I, J, Pixs, PixF),
    decompPix(PixF, CompVal, PixR),
    NewI is I+1,
    decompPixs(NewI, J, W, H, CompVal, Pixs, PixT).

% Dominio: Pixel, Valor (int | lista | string), Pixel
% Descomprime un pixel, si el valor es -1, significa que no existe en la imagen y debe ser restaurado
decompPix([X, Y, -1, 0], CompVal, DecomPix) :-  % Caso 1: Pixel eliminado
    pixel(X, Y, CompVal, 0, DecomPix).  % Profundidad por defecto en 0
decompPix(Pix, _, Pix). % Caso 2: El pixel existe, no se modifica


img1(I) :-
    pixbit( 0, 0, 1, 10, PA),
    pixbit( 0, 1, 0, 20, PB),
    pixbit( 1, 0, 0, 20, PC),
    pixbit( 1, 1, 1, 40, PD),
    image( 2, 2, [PB, PA, PD, PC], I).

img4(I) :-
    pixhex( 0, 0, "#FF0011", 10, PA),
    pixhex( 0, 1, "#FF5544", 20, PB),
    pixhex( 1, 0, "#00FF22", 30, PC),
    pixhex( 1, 1, "#0011FF", 40, PD),
    image( 2, 2, [PB, PA, PD, PC], I).

img2(I) :-
    pixrgb( 0, 0, 1, 3, 4, 10, PA),
    pixrgb( 0, 1, 0, 3, 5, 20, PB),
    pixrgb( 1, 0, 0, 6, 7, 30, PC),
    pixrgb( 1, 1, 1, 4, 7, 8, PD),
    image( 2, 2, [PA, PB, PC, PD], I).

img3(I) :-
    pixrgb( 0, 0, 1, 3, 4, 10, PA),
    pixrgb( 0, 1, 0, 3, 5, 20, PB),
    pixrgb( 1, 0, 0, 6, 7, 30, PC),
    pixrgb( 1, 1, 1, 4, 7, 8, PD),
    pixrgb( 0, 2, 1, 4, 7, 9, PE),
    pixrgb( 1, 2, 1, 4, 7, 80, PF),
    image( 2, 3, [PA, PB, PD, PC, PE, PF], I).

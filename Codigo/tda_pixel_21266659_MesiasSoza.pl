:- module(tda_pixel_21266659_MesiasSoza, [pixel/5, pixelGetVal/2, pixelGetDepth/2, pixbit/5, pixrgb/7, rgbcheck/1, pixhex/5, hexcheck/1, rgbToString/2]).

% TDA Pixel
% Dominio: X (int), Y (int), Valor (int | lista | string), Depth (int)
% Recorrido: Lista
% X e Y deben ser positivos, Depth debe ser un numero entre 0 y 255

% Predicados:
% pixel(X, Y, Valor, Depth, Pixel)
% pixelGetVal(Pixel, Valor)
% pixelGetDepth(Pixel, Depth)

%% Clausulas
% Reglas
% Dominio: X (int), Y (int), Valor (int | lista | string), Depth (int), pixel
% Constructor de pixeles
pixel(X, Y, Val, D, [X, Y, Val, D]) :-
    number(X),
    number(Y),
    between(0, 255, D).

% Dominio: pixel, Valor
% Selector del valor de un pixel
pixelGetVal([_, _, Val, _], Val).
% Dominio: pixel, Depth
% Selector de la profunidad de un pixel
pixelGetDepth([_, _, _, Depth], Depth).

% TDA Pixbit (Sub-tda de pixel)
% Dominio: X, Y, Bit (0 | 1), Depth
% Recorrido: pixbit
% pixel que almacena un bit

% Predicados:
% pixbit(X, Y, Bit, Depth, pixbit)

% Reglas
% Dominio: X, Y, Bit, Depth, pixbit
% Constructor de pixbit
pixbit(X, Y, Bit, D, P) :-
    pixel(X, Y, Bit, D, P),
    number(Bit),
    between(0, 1, Bit).

% TDA Pixrgb (Sub-tda de pixel)
% Dominio: X, Y, R (int), G (int), B (int), Depth
% Recorrido: pixrgb
% pixel que almacena 3 valores que van de 0 a 255, correspondiendo valor de color de los canales RGB

% Predicados:
% pixrgb(X, Y, R, G, B, Depth, pixrgb)
% rgbcheck(RGB)
% rgbToString(RGB, String)

% Reglas
% Dominio: X, Y, R, G, B, Depth, pixrgb
% Constructor de pixrgb
pixrgb(X, Y, R, G, B, D, P) :-
    pixel(X, Y, [R, G, B], D, P),
    number(R),
    number(G),
    number(B),
    rgbcheck([R, G, B]).
    
% Dominio: RGB (Lista)
% Revisa que los valores de RGB se encuentren en el rango aceptado
rgbcheck([R, G, B]) :-
    between(0, 255, R),
    between(0, 255, G),
    between(0, 255, B).

% Dominio: RGB (Lista), String
% Transforma de [R, G, B] a una String de formato "(R G B)"
rgbToString([R, G, B], Str) :-
    number_string(R, StrR),
    number_string(G, StrG),
    number_string(B, StrB),
    string_concat(StrR, " ", Str1),
    string_concat(StrG, " ", Str2),
    string_concat(Str1, Str2, Str3),
    string_concat(Str3, StrB, Str4),
    string_concat("(", Str4, Str5),
    string_concat(Str5, ")", Str).

% TDA Pixhex (Sub-tda de pixel)
% Dominio: X, Y, Hex (String), Depth
% Recorrido: pixhex
% pixel que almacena el valor de color hexadecimal

% Predicados:
% pixhex(X, Y, Hex, Depth, pixhex)
% hexcheck(String)

% Reglas
% Dominio: X, Y, Hex (String), Depth, pixhex
% Constructor de pixhex
pixhex(X, Y, Hex, D, P) :-
    pixel(X, Y, Hex, D, P),
    string(Hex),
    hexcheck(Hex).

% Dominio: String
% Revisa que los valores de color se encuentren en el rango requerido transformando la string a una lista de valores RGB
hexcheck(Str) :-
    name(Str, [_ | T]), % separar "#RRGGBB" en "#" y "RRGGBB"
    name(HexRGB, T), % reconstruir la string
    hex_bytes(HexRGB, Rgb), % transforma de string a [R, G, B]
    rgbcheck(Rgb).


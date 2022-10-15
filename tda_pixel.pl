:- module(tda_pixel, [pixel/5, pixelGetVal/2, pixelGetDepth/2, pixbit/5, pixrgb/7, rgbcheck/1, pixhex/5, hexcheck/1, pixbitToString/3, pixrgbToString/3, pixhexToString/3, rgbToString/2]).

pixel(X, Y, Val, D, [X, Y, Val, D]) :-
    number(X),
    number(Y),
    between(0, 255, D).

pixelGetVal([_, _, Val, _], Val).
pixelGetDepth([_, _, _, Depth], Depth).

pixbit(X, Y, Bit, D, P) :-
    pixel(X, Y, Bit, D, P),
    number(Bit),
    between(0, 1, Bit).


pixrgb(X, Y, R, G, B, D, P) :-
    pixel(X, Y, [R, G, B], D, P),
    number(R),
    number(G),
    number(B),
    rgbcheck([R, G, B]).
    
rgbcheck([R, G, B]) :-
    between(0, 255, R),
    between(0, 255, G),
    between(0, 255, B).


pixhex(X, Y, Hex, D, P) :-
    pixel(X, Y, Hex, D, P),
    string(Hex),
    hexcheck(Hex).

hexcheck(Str) :-
    name(Str, [_ | T]), % separar "#RRGGBB" en "#" y "RRGGBB"
    name(HexRGB, T), % reconstruir la string
    hex_bytes(HexRGB, Rgb), % transforma de string a [R, G, B]
    rgbcheck(Rgb).


pixbitToString(_, [], "").
pixbitToString(W, [[W, _, Val, _] | T], StrR) :-
    number_string(Val, StrVal),
    pixbitToString(W, T, StrT),
    string_concat(StrVal, "\n", StrValNewL),
    string_concat(StrValNewL, StrT, StrR).
pixbitToString(W, [Pix | T], StrR) :-
    pixelGetVal(Pix, Val),
    number_string(Val, StrVal),
    pixbitToString(W, T, StrT),
    string_concat(StrVal, " ", StrValSpace),
    string_concat(StrValSpace, StrT, StrR).


pixrgbToString(_, [], "").
pixrgbToString(W, [[W, _, Val, _] | T], StrR) :-
    rgbToString(Val, StrVal),
    pixrgbToString(W, T, StrT),
    string_concat(StrVal, "\n", StrValNewL),
    string_concat(StrValNewL, StrT, StrR).
pixrgbToString(W, [Pix | T], StrR) :-
    pixelGetVal(Pix, Val),
    rgbToString(Val, StrVal),
    pixrgbToString(W, T, StrT),
    string_concat(StrVal, " ", StrValSpace),
    string_concat(StrValSpace, StrT, StrR).

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
    
pixhexToString(_, [], "").
pixhexToString(W, [[W, _, Val, _] | T], StrR) :-
    pixhexToString(W, T, StrT),
    string_concat(Val, "\n", StrValNewL),
    string_concat(StrValNewL, StrT, StrR).
pixhexToString(W, [Pix | T], StrR) :-
    pixelGetVal(Pix, Val),
    pixhexToString(W, T, StrT),
    string_concat(Val, " ", StrValSpace),
    string_concat(StrValSpace, StrT, StrR).

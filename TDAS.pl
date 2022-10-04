pixel(X, Y, Val, D, [X, Y, Val, D]) :-
    number(X),
    number(Y),
    between(0, 255, D).

pixbit-d(X, Y, Bit, D, P) :-
    number(Bit),
    between(0, 1, Bit),
    pixel(X, Y, Bit, D, P).


pixrgb-d(X, Y, R, G, B, D, P) :-
    pixel(X, Y, [R, G, B], D, P),
    number(R),
    number(G),
    number(B),
    between(0, 255, R),
    between(0, 255, G),
    between(0, 255, B).
    
rgbcheck([R, G, B]) :-
    between(0, 255, R),
    between(0, 255, G),
    between(0, 255, B).


pixhex-d(X, Y, Hex, D, P) :-
    string(Hex),
    hexcheck(Hex),
    pixel(X, Y, Hex, D, P).

hexcheck(Str) :-
    name(Str, [H | T]), % separar "#RRGGBB" en "#" y "RRGGBB"
    name(HexRGB, T), % reconstruir la string
    hex_bytes(HexRGB, Rgb), % transforma de string a [R, G, B]
    rgbcheck(Rgb).


image(Width, Height, Pixs, [Width, Height, -1, Pixs]) :-
    Width > 0,
    Height > 0.


imagebitmap(I) :-
    image(_, _, [H | _], I),
    pixbit-d(_, _, _, _, H). % revisa el primer pixel, ya que la entrada tiene pixeles homogeneos

imagepixmap(I) :-
    image(_, _, [H | _], I),
    pixrgb-d(_, _, _, _, H). % revisa el primer pixel, ya que la entrada tiene pixeles homogeneos

imagehexmap(I) :-
    image(_, _, [H | _], I),
    pixhex-d(_, _, _, _, H). % revisa el primer pixel, ya que la entrada tiene pixeles homogeneos

imagecompressed([_, _, Comp, _]) :-
    Comp = -1.
    
% abs(pixY - imgH)
flipH(I, I2) :-
    image(W, H, Pixs, I),
    flipPixsH(W-1, Pixs, FPixs),
    image(W, H, FPixs, I2).

flipPixsH(_, [], []).
flipPixsH(Dim, [H | T], [PixR | ListaR]) :-
    pixel(X1, Y, Val, D, H),
    X2 is abs(X1 - Dim),
    pixel(X2, Y, Val, D, PixR),
    flipPixsH(Dim, T, ListaR).


flipV(I, I2) :-
    image(W, H, Pixs, I),
    flipPixsV(H-1, Pixs, FPixs),
    image(W, H, FPixs, I2).

% abs(pix(X|Y) - img(W|H)), misma funcion, distintas variables
flipPixsV(_, [], []).
flipPixsV(Dim, [H | T], [PixR | ListaR]) :-
    pixel(X, Y1, Val, D, H),
    Y2 is abs(Y1 - Dim),
    pixel(X, Y2, Val, D, PixR),
    flipPixsV(Dim, T, ListaR).


crop(I, X1, Y1, X2, Y2, I2) :-
    image(W, H, Pixs, I),
    cropPixs(X1, Y1, X2, Y2, Pixs, FPixs),
    NewW is 1+X2-X1,
    NewH is 1+Y2-Y1,
    image(NewW, NewH, FPixs, I2).

cropPixs(_, _, _, _, [], []).

cropPixs(X1, Y1, X2, Y2, [H | T], ListaR) :-
    cropPixs(X1, Y1, X2, Y2, T, ListaAux),
    pixel(X, Y, Val, D, H),
    ((X1 =< X, X =< X2, Y1 =< Y, Y =< Y2,
        NewX is X - X1,
        NewY is Y - Y1,
        pixel(NewX, NewY, Val, D, PixR),
        ListaR = [PixR | ListaAux] );
    (ListaR = ListaAux)).

rgbtohex(I, I2):-
    image(W, H, Pixs, I),
    pixsRGBtoHex(Pixs, RPixs),
    image(W, H, RPixs, I2).

pixsRGBtoHex([], []).
pixsRGBtoHex([H | T], [R | ListaR]) :-
    pixel(X, Y, RGB, D, H),
    hex_bytes(HexStr, RGB),
    string_concat("#", HexStr, NewVal),
    pixel(X, Y, NewVal, D, R),
    pixsRGBtoHex(T, ListaR).


img1(I) :-
    pixbit-d( 0, 0, 1, 10, PA),
    pixbit-d( 0, 1, 0, 20, PB),
    pixbit-d( 1, 0, 0, 30, PC),
    pixbit-d( 1, 1, 1, 4, PD),
    image( 2, 2, [PA, PB, PC, PD], I).

img2(I) :-
    pixrgb-d( 0, 0, 1, 3, 4, 10, PA),
    pixrgb-d( 0, 1, 0, 3, 5, 20, PB),
    pixrgb-d( 1, 0, 0, 6, 7, 30, PC),
    pixrgb-d( 1, 1, 1, 4, 7, 8, PD),
    image( 2, 2, [PA, PB, PC, PD], I).

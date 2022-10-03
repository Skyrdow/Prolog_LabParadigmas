pixel(X, Y, Val, D, [X, Y, Val, D]) :-
    number(X),
    number(Y),
    between(0, 255, D).

pixbit-d(X, Y, Bit, D, P) :-
    number(Bit),
    between(0, 1, Bit),
    pixel(X, Y, Bit, D, P).


pixrgb-d(X, Y, R, G, B, D, P) :-
    number(R),
    number(G),
    number(B),
    between(0, 255, R),
    between(0, 255, G),
    between(0, 255, B),
    pixel(X, Y, [R, G, B], D, P).
    
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
flipPixsH(Dim, [H | T], [PixR | ListaR2]) :-
    pixel(X1, Y, Val, D, H),
    X2 is abs(X1 - Dim),
    pixel(X2, Y, Val, D, PixR),
    flipPixsH(Dim, T, ListaR2).


flipV(I, I2) :-
    image(W, H, Pixs, I),
    flipPixsV(H-1, Pixs, FPixs),
    image(W, H, FPixs, I2).

% abs(pix(X|Y) - img(W|H)), misma funcion, distintas variables
flipPixsV(_, [], []).
flipPixsV(Dim, [H | T], [PixR | ListaR2]) :-
    pixel(X, Y1, Val, D, H),
    Y2 is abs(Y1 - Dim),
    pixel(X, Y2, Val, D, PixR),
    flipPixsV(Dim, T, ListaR2).



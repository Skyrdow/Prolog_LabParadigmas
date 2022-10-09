pixel(X, Y, Val, D, [X, Y, Val, D]) :-
    number(X),
    number(Y),
    between(0, 255, D).

pixelGetVal([_, _, Val, _], Val).

pixbit-d(X, Y, Bit, D, P) :-
    pixel(X, Y, Bit, D, P),
    number(Bit),
    between(0, 1, Bit).


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
    pixel(X, Y, Hex, D, P),
    string(Hex),
    hexcheck(Hex).

hexcheck(Str) :-
    name(Str, [H | T]), % separar "#RRGGBB" en "#" y "RRGGBB"
    name(HexRGB, T), % reconstruir la string
    hex_bytes(HexRGB, Rgb), % transforma de string a [R, G, B]
    rgbcheck(Rgb).


image(Width, Height, Pixs, [Width, Height, -1, Pixs]) :-
    Width > 0,
    Height > 0.

imageGetPixs([_, _, _, Pixs], Pixs).
imageSetComp([Width, Height, -1, Pixs], Val, [Width, Height, Val, Pixs]).



imageIsBitmap(I) :-
    image(_, _, [H | _], I),
    pixbit-d(_, _, _, _, H). % revisa el primer pixel, ya que la entrada tiene pixeles homogeneos

imageIsPixmap(I) :-
    image(_, _, [H | _], I),
    pixrgb-d(_, _, _, _, H). % revisa el primer pixel, ya que la entrada tiene pixeles homogeneos

imageIsHexmap(I) :-
    image(_, _, [H | _], I),
    pixhex-d(_, _, _, _, H). % revisa el primer pixel, ya que la entrada tiene pixeles homogeneos

imageIsCompressed([_, _, Comp, _]) :-
    Comp = -1.
    
% abs(pixY - imgH)
imageFlipH(I, I2) :-
    image(W, H, Pixs, I),
    flipPixsH(W-1, Pixs, FPixs),
    image(W, H, FPixs, I2).

flipPixsH(_, [], []).
flipPixsH(Dim, [H | T], [PixR | ListaR]) :-
    pixel(X1, Y, Val, D, H),
    X2 is abs(X1 - Dim),
    pixel(X2, Y, Val, D, PixR),
    flipPixsH(Dim, T, ListaR).


imageFlipV(I, I2) :-
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


imageCrop(I, X1, Y1, X2, Y2, I2) :-
    image(W, H, Pixs, I),
    cropPixs(X1, Y1, X2, Y2, Pixs, FPixs),
    NewW is 1+X2-X1,
    NewH is 1+Y2-Y1,
    image(NewW, NewH, FPixs, I2).

cropPixs(_, _, _, _, [], []).
cropPixs(X1, Y1, X2, Y2, [H | T], ListaR) :-
    cropPixs(X1, Y1, X2, Y2, T, ListaAux),
    pixel(X, Y, Val, D, H),
    X1 =< X, X =< X2, Y1 =< Y, Y =< Y2,
    NewX is X - X1,
    NewY is Y - Y1,
    pixel(NewX, NewY, Val, D, PixR),
    ListaR = [PixR | ListaAux].
cropPixs(X1, Y1, X2, Y2, [H | T], ListaR) :-
    cropPixs(X1, Y1, X2, Y2, T, ListaAux),
    pixel(X, Y, Val, D, H),
    ListaR = ListaAux.

imageRGBToHex(I, I2):-
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

imageToHistogram(I, R) :-
    image(_, _, _, I),
    imageGetPixs(I, Pixs),
    pixsToVal(Pixs, Vals),
    max_member(R, Vals).

pixsToVal([], []).
pixsToVal([Pix | T], [Val | ListaR]) :-
    pixelGetVal(Pix, Val),
    pixsToVal(T, ListaR).

imageRotate90(I, I2) :-
    image(W, H, Pixs, I),
    pixsSwapXY(Pixs, PixsSwap),
    flipPixsH(W-1, PixsSwap, PixsR),
    image(H, W, PixsR, I2).

pixsSwapXY([], []).
pixsSwapXY([Pix | T], [PixR | ListaR]) :-
    pixel(X, Y, Val, D, Pix),
    pixel(Y, X, Val, D, PixR),
    pixsSwapXY(T, ListaR).

imageCompress(I, [W, H, CompVal, PixsR]) :-
    imageToHistogram(I, CompVal),
    image(W, H, Pixs, I),
    erasePixs(Pixs, CompVal, PixsR).

erasePixs([], _, []).
% CASO 1, VAL = ERASEVAL
erasePixs([[X, Y, EraseVal, D] | T], EraseVal, ListaR) :-
    erasePixs(T, EraseVal, ListaR).
% CASO 2, VAL != ERASEVAL
erasePixs([Pix | T], EraseVal, [Pix | ListaR]) :-
    erasePixs(T, EraseVal, ListaR).

imageChangePixel(I, PMod, I2) :-
    image(W, H, Pixs, I),
    changePixs(Pixs, PMod, PixsR),
    image(W, H, PixsR, I2).

changePixs([], _, []).
changePixs([[X, Y, Val, D] | T], [X, Y, NewVal, NewD], [[X, Y, NewVal, NewD] | T]).
changePixs([Pix | T], PMod, [Pix | ListaR]) :-
    changePixs(T, PMod, ListaR).

invertColorRGB(PE, PR) :-
    pixrgb-d(X, Y, R, G, B, D, PE),
    NewR is 255 - R,
    NewG is 255 - G,
    NewB is 255 - B,
    pixrgb-d(X, Y, NewR, NewG, NewB, D, PR).

imageString(I, Str) :-
    image(W, H, Pixs, I),
    imageIsBitmap(I),
    W1 is W - 1,
    pixbitToString(W1, Pixs, Str).

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



sortImage(I, I2) :-
    image(W, H, Pixs, I),
    image(W, H, PixsR, I2),
    W1 is W - 1,
    H1 is H - 1,
    sortPixs(W1, H1, Pixs, PixsR).

sortPixs(W, H, [H | T], [PixR | PixT])

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

img3(I) :-
    pixrgb-d( 0, 0, 1, 3, 4, 10, PA),
    pixrgb-d( 0, 1, 0, 3, 5, 20, PB),
    pixrgb-d( 1, 0, 0, 6, 7, 30, PC),
    pixrgb-d( 1, 1, 1, 4, 7, 8, PD),
    pixrgb-d( 1, 1, 1, 4, 7, 9, PE),
    pixrgb-d( 1, 1, 1, 4, 7, 80, PF),
    image( 2, 3, [PA, PB, PC, PD, PE, PF], I).

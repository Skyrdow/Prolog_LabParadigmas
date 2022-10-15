:- use_module(tda_pixel).

image(Width, Height, Pixs, [Width, Height, -1, Pixs]) :-
    Width > 0,
    Height > 0.

imageGetPixs([_, _, _, Pixs], Pixs).
imageGetComp([_, _, Comp, _], Comp).
imageSetComp([Width, Height, -1, Pixs], Val, [Width, Height, Val, Pixs]).


imageIsBitmap(I) :-
    image(_, _, [H | _], I),
    pixbit(_, _, _, _, H). % revisa el primer pixel, ya que la entrada tiene pixeles homogeneos

imageIsPixmap(I) :-
    image(_, _, [H | _], I),
    pixrgb(_, _, _, _, _, _, H). % revisa el primer pixel, ya que la entrada tiene pixeles homogeneos

imageIsHexmap(I) :-
    image(_, _, [H | _], I),
    pixhex(_, _, _, _, H). % revisa el primer pixel, ya que la entrada tiene pixeles homogeneos

imageIsCompressed(I) :-
    imageGetComp(I, Comp),
    not(Comp = -1).
    
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
    image(_, _, Pixs, I),
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
cropPixs(X1, Y1, X2, Y2, [_ | T], ListaR) :-
    cropPixs(X1, Y1, X2, Y2, T, ListaAux),
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
    msort(Vals, SortVals),
    clumped(SortVals, Clump),
    compoundToHistogram(Clump, R).

compoundToHistogram([], []).
compoundToHistogram([Cl | T], [Histo | ListaR]) :-
    compound_name_arguments(Cl, -, Histo),
    compoundToHistogram(T, ListaR).


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
    imageToHistogram(I, Histo),
    maxHistogram(Histo, [0, 0], CompVal),
    image(W, H, Pixs, I),
    erasePixs(Pixs, CompVal, PixsR).

maxHistogram([], [MaxVal, _], MaxVal).
maxHistogram([[Val | Count] | T], [_, MaxCount], MaxHVal) :-
    Count > MaxCount,
    maxHistogram(T, [Val | Count], MaxHVal).
maxHistogram([_ | T], Max, MaxHVal) :-
    maxHistogram(T, Max, MaxHVal).

erasePixs([], _, []).
% CASO 1, VAL = ERASEVAL
erasePixs([[_, _, EraseVal, _] | T], EraseVal, ListaR) :-
    erasePixs(T, EraseVal, ListaR).
% CASO 2, VAL != ERASEVAL
erasePixs([Pix | T], EraseVal, [Pix | ListaR]) :-
    erasePixs(T, EraseVal, ListaR).

imageChangePixel(I, PMod, I2) :-
    image(W, H, Pixs, I),
    changePixs(Pixs, PMod, PixsR),
    image(W, H, PixsR, I2).

changePixs([], _, []).
changePixs([[X, Y, _, _] | T], [X, Y, NewVal, NewD], [[X, Y, NewVal, NewD] | T]).
changePixs([Pix | T], PMod, [Pix | ListaR]) :-
    changePixs(T, PMod, ListaR).

invertColorRGB(PE, PR) :-
    pixrgb(X, Y, R, G, B, D, PE),
    NewR is 255 - R,
    NewG is 255 - G,
    NewB is 255 - B,
    pixrgb(X, Y, NewR, NewG, NewB, D, PR).

imageString(I, Str) :-
    image(W, _, _, I),
    imageIsBitmap(I),
    W1 is W - 1,
    sortImage(I, I2),
    imageGetPixs(I2, Pixs),
    pixbitToString(W1, Pixs, Str).
imageString(I, Str) :-
    image(W, _, _, I),
    imageIsPixmap(I),
    W1 is W - 1,
    sortImage(I, I2),
    imageGetPixs(I2, Pixs),
    pixrgbToString(W1, Pixs, Str).
imageString(I, Str) :-
    image(W, _, _, I),
    imageIsHexmap(I),
    W1 is W - 1,
    sortImage(I, I2),
    imageGetPixs(I2, Pixs),
    pixhexToString(W1, Pixs, Str).


sortImage(I, I2) :-
    image(W, H, Pixs, I),
    image(W, H, PixsR, I2),
    W1 is W - 1,
    H1 is H - 1,
    sortPixs(0, 0, W1, H1, Pixs, PixsR).

sortPixs(W, H, W, H, Pixs, [PixR]) :-
    findPix(W, H, Pixs, PixR).

sortPixs(W, J, W, H, Pixs, [PixR | PixT]) :-
    findPix(W, J, Pixs, PixR),
    NewI is 0,
    NewJ is J+1,
    sortPixs(NewI, NewJ, W, H, Pixs, PixT).

sortPixs(I, J, W, H, Pixs, [PixR | PixT]) :-
    findPix(I, J, Pixs, PixR),
    NewI is I+1,
    sortPixs(NewI, J, W, H, Pixs, PixT).

findPix(X, Y, [], [X, Y, -1, 0]).
findPix(X, Y, [[X, Y, Val, D] | _], [X, Y, Val, D]).
findPix(X, Y, [_ | T], PixR) :-
    findPix(X, Y, T, PixR).

imageDepthLayers(I, LI) :-
    imageIsBitmap(I),
    imageGetPixs(I, Pixs),
    pixsToDepths(Pixs, Depths),
    bitDepthLayersGen(I, Depths, LI).

imageDepthLayers(I, LI) :-
    imageIsPixmap(I),
    imageGetPixs(I, Pixs),
    pixsToDepths(Pixs, Depths),
    rgbDepthLayersGen(I, Depths, LI).

imageDepthLayers(I, LI) :-
    imageIsHexmap(I),
    imageGetPixs(I, Pixs),
    pixsToDepths(Pixs, Depths),
    hexDepthLayersGen(I, Depths, LI).

pixsToDepths([], []).
pixsToDepths([Pix | T], [D | ListaR]) :-
    pixelGetDepth(Pix, D),
    pixsToDepths(T, ListaR).

bitDepthLayersGen(_, [], []).
bitDepthLayersGen(I, [IterDep | T], [DImg | ListaR]) :-
    image(W, H, Pixs, I),
    bitDepthPixs(Pixs, IterDep, PixsDep),
    image(W, H, PixsDep, DImg),
    bitDepthLayersGen(I, T, ListaR).

bitDepthPixs([], _, []).
bitDepthPixs([Pix | T], D, [Pix | ListaR]) :-
    pixelGetDepth(Pix, D),
    bitDepthPixs(T, D, ListaR).
bitDepthPixs([Pix | T], Depth, [P | ListaR]) :-
    pixbit(X, Y, _, D, Pix),
    pixbit(X, Y, 1, D, P),
    bitDepthPixs(T, Depth, ListaR).

rgbDepthLayersGen(_, [], []).
rgbDepthLayersGen(I, [IterDep | T], [DImg | ListaR]) :-
    image(W, H, Pixs, I),
    rgbDepthPixs(Pixs, IterDep, PixsDep),
    image(W, H, PixsDep, DImg),
    rgbDepthLayersGen(I, T, ListaR).

rgbDepthPixs([], _, []).
rgbDepthPixs([Pix | T], D, [Pix | ListaR]) :-
    pixelGetDepth(Pix, D),
    rgbDepthPixs(T, D, ListaR).
rgbDepthPixs([Pix | T], Depth, [P | ListaR]) :-
    pixrgb(X, Y, _, _, _, D, Pix),
    pixrgb(X, Y, 255, 255, 255, D, P),
    rgbDepthPixs(T, Depth, ListaR).

hexDepthLayersGen(_, [], []).
hexDepthLayersGen(I, [IterDep | T], [DImg | ListaR]) :-
    image(W, H, Pixs, I),
    hexDepthPixs(Pixs, IterDep, PixsDep),
    image(W, H, PixsDep, DImg),
    hexDepthLayersGen(I, T, ListaR).

hexDepthPixs([], _, []).
hexDepthPixs([Pix | T], D, [Pix | ListaR]) :-
    pixelGetDepth(Pix, D),
    hexDepthPixs(T, D, ListaR).
hexDepthPixs([Pix | T], Depth, [P | ListaR]) :-
    pixhex(X, Y, _, D, Pix),
    pixhex(X, Y, "#FFFFFF", D, P),
    hexDepthPixs(T, Depth, ListaR).

imageDecompress(I, I) :-
    not(imageIsCompressed(I)).

imageDecompress([W, H, Comp, Pixs], I2) :-
    W1 is W - 1,
    H1 is H - 1,
    decompPixs(0, 0, W1, H1, Comp, Pixs, DecompPixs),
    image(W, H, DecompPixs, I2).

decompPixs(W, H, W, H, CompVal, Pixs, [PixR]) :-
    findPix(W, H, Pixs, PixF),
    decompPix(PixF, CompVal, PixR).

decompPixs(W, J, W, H, CompVal, Pixs, [PixR | PixT]) :-
    findPix(W, J, Pixs, PixF),
    decompPix(PixF, CompVal, PixR),
    NewI is 0,
    NewJ is J+1,
    decompPixs(NewI, NewJ, W, H, CompVal, Pixs, PixT).

decompPixs(I, J, W, H, CompVal, Pixs, [PixR | PixT]) :-
    findPix(I, J, Pixs, PixF),
    decompPix(PixF, CompVal, PixR),
    NewI is I+1,
    decompPixs(NewI, J, W, H, CompVal, Pixs, PixT).

decompPix([X, Y, -1, 0], CompVal, DecomPix) :-
    pixel(X, Y, CompVal, 0, DecomPix).
decompPix(Pix, _, Pix).


img1(I) :-
    pixbit( 0, 0, 1, 10, PA),
    pixbit( 0, 1, 0, 20, PB),
    pixbit( 1, 0, 0, 30, PC),
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

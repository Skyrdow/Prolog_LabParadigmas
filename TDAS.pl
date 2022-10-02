pixbit(X, Y, Bit, Depth, [X, Y, Bit, Depth]) :-
    number(X),
    number(Y),
    between(0, 1, Bit),
    between(0, 255, Depth).

pixrgb(X, Y, [R, G, B], Depth, [X, Y, [R, G, B], Depth]) :-
    number(X),
    number(Y),
    between(0, 255, R),
    between(0, 255, G),
    between(0, 255, B),
    between(0, 255, Depth).

pixhex(X, Y, Hex, Depth, [X, Y, Hex, Depth]) :-
    number(X),
    number(Y),
    between(0, 255, Depth).

image(Width, Height, Pixs, [Width, Height, Pixs]) :-
    .
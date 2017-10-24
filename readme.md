haskello -- reversi game implementation with Haskell

usage :

    compile haskello.hs with ghc command here :

    $ghc --make haskello.hs

    and execute command as follows :

    $./hasello

    then, you will see reversi game board.
    to start game, put disk by using command shown below.
    if you want to quit this programme, then input C-d.

command :

    (1)

    xn D

    -- put disk command.
    x is [a-h], n is [1-8] D is @ or O(capital O).
    xn represent coordinate where you want to put disk and
    D means the kind of the disk.
    you have to separate xn and D with a blank charactor ' '.

    <e.g.>
    if you put disk @ at a4 on board, input

    a4 @

    (2)

    C-d

    -- quit game command.

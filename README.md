cookie-quicker
==============

Attempt calculate optimal build order for Cookie Clicker http://orteil.dashnet.org/cookieclicker/

Status
------

So far Cookie Quicker only models a simple version of the game
containing only cursors and grandmas. It does not yet make judgements
between different strategies.

Requirements
------------

Cookie clicker assumes the Haskell Platform, and also requires fclabels.

Usage
-----

There is no real main functionality but the module can be loaded and
used interactively, for example, the following will compute the game
state after saving up for, and buying a cursor, then a grandma.

    [cookie-quicker]$ ghci
    GHCi: http://www.haskell.org/ghc/  :? for help
    Prelude> :load cookie-quicker.hs
    [1 of 1] Compiling Main             ( cookies.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> playSequence [Buy Cursor, Buy Grandma]
    GameState {_time = 28.140244, _cookies = 0.0, _cursors = 1, _grandmas = 1}


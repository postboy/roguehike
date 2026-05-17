# roguehike

A simple roguelike hiking game in Clojure. Go climb a mountain here!

You are standing at the foot of the mountain. If you ascend the mountain and then return to wilderness edge, you win. If you give up and quit, you lose. There is no saving, just as in real life.

Use numpad or vi keys (hjklyubn) for moving. Press 5 or r to rest. Press q to exit.

|Symbol|Meaning|
|:----:|:-----:|
|i|that's you, hiker|
|space|ground|
|*|moss|
|"|tall grass|
|. , `|stone|
|o|small rock|
|0 O|big rock|
|w|small bush|
|W|big bush|
|t|small tree|
|T|big tree|
|~|puddle|

Everything is walkable except puddles, big rocks, bushes and trees.

And don't forget: mountains are always worth climbing!

## Building

Requires Leiningen.

    lein run

## License

Source code: MIT/X11

Copyright 2024 Ivan Zuboff

Copyright 2012 Steve Losh (see his [zen project](https://github.com/sjl/zen) used as base for this one)

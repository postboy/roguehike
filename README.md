# roguehike

A simple roguelike hiking game in Clojure. Go climb a mountain here!

You are standing at the foot of the mountain. If you ascend the mountain and then return to wilderness edge, you win. If you give up and quit, you lose. There is no saving, just as in real life.

Use numpad or vi keys (hjklyubn) for moving. Press 5 or r to rest. Press q to exit.

"i" symbol on the screen is you, hiker. Other used symbols and their meanings are:

Non-obstacles:
|Symbol|Meaning|
|:----:|:-----:|
|space|ground|
|. , `|stone|
|*|moss|
|"|tall grass|
|o|small rock|
|w|small bush|
|t|small tree|

Obstacles:
|Symbol|Meaning|
|:----:|:-----:|
|0 O|big rock|
|W|big bush|
|T|big tree|
|~|puddle|
|_|fallen tree|

And don't forget: mountains are always worth climbing!

## Building

Requires Leiningen.

    lein run

## License

Source code: MIT/X11

Copyright 2024 Ivan Zuboff

Copyright 2012 Steve Losh (see his [zen project](https://github.com/sjl/zen) used as base for this one)

# roguehike

A minimalistic roguelike hiking game in Clojure. Go climb a mountain here!

You are standing at foot of the mountain. If you ascend the mountain and then return to wilderness edge, you win. If you give up and quit, you lose. There is no saving, just as in real life.

Status bar is located at the bottom of the screen. It shows your current and maximum stamina, current and maximum altitude on this terrain, and status message. Everything apart the status bar is the map shown via top-down view. Top of the mountain is located to the north from starting point of your hike.

Use **numpad (1-4, 6-9)** or **vi keys (hjklyubn)** for moving. Press **5** or **r** to rest. Press **c** to redraw the screen placing user in its center. Press **q** to quit.

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
|@|puddle|
|=|fallen tree|

And don't forget: mountains are always worth climbing!

## Running

First optional argument is terminal type to launch the game in: auto (default)/swing/text/unix/cygwin.

## Building

Requires Leiningen.

To run:

    lein run

To build release jar:

    lein uberjar

## Reference

Zmeinaya Mountain (Karelia, Russia) located at 61.480991, 30.217564.

## License

Source code: MIT/X11

Copyright 2024 Ivan Zuboff

Copyright 2012 Steve Losh (his [zen project](https://github.com/sjl/zen) served as base for this one)

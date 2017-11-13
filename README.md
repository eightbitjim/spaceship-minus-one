# viccy-spaceship
A game for the Commodore Vic 20. This is very much work in progress, so is only partially working at present.

![screenshot](/screenshot.jpg)

If you want to have a go, you should assemble using the [dasm assembler](https://github.com/cprieto/dasm) as follows:

`dasm game.asm -ogame.prg`

Then load `game.prg` into a VIC20 emulator such as Vice. Then run using:

`SYS 4612`

The only control is pressing space!


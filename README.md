# viccy-spaceship
A very minimal game for the Commodore Vic 20. Still working on it, but it's just about playable.
The only control is pressing space!

Design goal: write a fairly minimalist game to run on an unexpanded Vic-20, to load off cassette. Focus on making a simple, playable game.

![screenshot](/screenshot.jpg)

To load, you need to load and run `loader`, which then loads and runs the `game` file.
Pre-compiled versions are already in the `disks/spaceship-1.d64` disk image. Or you can attach the pre-compiled cartridge image (in the `cartridges` directory) to an emulator such as VICE.

If you want to compile it, you should assemble using the [dasm assembler](https://github.com/cprieto/dasm). First you need to add the path to the dasm assembler to your path, make sure that the compilation script is executable and then run it. Something like:

`export PATH=$PATH:/pathToYourDasmInstallation/dasm/bin`

`chmod +x compile.sh`

`./compile.sh`

This will output files `loader` and `game` into the `output/tapeAndDisk` directory. It will also output two cartridge files into `output/cartridge`:

- `game-a000.bin`: a raw binary file suitable for putting onto a ROM
- `game.crt`: same as above except with 2 byte load address at the beginning, suitable to attach as a cartridge image on VICE.



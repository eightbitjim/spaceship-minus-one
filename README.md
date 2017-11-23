# viccy-spaceship
A game for the Commodore Vic 20. Still working on on, it it's just about playable.
The only control is pressing space!

![screenshot](/screenshot.jpg)

To load, you need either `loader1` (for tape) or `loader8` (for disk), and the `game` file.
Pre-compiled versions are already in the `disks/spaceship-1.d64` disk image. 

For the VICE emulator, mount the `output` directory as drive 8 (if you have compiled the game yourself) or attach the pre-made disk image `spaceship-1.d64` and make sure you are running a VIC-20. Don't use "smart attach"... just attach the disk image.

Enter:

`LOAD"LOADER8",8`

and then `run`.

If you want to compile it, you should assemble using the [dasm assembler](https://github.com/cprieto/dasm). First you need to add the path to the dasm assembler to your path, make sure that the compilation script is executable and then run it. Something like:

`export PATH=$PATH:/pathToYourDasmInstallation/dasm/bin`

`chmod +x compile.sh`

`./compile.sh`

This will output a new file `game` into the `output` directory.


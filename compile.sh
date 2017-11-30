#!/bin/sh
mkdir -p output/tapeAndDisk
mkdir -p output/cartridge
mkdir -p temp

# copy the basic loaders that cover disk and tape
cp loaders/loader output/tapeAndDisk

# compile the game for disk or tape, creating "game", which is a PRG file
dasm game.asm -ooutput/tapeAndDisk/game

# compile the cartridge version of the game as a raw binary
dasm game.asm -f3 -otemp/game.bin 

# compile the cartridge loader
dasm cartridgeStub.asm -f3 -ooutput/cartridge/game-a000.bin
dasm cartridgeStub.asm -f3 -ooutput/cartridge/game.crt -MCRT

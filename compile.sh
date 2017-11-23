#!/bin/sh
mkdir -p output
cp loaders/loader8 output/
cp loaders/loader1 output/
dasm game.asm -ooutput/game


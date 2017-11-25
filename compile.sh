#!/bin/sh
mkdir -p output
cp loaders/loader output/
dasm game.asm -ooutput/game


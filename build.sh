#!/bin/sh

cd nim
nim $1
cd ..
gprbuild -P steamsky.gpr -XMode=$1

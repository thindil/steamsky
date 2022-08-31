#!/bin/sh

cd nim && nim debug && cd .. && gprbuild -P steamsky.gpr

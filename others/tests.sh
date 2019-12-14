#!/bin/sh

cd "tests/driver"

for i in `seq 1 $1`;
do
   result=$(./test_runner)
   echo "$result"
   if [[ "$result" =~ "FAILED" ]] || [[ "$result" =~ "CRASHED" ]]; then
      exit 1
   fi
done

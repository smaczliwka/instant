#!/bin/bash
if [ $# -ne 2 ]; then
  echo "Invalid number of arguments"
else
  FILES="${2}/*.ins"
  for f in ${FILES}
  do
    echo $f
     ./"${1}" "${f}"
  done
  cd "${2}"
  FILES="*.ins"
  for f in ${FILES}
  do
    echo $f
    lli "${f::-4}".bc>tmp.out
    diff tmp.out "${f::-4}".output
    rm "${f::-4}".ll
    rm "${f::-4}".bc
  done
  rm tmp.out
fi

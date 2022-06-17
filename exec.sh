#!/usr/bin/env bash
k=0

compile_and_diff () {
  # Compilar e interpretar código
  ./MicroC casos/caso${k}.mc < casos/caso${k}.in > casos/salida${k}.out
  # Código optimizado
  ./MicroC -o casos/caso${k}.mc < casos/caso${k}.in > casos/salida${k}.opt

  # Diff opt
  if ! diff casos/caso${k}.opt casos/salida${k}.opt > /dev/null; then
    echo "salida${k}.opt";
    diff casos/caso${k}.opt casos/salida${k}.opt
    echo ""
  fi

  # Diff out
  if ! diff casos/caso${k}.out casos/salida${k}.out > /dev/null; then
    echo "salida${k}.out"
    diff casos/caso${k}.out casos/salida${k}.out
    echo ""
  fi
}

for i in {1..40}
do
  if [ "$i" -lt 10 ]; then
    k=0
    k+=$i
  else
    k=$i
  fi

  compile_and_diff
done

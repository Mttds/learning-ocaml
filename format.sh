#!/usr/bin/env bash

for f in $(ls *.ml); do
  echo "Formatting $f..."
  ocamlformat "$f" > ${f}.tmp && mv ${f}.tmp ${f}
done

echo "Done!"

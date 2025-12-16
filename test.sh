for file in tests/*; do
  if [ -f "$file" ]; then
    echo "Running check for: $file"
    dune exec ./lgc.exe -- "$file" -check
  fi
done

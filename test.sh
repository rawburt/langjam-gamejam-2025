for file in tests/*; do
  if [ -f "$file" ]; then
    echo "Running analysis for: $file"
    dune exec ./lgc.exe -- "$file" -analyze
  fi
done

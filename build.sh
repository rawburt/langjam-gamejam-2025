mkdir -p engine/assets
dune exec ./lgc.exe -- game/main.lg -o engine/game.js
cp -r game/assets/. engine/assets/

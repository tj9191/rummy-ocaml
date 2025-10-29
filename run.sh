#!/bin/bash

set -e

echo "Removing old build..."
rm -f ui_bonsai/src/app.bc.js

echo "Building project with dune..."
dune build @all

echo "Copying compiled JS to ui_bonsai/src/..."
cp _build/default/ui_bonsai/src/app.bc.js ui_bonsai/src/app.bc.js

echo "Starting local server at http://localhost:8001/ui_bonsai/src/index.html"
echo "Press Ctrl+C to stop the server."

# Open the game automatically in your browser (macOS only)
open "http://localhost:8001/ui_bonsai/src/index.html" &

# Start the server on port 8001
python3 -m http.server 8001
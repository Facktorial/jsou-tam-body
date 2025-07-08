#!/bin/bash
# Start your server in background
stack exec data-api &

# Run ghcid in foreground to watch and reload
ghcid --command="stack repl data-api:exe:data-api --allow-different-user --color always" --test=main

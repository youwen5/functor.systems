default:
  cabal build --allow-newer
  cp "$(cabal list-bin website --allow-newer).jsexe"/all.js out/all.js

full-build:
  mkdir -p out
  cabal update
  cabal build --allow-newer
  rsync -r static/* out
  cp "$(cabal list-bin website --allow-newer).jsexe"/all.js out/all.js

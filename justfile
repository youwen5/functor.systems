default:
  mkdir -p out
  cabal update
  cabal build --allow-newer
  rsync -r static/* out
  cp "$(cabal list-bin website --allow-newer).jsexe"/all.js out/all.js

partial:
  cabal build --allow-newer
  cp "$(cabal list-bin website --allow-newer).jsexe"/all.js out/all.js

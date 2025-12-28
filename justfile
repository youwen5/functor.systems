default: setup make-styles
  javascript-unknown-ghcjs-cabal build
  rsync -r static/* out
  cp "$(javascript-unknown-ghcjs-cabal list-bin website)" out/all.js

make-styles:
  cabal run styles

setup:
  mkdir -p out

[working-directory: 'out']
preview: default
  live-server --open

default:
  mkdir -p out
  javascript-unknown-ghcjs-cabal build
  rsync -r static/* out
  cp "$(javascript-unknown-ghcjs-cabal list-bin website)" out/all.js

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Clay
import qualified Clay.Media as M
import Data.Text.Lazy (unpack)
import ReadArgs (readArgs)

main :: IO ()
main = do
    a :: String <- readArgs
    writeFile a $ unpack $ render stylesheet

stylesheet :: Css
stylesheet = do
    query M.all [M.minWidth $ px 800] $ body ? fontSize (Clay.rem 1.2)
    ".dark-only" ? display none
    query M.all [M.prefersColorScheme M.dark] $ ".light-only" ? display none >> ".dark-only" ? display block
    ".main-logo" ? do
        maxWidth $ px 400
        width $ pct 100
        marginLeft auto
        marginRight auto
        aspectRatio 1
    ".services-table" ? do
        marginTop $ px 20
        marginBottom $ px 20
        maxWidth $ pct 100
        overflowX auto
    "projects-table" ? do
        marginTop $ px 10
        maxWidth $ pct 100
        overflowX auto

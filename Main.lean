import VersoBlog
import Site.Extensions
import Site.FrontPage
import Site.Theme

open Verso Genre Blog Site Syntax

def mySite : Site := site Site.FrontPage /
  static "" ← "static"
  "" Site.FrontPage

def main := blogMain Site.theme mySite

import VersoBlog

open Lean Elab Doc Syntax
open Verso Doc ArgParse Genre.Blog Output.Html
-- Ideally I'd like to open `Verso.Doc` too but it conflicts with `Lean.Doc`

@[inline_expander Lean.Doc.Syntax.text]
def emDashExpander : Elab.InlineExpander := fun stx =>
  match stx with
  | `(inline| $s:str) =>
    ``(Verso.Doc.Inline.text $(quote (s.getString.replace "--" "&mdash;")))
  | _ => throwUnsupportedSyntax

structure TableArgs where
  «class» : String

meta instance : FromArgs TableArgs Elab.DocElabM where
  fromArgs := TableArgs.mk <$> ArgParse.namedD `class ArgParse.ValDesc.string ""

def splitLines (inlines : Array Syntax) : Array (Array Syntax) := Id.run do
  let mut lines : Array (Array Syntax) := #[]
  let mut current : Array Syntax := #[]
  for stx in inlines do
    if let `(inline| line! $_:str) := stx then
      lines := lines.push current
      current := #[]
    else
      current := current.push stx
  return lines.push current

def splitOnPipes (inlines : Array Syntax) :
    Elab.DocElabM (Array (Array Syntax)) := do
  let mut cells : Array (Array Syntax) := #[]
  let mut current : Array Syntax := #[]
  for stx in inlines do
    if let `(inline| $s:str) := stx then
      let mut first := true
      for piece in s.getString.splitOn "|" do
        if !first then
          cells := cells.push current
          current := #[]
        first := false
        let piece := piece.trimAscii.toString
        if piece ≠ "" then
          current := current.push (← `(inline| $(quote piece):str))
    else
      current := current.push stx
  -- Ignore the first empty cell
  return cells.extract 1

meta def cellBlock (tag : String) (cell : Array Syntax) :
    Elab.DocElabM Term := do
  let inlines ← cell.mapM (Elab.elabInline ⟨·⟩)
  let para ← ``(Verso.Doc.Block.para #[$[$inlines],*])
  ``(Verso.Doc.Block.other (BlockExt.htmlWrapper $(quote tag) #[]) #[$para])

meta def rowBlock (tag : String) (cells : Array (Array Syntax)) :
    Elab.DocElabM Term := do
  let cellTerms ← cells.mapM fun cell => cellBlock tag cell
  ``(Verso.Doc.Block.other (BlockExt.htmlWrapper "tr" #[]) #[$[$cellTerms],*])

@[directive]
meta def table : Elab.DirectiveExpanderOf TableArgs
  | {«class»}, blocks => do
    let #[block] := blocks
      | throwError "Table cannot contain blank lines"
    let `(block| para[ $inlines* ]) := block.raw
      | throwErrorAt block "Expected a paragraph"
    let rows ← (splitLines inlines).mapM splitOnPipes
    if h : rows.size < 2 then
      throwErrorAt block "Expected at least two lines"
    else
      let headerRow ← rowBlock "th" rows[0]
      let bodyRows ← (rows.extract 2).mapM (rowBlock "td")
      let thead ← ``(Verso.Doc.Block.other (BlockExt.htmlWrapper "thead" #[]) #[$headerRow])
      let tbody ← ``(Verso.Doc.Block.other (BlockExt.htmlWrapper "tbody" #[]) #[$[$bodyRows],*])
      let attrs : Array (String × String) :=
        if «class» == "" then #[] else #[("class", «class»)]
      ``(Verso.Doc.Block.other (BlockExt.htmlWrapper "table" $(quote attrs)) #[$thead, $tbody])

structure DarkModePictureArgs where
  src : String
  dark : String
  alt : String

meta instance : FromArgs DarkModePictureArgs Elab.DocElabM where
  fromArgs :=
    DarkModePictureArgs.mk
      <$> ArgParse.named' `src false
      <*> ArgParse.named' `dark false
      <*> ArgParse.named' `alt false

set_option quotPrecheck false in
@[directive]
meta def darkModePicture : Elab.DirectiveExpanderOf DarkModePictureArgs
  | {src, dark, alt}, _ => do
    ``(Verso.Doc.Block.other (BlockExt.blob {{
        <picture>
          <source srcset=$(quote dark):str media="(prefers-color-scheme: dark)" />
          <img src=$(quote src):str alt=$(quote alt):str />
        </picture>
      }}) #[])

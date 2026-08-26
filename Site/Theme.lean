import VersoBlog
open Verso Genre Blog

def siteCSS : String := include_str "style.css"

namespace Site

open Output Html Template Theme in
def theme : Theme := { Theme.default with
  primaryTemplate := do
    return {{
      <html>
        <head>
          <meta charset="UTF-8" />
          <meta name="viewport" content="width=device-width, initial-scale=1.0" />
          <title>{{← param (α := String) "title"}}</title>
          <meta
            name="description"
            content="a homotopy-coherent free software collective."
          />
          <meta name="author" content="Youwen Wu <youwen@functor.systems>" />
          <meta property="og:title" content="functor.systems >> Home" />
          <meta property="og:type" content="website" />
          <meta property="og:url" content="https://functor.systems" />
          <meta
            property="og:description"
            content="a homotopy-coherent free software collective."
          />
          <link rel="icon" type="image/png" href="/favicon/favicon-96x96.png" sizes="96x96" />
          <link rel="icon" type="image/svg+xml" href="/favicon/favicon.svg" />
          <link rel="apple-touch-icon" sizes="180x180" href="/favicon/apple-touch-icon.png" />

          <link rel="stylesheet" href="latex.css" />
          <!-- 100% privacy-first analytics -->
          <script
            data-collect-dnt="true"
            async
            src="https://scripts.simpleanalyticscdn.com/latest.js"
          ></script>
          <noscript
            ><img
              src="https://queue.simpleanalyticscdn.com/noscript.gif?collect-dnt=true"
              alt=""
              referrerpolicy="no-referrer-when-downgrade"
          /></noscript>
          {{← builtinHeader}}
        </head>
        <body class="latex-dark-auto text-justify">
          {{← param "content"}}
        </body>
      </html>
    }}
  cssFiles := #[("style.css", siteCSS)]
}

end Site

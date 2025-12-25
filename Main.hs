{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

----------------------------------------------------------------------------
module Main where

----------------------------------------------------------------------------
import Miso
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import Miso.Lens

----------------------------------------------------------------------------

-- | Component model state
data Model where
    Model :: {_counter :: Int} -> Model
    deriving (Show, Eq)

----------------------------------------------------------------------------
counter :: Lens Model Int
counter = lens _counter $ \record field -> record{_counter = field}

----------------------------------------------------------------------------

-- | Sum type for App events
data Action
    = AddOne
    | SubtractOne
    | SayHelloWorld
    deriving (Show, Eq)

----------------------------------------------------------------------------

-- | Entry point for a miso application
main :: IO ()
main = run (startApp app)

----------------------------------------------------------------------------

-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------

-- | `component` takes as arguments the initial model, update function, view function
app :: App Model Action
app = component emptyModel updateModel viewModel

----------------------------------------------------------------------------

-- | Empty application state
emptyModel :: Model
emptyModel = Model 0

----------------------------------------------------------------------------

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Transition Model Action
updateModel = \case
    AddOne -> counter += 1
    SubtractOne -> counter -= 1
    SayHelloWorld -> io_ $ do
        alert "Hello World"
        consoleLog "Hello World"

----------------------------------------------------------------------------

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Model Action
viewModel x =
    H.div_
        [ H.h1_
            []
            [ H.img_
                [ P.src_ "https://code.functor.systems/functor.systems/identity/raw/branch/main/assets/canonical/hi-res-transparent-text.webp"
                , P.alt_ "functor.systems logo, a torus with three arrows pointing across"
                , P.class_ "main-logo light-only"
                ]
            , H.img_
                [ P.src_ "https://code.functor.systems/functor.systems/identity/raw/branch/main/assets/canonical/hi-res-transparent-text-dark.webp"
                , P.alt_ "functor.systems logo, a torus with three arrows pointing across"
                , P.class_ "main-logo dark-only"
                ]
            ]
        , H.div_
            [P.class_ "definition"]
            [ text "A "
            , H.a_
                [ P.target_ "_blank"
                , P.href_ "https://ncatlab.org/nlab/show/homotopy+coherent+diagram"
                ]
                [text "homotopy-coherent"]
            , text " collective of free software hackers. We are a "
            , H.em_ [] [text "proper subset"]
            , text " of the "
            , H.a_ [P.href_ "https://www.mit.edu/~ajzd/opencompute/"] [text " MIT OpenCompute Laboratory"]
            , text "."
            ]
        , H.div_
            [P.class_ "theorem"]
            [ text "functor.systems maintains vital digital infrastructure for its community of hackers."
            ]
        , H.div_
            [P.class_ "proof"]
            [ "We operate the following."
            , H.table_
                [P.class_ "services-table"]
                [ H.tr_
                    []
                    [ H.th_ [] [text "Service"]
                    , H.th_ [] [text "URL"]
                    , H.th_ [] [text "Status"]
                    ]
                , H.tr_
                    []
                    [ H.td_ [] [text "Matrix homeserver"]
                    , H.td_ [] [H.a_ [P.href_ "/matrix"] [text "matrix.functor.systems"]]
                    , H.td_ [] [text "Operational"]
                    ]
                , H.tr_
                    []
                    [ H.td_ [] [text "Git forge"]
                    , H.td_ [] [H.a_ [P.href_ "/code"] [text "code.functor.systems"]]
                    , H.td_ [] [text "Operational"]
                    ]
                , H.tr_
                    []
                    [ H.td_ [] [text "The Commutative Webring"]
                    , H.td_ [] [H.a_ [] [text "ring.functor.systems"]]
                    , H.td_ [] [text "Coming soon"]
                    ]
                , H.tr_
                    []
                    [ H.td_ [] [text "Attic, a Nix binary cache"]
                    , H.td_ [] [H.a_ [] [text "cache.functor.systems"]]
                    , H.td_ [] [text "Tentatively coming"]
                    ]
                , H.tr_
                    []
                    [ H.td_ [] [text "Hydra, Nix continuous integration"]
                    , H.td_ [] [H.a_ [] [text "hydra.functor.systems"]]
                    , H.td_ [] [text "Tentatively coming"]
                    ]
                , H.tr_
                    []
                    [ H.td_ [] [text "Grafana, resource monitoring"]
                    , H.td_ [] [H.a_ [] [text "status.functor.systems"]]
                    , H.td_ [] [text "Coming soon"]
                    ]
                ]
            , text "We also provide "
            , H.code_ [] [text "*.functor.systems"]
            , text " subdomains to our members. If you’d like a "
            , H.code_ [] [text "functor.systems"]
            , text " subdomain, please see the "
            , H.a_ [P.href_ "#contact"] [text "contact information"]
            , text " below."
            ]
        , text "Most of our infra is hosted on various homelabs operated by members, running functorOS 25.11 (Spivak), our custom distro based on NixOS 25.11 (Xantusia). See "
        , H.a_ [] [text "status.functor.systems"]
        , text " for status and vitals (coming soon)."
        , H.div_ [P.class_ "theorem"] [text "functor.systems is a project incubator for its members."]
        , H.div_ [P.class_ "proof"] [text " Following Theorem 1, we provide digital infrastructure for members’ projects as well as technical and community support. Additionally, the organization is suitable for hosting communal projects that make more sense managed and maintained collectively than in a personal account."]
        , H.h2_ [] [text "Projects"]
        , text "The following are maintained under the auspices of functor.systems."
        , H.table_
            [P.class_ "projects-table"]
            [ H.tr_
                []
                [ H.th_ [] [text "Name"]
                , H.th_ [] [text "URL"]
                , H.th_ [] [text "Description"]
                ]
            , H.tr_
                []
                [ H.td_ [] [text "eeXiv"]
                , H.td_ [] [H.a_ [P.href_ "https://eexiv.functor.systems"] [text "eexiv.functor.systems"]]
                , H.td_
                    []
                    [ text "A research repository for FIRST Robotics Competition related documents, inspired by the arXiv."
                    ]
                ]
            , H.tr_
                []
                [ H.td_ [] [text "functorOS"]
                , H.td_ [] [H.a_ [P.href_ "https://functor.systems/functorOS"] [text "functor.systems/functorOS"]]
                , H.td_
                    []
                    [ text "A highly experimental NixOS based Linux distribution."
                    ]
                ]
            ]
        , H.h2_ [] [text "Members"]
        , H.table_
            []
            [ H.tr_
                []
                [ H.th_ [] [text "User"]
                , H.th_ [] [text "Affiliation"]
                , H.th_ [] [text "Role"]
                ]
            , H.tr_
                []
                [ H.td_ [] [H.a_ [P.href_ "https://web.youwen.dev"] [text "Youwen"]]
                , H.td_ [] [text "UCSB Math + CS '28. MIT OCλ."]
                , H.td_
                    []
                    [ text "BDFL, Webmaster, chief NixOps engineer"
                    ]
                ]
            , H.tr_
                []
                [ H.td_ [] [H.a_ [P.href_ "https://kaitotlex.systems"] [text "Warren “Kaitotlex”"]]
                , H.td_ [] [text "SRVHS '26. MIT OCλ."]
                , H.td_
                    []
                    [ text "EE hacker, delinquent, inventor"
                    ]
                ]
            , H.tr_
                []
                [ H.td_ [] [H.a_ [P.href_ "https://monadi.cc"] [text "Ananth"]]
                , H.td_ [] [text "MIT Math w/ CS '28. MIT OCλ."]
                , H.td_
                    []
                    [ text "Intrepid Haskellian, undergrad category theorist, associate NixOps engineer"
                    ]
                ]
            , H.tr_
                []
                [ H.td_ [] [text "Anthony"]
                , H.td_ [] [text "MIT EECS '28. MIT OCλ."]
                , H.td_
                    []
                    [ text "EE hacker, delinquent, inventor"
                    ]
                ]
            , H.tr_
                []
                [ H.td_ [] [text "Colin"]
                , H.td_ [] [text "MIT EE + Physics '26. MIT OCλ."]
                , H.td_
                    []
                    [ text "javascript framework connoisseur, forklift certified"
                    ]
                ]
            , H.tr_
                []
                [ H.td_ [] [H.a_ [P.href_ "https://github.com/Nluo923"] [text "Nicholas “nluo”"]]
                , H.td_ [] [text "Berkeley EECS '29. MIT OCλ."]
                , H.td_
                    []
                    [ text "osu gamer, yuri enjoyer"
                    ]
                ]
            ]
        , text "There are no membership dues—rather, members are encouraged to donate infrastructure and time as available."
        , H.h2_ [P.id_ "contact"] [text "Contact and join"]
        , H.p_
            []
            [ text "It is sufficient but not necessary to be a member of functor.systems to use our infrastructure. That is, all functor.systems members can freely use or request infrastructure, and in addition friends and other non-members "
            , H.b_ [] [text "may"]
            , text " be given access upon request."
            ]
        , H.p_
            []
            [ text "If you’d like to request access to any infrastructure—or a membership—please contact the webmaster:"
            , H.code_ [] [H.a_ [P.href_ "mailto:youwen@functor.systems"] [text "youwen@functor.systems"]]
            ]
        , H.p_ [] [text "In general, any friends of existing members or anyone with a reasonable interest in our projects will be granted membership and/or infra access upon request."]
        , H.h2_ [] ["Miscellany"]
        , H.p_
            []
            [ text "This website was written in Haskell in the "
            , H.a_ [P.href_ "https://haskell-miso.org/"] [text "miso"]
            , text "web framework, and compiled to JavaScript via the GHCjs backend."
            ]
        , H.p_
            []
            [ text "Suggest an edit to this page on "
            , H.a_ [P.href_ "https://code.functor.systems/functor.systems/website/_edit/main/public/index.html", P.target_ "_blank"] [text "code.functor.systems"]
            , text ". Note that a login is required—please request an account through the contact information above if you don’t have one."
            ]
        ]

----------------------------------------------------------------------------

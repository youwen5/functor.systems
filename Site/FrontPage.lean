import VersoBlog
import Site.Extensions

open Verso.Genre.Blog

#doc (Page) "functor.systems >> Home" =>

%%%
showInNav := false
htmlId := some "frontpage"
%%%

:::darkModePicture (src := "img/hi-res-transparent-text.webp") (dark := "img/hi-res-transparent-text-dark.webp") (alt := "functor.systems logo, a torus with three arrows pointing across")
:::

:::htmlDiv (class := "definition")
A [homotopy-coherent](https://ncatlab.org/nlab/show/homotopy+coherent+diagram) collective of free software hackers. We are a _proper subset_ of the [MIT OpenCompute Laboratory](https://www.mit.edu/~ajzd/opencompute/).
:::

:::htmlDiv (class := "theorem")
functor.systems maintains vital digital infrastructure for its community of hackers.
:::

::::htmlDiv (class := "proof")
We operate the following.

:::table (class := "services-table")
| Service | URL | Status |
|---|---|---|
| Matrix homeserver | [matrix.functor.systems](/matrix.html) | Operational |
| Git forge | [code.functor.systems](/code.html) | Operational |
| The Commutative Webring | ring.functor.systems | Coming soon |
| Attic, a Nix binary cache | cache.functor.systems | Tentatively coming |
| Hydra, Nix continuous integration | hydra.functor.systems | Tentatively coming |
| Grafana, resource monitoring | status.functor.systems | Coming soon |
:::

We also provide `*.functor.systems` subdomains to our members. If you'd like a `functor.systems` subdomain, please see the [contact information](#contact) below.
::::

Most of our infra is hosted on various homelabs operated by members, running functorOS 26.05 (Zardini), our custom distro based on NixOS 26.05 (Yarara). See status.functor.systems for status and vitals (coming soon).

:::htmlDiv (class := "theorem")
functor.systems is a project incubator for its members.
:::

:::htmlDiv (class := "proof")
Following Theorem 1, we provide digital infrastructure for members' projects as well as technical and community support. Additionally, the organization is suitable for hosting communal projects that make more sense managed and maintained collectively than in a personal account.
:::

# Projects

The following are maintained under the auspices of functor.systems.

:::table (class := "projects-table")
| Name | URL | Description |
|---|---|---|
| eeXiv | [eexiv.functor.systems](https://eexiv.functor.systems) | A research repository for FIRST Robotics Competition related documents, inspired by the arXiv. |
| functorOS | [functor.systems/functorOS](/functorOS.html) | A highly experimental NixOS based Linux distribution. |
:::

# Members

:::table
| User | Affiliation | Role |
|---|---|---|
| [Youwen](https://web.youwen.dev) | Berkeley Math + CS '28. MIT OCλ. | BDFL, Webmaster, chief NixOps engineer |
| [Warren "Kaitotlex"](https://kaitotlex.systems) | SJSU '30. MIT OCλ. | EE hacker, delinquent, inventor |
| [Ananth](https://monadi.cc) | MIT Math w/ CS '28. MIT OCλ. | Intrepid Haskellian, undergrad category theorist, associate NixOps engineer |
| Anthony D. | Building. MIT EECS (on leave). MIT OCλ. | plays age of empires |
| Anthony W. | MIT Math + CS '26, MEng '27. | plays age of empires |
| Colin | MIT EE + Physics '26. MIT OCλ. | javascript framework connoisseur, forklift certified |
| [Nicholas "nluo"](https://github.com/Nluo923) | Berkeley EECS '29. MIT OCλ. | osu gamer, yuri enjoyer |
| [Arvind](https://a.rvind.cc) | MVHS/DVC '27. MIT OCλ. | Interested in non-linear dynamics, language shift, & transformational music theory. Rustacean. |
:::

There are no membership dues--rather, members are encouraged to donate infrastructure and time as available.

# Contact and join

It is sufficient but not necessary to be a member of functor.systems to use our infrastructure. That is, all functor.systems members can freely use or request infrastructure, and in addition friends and other non-members *may* be given access upon request.

If you'd like to request access to any infrastructure--or a membership--please contact the webmaster: [youwen@functor.systems](mailto:youwen@functor.systems)

In general, any friends of existing members or anyone with a reasonable interest in our projects will be granted membership and/or infra access upon request.

# Miscellany

This website was written in Lean using the [Verso](https://verso.lean-lang.org/) static site generator.

Suggest an edit to this page on [code.functor.systems](https://code.functor.systems/functor.systems/website/src/branch/main/Site/FrontPage.lean). Note that a login is required--please request an account through the contact information above if you don't have one. Alternatively, clone the repository and send patches via email to the webmaster.

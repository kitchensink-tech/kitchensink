# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Kitchen-Sink is a static-site generator *and* a dev/serve HTTP daemon (plus API gateway / reverse proxy). Docs live at https://kitchensink-tech.github.io/. The `website-src/` directory is both the project's real website and the main end-to-end example of the input format.

```
hs/           Haskell sources (the whole tool: library + 2 executables)
purs/         PureScript frontend widgets (graphexplorer, search-box, kitchen-sink-compat)
scaffolding/  Templates + shell scripts to bootstrap a new site
website-src/  Kitchen-Sink sources for the project website (doubles as a fixture)
scripts/      Dev helpers exposed as "commands" in the dev-server UI
dhall/        Dhall prelude pin used when evaluating `.dhall` sections
```

## Build & run

All Haskell work happens in `hs/`. GHC 9.8.2 / cabal 3.12; `cabal.project` pulls `prodapi-{core,web,proxy}`, `purescript-bridge`, and a `prometheus-client` fork from git.

```bash
cd hs
cabal build              # library + kitchen-sink + kitchen-sink-purescript-bridge
cabal install            # puts `kitchen-sink` on PATH
cabal build lib:kitchen-sink   # library only (fastest feedback loop)
```

The library is compiled with `-Wall -Werror`: any warning is a build failure.

Running the tool (three subcommands, all in `KitchenSink.Engine`):

```bash
# one-shot build to a directory
kitchen-sink produce --srcDir website-src --outDir www

# dev server: filesystem watch, on-the-fly target production, /dev/* API
kitchen-sink serve --srcDir website-src --outputDir www --servMode DEV --httpPort 7654

# production-ish serving of a single site
kitchen-sink serve --srcDir website-src --servMode SERVE --httpPort 7654

# many sites behind one daemon, configured in Dhall (SNI + per-domain proxying)
kitchen-sink multisite --configFile sites.dhall --httpPort 80
```

Flags are derived by `optparse-generic` from the `Action` record fields in `KitchenSink/Engine.hs` — that file is the authority when a flag name in the website docs looks stale (e.g. `serve` takes `--outputDir`, `produce` takes `--outDir`).

`--var name=value` injects variables available to Dhall sections.

Rebuilding the site the project serves as its own docs, from the repo root:

```bash
bash scaffolding/outputdir.sh www          # create the expected output tree
bash scaffolding/sourcedir.sh site-source  # scaffold a fresh source tree
bash scripts/test-scaffold.sh              # scaffold + serve smoke test
bash scripts/build-js-components.sh        # rebuild purs widgets into website-src/*.js
bash scripts/import-scaffold.sh            # copy website-src css/js back into scaffolding/
```

There is **no test suite** — no test-suite stanza in `kitchen-sink.cabal`. Verification is `cabal build` (warnings are errors) plus running `serve --servMode DEV` against `website-src/`.

## The source format

A page is a `.cmark`/`.md` file split into *sections*, each introduced by a header line `=<namespace>:<key>.<format>`:

```
=base:build-info.json      → chooses the layout, e.g. {"layout":"article","publicationStatus":"Public"}
=base:preamble.json        → author/title/date
=base:topic.json           → topics + keywords
=base:summary.cmark
=base:main-content.cmark
=base:main-css.css
=base:dataset.json my-name → named data cell, referenceable by later sections
=base:main-content.templating     → templating-lang expression → {format, contents}
=base:main-content.templating-doc → templating-lang document tree → HTML
=generator:cmd.json        → runs an external command, its stdout becomes an extra target
=ext:<key>.<fmt>           → layout-declared extension sections
```

Parsing lives in `Core/Section/Parser.hs` (megaparsec); the payload record types (`BuildInfoData`, `PreambleData`, `TopicData`, …) are in `Core/Section/Payloads.hs`. `Format` covers `cmark | json | css | csv | dhall | mustache | templating | templating-doc`; the last four are *evaluated at load time* (`Engine/SiteLoader.hs`) and rewritten into a concrete format, which is where `--var` and previously-declared datasets are threaded in.

`dhall` and `templating` are two backends for the same job — see `website-src/sections-templating.cmark`. Both answer with a `{format, contents}` value that says which concrete format (`json`/`cmark`/`html`) the section becomes; `SiteLoader.rewriteSection` is the shared tail that applies it, including registering a `=base:dataset.<fmt> name` cell generated that way. `templating`/`templating-doc` are backed by `templating-hs` (`Engine/Templating.hs`, pinned by git in `cabal.project`), respectively its expression mode (JSON out) and its document mode (a `Node` tree folded to HTML via lucid). The medium-term intent is to deprecate `dhall`, since `dhall-json` is a recurring obstacle to upgrading GHC — do not add new Dhall-only capability without a templating counterpart.

Everything else in a source directory is picked up by extension (`SiteLoader.loadSite`): `.jpg/.png`, `.css`, `.js`, `.html`, `.dot` (rendered via graphviz `dot`), `.webm/.mp4`, audio, `.pdf`, and raw `.txt/.csv/.json/.dhall`. `kitchen-sink.json` at the source root is the site config and is excluded from raw files.

## Architecture

The pipeline is `Site → [Target] → bytes`, and both `produce` and `serve` reuse it — the difference is only *when* a target is realized.

- **`Core/`** — format-agnostic machinery, parameterized over an extension type `ext`.
  - `Core.Section` — the section format above.
  - `Core.Build.Site` — `Site ext`, a record of `Sourced` articles, images, css, js, …
  - `Core.Build.Target` — `Target ext a = { destination, productionRule, summary }`. A `ProductionRule` is one of `ProduceAssembler` (pure, in-memory rendering), `ProduceGenerator` (IO/subprocess, e.g. `execCmd` for graphviz), or `ProduceFileCopy`.
  - `Core.Assembler` — `Assembler ext a = Either (AssemblerError ext) a`, a pure monad for turning sections into HTML/text. `Core.Generator` is its impure counterpart.
- **`Layout/`** — decides *which* targets a site produces. `Layout.Base.Layout` is the interface (`siteTargets` + `extraSectiontypes`); `Layout.Blog` is the only implementation, pinning `ext ~ ()` (see `Layout/Blog/Extensions.hs`, which exists purely to hide the `ext` parameter). `Layout/Blog/Targets.hs` enumerates every output (articles, index, topic and hashtag listings, atom feed, sitemap, JSON metadata, images, generated dot images, plain-text renders). `Layout/Blog/Analyses/` computes derived data (word counts, site graph, glossary grouping, skyline, topic stats) that both the HTML and the JSON/API outputs consume. Article layout is selected by the `layout` string in `=base:build-info.json` — see `Layout/Blog/ArticleTypes.hs`.
- **`Engine/`** — the drivers.
  - `Engine.Runtime.Engine` is the four-function seam shared by every mode: `execLoadSite`, `execLoadMetaExtradata`, `evalTargets`, `execProduceTarget`. `Produce.run` just folds all targets through it; `Serve.run` builds the same `Engine` and hands it to a web app.
  - `Engine.Runtime.Runtime` adds dev-server concerns: fsnotify watch with debounce, a `BackgroundVal (Site ext)` that gets swapped on reload, a fan-out watch queue backing long-poll auto-reload, prometheus counters, and an optional proxy runtime.
  - `Engine.OnTheFly` serves a request by evaluating targets *for that request* and matching on `destinationUrl` — nothing is written to disk in serve mode.
  - `Engine.Api` / `Engine.Handlers` — servant API. `DevApi` adds `/dev/watch`, `/dev/targets`, `/dev/produce`, `/dev/publish`, `/dev/commands`, `/dev/command`, `/dev/reload` on top of the `/api` proxy and the raw on-the-fly handler. The `commands` array in `kitchen-sink.json` is what `/dev/commands` exposes (the `scripts/*.sh` above).
  - `Engine.MultiSite` + `MultiSiteConfig` — Dhall-configured multi-tenant serving with per-domain TLS/SNI and proxy directives.

The DEV vs SERVE distinction is a metadata swap, not a separate code path: `Serve.run` builds `prodengine`, then `devengine = prodengine { execLoadMetaExtradata = loadDevModeExtraData … }`, which injects `autoreload.js` / `add-dev-route.js` / echarts into every page's headers.

## Haskell conventions

- `default-extensions: OverloadedStrings, TypeApplications, NoImplicitPrelude`, language `GHC2021`. Per-module extensions (notably `OverloadedRecordDot`, which is used heavily for config/record access) are declared with pragmas at the top of the file.
- `KitchenSink.Prelude` is a hand-curated re-export list replacing the standard Prelude. If you need a function it does not export (`error`, `Read`, `succ`, …) you must import it explicitly from its home module — that is why modules have imports like `import Prelude (Read, error)` or `import GHC.Err (error)`. Prefer adding the explicit import over widening `Prelude.hs`.
- Formatting is fourmolu-style: 4-space indent, leading commas, explicit export lists on `Core.*` modules.
- New library modules must be added to `exposed-modules` in `hs/kitchen-sink.cabal`.

## Note on the working tree

`hs/agents/`, `hs/tools/`, `hs/resources/*.db`, and `agents-exe.cfg.json` are untracked scratch config for an external agent runner (`agents-exe`) that is *not* part of this cabal project — ignore them when reasoning about the build. `.gitignore` already excludes `agent.json`, `agents-logfile`, and `website-www/` (the published output checkout used by `scripts/publish.sh`).

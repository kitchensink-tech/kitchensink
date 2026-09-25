Kitchen-Sink
============

<img src="website-src/logo.png" alt="The Kitchen-Sink mascot" width="320" align="right">

Kitchen-Sink is @lucasdicioccio's static-site generator.

The main documentation is to be found on the [GitHub pages](https://kitchensink-tech.github.io/).

For a list of features: refer to [the feature list](https://kitchensink-tech.github.io/features.html).

For some historical context and overview: refer to a [series of article](https://dicioccio.fr/topics/web.html).

## directory organization

```
hs/           -- haskell source files
purs/         -- purescript source files
scaffolding/  -- helper scripts and data to scaffold a project
website-src/  -- the Kitchen-Sink source for the main website, which doubles as a real-world example
```

# usage

## requirements

- some Haskell installation with cabal (tested with GHC 9.2.4)
- (optional) graphviz for generating images from dot-sources

## build and install

```
cd hs
cabal build
cabal install
```

## minimal site setup

```
kitchen-sink init --dir myblog
cd myblog
kitchen-sink serve --srcDir src --outDir www --servMode DEV --httpPort 7655
```

`init` bootstraps `myblog/src` (site source) and `myblog/www` (output
skeleton) from built-in scaffolding, plus a `README.md` with pointers to
the [feature list](https://kitchensink-tech.github.io/features.html) and
the [section format](https://kitchensink-tech.github.io/sections.html).
Pass `--force` to overwrite an existing non-empty directory.

The underlying `scaffolding/outputdir.sh` and `scaffolding/sourcedir.sh`
scripts are still available for repo-checkout workflows (they only work
from the repo root, unlike `kitchen-sink init`).

## running the official site

We need not scaffold anything here, thus we can directly serve the site that is along the source code.

```
kitchen-sink serve --srcDir website-src/  --servMode DEV --httpPort 7655
```

Then navigate to http://localhost:7655/ you'll notice there are a few things to tune.

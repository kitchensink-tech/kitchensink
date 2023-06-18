Kitchen-Sink
============

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
bash scaffolding/outputdir.sh www
bash scaffolding/sourcedir.sh site-source
kitchen-sink serve --srcDir site-source/ --outDir www/ --servMode DEV --httpPort 7654
```

Then navigate to http://localhost:7654/ you'll notice there are a few things to tune.

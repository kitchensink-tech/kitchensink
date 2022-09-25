Kitchen-Sink
============

Kitchen-Sink is @lucasdicioccio's static-site generator.

See a [series of article](https://lucasdicioccio.github.io/topics/web.html) for some context and overview.

```
hs/           -- haskell source files
purs/         -- purescript source files
scaffolding/  -- helper scripts and data to scaffold a project
```

# usage

## requirements

- some Haskell installation with cabal (tested with GHC 9.2.4)
- (optional) graphviz for generating images from dot-sources

## build and install

```
cabal build
cabal install
```

## minimal site setup

```
bash scaffolding/outputdir.sh www
bash scaffolding/sourcedir.sh site-source
kitchen-sink serve --srcDir site-source/ --outDir www/ --servMode DEV --port 7654
```

Then navigate to http://localhost:7654/ you'll notice there are a few things to tune.

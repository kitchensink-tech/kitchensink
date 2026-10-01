# kitchen-sink

Kitchen-Sink is a static-site generator and a dev/serve HTTP daemon (with an
API gateway / reverse proxy). Pages are CommonMark documents split into typed
sections (JSON metadata, CSS, datasets, tramaj templates, external generators).
The same pipeline powers a one-shot build, a filesystem-watching dev server
that renders targets on the fly, and a multi-site daemon with per-domain TLS.

Documentation: <https://kitchensink-tech.github.io/>

## Install

```bash
cabal install kitchen-sink
```

## Usage

One-shot build of a source directory into an output directory:

```bash
kitchen-sink produce --srcDir website-src --outDir www
```

Dev server (watches the filesystem, serves targets on the fly):

```bash
kitchen-sink serve --srcDir website-src --outDir www --servMode DEV --httpPort 7655
```

Serve a single site in production mode:

```bash
kitchen-sink serve --srcDir website-src --servMode SERVE --httpPort 7655
```

Many sites behind one daemon, configured in JSON (SNI and per-domain proxying):

```bash
kitchen-sink multisite --configFile sites.json --httpPort 80
```

Run `kitchen-sink --help` for the full list of flags. See the website above for
the source format and a complete walkthrough.

## License

BSD-3-Clause. See `LICENSE`.

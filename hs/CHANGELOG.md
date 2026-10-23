# 0.2.0.0

A major version per the PVP: a configuration feature and its dev-server
routes are removed, and the library's dependencies and exposed data types
change (tramaj-hs 0.4, tls 2.1+).

## Added

- ACME certificates (TLS-ALPN-01, answered by the HTTPS server itself, no
  port 80 needed). `serve` takes `--acmeDomain`, `--acmeDirectory`
  (`letsencrypt`, `letsencrypt-staging` or a directory URL) and
  `--acmeStateDir` (default `./acme-state`); HTTPS defaults to port 443 when
  ACME is on, and ACME is exclusive with `--tlsCertFile`/`--tlsKeyFile`.
  `multisite` takes `--acmeContact`, `--acmeDirectory` and `--acmeStateDir`,
  and a TLS stanza `{"tag": "AcmeCertificate"}` requests certificates for its
  `sniDomains` (or the site's domain). File and ACME certificates can be
  mixed in one daemon; a file certificate wins for a name listed with both.
- A `not-found` layout, always produced as `/404.html`, `noindex` by default
  and left out of the sitemap, feeds and article listings; `init` scaffolds
  one.
- `.gif`, `.svg`, `.webp`, `.avif` and `.ico` files are picked up as images.
- Documentation layout: a site-wide sidebar listing every documentation page,
  grouped by the new optional `group` field of `base:build-info.json`, and a
  search box backed by a new `json/doc-search.json` target (one entry per
  page and per heading; produced only when the site has documentation
  pages). `doc-search.js` ships in the scaffolding.
- Topic graph explorer: an enlarge / full-window toggle (Escape returns to
  the inline view), and a keyword search that highlights nodes whose key or
  URL matches.
- Topic graph explorer: same-named topics and hashtags of merged external
  sites are linked to each other.

## Changed

- tramaj-hs 0.4: integers and floats are distinct values end to end. A
  dataset's `3.0` reaches a template as a float, a `tramaj-json` section
  answering `json` keeps `1.0`, and a dynamic page's `.sql` dataset maps
  `INTEGER` to an integer and `REAL` to a float (the former string fallback
  above 2^53 is gone).
- `og:type` and JSON-LD follow the layout: article and documentation pages
  stay `article`/`BlogPosting`, every other page is `website`/`WebPage`.
- The bodies of `callout` sections and the answers of `faq` sections are
  rendered as Markdown.
- Requires `tls >= 2.1.3` and `warp-tls >= 3.4.13`, and depends on
  `acme-not-a-joke`, `acme-not-a-joke-wai` and `acme-not-a-joke-warp`.

## Removed

- The dev-server commands (script buttons): the `commands` array of
  `kitchen-sink.json`, the `/dev/commands` and `/dev/command` routes, the
  `ks_commands` counter and the buttons they drew. A `kitchen-sink.json` that
  still has a `commands` key keeps loading; the key is ignored.
  `publishScript` / `/dev/publish` and the built-in produce and reload
  buttons are unchanged.

## Fixed

- `multisite` routed on the raw `Host` header, so a client on a non-default
  port (`Host: example.com:8443`) fell through to the fallback site; the port
  is now dropped before routing.
- Topic graph pan and zoom did nothing (a non-finite node size for listing
  pages broke the graph's bounding box).
- The topics graph had no article-to-article edges on sites served under a
  `basePath`.
- Node URLs of a merged external site's graph resolved against the
  displaying site instead of the external one.

# 0.1.0.0

Initial release.

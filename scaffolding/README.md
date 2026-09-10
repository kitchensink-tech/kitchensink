# Your new Kitchen-Sink site

This directory was bootstrapped by `kitchen-sink init`.

[Kitchen-Sink](https://kitchensink-tech.github.io/) is a static-site
generator and dev/serve daemon for blogs. A page is a `.cmark`/`.md` file
split into *sections* (content, metadata, CSS, …); Kitchen-Sink assembles
those sections into a full site.

- `src/` — your site's source: `kitchen-sink.json` (site config),
  `index.cmark`, `first-article.cmark`, and the other `.cmark` pages, plus
  the CSS/JS assets they reference.
- `www/` — the output directory skeleton. Kitchen-Sink writes generated
  pages here; you don't need to touch it by hand.

## Next steps

Run the dev server, which rebuilds on file changes:

```
kitchen-sink serve --srcDir src --outputDir www --servMode DEV --httpPort 7655
```

Then open http://localhost:7655/ and start editing the `.cmark` files in
`src/`.

## Learn more

- [Features](https://kitchensink-tech.github.io/features.html) — what
  Kitchen-Sink can do.
- [Sections](https://kitchensink-tech.github.io/sections.html) — the
  section format used inside each `.cmark` file.

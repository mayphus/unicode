# Unicode Map

> **Status:** Active prototype — exploring the entire Unicode code-point space as one navigable canvas.

A first slice for exploring the whole Unicode code point space as one large
canvas map.

The split is:

- `main.rkt` builds static metadata used by the frontend.
- `src/unicode/map.cljs` is the ClojureScript source for the canvas app.
- `public/app.js` is a checked-in runnable build so the prototype works without
  setting up a CLJS compiler first.

## Try it

Fetch the official Unicode Character Database files:

```sh
racket main.rkt fetch
```

Build the map data:

```sh
racket main.rkt build
```

Serve the static app:

```sh
racket main.rkt serve
```

Then open:

```text
http://localhost:8080
```

## Font loading

Press `f` in the app to load a local font file. The app uses `FontFace`, so
`.ttf`, `.otf`, `.woff`, and `.woff2` files are good targets.

## Data

`main.rkt fetch` downloads these files under `data/ucd/` from the official UCD
latest URL:

- `Blocks.txt`
- `UnicodeData.txt`
- `Scripts.txt`

`main.rkt build` parses them and emits `public/data/map.json` with blocks,
scripts, assigned ranges, categories, and structural ranges. The checked-in data
does not include local font coverage, because that would reflect the build
machine's installed fonts. For local experiments, run
`UNICODE_MAP_FONT_COVERAGE=1 racket main.rkt build` to add aggregate font
coverage ranges to the generated JSON. The frontend uses typed arrays built from
that metadata so unassigned code points stay visually quiet and glyph drawing is
limited to assigned/renderable classes.

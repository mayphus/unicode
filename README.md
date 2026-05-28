# Unicode Map

A first slice for exploring the whole Unicode code point space as one large
canvas map.

The split is:

- `main.rkt` builds static metadata used by the frontend.
- `src/unicode/map.cljs` is the ClojureScript source for the canvas app.
- `public/app.js` is a checked-in runnable build so the prototype works without
  setting up a CLJS compiler first.

## Try it

Build the data:

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

Put font files in `public/fonts/` and choose one in the app. You can also load a
local font file from the font picker. The app uses `FontFace`, so `.ttf`,
`.otf`, `.woff`, and `.woff2` files are good targets.

## Next data step

This first version maps the whole code point space and highlights structural
ranges such as surrogates and private-use areas. The next useful backend step is
to add official Unicode data files under `data/ucd/`:

- `Blocks.txt`
- `UnicodeData.txt`
- `Scripts.txt`

Then `main.rkt` can emit richer block, name, category, and script chunks for the
canvas viewport to load on demand.

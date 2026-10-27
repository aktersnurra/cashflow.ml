# Cashflow icon pack

This folder records the **actual approved Sankey artwork**, supplied as
`cashflow-icon-pack(2).zip` on 2026-10-27. **No redrawn SVG is used.**
All 12 image files are exact, byte-for-byte copies from that archive.

The icon depicts a cream inlet dividing into four unequal teal, mint, gold and
coral bands against a near-black field.

## Installed app icons (in `web/`)

| File | Size | Use |
| --- | --- | --- |
| `apple-touch-icon.png` | 180 × 180 | iOS Home Screen |
| `favicon-16.png` | 16 × 16 | Browser tab |
| `favicon-32.png` | 32 × 32 | Browser tab |
| `favicon-48.png` | 48 × 48 | Browser/bookmark |
| `favicon.ico` | Multiple | Legacy browser/icon consumers |
| `icon-64.png` | 64 × 64 | Small in-app use |
| `icon-192.png` | 192 × 192 | PWA install |
| `icon-512.png` | 512 × 512 | PWA install |
| `icon-maskable-512.png` | 512 × 512 | PWA adaptive Android icon |

## Original/master files (in this folder)

| File | Use |
| --- | --- |
| `source-original.png` | Approved illustration including its original cream frame |
| `icon-1024.png` | Full-bleed 1024 px production master |
| `preview.png` | Visual review sheet |

Production raster files have the intended full-bleed dark background; mobile
platforms apply their own shape/mask. Avoid reintroducing the old trend-line icon
or a reconstructed vector.

## Install / deploy

These files are already wired into `index.src.html` and
`manifest.webmanifest`. The Dune web alias includes the referenced images;
`web/build-bundle.sh` copies the production assets into the flat deployment
directory.

From the repository root:

```sh
dune build --workspace dune-workspace.web --profile release @web/default
web/build-bundle.sh /path/to/new/empty/static-dir
```

Deploy that directory as `static_dir`. The icons are served via
`/assets/<filename>`. The 1024 px master, preview, and original illustration
are source material and are not copied into the deployment bundle.

For an existing iOS Home Screen install, removing/re-adding the shortcut may
be needed for the new icon to appear. Installed PWAs may cache app icons until
their manifest/icon change is recognized.

## Transparent background variants

The opaque iOS and PWA icons remain unchanged. Two optional types of transparent
PNG are available at 64, 192, 256, 512, and 1024 pixels:

- `icon-transparent-square-<size>.png`: dark rounded-square mark, but the cream
  outside corner area is alpha-transparent.
- `logo-transparent-<size>.png`: only the cream inlet and colored Sankey
  streams, with the charcoal square removed. Use on a contrasting background.

Run `python -m pip install Pillow` and then
`python web/icons/export_transparent.py` to regenerate from the checked-in,
approved `source-original.png`. The two 192 px variants are checked in; the
other sizes are generated locally. Run `python test/test_export_transparent.py`
to verify all sizes in a temporary directory without changing checked-in images.
Preserve the original opaque PWA and Apple icons because platforms may apply
opaque-background and adaptive-icon rules.

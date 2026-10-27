#!/usr/bin/env python3
"""Export transparent variants from the approved Cashflow raster master.

Requirements: Pillow (python -m pip install Pillow).
Usage: python web/icons/export_transparent.py
Never edits the existing opaque Apple / PWA icons.
"""
from pathlib import Path

from PIL import Image, ImageChops, ImageDraw, ImageFilter

ROOT = Path(__file__).resolve().parent
SIZES = (64, 192, 256, 512, 1024)
FAVICON_SIZES = (16, 32, 48)
# The approved source's charcoal is RGB (12, 12, 12).
CHARCOAL_CUTOFF = 14

def exterior_background(maximum_channel):
    mask = maximum_channel.point([
        0 if value <= CHARCOAL_CUTOFF else 255 for value in range(256)
    ])
    exterior_marker = 128
    ImageDraw.floodfill(mask, (0, 0), exterior_marker)
    return mask.point([
        255 if value == exterior_marker else 0 for value in range(256)
    ])

def export():
    original = Image.open(ROOT / "source-original.png").convert("RGB")
    red, green, blue = original.split()
    maximum_channel = ImageChops.lighter(ImageChops.lighter(red, green), blue)
    alpha = ImageChops.invert(exterior_background(maximum_channel))
    square = original.convert("RGBA")
    square.putalpha(alpha.filter(ImageFilter.GaussianBlur(0.4)))
    # Remove the exterior before resampling so its cream edge cannot bleed inward.
    opacity = maximum_channel.point([
        max(0, min(255, round((value - CHARCOAL_CUTOFF) * 255 / 70)))
        for value in range(256)
    ])
    logo = original.convert("RGBA")
    logo.putalpha(ImageChops.multiply(opacity, alpha))
    for size in SIZES:
        square.resize((size,size),Image.Resampling.LANCZOS).save(ROOT / f"icon-transparent-square-{size}.png")
        logo.resize((size,size),Image.Resampling.LANCZOS).save(ROOT / f"logo-transparent-{size}.png")
    for size in FAVICON_SIZES:
        logo.resize((size, size), Image.Resampling.LANCZOS).save(ROOT.parent / f"favicon-{size}.png")
    logo.save(ROOT.parent / "favicon.ico", sizes=[(size, size) for size in FAVICON_SIZES])
    for filename in ("icon-transparent-square-1024.png", "logo-transparent-1024.png"):
        with Image.open(ROOT / filename) as file:
            assert file.mode == "RGBA" and file.getchannel("A").getpixel((0, 0)) == 0, \
                "exported icons must be RGBA with a transparent top-left corner"

if __name__ == "__main__":
    export()

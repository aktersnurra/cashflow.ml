#!/usr/bin/env python3
"""Export transparent variants from the approved Cashflow raster master.

Requirements: Pillow (python -m pip install Pillow).
Usage: python web/icons/export_transparent.py
Never edits the existing opaque Apple / PWA icons.
"""
from collections import deque
from pathlib import Path
from PIL import Image, ImageFilter

ROOT = Path(__file__).resolve().parent
SIZES = (64, 192, 256, 512, 1024)

def exterior_background(image):
    width, height = image.size
    pixel = image.load()
    mask = Image.new("L", (width, height), 0)
    out = mask.load()
    pending = deque([(0, 0)])
    while pending:
        x, y = pending.popleft()
        if not (0 <= x < width and 0 <= y < height) or out[x,y]:
            continue
        if min(pixel[x,y]) < 210:
            continue
        out[x,y] = 255
        pending.extend(((x-1,y),(x+1,y),(x,y-1),(x,y+1)))
    return mask

def export():
    original = Image.open(ROOT / "source-original.png").convert("RGB")
    outside = exterior_background(original)
    alpha = outside.point(lambda value: 255-value).filter(ImageFilter.GaussianBlur(0.4))
    square = original.convert("RGBA")
    square.putalpha(alpha)
    # Key out the near-black charcoal without changing any source geometry.
    logo = original.resize((1024,1024), Image.Resampling.LANCZOS).convert("RGBA")
    px = logo.load()
    for y in range(1024):
        for x in range(1024):
            r,g,b,_ = px[x,y]
            opacity = max(0,min(255,round((max(r,g,b)-14)*255/70)))
            px[x,y] = (r,g,b,opacity)
    for size in SIZES:
        square.resize((size,size),Image.Resampling.LANCZOS).save(ROOT / f"icon-transparent-square-{size}.png")
        logo.resize((size,size),Image.Resampling.LANCZOS).save(ROOT / f"logo-transparent-{size}.png")
    for filename in ("icon-transparent-square-1024.png", "logo-transparent-1024.png"):
        with Image.open(ROOT / filename) as file:
            assert file.mode == "RGBA" and file.getpixel((0,0))[3] == 0

if __name__ == "__main__":
    export()

"""Exercise the real icon exporter with the approved source, outside the checkout."""
import pathlib
import shutil
import subprocess
import sys
import tempfile
import unittest

from PIL import Image


class TransparentIconExports(unittest.TestCase):
    def test_exports_remove_exterior_without_erasing_artwork(self):
        icons = pathlib.Path(__file__).resolve().parents[1] / "web" / "icons"
        with tempfile.TemporaryDirectory(prefix="cashflow-icons-") as directory:
            output = pathlib.Path(directory)
            for filename in ("export_transparent.py", "source-original.png"):
                shutil.copyfile(icons / filename, output / filename)
            result = subprocess.run(
                [sys.executable, str(output / "export_transparent.py")],
                capture_output=True, text=True,
            )
            for size in (64, 192, 256, 512, 1024):
                for variant in ("icon-transparent-square", "logo-transparent"):
                    with (self.subTest(size=size, variant=variant),
                          Image.open(output / f"{variant}-{size}.png") as image):
                        self.assertEqual(image.mode, "RGBA")
                        self.assertEqual(image.size, (size, size))
                        alpha = image.getchannel("A")
                        for corner in ((0, 0), (size - 1, 0),
                                       (0, size - 1), (size - 1, size - 1)):
                            self.assertEqual(alpha.getpixel(corner), 0)
                        # Keep the inlet and streams, allowing minor Lanczos ringing.
                        for x, y in ((0.15, 0.48), (0.8, 0.22), (0.8, 0.35),
                                     (0.8, 0.5), (0.8, 0.7)):
                            self.assertIn(
                                alpha.getpixel((int(size * x), int(size * y))), range(250, 256),
                            )
                        self.assertEqual(
                            alpha.getpixel((size // 2, size // 10)),
                            0 if variant == "logo-transparent" else 255,
                        )
                        if variant == "logo-transparent":
                            # Empty above the streams: no frame, except faint raster noise.
                            top = alpha.crop((0, 0, size, size // 10))
                            self.assertEqual(sum(top.histogram()[21:]), 0)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)


if __name__ == "__main__":
    unittest.main(verbosity=2)

#!/usr/bin/env python3
"""
Generate a perceptually uniform 256-color palette from base 16 colors using CIELAB interpolation.

Based on: https://gist.github.com/jake-stewart/0a8ea46159a7da2c808e5be2177e1783

Input: JSON from stdin with keys: bg, fg, normal (array of 8 hex colors)
Output: JSON array of 240 hex strings (colors 16-255) to stdout
"""

import json
import sys


def srgb_to_linear(c):
    """Convert sRGB component (0-1) to linear RGB."""
    if c <= 0.04045:
        return c / 12.92
    return ((c + 0.055) / 1.055) ** 2.4


def linear_to_srgb(c):
    """Convert linear RGB component to sRGB (0-1)."""
    if c <= 0.0031308:
        return 12.92 * c
    return 1.055 * (c ** (1 / 2.4)) - 0.055


def hex_to_rgb(hex_color):
    """Convert hex color (#RRGGBB) to linear RGB (0-1)."""
    hex_color = hex_color.lstrip('#')
    r, g, b = (int(hex_color[i:i+2], 16) / 255.0 for i in (0, 2, 4))
    return srgb_to_linear(r), srgb_to_linear(g), srgb_to_linear(b)


def rgb_to_hex(r, g, b):
    """Convert linear RGB (0-1) to hex color (#RRGGBB)."""
    r = max(0, min(1, linear_to_srgb(r)))
    g = max(0, min(1, linear_to_srgb(g)))
    b = max(0, min(1, linear_to_srgb(b)))
    return "#{:02x}{:02x}{:02x}".format(
        int(round(r * 255)),
        int(round(g * 255)),
        int(round(b * 255))
    )


def rgb_to_xyz(r, g, b):
    """Convert linear RGB to XYZ (D65 illuminant)."""
    x = 0.4124564 * r + 0.3575761 * g + 0.1804375 * b
    y = 0.2126729 * r + 0.7151522 * g + 0.0721750 * b
    z = 0.0193339 * r + 0.1191920 * g + 0.9503041 * b
    return x, y, z


def xyz_to_rgb(x, y, z):
    """Convert XYZ to linear RGB (D65 illuminant)."""
    r = +3.2404542 * x - 1.5371385 * y - 0.4985314 * z
    g = -0.9692660 * x + 1.8760108 * y + 0.0415560 * z
    b = +0.0556434 * x - 0.2040259 * y + 1.0572252 * z
    return r, g, b


def f_lab(t):
    """CIELAB f function for XYZ to Lab conversion."""
    delta = 6/29
    if t > delta ** 3:
        return t ** (1/3)
    return t / (3 * delta ** 2) + 4/29


def f_lab_inv(t):
    """Inverse CIELAB f function for Lab to XYZ conversion."""
    delta = 6/29
    if t > delta:
        return t ** 3
    return 3 * delta ** 2 * (t - 4/29)


def xyz_to_lab(x, y, z):
    """Convert XYZ to CIELAB (D65 illuminant)."""
    # D65 white point
    xn, yn, zn = 0.95047, 1.00000, 1.08883

    L = 116 * f_lab(y / yn) - 16
    a = 500 * (f_lab(x / xn) - f_lab(y / yn))
    b = 200 * (f_lab(y / yn) - f_lab(z / zn))

    return L, a, b


def lab_to_xyz(L, a, b):
    """Convert CIELAB to XYZ (D65 illuminant)."""
    # D65 white point
    xn, yn, zn = 0.95047, 1.00000, 1.08883

    fy = (L + 16) / 116
    fx = a / 500 + fy
    fz = fy - b / 200

    x = xn * f_lab_inv(fx)
    y = yn * f_lab_inv(fy)
    z = zn * f_lab_inv(fz)

    return x, y, z


def rgb_to_lab(rgb):
    """Convert linear RGB tuple to CIELAB tuple."""
    return xyz_to_lab(*rgb_to_xyz(*rgb))


def lab_to_rgb(lab):
    """Convert CIELAB tuple to linear RGB tuple."""
    return xyz_to_rgb(*lab_to_xyz(*lab))


def lerp_lab(t, lab1, lab2):
    """Linear interpolation in CIELAB space."""
    return (
        lab1[0] + t * (lab2[0] - lab1[0]),
        lab1[1] + t * (lab2[1] - lab1[1]),
        lab1[2] + t * (lab2[2] - lab1[2]),
    )


def generate_palette(bg, fg, normal):
    """
    Generate 256-color palette from base 16 colors.

    Args:
        bg: Background color (hex)
        fg: Foreground color (hex)
        normal: List of 8 normal colors (hex)

    Returns:
        List of 240 hex colors (indices 16-255)
    """
    # Convert base colors to CIELAB
    bg_lab = rgb_to_lab(hex_to_rgb(bg))
    fg_lab = rgb_to_lab(hex_to_rgb(fg))
    base8_lab = [rgb_to_lab(hex_to_rgb(c)) for c in normal]

    palette = []

    # Generate 6x6x6 RGB cube (colors 16-231)
    for r in range(6):
        c0 = lerp_lab(r / 5, bg_lab, base8_lab[1])      # black -> red
        c1 = lerp_lab(r / 5, base8_lab[2], base8_lab[3])  # green -> yellow
        c2 = lerp_lab(r / 5, base8_lab[4], base8_lab[5])  # blue -> magenta
        c3 = lerp_lab(r / 5, base8_lab[6], fg_lab)       # cyan -> white
        for g in range(6):
            c4 = lerp_lab(g / 5, c0, c1)
            c5 = lerp_lab(g / 5, c2, c3)
            for b in range(6):
                c6 = lerp_lab(b / 5, c4, c5)
                palette.append(rgb_to_hex(*lab_to_rgb(c6)))

    # Generate 24 grayscale colors (colors 232-255)
    for i in range(24):
        t = (i + 1) / 25
        lab = lerp_lab(t, bg_lab, fg_lab)
        palette.append(rgb_to_hex(*lab_to_rgb(lab)))

    return palette


def main():
    # Read input JSON from stdin
    input_data = json.load(sys.stdin)

    bg = input_data["bg"]
    fg = input_data["fg"]
    normal = input_data["normal"]

    # Generate palette
    palette = generate_palette(bg, fg, normal)

    # Write output JSON to stdout
    json.dump(palette, sys.stdout)


if __name__ == "__main__":
    main()

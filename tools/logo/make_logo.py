# Generates the tabularberg hex logo and README banner as SVG.
# The idea: a tabular berg. Above the waterline, a flat slab whose top is a
# table (header row plus rows and columns). Below the waterline, the larger
# hidden mass is drawn as chunks of an array.

SKY = "#e6f0f7"
SEA_TOP = "#1f5f8b"
SEA_BOTTOM = "#0a2540"
ICE = "#f7fbfd"
ICE_SIDE = "#c9dfee"
ICE_LINE = "#7fa8c4"
HEADER = "#9cc3dc"
INK = "#0a2540"
CHUNK_FILL = "#bcd7ea"
SIDE_FILL = "#7fa6c2"
# MASS vertices that get a receding edge drawn on the 3D side
EDGE_VERTS = [2, 4, 6, 7, 9, 11]


# Ragged outline of the submerged mass, local coords (waterline y = 0).
MASS = [(-30, 0), (258, 0), (263, 28), (255, 60), (267, 94), (251, 126),
        (238, 150), (212, 168), (180, 159), (151, 177), (119, 165), (88, 173),
        (60, 151), (31, 129), (9, 101), (-21, 86), (-37, 53), (-27, 25)]


def mass_points(ox, oy, s=1.0, depth=1.0):
    return " ".join(f"{ox + s * x:.1f},{oy + s * y * depth:.1f}" for x, y in MASS)


def berg(ox, oy, s=1.0, ragged=False, clip_id="mass", x0s=0, w=190, h=48,
         dx=45, dy=-55, depth=1.0, ncol=4, label=None, label_size=None, extrude=False,
         label_family="DejaVu Sans Condensed, Arial Narrow, Helvetica, sans-serif"):
    """Berg in local coords (waterline at y=0), placed at (ox, oy), scale s.

    The slab's front face spans x0s .. x0s + w and is h high; (dx, dy) is the
    depth of its table top. With ragged=True the underwater chunks are cut by
    an irregular ice outline (MASS, stretched down by depth). label, if
    given, is set on the slab's front face.
    """
    def p(x, y):
        return f"{ox + s * x:.1f},{oy + s * y:.1f}"

    out = []
    # --- submerged mass: array chunks ---
    cw, ch = 48, 40
    x0 = -30
    if ragged and extrude:
        # the mass outline pushed back by the slab's depth (dx, dy): only the
        # strip to the right and along the bottom shows, as the berg's side
        back = " ".join(f"{ox + s * (x + dx):.1f},{oy + s * (y * depth + dy):.1f}" for x, y in MASS)
        # show the side only below the waterline and outside the front outline
        front = mass_points(ox, oy, s, depth).replace(" ", " L ")
        out.append(f'<clipPath id="{clip_id}-side"><path clip-rule="evenodd" '
                   f'd="M -10000,{oy:.1f} L 10000,{oy:.1f} L 10000,10000 L -10000,10000 Z M {front} Z"/></clipPath>')
        out.append(f'<g clip-path="url(#{clip_id}-side)">')
        out.append(f'<polygon points="{back}" fill="{SIDE_FILL}" fill-opacity="0.55" stroke="{ICE}" '
                   f'stroke-opacity="0.45" stroke-width="{1.4 * s:.1f}" stroke-linejoin="round"/>')
        # a few receding edges from front vertices to back vertices
        for i in EDGE_VERTS:
            x, y = MASS[i]
            out.append(f'<line x1="{ox + s * x:.1f}" y1="{oy + s * y * depth:.1f}" '
                       f'x2="{ox + s * (x + dx):.1f}" y2="{oy + s * (y * depth + dy):.1f}" '
                       f'stroke="{ICE}" stroke-opacity="0.45" stroke-width="{1.2 * s:.1f}"/>')
        out.append('</g>')
    if ragged:
        nrow = int(180 * depth // ch) + 2
        rows = [range(-1, 7)] * nrow
        out.append(f'<clipPath id="{clip_id}"><polygon points="{mass_points(ox, oy, s, depth)}"/></clipPath>')
        out.append(f'<g clip-path="url(#{clip_id})">')
    else:
        rows = [range(0, 6), range(0, 6), range(1, 6), range(1, 5)]
    for r, cols in enumerate(rows):
        for c in cols:
            x, y = x0 + c * cw, r * ch
            out.append(
                f'<polygon points="{p(x, y)} {p(x + cw, y)} {p(x + cw, y + ch)} {p(x, y + ch)}" '
                f'fill="{CHUNK_FILL}" fill-opacity="{max(0.12, 0.42 - 0.06 * r):.2f}" '
                f'stroke="{ICE}" stroke-opacity="0.55" stroke-width="{1.6 * s:.1f}"/>'
            )
    if ragged:
        out.append('</g>')
        out.append(f'<polygon points="{mass_points(ox, oy, s, depth)}" fill="none" stroke="{ICE}" '
                   f'stroke-opacity="0.6" stroke-width="{1.6 * s:.1f}" stroke-linejoin="round"/>')
    # --- above water: the slab ---
    xa, xb = x0s, x0s + w
    # side face
    out.append(f'<polygon points="{p(xb, -h)} {p(xb + dx, -h + dy)} {p(xb + dx, dy)} {p(xb, 0)}" '
               f'fill="{ICE_SIDE}" stroke="{ICE_LINE}" stroke-width="{1.5 * s:.1f}" stroke-linejoin="round"/>')
    # front face (ice cliff): the label, or faint strata
    out.append(f'<polygon points="{p(xa, -h)} {p(xb, -h)} {p(xb, 0)} {p(xa, 0)}" '
               f'fill="{ICE}" stroke="{ICE_LINE}" stroke-width="{1.5 * s:.1f}" stroke-linejoin="round"/>')
    if label:
        fs = label_size or 0.5 * h
        out.append(f'<text x="{ox + s * (xa + xb) / 2:.1f}" y="{oy - s * h / 2 + s * fs * 0.36:.1f}" '
                   f'text-anchor="middle" font-family="{label_family}" font-weight="bold" '
                   f'font-size="{s * fs:.1f}" fill="{INK}">{label}</text>')
    else:
        for yy in (-2 * h / 3, -h / 3):
            out.append(f'<line x1="{ox + s * (xa + 8):.1f}" y1="{oy + s * yy:.1f}" x2="{ox + s * (xb - 8):.1f}" '
                       f'y2="{oy + s * yy:.1f}" stroke="{ICE_LINE}" stroke-opacity="0.35" stroke-width="{1.2 * s:.1f}"/>')
    # top face: the table
    def top(t, u):
        # t across columns (0..1 along width), u back toward header (0 front .. 1 back)
        return p(xa + t * w + u * dx, -h + u * dy)
    out.append(f'<polygon points="{top(0, 0)} {top(1, 0)} {top(1, 1)} {top(0, 1)}" fill="{ICE}"/>')
    # header row = back strip
    out.append(f'<polygon points="{top(0, 0.68)} {top(1, 0.68)} {top(1, 1)} {top(0, 1)}" fill="{HEADER}"/>')
    for i in range(1, ncol):
        t = i / ncol
        a, b = top(t, 0).split(","), top(t, 1).split(",")
        out.append(f'<line x1="{a[0]}" y1="{a[1]}" x2="{b[0]}" y2="{b[1]}" '
                   f'stroke="{ICE_LINE}" stroke-width="{1.3 * s:.1f}"/>')
    for u in (0.34, 0.68):
        a, b = top(0, u).split(","), top(1, u).split(",")
        out.append(f'<line x1="{a[0]}" y1="{a[1]}" x2="{b[0]}" y2="{b[1]}" '
                   f'stroke="{ICE_LINE}" stroke-width="{1.3 * s:.1f}"/>')
    out.append(f'<polygon points="{top(0, 0)} {top(1, 0)} {top(1, 1)} {top(0, 1)}" fill="none" '
               f'stroke="{ICE_LINE}" stroke-width="{1.5 * s:.1f}" stroke-linejoin="round"/>')
    return "\n".join(out)


def sea_defs(gid, y0, y1):
    return (f'<linearGradient id="{gid}" x1="0" y1="{y0}" x2="0" y2="{y1}" gradientUnits="userSpaceOnUse">'
            f'<stop offset="0" stop-color="{SEA_TOP}"/><stop offset="1" stop-color="{SEA_BOTTOM}"/></linearGradient>')


# ---------------- hex logo ----------------
W, H = 433, 500
hexpts = "216.5,2 431,126 431,374 216.5,498 2,374 2,126"
water = 292
hex_svg = f'''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" width="{W}" height="{H}">
<title>tabularberg</title>
<defs>
<clipPath id="hex"><polygon points="{hexpts}"/></clipPath>
{sea_defs("sea", water, H)}
</defs>
<g clip-path="url(#hex)">
<rect x="0" y="0" width="{W}" height="{water}" fill="{SKY}"/>
<rect x="0" y="{water}" width="{W}" height="{H - water}" fill="url(#sea)"/>
{berg(91, water, 1.1)}
<line x1="0" y1="{water}" x2="{W}" y2="{water}" stroke="{ICE}" stroke-opacity="0.7" stroke-width="2"/>
</g>
<text x="216.5" y="122" text-anchor="middle" font-family="DejaVu Sans, Helvetica, Arial, sans-serif"
 font-weight="bold" font-size="45" fill="{INK}" letter-spacing="0.5">tabularberg</text>
<polygon points="{hexpts}" fill="none" stroke="{INK}" stroke-width="8" stroke-linejoin="round"/>
</svg>
'''
open("tabularberg-hex.svg", "w").write(hex_svg)

# ---------------- banner ----------------
BW, BH, bwater = 1600, 400, 250
banner = f'''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {BW} {BH}" width="{BW}" height="{BH}">
<title>tabularberg</title>
<defs>{sea_defs("bsea", bwater, BH)}</defs>
<rect x="0" y="0" width="{BW}" height="{bwater}" fill="{SKY}"/>
<rect x="0" y="{bwater}" width="{BW}" height="{BH - bwater}" fill="url(#bsea)"/>
{berg(1180, bwater, 1.2)}
<line x1="0" y1="{bwater}" x2="{BW}" y2="{bwater}" stroke="{ICE}" stroke-opacity="0.7" stroke-width="2"/>
<text x="80" y="170" font-family="DejaVu Sans, Helvetica, Arial, sans-serif" font-weight="bold"
 font-size="104" fill="{INK}">tabularberg</text>
<text x="84" y="320" font-family="DejaVu Sans, Helvetica, Arial, sans-serif" font-size="38"
 fill="{ICE}">a table above the waterline, an array below</text>
</svg>
'''
open("tabularberg-banner.svg", "w").write(banner)

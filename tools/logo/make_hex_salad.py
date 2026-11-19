# Hex logo: the tabular berg in the centre, the sea-ice word salad all
# around it. "ICE" is tinted throughout. The name sits on the slab's front
# face, at the waterline; most of the berg, an array of chunks, is below.
import random, sys
sys.path.insert(0, ".")
import make_logo as L

SEED = 7
random.seed(SEED)
TERMS = ["ANCHOR ICE", "BERGY BIT", "BRASH ICE", "FAST ICE", "FIRN",
         "FIRST-YEAR ICE", "FLOE", "FRAZIL ICE", "GLACIER ICE", "GREASE ICE",
         "GROWLER", "ICE CAKE", "ICE EDGE", "ICE FOOT", "ICE RIND", "ICE SHELF",
         "ICE STREAM", "LEVEL ICE", "NEW ICE", "NILAS", "OLD ICE", "PACK ICE",
         "PANCAKE ICE", "PRESSURE ICE", "RAFTED ICE", "ROTTEN ICE", "SEA ICE",
         "SECOND-YEAR ICE", "SHUGA", "SLUSH", "YOUNG ICE"]
SEP = " * "
FONT = "DejaVu Sans Condensed, Arial Narrow, Helvetica, sans-serif"
FS, LH = 15, 20

W, H = 433, 500
hexpts = "216.5,2 431,126 431,374 216.5,498 2,374 2,126"
water = 236

# berg geometry (local units; see make_logo.berg)
S = 0.8
SLAB = dict(x0s=-30, w=288, h=62, dx=40, dy=-46, depth=1.3, ncol=5)
OX = 216.5 - S * (SLAB["x0s"] + (SLAB["w"] + SLAB["dx"]) / 2)
LABEL, LABEL_SIZE = "TABULARBERG", 34
EXTRUDE = True       # 3D ragged underwater side
SALAD_THROUGH = False  # True lets the word salad show through the underwater mass

SKY_TXT, SKY_ICE = "#bccfdc", "#8bb1cb"
SEA_TXT, SEA_ICE = "#36618a", "#5f90b6"


def esc(s):
    return s.replace("&", "&amp;").replace("<", "&lt;")


def tspans(terms, txt, ice):
    out = []
    for i, t in enumerate(terms):
        if i:
            out.append(f'<tspan fill="{txt}">{SEP}</tspan>')
        for j, w in enumerate(t.split(" ")):
            sp = " " if j else ""
            out.append(f'<tspan fill="{ice if w == "ICE" else txt}">{sp}{esc(w)}</tspan>')
    return "".join(out)


def run(n):
    k = random.randrange(len(TERMS))
    return [TERMS[(k + i) % len(TERMS)] for i in range(n)]


lines = []
y = 18
while y < H + LH:
    txt, ice = (SKY_TXT, SKY_ICE) if y < water - 4 else (SEA_TXT, SEA_ICE)
    x = -random.randrange(0, 120)
    lines.append(f'<text x="{x}" y="{y}" font-family="{FONT}" font-size="{FS}" '
                 f'xml:space="preserve">{tspans(run(10), txt, ice)}</text>')
    y += LH


def P(x, y):
    return f"{OX + S * x:.1f},{water + S * y:.1f}"


xa, xb = SLAB["x0s"], SLAB["x0s"] + SLAB["w"]
h, dx, dy = SLAB["h"], SLAB["dx"], SLAB["dy"]
slab_halo = (f'<polygon points="{P(xa, 0)} {P(xa, -h)} {P(xa + dx, -h + dy)} {P(xb + dx, -h + dy)} '
             f'{P(xb + dx, dy)} {P(xb, 0)}" fill="{L.SKY}" stroke="{L.SKY}" stroke-width="14" stroke-linejoin="round"/>')
under = (f'<polygon points="{L.mass_points(OX, water, S, SLAB["depth"])}" '
         f'fill="url(#sea)" stroke="url(#sea)" stroke-width="12" stroke-linejoin="round"/>')

under_g = f'<g clip-path="url(#below)">{under}</g>'
svg = f'''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" width="{W}" height="{H}">
<title>tabularberg</title>
<defs>
<clipPath id="hex"><polygon points="{hexpts}"/></clipPath>
<clipPath id="above"><rect x="0" y="0" width="{W}" height="{water}"/></clipPath>
<clipPath id="below"><rect x="0" y="{water}" width="{W}" height="{H - water}"/></clipPath>
{L.sea_defs("sea", water, H)}
</defs>
<g clip-path="url(#hex)">
<rect x="0" y="0" width="{W}" height="{water}" fill="{L.SKY}"/>
<rect x="0" y="{water}" width="{W}" height="{H - water}" fill="url(#sea)"/>
{chr(10).join(lines)}
<g clip-path="url(#above)">{slab_halo}</g>
{"" if SALAD_THROUGH else under_g}
{L.berg(OX, water, S, ragged=True, extrude=EXTRUDE, label=LABEL, label_size=LABEL_SIZE, **SLAB)}
<line x1="0" y1="{water}" x2="{W}" y2="{water}" stroke="{L.ICE}" stroke-opacity="0.7" stroke-width="2"/>
</g>
<polygon points="{hexpts}" fill="none" stroke="{L.INK}" stroke-width="8" stroke-linejoin="round"/>
</svg>
'''
open("tabularberg-hex-salad.svg", "w").write(svg)

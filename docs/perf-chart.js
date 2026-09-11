// Grouped column chart of sento throughput per implementation -> SVG on stdout.
// Usage: node perf-chart.js perf-results.json > perf.svg; rsvg-convert -o perf-M3Ultra.png perf.svg
const fs = require('fs');
const data = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const groups = data.groups;            // ["Pinned - tell", ...]
const series = data.series;            // [{name, version, values:[...]}]
const colors = ['#2a78d6', '#eb6834', '#1baf7a', '#eda100', '#e87ba4'];
const ink = '#0b0b0b', ink2 = '#52514e', grid = '#e6e5e1', surface = '#ffffff';

const barW = 34, barGap = 4, groupGap = 56;
const pitch = barW + barGap;
const groupW = series.length * pitch - barGap;
const mL = 90, mR = 24, mT = 56, mB = 110;
const plotW = groups.length * groupW + (groups.length - 1) * groupGap;
const plotH = 460;
const W = mL + plotW + mR, H = mT + plotH + mB;

const maxV = Math.max(...series.flatMap(s => s.values));
// clean tick step
const rawStep = maxV / 6;
const mag = Math.pow(10, Math.floor(Math.log10(rawStep)));
const step = [1, 2, 2.5, 5, 10].map(m => m * mag).find(s => s >= rawStep);
const yMax = Math.ceil(maxV / step) * step;
const y = v => mT + plotH - (v / yMax) * plotH;
const fmtTick = v => v >= 1e6 ? (v / 1e6).toFixed(v % 1e6 ? 1 : 0) + 'M' : v >= 1e3 ? (v / 1e3) + 'k' : String(v);
const fmtVal = v => v >= 1e6 ? (v / 1e6).toFixed(2) + 'M' : Math.round(v / 1e3) + 'k';

let out = [];
out.push(`<svg xmlns="http://www.w3.org/2000/svg" width="${W}" height="${H}" viewBox="0 0 ${W} ${H}" font-family="-apple-system, 'Helvetica Neue', Helvetica, Arial, sans-serif">`);
out.push(`<rect width="${W}" height="${H}" fill="${surface}"/>`);
out.push(`<text x="${mL}" y="30" font-size="17" font-weight="600" fill="${ink}">${data.title}</text>`);
if (data.subtitle) out.push(`<text x="${mL}" y="48" font-size="12" fill="${ink2}">${data.subtitle}</text>`);

// gridlines + y ticks
for (let v = 0; v <= yMax + 1e-9; v += step) {
  const yy = y(v);
  out.push(`<line x1="${mL}" x2="${mL + plotW}" y1="${yy}" y2="${yy}" stroke="${grid}" stroke-width="1"/>`);
  out.push(`<text x="${mL - 8}" y="${yy + 4}" font-size="12" text-anchor="end" fill="${ink2}">${fmtTick(v)}</text>`);
}
out.push(`<text transform="translate(22 ${mT + plotH / 2}) rotate(-90)" font-size="12" text-anchor="middle" fill="${ink2}">messages / second</text>`);

// bars
groups.forEach((g, gi) => {
  const gx = mL + gi * (groupW + groupGap);
  series.forEach((s, si) => {
    const v = s.values[gi];
    const x = gx + si * pitch;
    const top = y(v), h = mT + plotH - top;
    const r = Math.min(4, h);
    out.push(`<path d="M${x},${mT + plotH} v${-(h - r)} a${r},${r} 0 0 1 ${r},${-r} h${barW - 2 * r} a${r},${r} 0 0 1 ${r},${r} v${h - r} z" fill="${colors[si]}"/>`);
    out.push(`<text x="${x + barW / 2}" y="${top - 5}" font-size="11" text-anchor="middle" fill="${ink}">${fmtVal(v)}</text>`);
  });
  out.push(`<text x="${gx + groupW / 2}" y="${mT + plotH + 22}" font-size="13" font-weight="600" text-anchor="middle" fill="${ink}">${g}</text>`);
});
// baseline
out.push(`<line x1="${mL}" x2="${mL + plotW}" y1="${mT + plotH}" y2="${mT + plotH}" stroke="${ink2}" stroke-width="1"/>`);

// legend
const legendY = mT + plotH + 58;
let lx = mL;
series.forEach((s, si) => {
  const label = `${s.name} ${s.version}`;
  out.push(`<rect x="${lx}" y="${legendY - 10}" width="14" height="14" rx="3" fill="${colors[si]}"/>`);
  out.push(`<text x="${lx + 20}" y="${legendY + 2}" font-size="13" fill="${ink}">${label}</text>`);
  lx += 20 + label.length * 7.4 + 28;
});
if (data.footnote) out.push(`<text x="${mL}" y="${legendY + 30}" font-size="11" fill="${ink2}">${data.footnote}</text>`);
out.push('</svg>');
process.stdout.write(out.join('\n'));

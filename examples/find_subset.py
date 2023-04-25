#!/usr/bin/env python3
"""Example."""
import json

import numpy as np
import pyproj

import pysurfex

from_json = json.load(
    open(
        "/home/trygveasp/revision_control/pysurfex/examples/domains/met_nordic.json", "r"
    )
)
geo = pysurfex.get_geo_object(from_json)

wanted_lon = 10.0
nx = 120
wanted_lat = 60.0
ny = 100

earth = 6.37122e6
proj_string = (
    "+proj=lcc +lat_0="
    + str(geo.xlat0)
    + " +lon_0="
    + str(geo.xlon0)
    + " +lat_1="
    + str(geo.xlat0)
    + " +lat_2="
    + str(geo.xlat0)
    + " +units=m +no_defs +R="
    + str(earth)
)

proj = pyproj.CRS.from_string(proj_string)
wgs84 = pyproj.CRS.from_string("EPSG:4326")

xv, yv = np.meshgrid(geo.x, geo.y)
lons, lats = pyproj.Transformer.from_crs(proj, wgs84, always_xy=True).transform(xv, yv)

print(lons.shape)
print(lats.shape)

x0 = np.empty([geo.nimax])
y0 = np.empty([geo.njmax])
for i in range(0, geo.nimax):
    x0[i] = float(geo.x[i]) - (0.5 * ((float(nx) - 1.0) * geo.xdx))

for j in range(0, geo.njmax):
    y0[j] = float(geo.y[j]) - (0.5 * ((float(ny) - 1.0) * geo.xdy))

x0v, y0v = np.meshgrid(x0, y0)
lonc, latc = pyproj.Transformer.from_crs(proj, wgs84, always_xy=True).transform(x0v, y0v)
for i in range(0, geo.nimax):
    for j in range(0, geo.njmax):
        if abs(lonc[j][i] - wanted_lon) < 0.01 and abs(latc[j][i] - wanted_lat) < 0.01:
            print(
                "Possible subset centre points",
                i,
                j,
                xv[j, i],
                lonc[j][i],
                yv[j, i],
                latc[j][i],
            )

#!/usr/bin/env python3
"""Example."""
import json
import logging

import numpy as np
import pyproj

from pysurfex import PACKAGE_DIRECTORY
from pysurfex.geo import get_geo_object

test_domain = f"{PACKAGE_DIRECTORY}/../pysurfex/examples/domains/met_nordic.json"
with open(test_domain, mode="r", encoding="utf8") as fhandler:
    from_json = json.load(fhandler)
geo = get_geo_object(from_json)

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

x0 = np.empty([geo.nimax])
y0 = np.empty([geo.njmax])
for i in range(geo.nimax):
    x0[i] = float(geo.x[i]) - (0.5 * ((float(nx) - 1.0) * geo.xdx))

for j in range(geo.njmax):
    y0[j] = float(geo.y[j]) - (0.5 * ((float(ny) - 1.0) * geo.xdy))

x0v, y0v = np.meshgrid(x0, y0)
lonc, latc = pyproj.Transformer.from_crs(proj, wgs84, always_xy=True).transform(x0v, y0v)
for i in range(geo.nimax):
    for j in range(geo.njmax):
        if abs(lonc[j][i] - wanted_lon) < 0.01 and abs(latc[j][i] - wanted_lat) < 0.01:
            logging.info(
                "Possible subset centre points %s %s %s %s %s %s",
                i,
                j,
                xv[j, i],
                lonc[j][i],
                yv[j, i],
                latc[j][i],
            )

#!/bin/bash

vars="\
air_temperature_2m \
surface_geopotential \
precipitation_amount_acc \
snowfall_amount_acc \
relative_humidity_ml \
rainfall_amount \
integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time \
integral_of_surface_downwelling_longwave_flux_in_air_wrt_time \
air_temperature_ml \
x_wind_ml \
y_wind_ml \
x_wind_10m \
y_wind_10m \
specific_humidity_ml \
"

evars=""
for var in $vars; do
  evars="$evars --extract.selectVariables $var"
done

cat > naminterp << EOF
&naminterp
  order=0
  OUTGEO%NLON=101,
  OUTGEO%NLAT=101,
  OUTGEO%PROJECTION=3,
  OUTGEO%WEST=7.6414862985721399,
  OUTGEO%SOUTH=58.924737538984900,
  !outgeo%east=
  !outgeo%north=
  outgeo%dlon=2500.,
  outgeo%dlat=2500.,
  outgeo%lonc=15.
  outgeo%latc=63.
  outgeo%Projlon=15.000000000000000
  outgeo%Projlat=63.000000000000000
  outgeo%Projlat2=63.000000000000000
/
EOF

set -x
yyyy=2018
mm=01
dd=11

hhs="00 06 12 18"
for hh in $hhs; do
  mkdir -p netcdf
  fimex-0.65 --input.file=/lustre/storeB/immutable/archive/projects/metproduction/meps/$yyyy/$mm/$dd/meps_mbr0_extracted_2_5km_$yyyy$mm${dd}T${hh}Z.nc --input.type=nc4 \
--output.file=netcdf/SubsetMetCoOp_$yyyy$mm$dd$hh.nc --output.type=nc4  $evars \
--extract.reduceDimension.name=time --extract.reduceDimension.start=0   --extract.reduceDimension.end=24 \
--extract.reduceDimension.name=x    --extract.reduceDimension.start=240 --extract.reduceDimension.end=340 \
--extract.reduceDimension.name=y    --extract.reduceDimension.start=280 --extract.reduceDimension.end=380 || exit 1

  mkdir -p grib/$yyyy/$mm/$dd/$hh
  lls="000 001 002 003 004 005 006 007 008 009 010 011 012 013 014 015 016 017 018 019 020 021 022 023 024"
  for lll in $lls; do
    file=fc$yyyy$mm${dd}${hh}+${lll}grib_fp_mbr000
    idir=/lustre/storeB/project/metproduction/products/meps/member_0/grib/
    f=$idir/$file
    [ -f $f ] || exit 1
    #gl -n naminterp $f -o grib/$yyyy/$mm/$dd/$hh/$file -igd || exit 1
  done
done

rm naminterp

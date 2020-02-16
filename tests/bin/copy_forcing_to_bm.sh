#!/bin/bash

gtypes="LONLATVAL IGN LONLAT_REG CONF_PROJ"
forc_types="nc"
itypes="grib netcdf"
for gtype in $gtypes; do
  for ftype in $forc_types; do
    for itype in $itypes; do
      case $ftype in
        "nc")
          set -x
          mv ../data/$gtype/$ftype/FORCING_from_${itype}.nc ../data/$gtype/$ftype/FORCING_${itype}_benchmark.nc
          set +x
        ;;
        *)
          echo "$ftype not defined"
          exit 1
        ;;
      esac
    done
  done
done

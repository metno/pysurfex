#!/bin/bash

set -x
PGD=/home/trygveasp/nobackup/offline/bin/pgd.exe
PREP=/home/trygveasp/nobackup/offline/bin/prep.exe
OFFLINE=/home/trygveasp/nobackup/offline/bin/offline.exe
CLIMDIR=/lustre/storeB/project/nwp/surfex/PGD/
ECOCLIMAPDIR=/home/trygveasp/nobackup/offline/ecoclimap/
WORK=/home/trygveasp/nobackup/offline/work
GIT_ROOT=/home/trygveasp/PycharmProjects/offline-surfex-forcing/
FORCING_DIR=/home/trygveasp/nobackup/offline/data/
OUTPUT_DIR=/home/trygveasp/nobackup/offline/data

gridtypes="IGN LONLAT_REG LONLATVAL CONF_PROJ"
csurftype="ASCII"
ctimeseries="NETCDF ASCII TEXTE"
cforcingfiletype="NETCDF"

for type in $gridtypes; do
  for forc in $cforcingfiletype; do
    for surf in $csurftype; do
        
        
      mkdir -p $WORK
      cd $WORK || exit 1
      rm *

      ln -s $CLIMDIR/* .
      ln -s $ECOCLIMAPDIR/* .

      # Create namelists
      export CSURF_FILETYPE=$surf
      export CTIMESERIES_FILETYPE=$ts
      export CFORCING_FILETYPE=$forc
      export PATH=$GIT_ROOT/testing/bin:$PATH
      # cgridtype
      case $type in
        "IGN")
            Get_namelist ALL $GIT_ROOT/testing/etc/surfex_namelists.pm $GIT_ROOT/testing/etc/surfex_namelists_ign.pm > OPTIONS.nam
         ;;
         "LONLAT_REG")
            Get_namelist ALL $GIT_ROOT/testing/etc/surfex_namelists.pm $GIT_ROOT/testing/etc/surfex_namelists_lonlat_reg.pm > OPTIONS.nam
         ;;
         "LONLATVAL")
            Get_namelist ALL $GIT_ROOT/testing/etc/surfex_namelists.pm $GIT_ROOT/testing/etc/surfex_namelists_lonlatval.pm > OPTIONS.nam
         ;;
         "CONF_PROJ")
            Get_namelist ALL $GIT_ROOT/testing/etc/surfex_namelists.pm $GIT_ROOT/testing/etc/surfex_namelists_conf_proj.pm > OPTIONS.nam
         ;;
         *)
           echo "Define projection for $type"
           exit 1
         ;;
       esac
 
      # cforcingfiletype
      case $forc in
        "NETCDF")
           cp $FORCING_DIR/$type/nc/FORCING.nc . || exit 1
        ;;
        *)
          echo "Define forcing for $forc"
          exit 1
        ;;
      esac



      $PGD 2>&1 > /dev/null  || exit 1
      $PREP 2>&1 > /dev/null  || exit 1

      for ts in $ctimeseries; do
        # Create namelists
        export CSURF_FILETYPE=$surf
        export CTIMESERIES_FILETYPE=$ts
        export CFORCING_FILETYPE=$forc
        # cgridtype
        case $type in
          "IGN")
              Get_namelist ALL $GIT_ROOT/testing/etc/surfex_namelists.pm $GIT_ROOT/testing/etc/surfex_namelists_ign.pm $GIT_ROOT/testing/etc/surfex_namelists_set_forc_zs.pm > OPTIONS.nam
           ;;
           "LONLAT_REG")
              Get_namelist ALL $GIT_ROOT/testing/etc/surfex_namelists.pm $GIT_ROOT/testing/etc/surfex_namelists_lonlat_reg.pm $GIT_ROOT/testing/etc/surfex_namelists_set_forc_zs.pm > OPTIONS.nam
           ;;
           "LONLATVAL")
              Get_namelist ALL $GIT_ROOT/testing/etc/surfex_namelists.pm $GIT_ROOT/testing/etc/surfex_namelists_lonlatval.pm > OPTIONS.nam
           ;;
           "CONF_PROJ")
              Get_namelist ALL $GIT_ROOT/testing/etc/surfex_namelists.pm $GIT_ROOT/testing/etc/surfex_namelists_conf_proj.pm $GIT_ROOT/testing/etc/surfex_namelists_set_forc_zs.pm > OPTIONS.nam
           ;;
           *)
             echo "Define projection for $type for offline"
             exit 1
           ;;
         esac


        $OFFLINE 2>&1 > /dev/null  || exit 1

        # ctimeseries
        case $ts in
          "NETCDF")
            suffix="nc"
            save=$OUTPUT_DIR/$type/$suffix
            mkdir -p $save
            mv *.OUT.nc $save/.
           ;;
           "TEXTE")
              suffix="TXT"
              save=$OUTPUT_DIR/$type/$suffix
              mkdir -p $save
              mv *.TXT $save/.
           ;;
           "ASCII")
             suffix="txt"
              save=$OUTPUT_DIR/$type/$suffix
              mkdir -p $save
              mv S*.txt $save/.
           ;;
           *)
             echo "Define save for $ts"
             exit 1
           ;;
         esac
         save=$OUTPUT_DIR/$suffix  
      done

      # Save csurftype
      case $surf in
        "ASCII")
          save=$OUTPUT_DIR/$type/txt
          mkdir -p $save
          mv P*.txt $save/.
        ;;
        *)
          echo "$surf not defined!"
          exit 1
        ;;
      esac
    done
  done
done


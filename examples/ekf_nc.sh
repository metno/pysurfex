#!/bin/bash

set -x
#export ECCODES_DIR=/modules/xenial/user-apps/eccodes/2.14.1_gnu_5.4.0/
#export PYTHONPATH=$HOME/offline-surfex-forcing:$HOME/.local/lib/python3.5/site-packages/
#export PATH=$HOME/offline-surfex-forcing/bin:$PATH
#export SURFEX_SETTINGS_DIR=$HOME/offline-surfex-forcing/examples/settings
#export SURFEX_SETTINGS_DIR=/modules/centos7/user-apps/suv/surfex-api/0.0.1-dev/examples/settings
#export SURFEX_BIN_DIR=/modules/centos7/user-apps/suv/surfex/cy43-dev/bin/

mkdir -p $HOME/surfex-tests/mc_ekf
cd $HOME/surfex-tests/mc_ekf || exit 1


domain_name="NORWAY-SOUTH"
domains_json="$SURFEX_SETTINGS_DIR/domains.json"

# Set domain
set_domain -d $domain_name --domains $domains_json -o domain.json || exit 1

# Merge TOML settings
settings="$SURFEX_SETTINGS_DIR/config_exp_surfex.toml $SURFEX_SETTINGS_DIR/ekf_nc.toml"
merge_toml_files -t $settings -o config.toml || exit 1

settings_dir=$SURFEX_SETTINGS_DIR
system=$SURFEX_SETTINGS_DIR/system.ppi.json

nproc=8
rte=rte.json
cat > $rte << EOF
{
  "OMP_NUM_THREADS": "1",
  "PATH":"$PATH",
  "PYTHONPATH":"$PYTHONPATH",
  "LD_LIBRARY_PATH":"$LD_LIBRARY_PATH"
}
EOF

pgd_file=PGD.nc
if [ ! -f $pgd_file ]; then
  # Create json namelist
  create_surfex_json_namelist -c config.toml -s $system -p $settings_dir pgd || exit 1
  pgd -d domain.json -j options.json -e ecoclimap.json -i surfex_input_files.json -r $rte -w "mpiexec -np $nproc" -o $pgd_file pgd.exe  || exit 1
fi

FG_DTG=2020011303
prep_file=PREP_ORIG.nc
if [ ! -f $prep_file ]; then
  # Create json namelist
  create_surfex_json_namelist -c config.toml -s $system -p $settings_dir \
    --prep.file $SURFEX_SETTINGS_DIR/prep_from_namelist_values.json \
    --prep.filetype json \
    --dtg $FG_DTG prep || exit 1
#  --prep.file /lustre/storeA/users/trygveasp/surfex_test_data/sfx/2020/01/13/00/ICMSHHARM+0003.sfx \
#  --prep.filetype FA \
#  --prep.pgdfile /lustre/storeA/users/trygveasp/surfex_test_data/sfx/pgd/Const.Clim.sfx \
#  --prep.pgdfiletype FA \
 
  prep -d domain.json -j options.json -e ecoclimap.json -i surfex_input_files.json -r $rte -w "mpiexec -np $nproc" --pgd $pgd_file -o $prep_file prep.exe  || exit 1
fi

cat > domain.yml << EOF
conf_proj_domain:
  LONC: 10.0
  LATC: 60.0
  LON0: 0.0
  LAT0: 50.0
  GSIZE: 2500.0
  NLONS: 100
  NLATS: 100
  EZONE: 11
EOF

DTG=2020011306
if [ ! -f FORCING.nc ]; then
  create_forcing $FG_DTG $DTG domain.yml -m conf_proj_domain -p /lustre/storeA/users/trygveasp/surfex_test_data/grib1/2020/01/13/03/fc2020011303+@LLL@grib_fp --co2 constant --sca_sw constant -i grib --zsoro_converter phi2m --zref ml --uref ml --zval constant --uval constant || exit 1
fi

forecast=FG_SODA.nc
if [ ! -f $forecast ]; then
  # Create json namelist
  create_surfex_json_namelist -c config.toml -s $system -p $settings_dir --forc_zs offline || exit 1

  offline -d domain.json -j options.json -e ecoclimap.json -i surfex_input_files.json -r $rte -w "mpiexec -np $nproc" --pgd $pgd_file --prep $prep_file -o $forecast offline.exe  || exit 1
fi

# Bufr Obs
vars="airTemperatureAt2M relativeHumidityAt2M totalSnowDepth"
for var in $vars; do
  if [ ! -f $var.txt ]; then
    bufr2titan -f /lustre/storeA/users/trygveasp/surfex_test_data/obs/bufr/ob2020011306 --lonrange 5,15 --latrange 55,65 -v $var -o $var.txt || exit 1
  fi
done

# TITAN
module load R/R-3.5.2
for var in $vars; do
  [ $var == "airTemperatureAt2M" ] && cmd="--input.cfact 1 -v --variable T --spatconv  --thr.sct 16 --vmin 223.16 --vmax 333.16 --min.corep 0.9 --mean.corep 1 --max.corep 1.1 --eps2.sct 0.5"
  [ $var == "relativeHumidityAt2M" ] && cmd="--input.cfact 0.01 -v --variable RH  --spatconv  --thr.sct 16 --value.dig.out 2 --vmin 0.000001 --vmax 1 --min.corep 0.9 --mean.corep 1 --max.corep 1.1 --eps2.sct 0.5"
  [ $var == "totalSnowDepth" ] && cmd="--input.cfact 1 -v --variable SD  --spatconv  --thr.sct 16 --value.dig.out 2 --vmin 0 --vmax 10 --min.corep 0.9 --mean.corep 1 --max.corep 1.1 --eps2.sct 0.5"
  if [ ! -f obs_$var.txt ]; then
    Rscript $HOME/TITAN/titan.R $var.txt obs_$var.txt --prid 5 --dr.isol 15000 --n.isol 5  --doit.isol 0 --doit.sct 0 --doit.buddy 0 $cmd  || exit 1
  fi
  if [ ! -f obs_$var.nc ]; then
    create_gridpp_parameters obs_$var.txt -k 0 -o obs_$var.nc || exit 1 
  fi
done

# First guess
if [ ! -f fg.nc ]; then
    FirstGuess4gridpp /lustre/storeA/users/trygveasp/surfex_test_data/grib1/2020/01/13/03/fc2020011303+003grib_fp -sfx /lustre/storeA/users/trygveasp/surfex_test_data/grib1/2020/01/13/03/fc2020011303+003grib_sfx --altitude_converter phi2m -c $HOME/offline-surfex-forcing/examples/settings/grib_codes.yaml  --sdf PGD.nc -o fg.nc || exit 1
  fi

for var in $vars; do
  if [ ! -f fg_$var.nc ]; then
    ln -sf fg.nc fg_$var.nc
  fi
done

# gridpp
module load gridpp/0.3.2-dev
for var in $vars; do
  if [ ! -f an_$var.nc ]; then
    [ $var == "airTemperatureAt2M" ] && v="air_temperature_2m"
    [ $var == "relativeHumidityAt2M" ] && v="relative_humidity_2m"
    [ $var == "totalSnowDepth" ] && v="surface_snow_thickness"
    cp fg_$var.nc an_$var.nc
    # -c oi $d $h useEns=0 $s $g $e minObs=0 landOnly=$landOnly diaFile="$diaFile" -p $param type=netcdf dimName=coefficient varName=coefficients $limits --debug info
    gridpp  fg_$var.nc an_$var.nc -v $v -c oi useEns=0 minObs=0 -p obs_$var.nc type=netcdf dimName=coefficient varName=coefficients --debug info || exit 1
  fi
done

# oi2soda
if [ ! -f OBSERVATIONS_200113H06.DAT ]; then
  oi2soda --t2m_file an_airTemperatureAt2M.nc --rh2m_file an_relativeHumidityAt2M.nc --sd_file an_totalSnowDepth.nc $DTG || exit 1
fi

# Perturbed runs
perturbed_runs=""
for pert in 0 1 2; do
  pert_forecast=EKF_PERT$pert.nc
  perturbed_runs="$perturbed_runs $pert_forecast"
  if [ ! -f $pert_forecast ]; then
    # Create json namelist
    create_surfex_json_namelist -c config.toml -s $system -p $settings_dir --forc_zs offline || exit 1

    perturbed_offline -d domain.json -j options.json -e ecoclimap.json -i surfex_input_files.json -r $rte -w "mpiexec -np $nproc" --pert $pert --pgd $pgd_file --prep $prep_file -o $pert_forecast offline.exe  || exit 1
  fi
done

# soda
soda=SODA.nc
if [ ! -f $soda ]; then
  # Create json namelist
  create_surfex_json_namelist -c config.toml -s $system -p $settings_dir --dtg $DTG soda || exit 1

  assim_input="assim_input.json"
  set_assimilation_input -o $assim_input --perts $perturbed_runs --sfx_fg $forecast $DTG options.json || exit 1
  soda -d domain.json -j options.json -e ecoclimap.json -i surfex_input_files.json -r $rte -w "mpiexec -np $nproc" --pgd $pgd_file --prep $forecast --assim_input $assim_input -o $soda soda.exe  || exit 1
fi


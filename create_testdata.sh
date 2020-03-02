#!/bin/bash

module load Python/3.7.3
module load gsl/2.5
export PYTHONPATH=$HOME/revision_control/titanlib/build/SWIG/python/:$PYTHONPATH
export PYTHONPATH=$PYTHONPATH:$HOME/revision_control/offline-surfex-forcing/
module load suv/surfex-api

set -x
bindir=$HOME/revision_control/offline-surfex-forcing/bin

PROJS="CONF_PROJ LONLAT_REG LONLATVAL IGN CARTESIAN"
PROJS="CONF_PROJ LONLAT_REG"
#PROJS="LONLATVAL IGN CARTESIAN"
#PROJS="CARTESIAN"

suffixes="txt nc"
tasks="pgd prep"

system=$HOME/revision_control/offline-surfex-forcing/examples/settings/system.ppi.json
rte=rte.json
cat > $rte << EOF
{
  "OMP_NUM_THREADS": "1",
  "PATH":"$PATH",
  "PYTHONPATH":"$PYTHONPATH",
  "LD_LIBRARY_PATH":"$LD_LIBRARY_PATH"
}
EOF

for proj in $PROJS; do
  domain=${proj}_TEST
  for suffix in $suffixes; do
    for task in $tasks; do
      dtg=2020011306

      mod="mods.toml"
      if [ "$suffix" == "txt" ]; then
        cat > $mod <<  EOF
[SURFEX_IO]
CSURF_FILETYPE="ASCII"
[ISBA]
SNOW="D95"
EOF
      elif [ "$suffix" == "nc" ]; then
        cat > $mod <<  EOF
[SURFEX_IO]
CSURF_FILETYPE="NC"
[ISBA]
SNOW="D95"
EOF
      elif [ "$suffix" == "fa" ]; then
        cat > $mod <<  EOF
[SURFEX_IO]
CSURF_FILETYPE="FA"
[ISBA]
SNOW="D95"
EOF
      fi
      mkdir -p testdata
      $bindir/merge_toml_files -t $HOME/revision_control/offline-surfex-forcing/examples/settings/config_exp_surfex.toml $mod -o config.toml || exit 1
      $bindir/set_domain -d $domain --domains $HOME/revision_control/offline-surfex-forcing/tests/settings/domains.json -o domain.json || exit 1
      if [ $task == "pgd" ]; then
        $bindir/create_surfex_json_namelist  -c config.toml -p $HOME/revision_control/offline-surfex-forcing/examples/settings/  -s $system $task || exit 1     
        $bindir/pgd -j options.json -f -r $rte -e ecoclimap.json -d domain.json -i surfex_input_files.json -o testdata/PGD_${proj}.$suffix /home/trygveasp/surfex-tests/bin/PGD.exe || exit 1
        rm -f PGD.$suffix
      elif [ $task == "prep" ]; then
        $bindir/create_surfex_json_namelist  -c config.toml -p $HOME/revision_control/offline-surfex-forcing/examples/settings/ --prep.file $HOME/revision_control/offline-surfex-forcing/examples/settings/prep_from_namelist_values.json --prep.filetype json --dtg=$dtg -s $system $task || exit 1
        $bindir/prep -j options.json -f -r $rte -e ecoclimap.json --pgd testdata/PGD_${proj}.$suffix -d domain.json -i surfex_input_files.json -o testdata/PREP_${proj}.$suffix /home/trygveasp/surfex-tests/bin/PREP.exe || exit 1
        rm -f PGD.$suffix
        rm -f PREP.$suffix
      fi
    done
  done
done


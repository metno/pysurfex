#!/bin/bash

# Extract observations and plot
./bin/plot_field -it obs -t 2020033006 -v airTemperatureAt2M --debug -i testdata_obs/ob2020033006 --obs_type bufr
./bin/plot_field -it obs -t 2020033006 -v Temperature --debug -i testdata_obs/20200330T055501Z_all.json --obs_type netatmo
./bin/plot_field -it obs -t 2020033006 -v air_temperature --debug --obs_type frost

# Plot extraxted observations in json file format
./bin/plot_field -it obs -t 2020033006 -v air_temperature --debug -i testdata_obs/frost_t2m.json --obs_type json
./bin/plot_field -it obs -t 2020033006 -v Temperature --debug -i testdata_obs/netatmo_t2m.json --obs_type json
./bin/plot_field -it obs -t 2020033006 -v airTemperatureAt2M --debug -i testdata_obs/bufr_t2m.json --obs_type json

# Plot NectDF field
./bin/plot_field -it netcdf -g test/settings/conf_proj_test.json -t 2020022001 -v air_temperature_2m --debug -i testdata/meps_det_2_5km_20200220T00Z.nc

# Plot grib1
./bin/plot_field -it grib1 -g test/settings/conf_proj_test.json -t 2020033006 --indicatorOfParameter 11 --level 2 --levelType 105 --debug -i testdata_obs/test.grib1

# Plot grib2
./bin/plot_field -it grib2 -g test/settings/conf_proj_test.json -t 2020033006 --levelType 103 --level 2 --discipline 0 --parameterCategory 0 --parameterNumber 0 --debug -i testdata_obs/test.grib2


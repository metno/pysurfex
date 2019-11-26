import f90nml
import os
from datetime import datetime
import surfex
import json


if __name__ == "__main__":

    my_geo = surfex.ConfProj(100, 100, 10, 60, 15, 50, 2500, ezone=11)

    root_dir = os.getcwd()
    my_settings = f90nml.read(root_dir + '/settings/oi_fa_canari.nml')
    test_dir = os.environ["HOME"] + "/surfex-tests"
    os.makedirs(test_dir, exist_ok=True)

    my_rte = os.environ.copy()
    my_rte.update(json.loads('{"DR_HOOK": "0","DR_HOOK_NOT_MPI": "1","OMP_NUM_THREADS":"1"}'))

    my_settings = my_geo.update_namelist(my_settings)
    my_batch = surfex.BatchJob(my_rte, wrapper="mpiexec -np 8")

    my_ecoclimap = surfex.JsonInputDataFromFile(root_dir + "/settings/ecoclimap.json")

    my_format = str(my_settings["nam_io_offline"]["csurf_filetype"]).lower()
    ext = my_format
    if my_format == "fa" and my_settings["nam_io_offline"]["lfagmap"]:
        ext = ".sfx"

    # PGD
    my_input = surfex.JsonInputData(json.loads(
        '{"gtopo30.dir": "/lustre/storeB/project/nwp/surfex/PGD/gtopo30.dir", ' +
        '"gtopo30.hdr": "/lustre/storeB/project/nwp/surfex/PGD/gtopo30.hdr",' +
        '"ECOCLIMAP_II_EUROP.dir": "/lustre/storeB/project/nwp/surfex/PGD/ECOCLIMAP_II_EUROP_V2.2.dir",' +
        '"ECOCLIMAP_II_EUROP.hdr": "/lustre/storeB/project/nwp/surfex/PGD/ECOCLIMAP_II_EUROP_V2.2.hdr",' +
        '"GlobalLakeDepth.dir": "/lustre/storeB/project/nwp/surfex/PGD/GlobalLakeDepth.dir",' +
        '"GlobalLakeDepth.hdr": "/lustre/storeB/project/nwp/surfex/PGD/GlobalLakeDepth.hdr",' +
        '"GlobalLakeStatus.dir": "/lustre/storeB/project/nwp/surfex/PGD/GlobalLakeStatus.dir",' +
        '"GlobalLakeStatus.hdr": "/lustre/storeB/project/nwp/surfex/PGD/GlobalLakeStatus.hdr"' +
        '}'))

    pgd = test_dir + "/archive/oi_" + my_format + "/" + my_settings["nam_io_offline"]["cpgdfile"] + ext
    if not os.path.exists(pgd):
        workdir = test_dir + "/pgd_" + my_format
        surfex.create_working_dir(workdir)
        my_pgdfile = surfex.file.PGDFile(my_format, my_settings["nam_io_offline"]["cpgdfile"], my_geo, archive_file=pgd)
        surfex.SURFEXBinary(test_dir + "/bin/PGD.exe", my_batch, my_pgdfile, my_settings, my_ecoclimap, input=my_input)
        # surfex.clean_working_dir(workdir)

    # PREP
    prep = test_dir + "/archive/oi_" + my_format + "/" + my_settings["nam_io_offline"]["cprepfile"] + ext
    if not os.path.exists(prep):
        workdir = test_dir + "/prep_" + my_format
        surfex.create_working_dir(workdir)
        my_pgdfile = surfex.file.PGDFile(my_format, my_settings["nam_io_offline"]["cpgdfile"], my_geo, input_file=pgd)

        my_prepfile = surfex.PREPFile(my_format, my_settings["nam_io_offline"]["cprepfile"], my_geo, archive_file=prep)
        surfex.SURFEXBinary(test_dir + "/bin/PREP.exe", my_batch, my_prepfile, my_settings, my_ecoclimap,
                            pgdfile=my_pgdfile)
        # surfex.clean_working_dir(workdir)

    # Forcing

    # Offline
    my_input = surfex.JsonInputData(json.loads({"FORCING.nc": test_dir+"/forcing/FORCING.nc"}))
    first_guess = test_dir + "/archive/oi_" + my_format + "/" + my_settings["nam_io_offline"]["cpgdfile"] + ext
    if not os.path.exists(first_guess):
        workdir = test_dir + "/offline_" + my_format
        surfex.create_working_dir(workdir)
        my_csurffile = "SURFOUT"
        my_pgdfile = surfex.PGDFile(my_format, my_settings["nam_io_offline"]["cpgdfile"], my_geo, input_file=pgd)
        my_prepfile = surfex.PREPFile(my_format, my_settings["nam_io_offline"]["cprepfile"], my_geo, input_file=prep)
        my_surffile = surfex.SURFFile(my_format, my_settings["nam_io_offline"]["csurffile"], my_geo,
                                      archive_file=first_guess)
        surfex.SURFEXBinary(test_dir + "/bin/OFFLINE.exe", my_batch, my_prepfile, my_settings, my_ecoclimap,
                            surfout=my_surffile, input=my_input, pgdfile=my_pgdfile)
        # surfex.clean_working_dir(workdir)

    canari = test_dir + "/archive/oi_" + my_format + "/ICMSHHARM+0000" + ext
    if not os.path.exists(canari):

        # Set up assimilation
        my_dtg = datetime(2019, 11, 19, 6)
        gridpp_t2m = surfex.GridPP(test_dir + "/bin/gridpp", "filename_t2m", my_dtg, my_batch,
                                   ["air_temperature_2m"])
        gridpp_rh2m = surfex.GridPP(test_dir + "/bin/gridpp", "filename_rh2m", my_dtg, my_batch,
                                    ["relative_humidity_2m"])
        gridpp_sd = surfex.GridPP(test_dir + "/bin/gridpp", "filename_snow", my_dtg, my_batch,
                                  ["surface_snow_thickness"])

        my_sstfile = "SST_SIC"
        my_first_guess = "PREPFILE"
        my_ua_first_guess = "ICMSHHARM+0003"
        my_oi_coeffs = "polym"
        my_climfile = "climinput"
        my_lsmfile = "landseamask"

        assim_input_json = surfex.set_assimilation_input(my_dtg, my_settings, sstfile=my_sstfile,
                                                         ua_first_guess=my_ua_first_guess, climfile=my_climfile,
                                                         lsmfile=my_lsmfile, oi_coeffs=my_oi_coeffs)
        assim = surfex.Assimilation(ass_input=assim_input_json)

        workdir = test_dir + "/canari"
        surfex.create_working_dir(workdir)
        my_pgdfile = surfex.PGDFile(my_format, my_settings["nam_io_offline"]["cpgdfile"], my_geo, input_file=pgd, )
        my_prepfile = surfex.PREPFile(my_format, my_settings["nam_io_offline"]["cprepfile"], my_geo, input_file=prep)
        my_canarifile = surfex.SURFFile(my_format, my_settings["nam_io_offline"]["csurffile"], my_geo,
                                        archive_file=canari)

        # binary = test_dir + "/bin/MASTERODB"
        binary = None
        masterodb = surfex.Masterodb(my_settings, my_batch, my_pgdfile, my_prepfile, my_canarifile, my_ecoclimap,
                                     assim=assim, binary=binary, input=my_input, print_namelist=True)

        # Archive output
        if binary is None:
            masterodb.archive_output()
        # surfex.clean_working_dir(workdir)

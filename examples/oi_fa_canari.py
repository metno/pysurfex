import f90nml
import os
from datetime import datetime
import surfex
import json


if __name__ == "__main__":

    root_dir = os.getcwd()

    domain_name = "NORWAY-SOUTH"
    domain_json = surfex.set_domain(json.load(open(root_dir + "/settings/domains.json", "r")), domain_name)
    print(domain_json)
    my_geo = surfex.ConfProj(domain_json)

    toml_files = [root_dir + "/settings/config_exp_surfex.toml"]
    merged_toml_env = surfex.merge_toml_env_from_files(toml_files)
    my_settings, my_ecoclimap, my_input = surfex.set_json_namelist_from_toml_env("pgd", merged_toml_env,
                                                                                 root_dir + "/settings/",
                                                                                 root_dir + "/settings/system.ppi.json")
    print(my_settings)

    test_dir = os.environ["HOME"] + "/surfex-tests"
    os.makedirs(test_dir, exist_ok=True)

    my_rte = os.environ.copy()
    my_rte.update(json.loads('{"DR_HOOK": "0","DR_HOOK_NOT_MPI": "1","OMP_NUM_THREADS":"1"}'))

    my_settings = my_geo.update_namelist(my_settings)
    #my_batch = surfex.BatchJob(my_rte, wrapper="mpiexec -np 8")
    my_batch = surfex.BatchJob(my_rte, wrapper="")

    my_format = str(my_settings["nam_io_offline"]["csurf_filetype"]).lower()
    ext = my_format
    if my_format == "fa" and my_settings["nam_io_offline"]["lfagmap"]:
        ext = ".sfx"

    # PGD
    pgd = test_dir + "/archive/oi_" + my_format + "/" + my_settings["nam_io_offline"]["cpgdfile"] + ext
    if not os.path.exists(pgd):
        workdir = test_dir + "/pgd_" + my_format
        surfex.create_working_dir(workdir)
        my_pgdfile = surfex.file.PGDFile(my_format, my_settings["nam_io_offline"]["cpgdfile"], my_geo, archive_file=pgd)
        surfex.SURFEXBinary(test_dir + "/bin/PGD.exe", my_batch, my_pgdfile, my_settings, my_ecoclimap, input=my_input)
        # surfex.clean_working_dir(workdir)

    # PREP
    my_dtg_fg = datetime(2019, 11, 19, 3)
    prep = test_dir + "/archive/oi_" + my_format + "/" + my_settings["nam_io_offline"]["cprepfile"] + ext
    if not os.path.exists(prep):
        workdir = test_dir + "/prep_" + my_format
        surfex.create_working_dir(workdir)

        my_settings, my_ecoclimap, my_input = surfex.set_json_namelist_from_toml_env("prep", merged_toml_env,
                                            root_dir + "/settings/",
                                            root_dir + "/settings/system.ppi.json",
                                            prep_file = root_dir + "/settings/prep_from_namelist_values.json",
                                            prep_filetype = "json",
                                            dtg=my_dtg_fg.strftime("%Y%m%d%H"))

        my_pgdfile = surfex.file.PGDFile(my_format, my_settings["nam_io_offline"]["cpgdfile"], my_geo, input_file=pgd)

        my_prepfile = surfex.PREPFile(my_format, my_settings["nam_io_offline"]["cprepfile"], my_geo, archive_file=prep)
        surfex.SURFEXBinary(test_dir + "/bin/PREP.exe", my_batch, my_prepfile, my_settings, my_ecoclimap,
                            input=my_input, pgdfile=my_pgdfile)
        # surfex.clean_working_dir(workdir)

    # Forcing

    # Offline
    my_forcing_input = surfex.JsonInputData(json.loads('{"FORCING.nc": "' + test_dir + '/forcing/FORCING.nc"}'))

    first_guess = test_dir + "/archive/oi_" + my_format + "/" + my_settings["nam_io_offline"]["cpgdfile"] + ext
    if not os.path.exists(first_guess):
        workdir = test_dir + "/offline_" + my_format
        surfex.create_working_dir(workdir)

        my_settings, my_ecoclimap, my_input = surfex.set_json_namelist_from_toml_env("offline", merged_toml_env,
                                                                                     root_dir + "/settings/",
                                                                                     root_dir + "/settings/system.ppi.json",
                                                                                     forc_zs=True)

        my_csurffile = "SURFOUT"
        my_pgdfile = surfex.PGDFile(my_format, my_settings["nam_io_offline"]["cpgdfile"], my_geo, input_file=pgd)
        my_prepfile = surfex.PREPFile(my_format, my_settings["nam_io_offline"]["cprepfile"], my_geo, input_file=prep)
        my_surffile = surfex.SURFFile(my_format, my_settings["nam_io_offline"]["csurffile"], my_geo,
                                      archive_file=first_guess)
        surfex.SURFEXBinary(test_dir + "/bin/OFFLINE.exe", my_batch, my_prepfile, my_settings, my_ecoclimap,
                            surfout=my_surffile, input=my_forcing_input, pgdfile=my_pgdfile)
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

        workdir = test_dir + "/canari"
        surfex.create_working_dir(workdir)

        my_settings, my_ecoclimap, my_input = surfex.set_json_namelist_from_toml_env("soda", merged_toml_env,
                                                                                     root_dir + "/settings/",
                                                                                     root_dir + "/settings/system.ppi.json")

        my_sstfile = "SST_SIC"
        my_first_guess = "PREPFILE"
        my_ua_first_guess = "ICMSHHARM+0003"
        my_climfile = "climinput"
        my_lsmfile = "landseamask"

        print(my_input)
        assim_input = surfex.set_assimilation_input(my_dtg, my_settings, sstfile=my_sstfile,
                                                         ua_first_guess=my_ua_first_guess, climfile=my_climfile,
                                                         lsmfile=my_lsmfile)
        assim = surfex.Assimilation(ass_input=assim_input)


        my_pgdfile = surfex.PGDFile(my_format, my_settings["nam_io_offline"]["cpgdfile"], my_geo, input_file=pgd, )
        my_prepfile = surfex.PREPFile(my_format, my_settings["nam_io_offline"]["cprepfile"], my_geo, input_file=prep)
        my_canarifile = surfex.SURFFile(my_format, my_settings["nam_io_offline"]["csurffile"], my_geo,
                                        archive_file=canari)

        # binary = test_dir + "/bin/MASTERODB"
        binary = None
        masterodb = surfex.Masterodb(my_settings, my_batch, my_pgdfile, my_prepfile, my_canarifile, my_ecoclimap,
                                     assim=assim, binary=binary, input=my_input, print_namelist=True)

        # Archive output
        if binary is not None:
            masterodb.archive_output()
        # surfex.clean_working_dir(workdir)

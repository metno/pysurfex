import f90nml
import os
from datetime import datetime
import surfex
import json

if __name__ == "__main__":

    my_geo = surfex.ConfProj(100, 100, 10, 60, 15, 50, 2500, ezone=11)

    root_dir = os.getcwd()
    my_settings = surfex.io.ascii2nml(root_dir + "/settings/ekf_nc.json")
    test_dir = os.environ["HOME"]+"/surfex-tests"
    os.makedirs(test_dir, exist_ok=True)

    my_rte = os.environ.copy()
    my_rte.update(json.loads('{"DR_HOOK": "0","DR_HOOK_NOT_MPI": "1","OMP_NUM_THREADS":"1"}'))

    my_settings = my_geo.update_namelist(my_settings)
    my_batch = surfex.BatchJob(my_rte, wrapper="mpiexec -np 8")

    my_ecoclimap = surfex.JsonInputDataFromFile(root_dir+"/settings/ecoclimap.json")

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

    my_format = str(my_settings["nam_io_offline"]["csurf_filetype"]).lower()
    pgd = test_dir + "/archive/ekf_" + my_format + "/PGD." + my_format
    print(pgd)
    if not os.path.exists(pgd):
        workdir = test_dir + "/pgd_" + my_format
        surfex.create_working_dir(workdir)
        my_pgdfile = surfex.file.PGDFile(my_format, my_settings["nam_io_offline"]["cpgdfile"], my_geo, archive_file=pgd)
        surfex.SURFEXBinary(test_dir + "/bin/PGD.exe", my_batch, my_pgdfile, my_settings, my_ecoclimap, input=my_input)
        # surfex.clean_working_dir(workdir)

    # PREP
    prep = test_dir + "/archive/ekf_" + my_format + "/PREP." + my_format
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
    my_forcing_input = surfex.JsonInputData(json.loads('{"FORCING.nc": "' + test_dir + '/forcing/FORCING.nc"}'))
    first_guess = test_dir + "/archive/ekf_" + my_format + "/EKF_FG." + my_format
    if not os.path.exists(first_guess):
        workdir = test_dir + "/offline_" + my_format
        surfex.create_working_dir(workdir)
        my_pgdfile = surfex.PGDFile(my_format, my_settings["nam_io_offline"]["cpgdfile"], my_geo, input_file=pgd)
        my_prepfile = surfex.PREPFile(my_format, my_settings["nam_io_offline"]["cprepfile"], my_geo, input_file=prep)
        my_surffile = surfex.SURFFile(my_format, my_settings["nam_io_offline"]["csurffile"], my_geo,
                                      archive_file=first_guess)
        archive_dir = test_dir + '/archive/ekf_' + my_format
        my_archive = surfex.JsonOutputData(json.loads('{"SURFOUT.20191119_04h00.' + my_format + '": "' +
                                                      archive_dir + '/SURFOUT.20191119_04h00.' + my_format + '", ' +
                                                      '"SURFOUT.20191119_05h00.' + my_format + '": "' +
                                                      archive_dir + '/SURFOUT.20191119_05h00.' + my_format + '", ' +
                                                      '"SURFOUT.20191119_06h00.' + my_format + '": "' +
                                                      archive_dir + '/SURFOUT.20191119_06h00.' + my_format + '"}'))

        surfex.SURFEXBinary(test_dir + "/bin/OFFLINE.exe", my_batch, my_prepfile, my_settings, my_ecoclimap,
                            surfout=my_surffile, input=my_forcing_input, pgdfile=my_pgdfile, archive=my_archive)
        # surfex.clean_working_dir(workdir)

    # Set up assimilation
    my_dtg_fg = datetime(2019, 11, 19, 3)
    my_dtg = datetime(2019, 11, 19, 6)
    gridpp_t2m = surfex.GridPP(test_dir + "/bin/gridpp", "filename_t2m", my_dtg, my_batch, ["air_temperature_2m"])
    gridpp_rh2m = surfex.GridPP(test_dir + "/bin/gridpp", "filename_rh2m", my_dtg, my_batch, ["relative_humidity_2m"])
    gridpp_sd = surfex.GridPP(test_dir + "/bin/gridpp", "filename_snow", my_dtg, my_batch, ["surface_snow_thickness"])

    my_obsfile = "result_of_above"

    # EKF
    my_incvars = ["TG2", "WG2"]
    my_perturbed_runs = []
    for pert in range(0, len(my_incvars)+1):
        pert_name = test_dir + "/archive/ekf_" + my_format + "/PREP_EKF_PERT" + str(pert) + "." + my_format
        if not os.path.exists(pert_name):
            workdir = test_dir + "/offline_pert_" + str(pert) + "_" + my_format
            surfex.create_working_dir(workdir)
            my_pgdfile = surfex.PGDFile(my_format, my_settings["nam_io_offline"]["cpgdfile"], my_geo, input_file=pgd)
            my_prepfile = surfex.PREPFile(my_format, my_settings["nam_io_offline"]["cprepfile"], my_geo,
                                          input_file=prep)
            surfout = surfex.SURFFile("NC", "SURFOUT", my_geo, archive_file=pert_name)
            surfex.PerturbedOffline(test_dir + "/bin/OFFLINE.exe", my_batch, my_prepfile, pert, my_settings,
                                    my_ecoclimap, surfout=surfout, input=my_forcing_input, pgdfile=my_pgdfile)
            # surfex.clean_working_dir(workdir)
        my_perturbed_runs.append(pert_name)

    # EKF assmilation
    soda = test_dir + "/archive/ekf_" + my_format + "/SODA." + my_format
    if not os.path.exists(soda):

        # EKF need lassim true only for analysis
        my_settings["NAM_ASSIM"]["LASSIM"] = True
        my_settings["NAM_VAR"]['NVAR'] = len(my_incvars)

        workdir = test_dir + "/soda_ekf_" + my_format
        surfex.create_working_dir(workdir)

        my_sfx_first_guess = test_dir + "/archive/ekf_" + my_format + "/EKF_FG." + my_format
        my_sstfile = "SST_SIC"

        assim_input_json = surfex.set_assimilation_input(my_dtg, my_settings, sstfile=my_sstfile,
                                                         sfx_first_guess=my_sfx_first_guess, obsfile=my_obsfile,
                                                         perturbed_runs=my_perturbed_runs)
        assim = surfex.Assimilation(ass_input=assim_input_json)

        my_input = None
        my_pgdfile = surfex.PGDFile(my_format, my_settings["nam_io_offline"]["cpgdfile"], my_geo, input_file=pgd)
        my_prepfile = surfex.PREPFile(my_format, my_settings["nam_io_offline"]["cprepfile"], my_geo,
                                      input_file=my_sfx_first_guess)
        my_sodafile = surfex.SURFFile(my_format, my_settings["nam_io_offline"]["csurffile"], my_geo, archive_file=soda)
        surfex.SURFEXBinary(test_dir + "/bin/SODA.exe", my_batch, my_prepfile, my_settings, my_ecoclimap, assim=assim,
                            input=my_input, pgdfile=my_pgdfile, surfout=my_sodafile)
        # surfex.clean_working_dir(workdir)

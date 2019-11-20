import f90nml
import os
from datetime import datetime
import surfex
import json


if __name__ == "__main__":

    my_geo = surfex.ConfProj(100, 100, 10, 60, 15, 50, 2500, ezone=11)

    root_dir = os.getcwd()
    my_settings = f90nml.read(root_dir + '/settings/oi_nc.nml')
    test_dir = os.environ["HOME"]+"/surfex-tests"
    os.makedirs(test_dir, exist_ok=True)

    my_rte = os.environ.copy()
    my_rte.update(json.loads('{"DR_HOOK": "0","DR_HOOK_NOT_MPI": "1","OMP_NUM_THREADS":"1"}'))

    my_settings = my_geo.update_namelist(my_settings)
    my_batch = surfex.BatchJob(my_rte, wrapper="mpiexec -np 8")

    my_ecoclimap_files = {
        "ecoclimapI_covers_param.bin": "/home/asmundb/git/Harmonie/util/offline/MY_RUN/ECOCLIMAP/ecoclimapI_covers_param.bin",
        "ecoclimapII_af_covers_param.bin": "/home/asmundb/git/Harmonie/util/offline/MY_RUN/ECOCLIMAP/ecoclimapII_af_covers_param.bin",
        "ecoclimapII_eu_covers_param.bin": "/home/asmundb/git/Harmonie/util/offline/MY_RUN/ECOCLIMAP/ecoclimapII_eu_covers_param.bin"
    }

    my_ecoclimap_input = surfex.InputData(files=my_ecoclimap_files)
    format = str(my_settings["nam_io_offline"]["csurf_filetype"]).lower()

    # PGD
    my_input_files = {"gtopo30.dir": "/lustre/storeB/project/nwp/surfex/PGD/gtopo30.dir",
                      "gtopo30.hdr": "/lustre/storeB/project/nwp/surfex/PGD/gtopo30.hdr",
                      "ECOCLIMAP_II_EUROP.dir": "/lustre/storeB/project/nwp/surfex/PGD/ECOCLIMAP_II_EUROP_V2.2.dir",
                      "ECOCLIMAP_II_EUROP.hdr": "/lustre/storeB/project/nwp/surfex/PGD/ECOCLIMAP_II_EUROP_V2.2.hdr",
                      "GlobalLakeDepth.dir": "/lustre/storeB/project/nwp/surfex/PGD/GlobalLakeDepth.dir",
                      "GlobalLakeDepth.hdr": "/lustre/storeB/project/nwp/surfex/PGD/GlobalLakeDepth.hdr",
                      "GlobalLakeStatus.dir": "/lustre/storeB/project/nwp/surfex/PGD/GlobalLakeStatus.dir",
                      "GlobalLakeStatus.hdr": "/lustre/storeB/project/nwp/surfex/PGD/GlobalLakeStatus.hdr"
                      }

    my_input = [my_ecoclimap_input]
    my_input.append(surfex.InputData(files=my_input_files))
    pgd = test_dir + "/archive/oi_"+format+"/PGD."+format
    if not os.path.exists(pgd):
        workdir = test_dir + "/pgd_"+format
        surfex.create_working_dir(workdir)
        my_pgdfile = surfex.file.PGDFile(format,my_settings["nam_io_offline"]["cpgdfile"], my_geo, archive_file=pgd)
        surfex.SURFEXBinary(test_dir + "/bin/PGD.exe", my_batch, my_pgdfile, my_settings, input=my_input)
        # surfex.clean_working_dir(workdir)

    # PREP
    prep = test_dir + "/archive/oi_"+format+"/PREP."+format
    if not os.path.exists(prep):
        workdir = test_dir + "/prep_"+format
        surfex.create_working_dir(workdir)
        my_pgdfile = surfex.file.PGDFile(format, my_settings["nam_io_offline"]["cpgdfile"], my_geo, input_file=pgd)

        my_input = [my_ecoclimap_input]
        my_prepfile = surfex.PREPFile(format, my_settings["nam_io_offline"]["cprepfile"], my_geo, archive_file=prep)
        surfex.SURFEXBinary(test_dir + "/bin/PREP.exe", my_batch, my_prepfile, my_settings, input=my_input,
                            pgdfile=my_pgdfile)
        # surfex.clean_working_dir(workdir)

    my_input = [my_ecoclimap_input]

    # Forcing

    # Offline
    my_input = [my_ecoclimap_input]
    my_input.append(surfex.InputData({"FORCING.nc": test_dir+"/forcing/FORCING.nc"}))

    first_guess = test_dir+"/archive/oi_"+format+"/OI_FG."+format
    if not os.path.exists(first_guess):
        workdir = test_dir + "/offline_"+format
        surfex.create_working_dir(workdir)
        my_pgdfile = surfex.PGDFile(format, my_settings["nam_io_offline"]["cpgdfile"], my_geo, input_file=pgd)
        my_prepfile = surfex.PREPFile(format, my_settings["nam_io_offline"]["cprepfile"], my_geo, input_file=prep)
        my_surffile = surfex.SURFFile(format, my_settings["nam_io_offline"]["csurffile"], my_geo, archive_file=first_guess)
        my_archive = surfex.ArchiveData(files=["SURFOUT.20191119_04h00."+format, "SURFOUT.20191119_05h00."+format,
                                       "SURFOUT.20191119_06h00."+format],
                                archive_dir=test_dir + "/archive/oi_"+format)
        surfex.SURFEXBinary(test_dir + "/bin/OFFLINE.exe", my_batch, my_prepfile, my_settings,
                                  surfout=my_surffile, input=my_input, pgdfile=my_pgdfile, archive=my_archive)
        # surfex.clean_working_dir(workdir)

    # Set up assimilation
    my_dtg = datetime(2019, 11, 19, 6)
    gridpp_t2m = surfex.GridPP(test_dir + "/bin/gridpp", "filename_t2m", my_dtg, my_batch, ["air_temperature_2m"])
    gridpp_rh2m = surfex.GridPP(test_dir + "/bin/gridpp", "filename_rh2m", my_dtg, my_batch, ["relative_humidity_2m"])
    gridpp_sd = surfex.GridPP(test_dir + "/bin/gridpp", "filename_snow", my_dtg, my_batch, ["surface_snow_thickness"])

    my_sstfile = "SST_SIC"
    assim_sea = surfex.SeaAssimilation(my_settings, my_sstfile)
    my_first_guess = "PREPFILE"
    my_oi_coeffs = "polym"
    my_climfile = "climinput"
    my_lsmfile = "landseamask"
    oi = surfex.VerticalOI(my_settings, my_first_guess, my_oi_coeffs, climfile=my_climfile, lsmfile=my_lsmfile)
    assim_nature = surfex.NatureAssimilation(my_settings, oi)

    # assim = Assimilation(my_dtg, my_settings, sea=assim_sea, nature=assim_nature,
    #                     obs=[gridpp_t2m, gridpp_rh2m, gridpp_sd])
    assim = surfex.Assimilation(my_dtg, my_settings, sea=assim_sea, nature=assim_nature)

    soda = test_dir + "/archive/oi_" + format + "/SODA."+format
    if not os.path.exists(soda):
        my_input = [my_ecoclimap_input]
        workdir = root_dir + "/soda_oi_nc"
        surfex.create_working_dir(workdir)
        my_pgdfile = surfex.PGDFile(format, my_settings["nam_io_offline"]["cpgdfile"], my_geo, input_file=pgd)
        my_prepfile = surfex.PREPFile(format, my_settings["nam_io_offline"]["cprepfile"], my_geo, input_file=prep)
        my_sodafile = surfex.SURFFile(format, my_settings["nam_io_offline"]["csurffile"], my_geo, archive_file=soda)
        surfex.SURFEXBinary(test_dir + "/bin/SODA.exe", my_batch, my_prepfile, my_settings, surfout=my_sodafile,
                        input=my_input, pgdfile=my_pgdfile)
        # surfex.clean_working_dir(workdir)



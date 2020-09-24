import os
import surfex
import jsonmerge
import numpy as np
try:
    import gridpp
except ImportError:
    gridpp = None


def horizontal_oi(geo, background, observations, gelevs, glafs, hlength=10000.,
                  vlength=10000., wlength=0.5, elev_gradient=0, structure_function="Barnes",
                  land_only=False, max_locations=50, epsilon=0.5, minvalue=None, maxvalue=None, interpol="bilinear"):

    if gridpp is None:
        raise Exception("You need gridpp to perform OI")

    glats = geo.lats
    glons = geo.lons

    def obs2vectors(my_obs):
        return my_obs.lons, my_obs.lats, my_obs.stids, my_obs.elevs, \
               my_obs.values, my_obs.cis, my_obs.lafs

    vectors = np.vectorize(obs2vectors)
    lons, lats, stids, elevs, values, pci, lafs = vectors(observations)

    glats = np.transpose(glats)
    glons = np.transpose(glons)
    background = np.transpose(background)
    gelevs = np.transpose(gelevs)
    glafs = np.transpose(glafs)

    bgrid = gridpp.Grid(glats, glons, gelevs)
    points = gridpp.Points(lats, lons, elevs)
    if interpol == "bilinear":
        pbackground = gridpp.bilinear(bgrid, points, background)
    elif interpol == "nearest":
        pbackground = gridpp.nearest(bgrid, points, background, elev_gradient)
    else:
        raise NotImplementedError
    variance_ratios = np.full(points.size(), epsilon)

    if structure_function == "Barnes":
        # No land/sea weight when land_only = True
        if land_only:
            wlength = 0.
        structure = gridpp.BarnesStructure(hlength, vlength, wlength)
    else:
        raise NotImplementedError

    field = gridpp.optimal_interpolation(bgrid, background, points, values, variance_ratios, pbackground, structure,
                                         max_locations)
    field = np.asarray(field)
    if minvalue is not None:
        field[field < minvalue] = minvalue
    if maxvalue is not None:
        field[field > maxvalue] = maxvalue
    if land_only:
        no_land = np.where(glafs == 0)
        field[no_land] = background[no_land]
    return np.transpose(field)


class Assimilation(object):
    def __init__(self, ass_input=None, ass_output=None):
        self.ass_input = ass_input
        self.ass_output = ass_output


def set_assimilation_input(dtg, settings, sstfile=None, ua_first_guess=None, perturbed_runs=None, lsmfile=None,
                           obsfile=None, check_existence=False, oi_coeffs=None, climfile=None, sfx_first_guess=None,
                           ascatfile=None):

    settings = surfex.capitalize_namelist_dict(settings)
    data = None
    if settings["NAM_ASSIM"]['CASSIM_SEA'] == "INPUT":
        sea_data = set_input_sea_assimilation(settings, sstfile)
        print(sea_data)
        print(data)
        if data is not None:
            data = jsonmerge.merge(data, sea_data)
        else:
            data = sea_data

    if settings["NAM_ASSIM"]['CASSIM_ISBA'] == "OI":
        oi_data = set_input_vertical_soil_oi(dtg, settings, ua_first_guess, oi_coeffs=oi_coeffs, climfile=climfile,
                                             lsmfile=lsmfile, ascatfile=ascatfile, check_existence=check_existence)
        if data is not None:
            print(oi_data)
            data = jsonmerge.merge(data, oi_data)
            print(data)
        else:
            data = oi_data

    if settings["NAM_ASSIM"]['CASSIM_ISBA'] == "EKF":
        ekf_data = set_input_vertical_soil_ekf(dtg, settings, sfx_first_guess, perturbed_runs, lsmfile=lsmfile,
                                               check_existence=check_existence)
        if data is not None:
            data = jsonmerge.merge(data, ekf_data)
        else:
            data = ekf_data

    obs_data = set_input_observations(dtg, settings, obsfile, check_existence=check_existence)
    if data is not None:
        data = jsonmerge.merge(data, obs_data)
    else:
        data = obs_data

    return surfex.JsonInputData(data)


def set_input_observations(dtg, settings, obsfile, check_existence=False):
    yy = dtg.strftime("%y")
    mm = dtg.strftime("%m")
    dd = dtg.strftime("%d")
    hh = dtg.strftime("%H")

    obssettings = {}
    cfile_format_obs = settings["NAM_OBS"]['CFILE_FORMAT_OBS']
    if cfile_format_obs == "ASCII":
        target = "OBSERVATIONS_" + yy + mm + dd + "H" + hh + ".DAT"
    elif cfile_format_obs == "FA":
        target = "CANARI"
    else:
        print(cfile_format_obs)
        raise NotImplementedError

    if obsfile is not None:
        if os.path.exists(obsfile):
            obssettings.update({target: obsfile})
        else:
            if check_existence:
                print(cfile_format_obs)
                raise FileNotFoundError(obsfile)

    return obssettings


def set_input_sea_assimilation(settings, sstfile, check_existence=False):

    if sstfile is None:
        print("You must set sstfile")
        raise Exception

    settings = surfex.capitalize_namelist_dict(settings)
    sea_settings = {}
    cfile_format_sst = settings["NAM_ASSIM"]['CFILE_FORMAT_SST']
    if cfile_format_sst.upper() == "ASCII":
        target = "SST_SIC.DAT"
    elif cfile_format_sst.upper() == "FA":
        target = "SST_SIC"
    else:
        print(cfile_format_sst)
        raise NotImplementedError
    sea_settings.update({target: sstfile})
    if not os.path.exists(sstfile) and check_existence:
        print("Needed file missing: " + sstfile)
        raise FileNotFoundError

    return sea_settings


def set_input_vertical_soil_oi(dtg, settings, first_guess, oi_coeffs=None, climfile=None, lsmfile=None, ascatfile=None,
                               check_existence=False):

    settings = surfex.capitalize_namelist_dict(settings)
    if first_guess is None:
        print("You must set first guess for OI")
        raise Exception

    yy = dtg.strftime("%y")
    mm = dtg.strftime("%m")
    dd = dtg.strftime("%d")
    hh = dtg.strftime("%H")
    oi_settings = {}

    # Climate
    cfile_format_clim = settings["NAM_ASSIM"]['CFILE_FORMAT_CLIM']
    if cfile_format_clim.upper() == "ASCII":
        target = "CLIMATE.DAT"
    elif cfile_format_clim.upper() == "FA":
        target = "clim_isba"
    else:
        print(cfile_format_clim)
        raise NotImplementedError
    if climfile is not None:
        if os.path.exists(climfile):
            oi_settings.update({target: climfile})
        else:
            if check_existence:
                print("Needed file missing: " + climfile)
                raise FileNotFoundError
    else:
        raise FileNotFoundError("OI needs a climate file")

    # First guess for SURFEX
    cfile_format_fg = settings["NAM_ASSIM"]['CFILE_FORMAT_FG']
    if cfile_format_fg.upper() == "ASCII":
        target = "FIRST_GUESS_" + yy + mm + dd + "H" + hh + ".DAT"
    elif cfile_format_fg.upper() == "FA":
        target = "FG_OI_MAIN"
    else:
        print(cfile_format_fg)
        raise NotImplementedError

    if os.path.exists(first_guess):
        oi_settings.update({target: first_guess})
    else:
        print("First guess not found")
        print("Needed file missing: " + first_guess)
        raise FileNotFoundError

    if ascatfile is not None:
        if os.path.exists(ascatfile):
            oi_settings.update({"ASCAT_SM.DAT": ascatfile})
        else:
            if check_existence:
                print("Needed file missing: " + ascatfile)
                raise FileNotFoundError

    # OI coefficients
    if oi_coeffs is not None:
        if os.path.exists(oi_coeffs):
            oi_settings.update({"fort.61": oi_coeffs})
        else:
            print("Needed file missing for OI coefficients: " + oi_coeffs)
            raise FileNotFoundError

    # LSM
    cfile_format_lsm = settings["NAM_ASSIM"]['CFILE_FORMAT_CLIM']
    if cfile_format_lsm.upper() == "ASCII":
        target = "LSM.DAT"
    elif cfile_format_lsm.upper() == "FA":
        target = "FG_OI_MAIN"
    else:
        print(cfile_format_lsm)
        raise NotImplementedError
    if lsmfile is not None:
        if os.path.exists(lsmfile):
            oi_settings.update({target: lsmfile})
        else:
            if check_existence:
                print("Needed file missing: " + lsmfile)
                raise FileNotFoundError
    else:
        print("OI needs a LSM file")
        raise FileNotFoundError

    return oi_settings


def set_input_vertical_soil_ekf(dtg, settings, first_guess, perturbed_runs, lsmfile=None, check_existence=False):

    settings = surfex.capitalize_namelist_dict(settings)
    if first_guess is None or perturbed_runs is None:
        raise Exception("You must set input files (first_guess and/or perturbed_runs)")

    yy = dtg.strftime("%y")
    mm = dtg.strftime("%m")
    dd = dtg.strftime("%d")
    hh = dtg.strftime("%H")
    ekf_settings = {}

    # First guess for SURFEX

    extension = settings["NAM_IO_OFFLINE"]['CSURF_FILETYPE'].lower()
    if extension == "ascii":
        extension = ".txt"
    if first_guess is not None:
        if os.path.exists(first_guess):
            ekf_settings.update({"PREP_INIT." + extension: first_guess})
            ekf_settings.update({"PREP_" + yy + mm + dd + "H" + hh + "." + extension: first_guess})
        else:
            if check_existence:
                print("Needed file missing: " + first_guess)
                raise FileNotFoundError

    for p in range(0, len(perturbed_runs)):
        target = "PREP_" + yy + mm + dd + "H" + hh + "_EKF_PERT" + str(p) + "." + extension
        ekf_settings.update({target: perturbed_runs[p]})
        if check_existence and not os.path.exists(perturbed_runs[p]):
            print("Needed file missing: " + perturbed_runs[p])
            raise FileNotFoundError

    # LSM
    # Fetch first_guess needed for LSM for extrapolations
    if settings["NAM_ASSIM"]['LEXTRAP_SEA'] or settings["NAM_ASSIM"]['LEXTRAP_WATER'] or \
            settings["NAM_ASSIM"]['LEXTRAP_NATURE'] or settings["NAM_ASSIM"]['LEXTRAP_SNOW']:

        print(settings)
        cfile_format_lsm = settings["NAM_ASSIM"]['CFILE_FORMAT_LSM']
        if cfile_format_lsm.upper() == "ASCII":
            target = "LSM.DAT"
        elif cfile_format_lsm.upper() == "FA":
            target = "FG_OI_MAIN"
        else:
            print(cfile_format_lsm)
            raise NotImplementedError
        if lsmfile is not None:
            if os.path.exists(lsmfile):
                ekf_settings.update({target: lsmfile})
            else:
                if check_existence:
                    print("Needed file missing: " + lsmfile)
                    raise FileNotFoundError
        else:
            print("EKF needs a LSM file to extrapolate values")
            raise FileNotFoundError
    return ekf_settings


def set_assimilation_output(settings):

    settings = surfex.capitalize_namelist_dict(settings)
    data = None
    if settings["NAM_ASSIM"]['CASSIM_ISBA'] == "EKF":
        ekf_data = set_output_vertical_soil_ekf(settings)
        if data is not None:
            data = jsonmerge.merge(data, ekf_data)
        else:
            data = ekf_data

    if data is None:
        data = "{}"
    return data


def set_output_vertical_soil_ekf(settings):

    settings = surfex.capitalize_namelist_dict(settings)
    data = "{}"
    if settings["NAM_ASSIM"]['LBEV']:
        # TODO fill with wanted values
        data = "{}"

    print("WARNING: not implemented yet")

    '''
    Fromm SODA script
    mv ANAL_INCR*  $WRK/.
    mv INNOV* $WRK/.
    mv OBSout* $WRK/.
    mv OBSERRORout* $WRK/.
    [ -f HO_WG2_v1 ] && mv HO_WG2_v1  $WRK/HO_WG2_T2M_$FGYY$FGMM${FGDD}_r$NT.dat
    [ -f HO_WG2_v2 ] && mv HO_WG2_v2  $WRK/HO_WG2_HU2M_$FGYY$FGMM${FGDD}_r$NT.dat
    [ -f HO_WG2_v3 ] && mv HO_WG2_v3  $WRK/HO_WG2_WG1_$FGYY$FGMM${FGDD}_r$NT.dat
    [ -f HO_WG1_v1 ] && mv HO_WG1_v1  $WRK/HO_WG1_T2M_$FGYY$FGMM${FGDD}_r$NT.dat
    [ -f HO_WG1_v2 ] && mv HO_WG1_v2  $WRK/HO_WG1_HU2M_$FGYY$FGMM${FGDD}_r$NT.dat
    [ -f HO_WG1_v3 ] && mv HO_WG1_v3  $WRK/HO_WG1_WG1_$FGYY$FGMM${FGDD}_r$NT.dat
    [ -f HO_TG2_v1 ] && mv HO_TG2_v1  $WRK/HO_TG2_T2M_$FGYY$FGMM${FGDD}_r$NT.dat
    [ -f HO_TG2_v2 ] && mv HO_TG2_v2  $WRK/HO_TG2_HU2M_$FGYY$FGMM${FGDD}_r$NT.dat
    [ -f HO_TG2_v3 ] && mv HO_TG2_v3  $WRK/HO_TG2_WG1_$FGYY$FGMM${FGDD}_r$NT.dat
    [ -f HO_TG1_v1 ] && mv HO_TG1_v1  $WRK/HO_TG1_T2M_$FGYY$FGMM${FGDD}_r$NT.dat
    [ -f HO_TG1_v2 ] && mv HO_TG1_v2  $WRK/HO_TG1_HU2M_$FGYY$FGMM${FGDD}_r$NT.dat
    [ -f HO_TG1_v3 ] && mv HO_TG1_v3  $WRK/HO_TG1_WG1_$FGYY$FGMM${FGDD}_r$NT.dat
    mv BGROUNDout_ASSIM.* $ARCHIVE

    '''
    return data


def create_lsm_ascii_file():
    pass

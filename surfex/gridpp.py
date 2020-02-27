from netCDF4 import Dataset
import numpy as np


def create_gridpp_parameters(files, keep, providers, lonrange, latrange, override_ci, default_ci):
    try:
        import csv
    except ModuleNotFoundError:
        print("Could not import needed modules")

    if latrange is not None:
        latrange = [float(x) for x in latrange.split(',')]
    else:
        latrange = [-180, 180]
    if lonrange is not None:
        lonrange = [float(x) for x in lonrange.split(',')]
    else:
        lonrange = [-180, 180]

    lats = list()
    lons = list()
    elevs = list()
    values = list()
    cis = list()
    for file in files:
        ifile = open(file, 'r')
        header = ifile.readline().strip().split(';')
        Ilat = header.index("lat")
        Ilon = header.index("lon")
        Ielev = header.index("elev")
        Ici = None
        if "rep" in header:
            Ici = header.index("rep")
        Ivalue = header.index("value")

        if keep is not None:
            keep = [int(q) for q in keep.split(',')]
            if "dqc" not in header:
                print("File '%s' missing 'dqc' column. Cannot select based on dqc." % file)
                continue
            Idqc = header.index("dqc")

        if providers is not None:
            providers = [int(q) for q in providers.split(',')]
            if "prid" not in header:
                print("File '%s' missing 'prid' column. Cannot select based on provider." % file)
                continue
            Iprovider = header.index("prid")

        for line in ifile:
            words = line.strip().split(";")
            lat = float(words[Ilat])
            lon = float(words[Ilon])
            if lat > latrange[0] and lat < latrange[1] and lon > lonrange[0] and lon < lonrange[1]:
                if keep is not None:
                    dqc = int(words[Idqc])
                    if dqc not in keep:
                        continue
                if providers is not None:
                    provider = int(words[Iprovider])
                    if provider not in providers:
                        continue
                lats += [lat]
                lons += [lon]
                elevs += [float(words[Ielev])]
                values += [float(words[Ivalue])]
                ci_value = default_ci
                if override_ci is not None:
                    ci_value = override_ci
                elif Ici is not None:
                    try:
                        ci_value = float(words[Ici])
                    except Exception as e:
                        ci_value = default_ci
                cis += [ci_value]

    return lons, lats, elevs, values, cis


def check_input_to_soda_dimensions(nx, ny, nx1, ny1):

    if nx < 0:
        nx = nx1
    if ny < 0:
        ny = ny1
    if nx != nx1:
        raise Exception("Mismatch in nx dimension "+str(nx)+" != "+str(nx1))
    if ny != ny1:
        raise Exception("Mismatch in ny dimension "+str(ny)+" != "+str(ny1))

    return nx, ny


def var2ascii(t2m, rh2m, sd, yy, mm, dd, hh):

    nx = -1
    ny = -1
    i = 0
    t2m_var = None
    if t2m["file"] is not None:
        t2m_fh = Dataset(t2m["file"], "r")
        print(t2m["var"], t2m_fh.variables[t2m["var"]].shape)
        t2m_var = t2m_fh.variables[t2m["var"]]
        i = i + 1
        nx, ny = check_input_to_soda_dimensions(nx, ny, t2m_fh.variables[t2m["var"]].shape[2],
                                                t2m_fh.variables[t2m["var"]].shape[1])
    rh2m_var = None
    if rh2m["file"] is not None:
        rh2m_fh = Dataset(rh2m["file"], "r")
        print(rh2m["var"], rh2m_fh.variables[rh2m["var"]].shape)
        rh2m_var = rh2m_fh.variables[rh2m["var"]]
        i = i + 1
        nx, ny = check_input_to_soda_dimensions(nx, ny, rh2m_fh.variables[rh2m["var"]].shape[2],
                                                rh2m_fh.variables[rh2m["var"]].shape[1])
    sd_var = None
    if sd["file"] is not None:
        sd_fh = Dataset(sd["file"], "r")
        print(sd["var"], sd_fh.variables[sd["var"]].shape)
        sd_var = sd_fh.variables[sd["var"]]
        i = i + 1
        nx, ny = check_input_to_soda_dimensions(nx, ny, sd_fh.variables[sd["var"]].shape[2],
                                                sd_fh.variables[sd["var"]].shape[1])

    if i == 0:
        raise Exception("You must specify at least one file to read from!")

    out = open("OBSERVATIONS_" + str(yy) + str(mm) + str(dd) + "H" + str(hh)+".DAT", "w")
    for j in range(0, ny):
        for i in range(0, nx):
            # out.write(str(array1[0,j,i])+" "+str(array2[0,j,i])+" 999 999 "+str(array3[0,j,i])+"\n")
            undef = "999"
            if t2m_var is not None:
                if np.ma.is_masked(t2m_var[0, j, i]):
                    t2m_val = undef
                else:
                    t2m_val = str(t2m_var[0, j, i])
            else:
                t2m_val = undef
            if rh2m_var is not None:
                if np.ma.is_masked(rh2m_var[0, j, i]):
                    rh2m_val = undef
                else:
                    rh2m_val = str(rh2m_var[0, j, i])
            else:
                rh2m_val = undef
            if sd_var is not None:
                if np.ma.is_masked(sd_var[0, j, i]):
                    sd_val = undef
                else:
                    sd_val = str(sd_var[0, j, i])
            else:
                sd_val = undef
            out.write(t2m_val + " " + rh2m_val + " " + sd_val + "\n")


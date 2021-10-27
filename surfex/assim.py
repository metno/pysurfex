import numpy as np
try:
    import gridpp
except ImportError:
    gridpp = None


def horizontal_oi(geo, background, observations, gelevs, hlength=10000.,
                  vlength=10000., wlength=0.5, elev_gradient=0, structure_function="Barnes",
                  max_locations=50, epsilon=0.5, minvalue=None, maxvalue=None,
                  interpol="bilinear", only_diff=False, debug=False):

    if gridpp is None:
        raise Exception("You need gridpp to perform OI")

    if debug:
        print(gridpp.__file__)
        print(gridpp.__version__)
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

    bgrid = gridpp.Grid(glats, glons, gelevs)
    points = gridpp.Points(lats, lons, elevs)
    if interpol == "bilinear":
        pbackground = gridpp.bilinear(bgrid, points, background)
    elif interpol == "nearest":
        pbackground = gridpp.nearest(bgrid, points, background)
    else:
        raise NotImplementedError

    # Remove undefined backgrounds    
    if any(np.isnan(pbackground)):
       print("Found undefined backgrounds. Remove them")
       lons2 = []
       lats2 = []
       elevs2 = []
       values2 = []
       for p in range(0, len(lons)):
          if np.isnan(pbackground[p]):
             print("Undefined background in lon=", lons[p], " lat=", lats[p]," value=", values[p])
          else:
             lons2.append(lons[p])
             lats2.append(lats[p])
             elevs2.append(elevs[p])
             values2.append(values[p])
       values = values2
       points = gridpp.Points(lats2, lons2, elevs2)
       if interpol == "bilinear":
           pbackground = gridpp.bilinear(bgrid, points, background)
       elif interpol == "nearest":
           pbackground = gridpp.nearest(bgrid, points, background)
       else:
           raise NotImplementedError

    variance_ratios = np.full(points.size(), epsilon)

    if structure_function == "Barnes":
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
    if only_diff:
        field[field == background] = np.nan
    return np.transpose(field)

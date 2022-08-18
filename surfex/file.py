"""Surfex file related stuff."""
import os
import shutil
import logging
import re
import abc
from datetime import timedelta, datetime
# from netCDF4 import Dataset, num2date, chartostring
import netCDF4
import pyproj
import numpy as np
import surfex


class SurfexIO(object):
    """Abstract Surfex IO class.

    Used for internal surfex file format files.

    """

    def __init__(self, filename, geo, extension):
        """Construct a surfex IO object.

        Args:
            filename (str): Name of file
            geo (surfex.Geometry): Geometry
            extension (str): File extension
        """
        self.filename = filename
        self.geo = geo
        self.extension = extension

    @abc.abstractmethod
    def field(self, var, validtime=None):
        """Abstract method to read field.

        Args:
            var (SurfexFileVariable): Variable in surfex file.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.

        Raises:
            NotImplementedError: Must be implemented by child class.

        """
        raise NotImplementedError("This method is not implemented for this class!")

    @abc.abstractmethod
    def points(self, var, geo_out, validtime=None, interpolation="bilinear"):
        """Abstract method to read points.

        Args:
            var (SurfexFileVariable): Variable in surfex file.
            geo_out (surfex.Geometry): Surfex geometry to interpolate to.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.
            interpolation (str, optional): Interpolation type.. Defaults to "nearest".

        Raises:
            NotImplementedError: Must be implemented by child class.

        """
        raise NotImplementedError("This method is not implemented for this class!")

    @staticmethod
    def interpolate_field(field, geo_in, geo_out, interpolation="bilinear"):
        """Interpolate a field to points.

        Args:
            field (np.darray): Field to interpolate
            geo_in (surfex.Geo): Input geometry
            geo_out (surfex.Geo): Target geometry
            interpolation (str, optional): Interpolation type. Defaults to "bilinear".

        Returns:
            tuple: (np.array, surfex.Interpolator)

        """
        interpolator = surfex.interpolation.Interpolation(interpolation, geo_in, geo_out)
        field = interpolator.interpolate(field)
        return field, interpolator


class SurfexSurfIO(object):
    """Surfex surf file.

    Class for surfex restart files.

    """

    def __init__(self, surfexfile, csurf_filetype, input_file=None, symlink=True,
                 archive_file=None):
        """Construct the surfex surf file.

        Args:
            surfexfile (str): Internal file name of surfex file.
            csurf_filetype (str): File type
            input_file (str, optional): File name to be used as surfexfile. Defaults to None.
            symlink (bool, optional): Symlink input_file to surfexfile. Defaults to True.
            archive_file (str, optional): Location to store the result. Defaults to None.

        """
        self.filename = surfexfile.filename
        self.csurf_filetype = csurf_filetype
        self.need_pgd = False
        logging.debug("Input file: %s", input_file)
        self.input_file = input_file
        self.symlink = symlink
        if self.input_file is not None:
            if self.symlink:
                self.symlink_input()
            else:
                self.copy_input()
        self.archive_file = archive_file

    def symlink_input(self):
        """Symlink the input file."""
        if self.input_file is not None:
            f_out = os.getcwd() + "/" + self.filename
            logging.debug("input_file: %s file_out: %s", self.input_file, f_out)
            surfex.read.remove_existing_file(self.input_file, f_out)
            if os.path.abspath(self.input_file) != f_out:
                logging.info("Symlink " + self.input_file + " -> " + f_out)
                os.symlink(self.input_file, f_out)

    def copy_input(self):
        """Copy the input file."""
        if self.input_file is not None:
            f_out = os.getcwd() + "/" + self.filename
            surfex.read.remove_existing_file(self.input_file, f_out)
            if os.path.abspath(self.input_file) != f_out:
                logging.info("Copy " + self.input_file + " -> " + f_out)
                shutil.copy2(self.input_file, f_out)

    def archive_output_file(self):
        """Archive the output file."""
        if self.archive_file is not None:
            dirname = os.path.dirname(os.path.abspath(self.archive_file))
            os.makedirs(dirname, exist_ok=True)
            f_in = os.getcwd() + "/" + self.filename
            if os.path.abspath(self.archive_file) != f_in:
                logging.info("Move %s to %s", f_in, self.archive_file)
                if os.path.islink(self.archive_file):
                    # print("is link")
                    os.unlink(self.archive_file)
                if os.path.isfile(self.archive_file):
                    # print("is file")
                    os.remove(self.archive_file)
                if os.path.isdir(self.archive_file):
                    shutil.rmtree(self.archive_file)
                shutil.move(f_in, self.archive_file)


class PGDFile(SurfexSurfIO):
    """PGD file."""

    def __init__(self, csurf_filetype, cpgdfile, input_file=None, symlink=True,
                 archive_file=None, lfagmap=False, masterodb=False):
        """Construct PGD file object.

        Args:
            csurf_filetype (str): File type
            cpgdfile (str): Name of the PGD file
            input_file (str, optional): Input file. Defaults to None.
            symlink (bool, optional): Symlink input_file to surfexfile. Defaults to True.
            archive_file (str, optional): Location to store the result. Defaults to None.
            lfagmap (bool, optional): File use LFAGMAP. Defaults to False.
            masterodb (bool, optional): File produced by masterodb. Defaults to False.

        """
        logging.debug("PGDFile")
        logging.debug("%s %s %s", cpgdfile, csurf_filetype, masterodb)

        cpgdfile = get_surfex_io_object(cpgdfile, filetype="surf",
                                        fileformat=csurf_filetype,
                                        lfagmap=lfagmap, masterodb=masterodb)

        SurfexSurfIO.__init__(self, cpgdfile, csurf_filetype, input_file=input_file,
                              archive_file=archive_file, symlink=symlink)
        self.need_pgd = False


class PREPFile(SurfexSurfIO):
    """PREP file."""

    def __init__(self, csurf_filetype, cprepfile, input_file=None, symlink=True,
                 archive_file=None, lfagmap=False, masterodb=False):
        """Construct PREP file object.

        Args:
            csurf_filetype (str): File type
            cprepfile (str): Name of the PREP file
            input_file (str, optional): Input file. Defaults to None.
            symlink (bool, optional): Symlink input_file to surfexfile. Defaults to True.
            archive_file (str, optional): Location to store the result. Defaults to None.
            lfagmap (bool, optional): File use LFAGMAP. Defaults to False.
            masterodb (bool, optional): File produced by masterodb. Defaults to False.

        """
        logging.debug("PREPFile %s", input_file)
        cprepfile = get_surfex_io_object(cprepfile, filetype="surf",
                                         fileformat=csurf_filetype,
                                         lfagmap=lfagmap, masterodb=masterodb,)

        SurfexSurfIO.__init__(self, cprepfile, csurf_filetype, input_file=input_file,
                              archive_file=archive_file, symlink=symlink)
        self.need_pgd = True


class SURFFile(SurfexSurfIO):
    """SURFOUT file."""

    def __init__(self, csurf_filetype, csurffile, archive_file=None, input_file=None,
                 lfagmap=False, masterodb=False):
        """Construct SURFOUT file object.

        Result of a surfex binary.

        Args:
            csurf_filetype (str): File type
            csurffile (str): Name of the PREP file
            input_file (str, optional): Input file. Defaults to None.
            symlink (bool, optional): Symlink input_file to surfexfile. Defaults to True.
            archive_file (str, optional): Location to store the result. Defaults to None.
            lfagmap (bool, optional): File use LFAGMAP. Defaults to False.
            masterodb (bool, optional): File produced by masterodb. Defaults to False.

        """
        logging.debug("SURFFile")
        csurffile = get_surfex_io_object(csurffile, filetype="surf",
                                         fileformat=csurf_filetype,
                                         lfagmap=lfagmap, masterodb=masterodb)

        SurfexSurfIO.__init__(self, csurffile, csurf_filetype, input_file=input_file,
                              archive_file=archive_file)
        self.need_pgd = True


class SurfexFileVariable(object):
    """Surfex Variable."""

    def __init__(self, varname, validtime=None, patches=1, layers=1, basetime=None,
                 interval=None, datatype="float"):
        """Construct a surfex file variable.

        Args:
            varname (str): Variable name.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.
            patches (int, optional): Number of patches. Defaults to 1.
            layers (int, optional): Number of layers. Defaults to 1.
            basetime (datetime.datetime, optional): _description_. Defaults to None.
            interval (int, optional): Interval. Defaults to None.
            datatype (str, optional): Data type for variable. Defaults to "float".

        """
        self.varname = varname
        self.type = datatype
        self.patches = patches
        self.layers = layers
        self.basetime = basetime
        self.interval = interval
        self.validtime = validtime

    def print_var(self):
        """Print variable information."""
        return self.varname


def get_surfex_io_object(fname, filetype="surf", fileformat=None, geo=None, lfagmap=False,
                         masterodb=False):
    """Get the surfexIO object.

    Args:
        fname (str): File name.
        filetype (str, optional): File type. Defaults to "surf".
        fileformat (str, optional): File format. Defaults to None.
        geo (surfex.Geo, optional): Geometry. Defaults to None.
        lfagmap (bool, optional): File use LFAGMAP. Defaults to False.
        masterodb (bool, optional): File produced by masterodb. Defaults to False.

    Returns:
        SurfexIO: SurfexIO object.

    """
    logging.debug("get_surfex_io_object")
    if filetype is not None:
        if filetype.lower() != "surf" and filetype.lower() != "ts" \
                and filetype.lower() != "forcing":
            raise Exception("Invalid filetype: " + filetype + " Allowed: surf/ts/forcing")

    if fileformat is None:
        fileformat, filetype = guess_file_format(fname, filetype)

    if fileformat.lower() == "ascii":
        if filetype.lower() == "surf":
            obj = AsciiSurfexFile(fname, geo=geo)
        elif filetype.lower() == "forcing":
            raise NotImplementedError("Not implemented yet")
        else:
            raise NotImplementedError

    elif fileformat.lower() == "nc":
        if filetype.lower() == "surf":
            obj = NCSurfexFile(fname, geo=geo)
        else:
            raise NotImplementedError
    elif fileformat.lower() == "netcdf":
        if filetype.lower() == "ts":
            if geo is None:
                raise Exception("Format NetCDF needs a geometry")
            obj = NetCDFSurfexFile(fname, geo)
        elif filetype.lower() == "forcing":
            if geo is None:
                raise Exception("Format NetCDF needs a geometry for reading forcing files")
            obj = ForcingFileNetCDF(fname, geo)
        else:
            raise NotImplementedError
    elif fileformat.lower() == "texte":
        if geo is None:
            raise Exception("Format TEXTE needs a geometry")
        obj = TexteSurfexFile(fname, geo)
    elif fileformat.lower() == "fa":
        if filetype.lower() == "surf":
            obj = FaSurfexFile(fname, geo=geo, lfagmap=lfagmap, masterodb=masterodb)
        else:
            raise NotImplementedError
    # elif fileformat.lower() == "sfx":
    #    if filetype.lower() == "surf":
    #        obj = FaSurfexFile(fname, geo=geo, lfagmap=True, masterodb=masterodb)
    #    else:
    #        raise NotImplementedError
    else:
        raise NotImplementedError("Format not implemented: " + fileformat)

    logging.debug("Returning object: %s", obj)
    return obj


def guess_file_format(fname, ftype=None):
    """Guess the file format.

    Args:
        fname (str): Filename
        ftype (str, optional): Filetype if known/wished. Defaults to None.

    Raises:
        Exception: _description_
        Exception: _description_

    Returns:
        tuple: fileformat, filetype

    """
    f_n = str(os.path.basename(fname))
    ext = str(f_n.rsplit(".", maxsplit=1)[-1])
    if ftype is None:
        logging.info("Trying to guess the file type")
        needles = ["PREP", "PGD"]
        for needle in needles:
            if re.search(needle, f_n):
                ftype = "surf"

        if ext.endswith("nc"):
            ftype = "surf"
        needles = [".*PROGNOSTIC.*", ".*DIAGNOSTICS.*", "SURF_ATM.*"]
        for needle in needles:
            if re.search(needle, f_n):
                ftype = "ts"
        for needle in needles:
            if ext.endswith("TXT"):
                ftype = "ts"
        needles = ["Forc_.*", "FORCING.*"]
        for needle in needles:
            if re.search(needle, f_n):
                ftype = "forcing"

        if re.search("SURFOUT.*", f_n) and ftype is None:
            raise Exception("Can not-auto decide filetype for files called SURFOUT.*.txt. "
                            + "Specify either surf or ts")

    fileformat = None
    logging.info("Trying to guess the file format from extension: %s", ext)
    if ext.endswith("txt"):
        fileformat = "ascii"
    if ext.endswith("TXT"):
        fileformat = "texte"
    if ext.endswith("nc"):
        if ftype == "surf":
            fileformat = "nc"
        else:
            fileformat = "netcdf"
    if ext.endswith("fa"):
        fileformat = "fa"
    if ext.endswith("sfx"):
        fileformat = "fa"

    if ftype is None or fileformat is None:
        raise Exception("Filetype and/or format not set: " + str(ftype) + " & " + str(fileformat))
    logging.info("Filetype: %s format: %s", ftype, fileformat)

    return fileformat, ftype


class AsciiSurfexFile(SurfexIO):
    """Input from an ASCII surfex file (.txt)."""

    def __init__(self, filename, geo=None):
        """Construct the ASCII object.

        Args:
            filename (str): Filename

        """
        suffix = SurfFileTypeExtension("ASCII").suffix
        self.filename = filename

        if geo is None:
            geo = self.get_geo()

        if not filename.endswith(suffix):
            filename = filename + suffix

        SurfexIO.__init__(self, filename, geo, "txt")

    def get_geo(self):
        """Get the geometry object.

        Raises:
            FileNotFoundError: _description_
            Exception: _description_
            NotImplementedError: _description_

        Returns:
            surfex.Geometry: Surfex geometry
        """
        if not os.path.isfile(self.filename):
            raise FileNotFoundError("File does not exist: " + str(self.filename))

        grid = self.read("GRID_TYPE", "FULL", "string")
        if len(grid) == 0:
            raise Exception("No grid found")

        if grid[0] == "IGN":
            domain = {
                "nam_ign": {
                    "clambert": self.read("LAMBERT", "&FULL", "integer")[0],
                    "xx": self.read("XX", "&FULL", "float"),
                    "xy": self.read("XY", "&FULL", "float"),
                    "xdx": self.read("XDX", "&FULL", "float"),
                    "xdy": self.read("XY", "&FULL", "float")
                }
            }
            return surfex.geo.IGN(domain)

        elif grid[0] == "LONLATVAL":
            domain = {
                "nam_lonlatval": {
                    "xx": self.read("XX", "&FULL", "float"),
                    "xy": self.read("XY", "&FULL", "float"),
                    "xdx": self.read("DX", "&FULL", "float"),
                    "xdy": self.read("DY", "&FULL", "float")
                }
            }
            return surfex.geo.LonLatVal(domain)

        elif grid[0] == "LONLAT REG":
            domain = {
                "nam_lonlatval_reg": {
                    "lonmin": self.read("LONMIN", "&FULL", "float")[0],
                    "latmin": self.read("LATMIN", "&FULL", "float")[0],
                    "lonmax": self.read("LONMAX", "&FULL", "float")[0],
                    "latmax": self.read("LATMAX", "&FULL", "float")[0],
                    "nlon": self.read("NLON", "&FULL", "integer")[0],
                    "nlat": self.read("NLAT", "&FULL", "integer")[0],
                    "reg_lon": self.read("REG_LON", "&FULL", "float")[0],
                    "reg_lat": self.read("REG_LAT", "&FULL", "float")[0]
                }
            }
            return surfex.geo.LonLatReg(domain)

        elif grid[0] == "CONF PROJ":
            lon0 = self.read("LON0", "&FULL", "float")[0]
            lat0 = self.read("LAT0", "&FULL", "float")[0]
            n_x = self.read("IMAX", "&FULL", "integer")[0]
            n_y = self.read("JMAX", "&FULL", "integer")[0]
            d_x = self.read("XX", "&FULL", "float")[0]
            d_y = self.read("XX", "&FULL", "float")[0]

            ll_lon = self.read("LONORI", "&FULL", "float")[0]
            ll_lat = self.read("LATORI", "&FULL", "float")[0]

            earth = 6.37122e+6
            proj_string = f"+proj=lcc +lat_0={str(lat0)} +lon_0={str(lon0)} +lat_1={str(lat0)} " \
                          f"+lat_2={str(lat0)} +units=m +no_defs +R={str(earth)}"

            proj = pyproj.CRS.from_string(proj_string)
            wgs84 = pyproj.CRS.from_string("EPSG:4326")
            x_0, y_0 = pyproj.Transformer.from_crs(wgs84, proj,
                                                   always_xy=True).transform(ll_lon, ll_lat)
            x_c = x_0 + 0.5 * (n_x - 1) * d_x
            y_c = y_0 + 0.5 * (n_y - 1) * d_y
            lonc, latc = pyproj.Transformer.from_crs(proj, wgs84,
                                                     always_xy=True).transform(x_c, y_c)

            domain = {
                "nam_conf_proj": {
                    "xlon0": lon0,
                    "xlat0": lat0
                },
                "nam_conf_proj_grid": {
                    "xloncen": lonc,
                    "xlatcen": latc,
                    "nimax": n_x,
                    "njmax": n_y,
                    "xdx": d_x,
                    "xdy": d_x,
                    "ilone": 0,
                    "ilate": 0
                }
            }
            # print(domain)
            return surfex.geo.ConfProj(domain)
        else:
            raise NotImplementedError("Grid " + str(grid[0]) + " not implemented!")

    def read(self, read_par, read_tile, datatype):
        """Read the file.

        Args:
            read_par (_type_): _description_
            read_tile (_type_): _description_
            datatype (_type_): _description_

        Raises:
            NotImplementedError: _description_
            Exception: _description_

        Returns:
            _type_: _description_

        """
        # Add & if not given
        if read_tile.find('&') < 0:
            read_tile = '&' + read_tile
        # print read_tile,read_par
        file = open(self.filename, mode="r", encoding="utf-8")
        read_desc = False
        read_value = False
        values = []
        for line in file:
            # for line in file.read().splitlines():

            # print "T:"+line
            words = line.split()
            if len(words) > 0:
                # print "Line:",read_desc,read_value,":",line
                if read_value and not read_desc:
                    if words[0].find('&') < 0:
                        # print "Value:", line
                        try:
                            if datatype.lower() == "float":
                                for word in words:
                                    val = float(word.replace("D", "E"))
                                    if val == 1e+20:
                                        val = np.nan
                                    values.append(val)
                            elif datatype.lower() == "string":
                                str_words = []
                                for word in words:
                                    str_words.append(word)
                                values.append(" ".join(str_words))
                            elif datatype.lower() == "integer" or datatype.lower() == "int":
                                for word in words:
                                    values.append(int(word))
                            else:
                                raise NotImplementedError("Type not implemented " + str(datatype))
                        except ValueError:
                            raise Exception(f"Conversion from {str(words)} to {str(datatype)} "
                                            "does not work! Try a different datatype!") \
                                from ValueError

                if read_desc:
                    # print "Description: ", words[0]
                    read_desc = False
                    read_value = True

                if words[0].find('&') >= 0:
                    tile = words[0]
                    par = words[1]

                    read_value = False
                    if tile.strip().lower() == read_tile.lower() \
                            and par.lower() == read_par.lower():
                        read_desc = True
                        read_value = False
                        logging.info("Found: %s %s", str(tile), str(par))

            # Description could be empty
            else:
                if read_desc:
                    # print "Description: ", words[0]
                    read_desc = False
                    read_value = True

        if len(values) == 0:
            logging.info("No values found!")

        values = np.asarray(values)
        return values

    def field(self, var, validtime=None):
        """Read a field in file.

        Args:
            var (SurfexFileVariable): Variable in surfex file.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.

        Returns:
            np.darray: Field, surfex.Geo in read file

        """
        # TODO
        read_par = var.varname
        read_tile = "&FULL"
        datatype = "float"
        field = self.read(read_par, read_tile, datatype)
        geo_in = self.get_geo()
        field = np.reshape(field, [geo_in.nlons, geo_in.nlats], order="F")
        # field = np.transpose(field)
        return field, geo_in

    def points(self, var, geo_out, validtime=None, interpolation="nearest"):
        """Read points.

        Args:
            var (SurfexFileVariable): Variable in surfex file.
            geo_out (surfex.Geo): Surfex geometry for points.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.
            interpolation (str, optional): Interpolation type. Defaults to "nearest".

        Returns:
            np.darray: Interpolated points

        """
        field, geo_in = self.field(var, validtime=validtime)

        points, interpolator = SurfexIO.interpolate_field(field, geo_in, geo_out,
                                                          interpolation=interpolation)
        return points, interpolator


class NCSurfexFile(SurfexIO):
    """NetCDF surfex file (restart type)."""

    def __init__(self, filename, geo=None):
        """Construct NC file object.

        Args:
            filename (str): File name
            geo (surfex.Geo, optional): Surfex geometry. Defaults to None.

        """
        suffix = SurfFileTypeExtension("NC").suffix
        if not filename.endswith(suffix):
            filename = filename + suffix

        self.filename = filename
        if geo is None:
            geo = self.get_geo()

        SurfexIO.__init__(self, filename, geo, "nc")

    def get_geo(self):
        """Get geometry in file.

        Returns:
            surfex.Geometry: Surfex geometry in file

        """
        try:
            f_h = netCDF4.Dataset(self.filename, "r")
        except OSError:
            logging.debug("Geo not open %s as a netCDF file", self.filename)
            return None
        cgrid = str(netCDF4.chartostring(f_h["GRID_TYPE"][:])).strip()
        # print(":" + cgrid + ":")
        if cgrid == "CONF PROJ":
            lon0 = f_h["LON0"][:]
            lat0 = f_h["LAT0"][:]
            n_x = int(f_h["IMAX"][0])
            n_y = int(f_h["JMAX"][0])
            d_x = float(f_h["DX"][0][0])
            d_y = float(f_h["DY"][0][0])

            ll_lon = f_h["LONORI"][:]
            ll_lat = f_h["LATORI"][:]
            earth = 6.37122e+6
            proj_string = f"+proj=lcc +lat_0={str(lat0)} +lon_0={str(lon0)} +lat_1={str(lat0)} " \
                          f"+lat_2={str(lat0)} +units=m +no_defs +R={str(earth)}"

            proj = pyproj.CRS.from_string(proj_string)
            wgs84 = pyproj.CRS.from_string("EPSG:4326")
            x_0, y_0 = pyproj.Transformer.from_crs(wgs84, proj,
                                                   always_xy=True).transform(ll_lon, ll_lat)
            x_c = x_0 + 0.5 * (n_x + 1) * d_x
            y_c = y_0 + 0.5 * (n_y + 1) * d_y
            lonc, latc = pyproj.Transformer.from_crs(proj, wgs84,
                                                     always_xy=True).transform(x_c, y_c)

            domain = {
                "nam_conf_proj": {
                    "xlon0": lon0,
                    "xlat0": lat0
                },
                "nam_conf_proj_grid": {
                    "xloncen": lonc,
                    "xlatcen": latc,
                    "nimax": n_x,
                    "njmax": n_y,
                    "xdx": d_x,
                    "xdy": d_y,
                    "ilone": 0,
                    "ilate": 0
                }
            }
            return surfex.geo.ConfProj(domain)
        elif cgrid == "IGN":
            domain = {
                "nam_ign": {
                    "clambert": f_h["CLAMBERT"][0],
                    "xx": f_h["XX"][:],
                    "xy": f_h["XY"][:],
                    "xdx": f_h["DX"][:],
                    "xdy": f_h["DY"][:]
                }
            }
            return surfex.geo.IGN(domain)

        elif cgrid == "LONLATVAL":
            domain = {
                "nam_lonlatval": {
                    "xx": f_h["XX"][:],
                    "xy": f_h["XY"][:],
                    "xdx": f_h["DX"][:],
                    "xdy": f_h["DY"][:]
                }
            }
            return surfex.geo.LonLatVal(domain)

        elif cgrid == "LONLAT REG":
            domain = {
                "nam_lonlatval_reg": {
                    "lonmin": f_h["LONMIN"][0],
                    "latmin": f_h["LATMIN"][0],
                    "lonmax": f_h["LONMAX"][0],
                    "latmax": f_h["LATMAX"][0],
                    "nlon": f_h["NLON"][0],
                    "nlat": f_h["NLAT"][0],
                    "reg_lon": f_h["REG_LON"][0],
                    "reg_lat": f_h["REG_LAT"][0],
                }
            }
            return surfex.geo.LonLatReg(domain)
        else:
            raise NotImplementedError(cgrid + " is not implemented")

    def field(self, var, validtime=None):
        """Read field.

        Args:
            var (SurfexFileVariable): Variable in surfex file.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.
            validtime (_type_, optional): _description_. Defaults to None.

        Returns:
            np.darray: Field, surfex.Geo in read file

        """
        f_h = netCDF4.Dataset(self.filename, "r")
        if validtime is None:
            pass
        else:
            if hasattr(f_h, "DTCUR-YEAR"):
                year = f_h["DTCUR-YEAR"][0]
                month = f_h["DTCUR-MONTH"][0]
                day = f_h["DTCUR-DAY"][0]
                time = int(f_h["DTCUR-TIME"][0])
                hour = int(time / 3600)

                # TODO minutes
                time_in_file = datetime(year=year, month=month, day=day, hour=hour)
                if validtime != time_in_file:
                    logging.error("%s %s", time_in_file, validtime)
                    raise Exception("Mismatch in times in file and the wanted time")
            else:
                print("Not checking time")

        geo_in = self.get_geo()
        field = f_h[var.varname][:]
        # print(fh[var.varname])
        fillvalue = f_h[var.varname].getncattr("_FillValue")
        # if np.any(np.isnan(field)):
        logging.info("Set %s to nan", fillvalue)
        field = field.filled(np.nan)
        # print(field)

        # Reshape to fortran 2D style
        # field = np.reshape(field, [geo_in.nlons, geo_in.nlats], order="F")
        # Does not work wih interpolator
        field = np.transpose(field)
        return field, geo_in

    def points(self, var, geo_out, validtime=None, interpolation="nearest"):
        """Read points.

        Args:
            var (SurfexFileVariable): Variable in surfex file.
            geo_out (surfex.Geo): Surfex geometry for points.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.
            interpolation (str, optional): Interpolation type. Defaults to "nearest".

        Returns:
            np.darray: Interpolated points

        """
        field, geo_in = self.field(var, validtime=validtime)
        points, interpolator = SurfexIO.interpolate_field(field, geo_in, geo_out,
                                                          interpolation=interpolation)
        return points, interpolator


class FaSurfexFile(SurfexIO):
    """FA surfex file."""

    def __init__(self, filename, geo=None, masterodb=False, lfagmap=True):
        """Construct the surfex FA file.

        Args:
            filename (str): File name.
            geo (surfex.Geo, optional): Surfex geometry. Defaults to None.
            masterodb (bool, optional): _description_. Defaults to False.
            lfagmap (bool, optional): _description_. Defaults to True.

        """
        extension = SurfFileTypeExtension("FA", masterodb=masterodb, lfagmap=lfagmap)
        extension_suffix = extension.suffix

        # Surfex binaries use .fa suffix for all input files no matter if they are lfagmap files

        if not filename.endswith(extension_suffix):
            filename = filename + extension_suffix

        # if geo is None:
        #    geo = self.get_geo()

        SurfexIO.__init__(self, filename, geo, extension)
        self.lfagmap = lfagmap

    # def get_geo(self):
    #    # TODO read geo from SURFEX FA file
    #    # geo = None
    #    return None

    def field(self, var, validtime=None):
        """Read field from FA file.

        Args:
            var (SurfexFileVariable): Variable in surfex file.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.
            validtime (_type_, optional): _description_. Defaults to None.

        Returns:
            np.darray: Field, surfex.Geo in read file

        """
        file_handler = surfex.fa.Fa(self.filename)
        if validtime is None:
            pass
        elif not isinstance(validtime, datetime):
            raise Exception("validime must be a datetime object")

        geo_in = self.geo
        field = file_handler.field(var.varname, validtime)

        # Reshape to fortran 2D style
        field = np.reshape(field, [geo_in.nlons, geo_in.nlats], order="F")
        field = np.transpose(field)
        return field, geo_in

    def points(self, var, geo_out, validtime=None, interpolation="nearest"):
        """Read points.

        Args:
            var (SurfexFileVariable): Variable in surfex file.
            geo_out (surfex.Geo): Surfex geometry for points.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.
            interpolation (str, optional): Interpolation type. Defaults to "nearest".

        Returns:
            tuple: Interpolated points, surfex.Interpolator

        """
        field, geo_in = self.field(var, validtime=validtime)

        points, interpolator = SurfexIO.interpolate_field(field, geo_in, geo_out,
                                                          interpolation=interpolation)
        return points, interpolator


class SurfFileTypeExtension(object):
    """Extension of a surfex file."""

    def __init__(self, csurf_filetype, masterodb=False, lfagmap=True):
        """Construct an extension object.

        Args:
            csurf_filetype (str): Surfex file type.
            masterodb (bool, optional): File created with masterodb. Defaults to False.
            lfagmap (bool, optional): File created with lfagmap=True. Defaults to True.

        """
        suffix = None
        extension = ""
        if csurf_filetype.lower() == "nc":
            extension = "nc"
        elif csurf_filetype.lower() == "ascii":
            extension = "txt"
        elif csurf_filetype.lower() == "fa":
            if lfagmap:
                extension = "sfx"
                if not masterodb:
                    suffix = ".fa"
            else:
                extension = "fa"

        if suffix is None:
            suffix = "." + extension

        self.type = extension
        self.suffix = suffix


class NetCDFSurfexFile(SurfexIO):
    """Reading surfex NetCDF time series output."""

    def __init__(self, filename, geo):
        """Construct a NetCDF surfex time series file.

        Args:
            filename (str): Name of file.
            geo (surfex.Geometry): Surfex geometry for time series.

        """
        self.file_handler = netCDF4.Dataset(filename, "r")
        SurfexIO.__init__(self, filename, geo, "nc")

    def read(self, var, times):
        """Read a field, return a 2D array.

        Args:
            var (SurfexFileVariable): Variable in surfex file.
            times (list): List of datetime.datetime objects

        Returns:
            (tuple): (np.array. surfex.Geometry)

        """
        layers = var.layers
        patches = var.patches

        if not isinstance(times, (list, tuple)):
            raise Exception("times must be list or tuple")
        if not isinstance(layers, (list, tuple)):
            raise Exception("patches must be list or tuple")
        if not isinstance(patches, (list, tuple)):
            raise Exception("patches must be list or tuple")

        values = np.array([])
        times_read = []
        ndims = 0
        dim_indices = []
        mapping = {}
        npatch = 1

        if self.file_handler.variables[var].shape[0] > 0:
            # p rint self.fh.variables[var]
            for dim in self.file_handler.variables[var].dimensions:
                # print dim,ndims
                dimlen = self.file_handler.variables[var].shape[ndims]
                this_dim = []

                if dim == "time":
                    mapping[0] = ndims
                    times_for_var = self.file_handler.variables['time']
                    units = times_for_var.units
                    try:
                        t_cal = times_for_var.calendar
                    except AttributeError:  # Attribute doesn't exist
                        t_cal = u"gregorian"  # or standard

                    indices = list(range(0, dimlen))
                    times_for_var = netCDF4.num2date(times_for_var[indices], units=units,
                                                     calendar=t_cal)
                    if len(times) > 0:
                        for t_to_find, t_to_find_val in enumerate(times):
                            for tstep in range(0, len(indices)):
                                if times_for_var[tstep] == t_to_find_val:
                                    # print t, t_to_find, times_for_var[t], times[t_to_find]
                                    this_dim.append(tstep)
                                    times_read.append(times[t_to_find])
                    else:
                        times_read = times_for_var
                        this_dim = list(range(0, dimlen))

                elif dim == "Number_of_points":
                    mapping[1] = ndims
                    this_dim = list(range(0, dimlen))
                elif dim == "xx":
                    mapping[1] = ndims
                    this_dim = list(range(0, dimlen))
                elif dim == "yy":
                    mapping[2] = ndims
                    this_dim = list(range(0, dimlen))
                elif dim == "Number_of_Tile":
                    mapping[3] = ndims
                    npatch = dimlen
                    if len(patches) > 0:
                        npatch = len(patches)
                        this_dim = patches
                    else:
                        this_dim = list(range(0, dimlen))
                elif dim == "Number_of_Layers":
                    mapping[4] = ndims
                    npatch = dimlen
                    if len(layers) > 0:
                        # nlayers = len(layers)
                        this_dim = layers
                    else:
                        this_dim = list(range(0, dimlen))
                elif dim == "lon":
                    mapping[1] = ndims
                    this_dim = list(range(0, dimlen))
                elif dim == "lat":
                    mapping[2] = ndims
                    this_dim = list(range(0, dimlen))
                else:
                    raise NotImplementedError("Not implemented for: " + dim)

                dim_indices.append(this_dim)
                ndims = ndims + 1

            field = self.file_handler.variables[var][dim_indices]

            # Add extra dimensions
            # print mapping
            i = 0
            reverse_mapping = []
            for dim in range(0, 5):
                if dim not in mapping:
                    # print "Adding dimension " + str(d)
                    field = np.expand_dims(field, len(dim_indices) + i)
                    reverse_mapping.append(len(dim_indices) + i)
                    i = i + 1
                else:
                    reverse_mapping.append(mapping[dim])

            # Transpose to 5D array
            # print "Transpose to 5D array"
            # print reverse_mapping
            field = np.transpose(field, reverse_mapping)
            npoints = self.geo.npoints * npatch

            # Create 2-D array with times and points as for the other formats
            for tstep in range(0, field.shape[0]):
                field2d = np.empty(npoints)
                i = 0
                # print t,npatch,npoints,field.shape,field2d.shape
                for patch, in range(0, npatch):
                    if self.geo.mask is not None:
                        iii = 0
                        j = 0
                        # For some reason the NetCDF files have one less dimension in all
                        # dimensions than the PGD dimension and mask needs x first.
                        for xxx in range(-1, field.shape[1] + 1):
                            for yyy in range(-1, field.shape[2] + 1):
                                if xxx in range(0, field.shape[1]) and \
                                        yyy in range(0, field.shape[2]):
                                    # print i, ii,j, t, x, y, p, self.geo.mask[j]
                                    if self.geo.mask[j] == iii:
                                        field2d[i] = np.nan
                                        if field[tstep, xxx, yyy, patch] != np.nan:
                                            field2d[i] = field[tstep, xxx, yyy, patch]
                                        i = i + 1
                                        j = j + 1
                                iii = iii + 1
                    else:
                        for yyy in range(0, field.shape[2]):
                            for xxx in range(0, field.shape[1]):
                                field2d[i] = np.nan
                                if field[tstep, xxx, yyy, patch] != np.nan:
                                    field2d[i] = field[tstep, xxx, yyy, patch]
                                i = i + 1
                if i != npoints:
                    raise Exception("Mismatch in points " + str(i) + "!=" + str(npoints))

                values = np.append(values, field2d)
            # Re-shape to proper format
            values = np.reshape(values, [field.shape[0], npoints])

        else:
            raise Exception("Variable " + var + " not found!")

        return values, self.geo

    def field(self, var, validtime=None):
        """Read field.

        Args:
            var (SurfexFileVariable): Variable in surfex file.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.

        Returns:
            tuple: (np.array, surfex.Geometry)

        """
        if validtime is None:
            validtime = []
        else:
            validtime = [validtime]

        field, geo_in = self.read(var, validtime)
        # Reshape to fortran 2D style
        field = np.reshape(field, [geo_in.nlons, geo_in.nlats], order="F")
        return field, geo_in

    def points(self, var, geo_out, validtime=None, interpolation="nearest"):
        """Read points.

        Args:
            var (SurfexFileVariable): Variable in surfex file.
            geo_out (surfex.Geometry): Surfex geometry to interpolate to.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.
            interpolation (str, optional): Interpolation type.. Defaults to "nearest".

        Returns:
            tuple: points, surfex.Interpolator

        """
        field, geo_in = self.field(var, validtime=validtime)

        points, interpolator = SurfexIO.interpolate_field(field, geo_in, geo_out,
                                                          interpolation=interpolation)
        return points, interpolator


class TexteSurfexFile(SurfexIO):
    """Reading surfex TEXTE output."""

    def __init__(self, filename, geo):
        """Construct the Texte file.

        Args:
            filename (str): Filename
            geo (surfex.Geometry): Geometry

        """
        self.file = None
        SurfexIO.__init__(self, filename, geo, "TXT")

    def read(self, variable, times):
        """Read file.

        Args:
            var (SurfexFileVariable): Variable in surfex file.
            times (list): List of datetime.datetime to read.

        Raises:
            Exception: _description_
            Exception: _description_
            Exception: _description_
            Exception: _description_

        Returns:
            tuple: (np.array, surfex.Geometry)

        """
        self.file = open(self.filename, mode="r", encoding="utf-8")

        base_time = variable.basetime
        interval = variable.interval
        npatch = variable.patches

        if base_time is None:
            raise Exception("Basetime must be set for TEXTE")
        if interval is None:
            raise Exception("Interval must be set for TEXTE")

        if not isinstance(times, (list, tuple)):
            raise Exception("times must be list or tuple")

        values = np.array([])
        times_read = np.array([])
        end_of_line = self.geo.npoints * npatch
        this_time = np.empty(self.geo.npoints * npatch)

        tstep = 1
        col = 0
        for line in self.file.read().splitlines():

            words = line.split()
            if len(words) > 0:
                for i, word in enumerate(words):
                    val = float(word.replace("D", "E"))
                    if val == 1e+20:
                        val = np.nan
                    this_time[col] = val

                    col = col + 1
                    if col == end_of_line:

                        if times is None or (base_time
                                             + timedelta(seconds=(tstep * interval))) in times:
                            values = np.append(values, this_time)
                            times_read = np.append(times_read, base_time
                                                   + timedelta(seconds=(tstep * interval)))
                            # print i, col, base_time + timedelta(seconds=(t * interval)), this_time

                        tstep = tstep + 1
                        col = 0
                        this_time[:] = np.nan
                        if i != len(words) - 1:
                            raise Exception("Dimension of domain does not match end of line! "
                                            + str(i) + " != " + str(len(words) - 1))

        if times_read.shape[0] > 0:
            values = np.reshape(values, [times_read.shape[0], this_time.shape[0]])
        else:
            logging.info("No data found!")

        # print values.shape
        self.file.close()
        return values, self.geo

    def field(self, var, validtime=None):
        """Read field.

        Args:
            var (SurfexFileVariable): Variable in surfex file.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.

        Returns:
            tuple: (np.array, surfex.Geometry)

        """
        if validtime is None:
            raise Exception("You must set times to read forcing data")
        else:
            validtime = [validtime]

        field, geo_in = self.read(var, validtime)
        # Reshape to fortran 2D style
        field = np.reshape(field, [geo_in.nlons, geo_in.nlats], order="F")
        return field, geo_in

    def points(self, var, geo_out, validtime=None, interpolation="nearest"):
        """Read points.

        Args:
            var (SurfexFileVariable): Variable in surfex file.
            geo_out (surfex.Geometry): Geometry in file.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.
            interpolation (str, optional): Interpolation type. Defaults to "nearest".

        Returns:
            tuple: (np.array, surfex.Interpolator)

        """
        field, geo_in = self.field(var, validtime=validtime)
        points, interpolator = SurfexIO.interpolate_field(field, geo_in, geo_out,
                                                          interpolation=interpolation)
        return points, interpolator


class ForcingFileNetCDF(SurfexIO):
    """Forcing netCDF file."""

    def __init__(self, fname, geo):
        """Construct forcing netcdf file.

        Args:
            fname (str): File name.
            geo (surfex.Geo): Surfex input geometry.

        """
        self.fname = fname
        self.file_handler = netCDF4.Dataset(fname, "r")
        self.lons = self.file_handler.variables["LON"]
        self.lats = self.file_handler.variables["LAT"]
        # self.n_x = self.lons.shape[0]
        # self.n_y = self.lats.shape[0]
        SurfexIO.__init__(self, fname, geo, "nc")

    def read_field(self, variable, times):
        """Read file.

        Args:
            variable (_type_): _description_
            times (_type_): _description_

        Raises:
            Exception: _description_
            Exception: _description_
            Exception: _description_

        Returns:
            _type_: _description_

        """
        var = variable.varname
        field = None
        if var in self.file_handler.variables:
            if self.file_handler.variables[var].shape[0] > 0:
                if len(self.file_handler.variables[var].dimensions) == 1:
                    dimlen = self.file_handler.variables[var].shape[0]
                    field = self.file_handler.variables[var][0:dimlen]
                else:
                    if len(times) == 0:
                        raise Exception("You must set time!")

                    times_read = []
                    ndims = 0
                    npoints = 0
                    for dim in self.file_handler.variables[var].dimensions:
                        dimlen = self.file_handler.variables[var].shape[ndims]

                        if dim == "time":
                            times_for_var = self.file_handler.variables['time']
                            units = times_for_var.units
                            try:
                                t_cal = times_for_var.calendar
                            except AttributeError:  # Attribute doesn't exist
                                t_cal = u"gregorian"  # or standard

                            indices = list(range(0, dimlen))
                            times_for_var = netCDF4.num2date(times_for_var[indices], units=units,
                                                             calendar=t_cal)
                            # print(times_for_var)
                            for times_to_read_val in times:
                                # print(times_to_read, times[times_to_read])
                                for tstep, times_for_var_val in enumerate(times_for_var):
                                    # print(t, times_for_var[t], times[times_to_read])
                                    test_time = times_for_var_val.strftime("%Y%m%d%H")
                                    test_time = datetime.strptime(test_time, "%Y%m%d%H")
                                    if test_time == times_to_read_val:
                                        times_read.append(tstep)
                                        logging.debug("%s %s", tstep, times_to_read_val)
                        else:
                            npoints = dimlen

                        ndims = ndims + 1

                    if npoints == 0:
                        raise Exception("No points found")

                    if len(times_read) == 0 and len(times) > 0:
                        logging.error("%s", times)
                        raise Exception("Valid time not found in file!")

                    field = self.file_handler.variables[var][times_read, 0: npoints]
        else:
            logging.warning("Variable %s not found!", var)
        return field, self.geo

    def field(self, var, validtime=None):
        """Read field.

        Args:
            var (_type_): _description_
            validtime (_type_, optional): _description_. Defaults to None.

        Raises:
            Exception: _description_

        Returns:
            _type_: _description_

        """
        if validtime is None:
            validtime = []
            # raise Exception("You must set times to read forcing data")
        else:
            validtime = [validtime]

        field, geo_in = self.read_field(var, validtime)
        # Reshape to fortran 2D style
        if field is not None:
            field = np.reshape(field, [geo_in.nlons, geo_in.nlats], order="F")
        return field, geo_in

    def points(self, var, geo_out, validtime=None, interpolation=None):
        """Read points.

        Args:
            var (SurfexFileVariable): Variable in surfex file.
            geo_out (surfex.Geo): Surfex geometry for points.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.
            interpolation (str, optional): Interpolation type. Defaults to "nearest".

        Returns:
            tuple: Interpolated points, surfex.Interpolator

        """
        field, geo_in = self.field(var, validtime=validtime)

        points, interpolator = SurfexIO.interpolate_field(field, geo_in, geo_out,
                                                          interpolation=interpolation)

        return points, interpolator


def read_surfex_field(varname, filename, validtime=None, basetime=None, patches=-1, layers=-1,
                      fileformat=None, filetype=None, geo=None, datatype=None, interval=None):
    """Read surfex field.

    Args:
        varname (_type_): _description_
        filename (_type_): _description_
        validtime (_type_, optional): _description_. Defaults to None.
        basetime (_type_, optional): _description_. Defaults to None.
        patches (int, optional): _description_. Defaults to -1.
        layers (int, optional): _description_. Defaults to -1.
        fileformat (_type_, optional): _description_. Defaults to None.
        filetype (_type_, optional): _description_. Defaults to None.
        geo (_type_, optional): _description_. Defaults to None.
        datatype (_type_, optional): _description_. Defaults to None.
        interval (_type_, optional): _description_. Defaults to None.

    Raises:
        NotImplementedError: _description_
        Exception: _description_

    Returns:
        _type_: _description_

    """
    if fileformat is None:
        fileformat, filetype = surfex.file.guess_file_format(filename, filetype)

    if filetype == "surf":
        if fileformat.lower() == "ascii":
            geo = surfex.file.AsciiSurfexFile(filename).geo
        elif fileformat.lower() == "nc":
            geo = surfex.file.NCSurfexFile(filename).geo
        else:
            if geo is None:
                raise NotImplementedError("Not implemnted and geo is None")
    elif geo is None:
        raise Exception("You need to provide a geo object. Filetype is: " + str(filetype))

    sfx_io = surfex.file.get_surfex_io_object(filename, filetype=filetype, fileformat=fileformat,
                                              geo=geo)
    var = surfex.file.SurfexFileVariable(varname, validtime=validtime, patches=patches,
                                         layers=layers, basetime=basetime, interval=interval,
                                         datatype=datatype)
    field, __ = sfx_io.field(var, validtime=validtime)
    return field


def read_surfex_points(varname, filename, geo_out, validtime=None, basetime=None, patches=-1,
                       layers=-1, fileformat=None, filetype=None, geo=None, datatype=None,
                       interval=None, interpolation="nearest"):
    """Read surfex points.

    Args:
        varname (str): Variable name.
        filename (str): File name.
        geo_out (surfex.Geo): Surfex geometry
        validtime (datetime.datetime, optional): Valid time. Defaults to None.
        basetime (datetime.datetime, optional): Base time. Defaults to None.
        patches (int, optional): Number of patches. Defaults to -1.
        layers (int, optional): Numbers of layers. Defaults to -1.
        fileformat (str, optional): File format. Defaults to None.
        filetype (str, optional): File type. Defaults to None.
        geo (surfex.Geo, optional): Input geometry if needed. Defaults to None.
        datatype (str, optional): Data type. Defaults to None.
        interval (int, optional): Interval between times. Defaults to None.
        interpolation (str, optional): Interpolation method. Defaults to "nearest".

    Raises:
        NotImplementedError: _description_
        Exception: _description_

    Returns:
        np.darray: Field

    """
    if fileformat is None:
        fileformat, filetype = surfex.file.guess_file_format(filename, filetype)

    if filetype == "surf":
        if fileformat.lower() == "ascii":
            geo = surfex.file.AsciiSurfexFile(filename).geo
        elif fileformat.lower() == "nc":
            geo = surfex.file.NCSurfexFile(filename).geo
        else:
            if geo is None:
                raise NotImplementedError(f"{fileformat} is not implemented and geo is None")
    elif geo is None:
        raise Exception("You need to provide a geo object. Filetype is: " + str(filetype))

    sfx_io = surfex.file.get_surfex_io_object(filename, filetype=filetype, fileformat=fileformat,
                                              geo=geo)
    var = surfex.file.SurfexFileVariable(varname, validtime=validtime, patches=patches,
                                         layers=layers, basetime=basetime, interval=interval,
                                         datatype=datatype)
    field, geo_out = sfx_io.points(var, geo_out, validtime=validtime, interpolation=interpolation)
    return field


def parse_filepattern(file_pattern, basetime, validtime):
    """Parse the file pattern.

    Args:
        file_pattern (str): File pattern.
        basetime (datetime.datetime): Base time.
        validtime (datetime.datetime): Valid time.

    Returns:
        str: File name

    """
    if basetime is None or validtime is None:
        return file_pattern

    logging.debug("file_pattern=%s basetime=%s validtime=%s", file_pattern, basetime, validtime)
    file_name = str(file_pattern)
    year = basetime.strftime('%Y')
    year2 = basetime.strftime('%y')
    month = basetime.strftime('%m')
    day = basetime.strftime('%d')
    hour = basetime.strftime('%H')
    mins = basetime.strftime('%M')
    d_t = validtime - basetime
    ll_d = f"{int(d_t.seconds / 3600):d}"
    ll_2 = f"{int(d_t.seconds / 3600):02d}"
    ll_3 = f"{int(d_t.seconds / 3600):03d}"
    ll_4 = f"{int(d_t.seconds / 3600):04d}"
    file_name = file_name.replace('@YYYY@', year)
    file_name = file_name.replace('@YY@', year2)
    file_name = file_name.replace('@MM@', month)
    file_name = file_name.replace('@DD@', day)
    file_name = file_name.replace('@HH@', hour)
    file_name = file_name.replace('@mm@', mins)
    file_name = file_name.replace('@L@', ll_d)
    file_name = file_name.replace('@LL@', ll_2)
    file_name = file_name.replace('@LLL@', ll_3)
    file_name = file_name.replace('@LLLL@', ll_4)
    return file_name

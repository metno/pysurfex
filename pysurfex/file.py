"""Surfex file related stuff."""
import abc
import logging
import os
import re
import shutil

import netCDF4
import numpy as np
import pyproj

from .datetime_utils import as_datetime, as_datetime_args, as_timedelta
from .fa import Fa
from .geo import IGN, ConfProj, LonLatReg, LonLatVal
from .interpolation import Interpolation
from .util import remove_existing_file


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
        interpolator = Interpolation(interpolation, geo_in, geo_out)
        field = interpolator.interpolate(field)
        return field, interpolator


class SurfexSurfIO(object):
    """Surfex surf file.

    Class for surfex restart files.

    """

    def __init__(
        self, surfexfile, csurf_filetype, input_file=None, symlink=True, archive_file=None
    ):
        """Construct the surfex surf file.

        Args:
            surfexfile (str): Internal file name of surfex file.
            csurf_filetype (str): File type
            input_file (str, optional): File name to be used as surfexfile.
                                        Defaults to None.
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
            remove_existing_file(self.input_file, f_out)
            if os.path.abspath(self.input_file) != f_out:
                logging.info("Symlink %s -> %s", self.input_file, f_out)
                os.symlink(self.input_file, f_out)

    def copy_input(self):
        """Copy the input file."""
        if self.input_file is not None:
            f_out = os.getcwd() + "/" + self.filename
            remove_existing_file(self.input_file, f_out)
            if os.path.abspath(self.input_file) != f_out:
                logging.info("Copy %s -> %s", self.input_file, f_out)
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
                    os.unlink(self.archive_file)
                if os.path.isfile(self.archive_file):
                    os.remove(self.archive_file)
                if os.path.isdir(self.archive_file):
                    shutil.rmtree(self.archive_file)
                shutil.move(f_in, self.archive_file)


class PGDFile(SurfexSurfIO):
    """PGD file."""

    def __init__(
        self,
        csurf_filetype,
        cpgdfile,
        input_file=None,
        symlink=True,
        archive_file=None,
        lfagmap=False,
        masterodb=False,
    ):
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

        cpgdfile = get_surfex_io_object(
            cpgdfile,
            filetype="surf",
            fileformat=csurf_filetype,
            lfagmap=lfagmap,
            masterodb=masterodb,
        )

        SurfexSurfIO.__init__(
            self,
            cpgdfile,
            csurf_filetype,
            input_file=input_file,
            archive_file=archive_file,
            symlink=symlink,
        )
        self.need_pgd = False


class PREPFile(SurfexSurfIO):
    """PREP file."""

    def __init__(
        self,
        csurf_filetype,
        cprepfile,
        input_file=None,
        symlink=True,
        archive_file=None,
        lfagmap=False,
        masterodb=False,
    ):
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
        cprepfile = get_surfex_io_object(
            cprepfile,
            filetype="surf",
            fileformat=csurf_filetype,
            lfagmap=lfagmap,
            masterodb=masterodb,
        )

        SurfexSurfIO.__init__(
            self,
            cprepfile,
            csurf_filetype,
            input_file=input_file,
            archive_file=archive_file,
            symlink=symlink,
        )
        self.need_pgd = True


class SURFFile(SurfexSurfIO):
    """SURFOUT file."""

    def __init__(
        self,
        csurf_filetype,
        csurffile,
        archive_file=None,
        input_file=None,
        lfagmap=False,
        masterodb=False,
    ):
        """Construct SURFOUT file object.

        Result of a surfex binary.

        Args:
            csurf_filetype (str): File type
            csurffile (str): Name of the PREP file
            input_file (str, optional): Input file. Defaults to None.
            archive_file (str, optional): Location to store the result. Defaults to None.
            lfagmap (bool, optional): File use LFAGMAP. Defaults to False.
            masterodb (bool, optional): File produced by masterodb. Defaults to False.

        """
        logging.debug("SURFFile")
        csurffile = get_surfex_io_object(
            csurffile,
            filetype="surf",
            fileformat=csurf_filetype,
            lfagmap=lfagmap,
            masterodb=masterodb,
        )

        SurfexSurfIO.__init__(
            self,
            csurffile,
            csurf_filetype,
            input_file=input_file,
            archive_file=archive_file,
        )
        self.need_pgd = True


class SurfexFileVariable(object):
    """Surfex Variable."""

    def __init__(
        self,
        varname,
        validtime=None,
        patches=1,
        layers=1,
        basetime=None,
        interval=None,
        datatype="float",
        tiletype="FULL",
    ):
        """Construct a surfex file variable.

        Args:
            varname (str): Variable name.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.
            patches (int, optional): Number of patches. Defaults to 1.
            layers (int, optional): Number of layers. Defaults to 1.
            basetime (datetime.datetime, optional): _description_. Defaults to None.
            interval (int, optional): Interval. Defaults to None.
            datatype (str, optional): Data type for variable. Defaults to "float".
            tiletype (str, optional): Tiletype

        """
        self.varname = varname
        self.type = datatype
        self.patches = patches
        self.layers = layers
        self.basetime = basetime
        self.interval = interval
        self.validtime = validtime
        self.datatype = datatype
        self.tiletype = tiletype

    def print_var(self):
        """Print variable information."""
        return self.varname


def get_surfex_io_object(
    fname, filetype="surf", fileformat=None, geo=None, lfagmap=False, masterodb=False
):
    """Get the surfexIO object.

    Args:
        fname (str): File name.
        filetype (str, optional): File type. Defaults to "surf".
        fileformat (str, optional): File format. Defaults to None.
        geo (surfex.Geo, optional): Geometry. Defaults to None.
        lfagmap (bool, optional): File use LFAGMAP. Defaults to False.
        masterodb (bool, optional): File produced by masterodb. Defaults to False.

    Raises:
        RuntimeError: Invalid filetype
        NotImplementedError: Filetype not implemented
        RuntimeError: Format needs a geometry

    Returns:
        SurfexIO: SurfexIO object.

    """
    logging.debug("get_surfex_io_object")
    if filetype is not None and (
        filetype.lower() != "surf"
        and filetype.lower() != "ts"
        and filetype.lower() != "forcing"
    ):
        raise RuntimeError("Invalid filetype: " + filetype + " Allowed: surf/ts/forcing")

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
                raise RuntimeError("Format NetCDF needs a geometry")
            obj = NetCDFSurfexFile(fname, geo)
        elif filetype.lower() == "forcing":
            if geo is None:
                raise RuntimeError(
                    "Format NetCDF needs a geometry for reading forcing files"
                )
            obj = ForcingFileNetCDF(fname, geo)
        else:
            raise NotImplementedError
    elif fileformat.lower() == "texte":
        if geo is None:
            raise RuntimeError("Format TEXTE needs a geometry")
        obj = TexteSurfexFile(fname, geo)
    elif fileformat.lower() == "fa":
        if filetype.lower() == "surf":
            obj = FaSurfexFile(fname, geo=geo, lfagmap=lfagmap, masterodb=masterodb)
        else:
            raise NotImplementedError
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
        RuntimeError: Can not-auto decide filetype for files
        RuntimeError: Filetype and/or format not set

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
        for __ in needles:
            if ext.endswith("TXT"):
                ftype = "ts"
        needles = ["Forc_.*", "FORCING.*"]
        for needle in needles:
            if re.search(needle, f_n):
                ftype = "forcing"

        if re.search("SURFOUT.*", f_n) and ftype is None:
            raise RuntimeError(
                "Can not-auto decide filetype for files called SURFOUT.*.txt. "
                + "Specify either surf or ts"
            )

    fileformat = None
    logging.info("Trying to guess the file format from extension: %s", ext)
    if ext.endswith("txt"):
        fileformat = "ascii"
    if ext.endswith("TXT"):
        fileformat = "texte"
    if ext.endswith("nc"):
        fileformat = "nc" if ftype == "surf" else "netcdf"
    if ext.endswith("fa"):
        fileformat = "fa"
    if ext.endswith("sfx"):
        fileformat = "fa"

    if ftype is None or fileformat is None:
        raise RuntimeError(
            "Filetype and/or format not set: " + str(ftype) + " & " + str(fileformat)
        )
    logging.info("Filetype: %s format: %s", ftype, fileformat)

    return fileformat, ftype


class AsciiSurfexFile(SurfexIO):
    """Input from an ASCII surfex file (.txt)."""

    def __init__(self, filename, geo=None):
        """Construct the ASCII object.

        Args:
            filename (str): Filename
            geo(surfex.geo.Geo, optional): Geometry, Defaults to None.

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
            RuntimeError: No grid found
            NotImplementedError: _description_

        Returns:
            surfex.Geometry: Surfex geometry
        """
        if not os.path.isfile(self.filename):
            raise FileNotFoundError("File does not exist: " + str(self.filename))

        grid = self.read("GRID_TYPE", "FULL", "string")
        if grid is None:
            raise RuntimeError("No grid found")

        if grid == "IGN":
            domain = {
                "nam_ign": {
                    "clambert": self.read("LAMBERT", "&FULL", "integer"),
                    "npoints": self.read("NPOINTS", "&FULL", "integer"),
                    "xx": self.read("XX", "&FULL", "float"),
                    "xy": self.read("XY", "&FULL", "float"),
                    "xdx": self.read("XDX", "&FULL", "float"),
                    "xdy": self.read("XY", "&FULL", "float"),
                    "xx_llcorner": self.read("XX_LLCORNER", "&FULL", "float"),
                    "xy_llcorner": self.read("XY_LLCORNER", "&FULL", "float"),
                    "xcellsize": self.read("XCELLSIZE", "&FULL", "float"),
                    "ncols": self.read("NCOLS", "&FULL", "integer"),
                    "nrows": self.read("NROWS", "&FULL", "integer"),
                }
            }
            return IGN(domain)

        if grid == "LONLATVAL":
            domain = {
                "nam_lonlatval": {
                    "xx": self.read("XX", "&FULL", "float"),
                    "xy": self.read("XY", "&FULL", "float"),
                    "xdx": self.read("DX", "&FULL", "float"),
                    "xdy": self.read("DY", "&FULL", "float"),
                }
            }
            return LonLatVal(domain)

        if grid == "LONLAT REG":
            domain = {
                "nam_lonlat_reg": {
                    "xlonmin": self.read("LONMIN", "&FULL", "float"),
                    "xlatmin": self.read("LATMIN", "&FULL", "float"),
                    "xlonmax": self.read("LONMAX", "&FULL", "float"),
                    "xlatmax": self.read("LATMAX", "&FULL", "float"),
                    "nlon": self.read("NLON", "&FULL", "integer"),
                    "nlat": self.read("NLAT", "&FULL", "integer"),
                }
            }
            return LonLatReg(domain)

        if grid == "CONF PROJ":
            lon0 = self.read("LON0", "&FULL", "float")
            lat0 = self.read("LAT0", "&FULL", "float")
            n_x = self.read("IMAX", "&FULL", "integer")
            n_y = self.read("JMAX", "&FULL", "integer")
            d_x = self.read("XX", "&FULL", "float")
            d_y = self.read("XX", "&FULL", "float")
            if d_x.shape[0] > 1:
                d_x = d_x[1] - d_x[0]
            if d_y.shape[0] > 1:
                d_y = d_y[1] - d_y[0]
            ll_lon = self.read("LONORI", "&FULL", "float")
            ll_lat = self.read("LATORI", "&FULL", "float")

            logging.info(
                "lon0=%s lat0=%s n_x=%s, n_y=%s d_x=%s dy=%s, ll_lon=%s, ll_lat=%s",
                lon0,
                lat0,
                n_x,
                n_y,
                d_x,
                d_y,
                ll_lon,
                ll_lat,
            )
            earth = 6.37122e6
            proj_string = (
                f"+proj=lcc +lat_0={lat0!s} +lon_0={lon0!s} +lat_1={lat0!s} "
                f"+lat_2={lat0!s} +units=m +no_defs +R={earth!s}"
            )

            proj = pyproj.CRS.from_string(proj_string)
            wgs84 = pyproj.CRS.from_string("EPSG:4326")
            x_0, y_0 = pyproj.Transformer.from_crs(wgs84, proj, always_xy=True).transform(
                ll_lon, ll_lat
            )
            x_c = x_0 + 0.5 * (n_x - 1) * d_x
            y_c = y_0 + 0.5 * (n_y - 1) * d_y
            lonc, latc = pyproj.Transformer.from_crs(
                proj, wgs84, always_xy=True
            ).transform(x_c, y_c)

            domain = {
                "nam_conf_proj": {"xlon0": lon0, "xlat0": lat0},
                "nam_conf_proj_grid": {
                    "xloncen": lonc,
                    "xlatcen": latc,
                    "nimax": n_x,
                    "njmax": n_y,
                    "xdx": d_x,
                    "xdy": d_x,
                    "ilone": 0,
                    "ilate": 0,
                },
            }
            return ConfProj(domain)
        raise NotImplementedError("Grid " + str(grid[0]) + " not implemented!")

    def read(self, read_par, read_tile, datatype):
        """Read the file.

        Args:
            read_par (str): Parameter to read
            read_tile (str): Tile to read
            datatype (str): Datatype

        Raises:
            NotImplementedError: Datatype not implemented
            RuntimeError: Could not read datatype

        Returns:
            numpy.array: Values read

        """
        # Add & if not given
        if read_tile.find("&") < 0:
            read_tile = "&" + read_tile
        file = open(self.filename, mode="r", encoding="utf-8")  # noqa SIM115
        read_desc = False
        read_value = False
        values = []
        for line in file:
            words = line.split()
            logging.debug("read_value=%s, read_desc=%s", read_value, read_desc)
            logging.debug("words=%s", words)

            if len(words) > 0:
                if read_value and not read_desc and words[0].find("&") < 0:
                    try:
                        if datatype.lower() == "float":
                            for word in words:
                                val = float(word.replace("D", "E"))
                                if val == 1e20:
                                    val = np.nan
                                values.append(val)
                        elif datatype.lower() == "string":
                            str_words = []
                            for word in words:
                                str_words.append(word)  # noqa PERF402
                            values = " ".join(str_words)
                        elif datatype.lower() == "integer" or datatype.lower() == "int":
                            for word in words:
                                values.append(int(word))
                        elif datatype.lower() == "logical" or datatype.lower() == "bool":
                            for word in words:
                                values = word.lower().strip()[0] == "t"
                        else:
                            raise NotImplementedError(
                                "Type not implemented " + str(datatype)
                            )
                    except ValueError:
                        raise RuntimeError(
                            f"Conversion from {words!s} to {datatype!s} "
                            "does not work! Try a different datatype!"
                        ) from ValueError
                if read_desc:
                    read_desc = False
                    read_value = True

                if words[0].find("&") >= 0:
                    tile = words[0]
                    par = words[1]

                    read_value = False
                    if (
                        tile.strip().lower() == read_tile.lower()
                        and par.lower() == read_par.lower()
                    ):
                        read_desc = True
                        read_value = False
                        logging.info("Found: %s %s", str(tile), str(par))
                    logging.info("Found: %s %s", str(read_tile), str(read_par))

            # Description could be empty
            elif read_desc:
                read_desc = False
                read_value = True

        if isinstance(values, list):
            if len(values) == 0:
                logging.info("No values found for %s", read_par)
                return None

            values = np.asarray(values) if len(values) > 1 else values[0]
        logging.info("Returning values: %s", values)
        return values

    def field(self, var, validtime=None):  # noqa ARG002
        """Read a field in file.

        Args:
            var (SurfexFileVariable): Variable in surfex file.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.

        Returns:
            np.darray: Field, surfex.Geo in read file

        """
        read_par = var.varname
        read_tile = var.tiletype
        datatype = var.datatype
        field = self.read(read_par, read_tile, datatype)

        geo_in = self.get_geo()
        field = np.reshape(field, [geo_in.nlons, geo_in.nlats], order="F")
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

        points, interpolator = SurfexIO.interpolate_field(
            field, geo_in, geo_out, interpolation=interpolation
        )
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

        Raises:
            NotImplementedError: Grid not implemented

        Returns:
            surfex.Geometry: Surfex geometry in file

        """
        try:
            f_h = netCDF4.Dataset(self.filename, "r")
        except OSError:
            logging.debug("Geo not open %s as a netCDF file", self.filename)
            return None
        cgrid = str(netCDF4.chartostring(f_h["GRID_TYPE"][:])).strip()
        if cgrid == "CONF PROJ":
            lon0 = float(f_h["LON0"][0])
            lat0 = float(f_h["LAT0"][0])
            n_x = int(f_h["IMAX"][0])
            n_y = int(f_h["JMAX"][0])
            d_x = float(f_h["DX"][0][0])
            d_y = float(f_h["DY"][0][0])

            ll_lon = float(f_h["LONORI"][0])
            ll_lat = float(f_h["LATORI"][0])
            earth = 6.37122e6
            proj_string = (
                f"+proj=lcc +lat_0={lat0!s} +lon_0={lon0!s} +lat_1={lat0!s} "
                f"+lat_2={lat0!s} +units=m +no_defs +R={earth!s}"
            )

            proj = pyproj.CRS.from_string(proj_string)
            wgs84 = pyproj.CRS.from_string("EPSG:4326")
            x_0, y_0 = pyproj.Transformer.from_crs(wgs84, proj, always_xy=True).transform(
                ll_lon, ll_lat
            )
            x_c = x_0 + 0.5 * (n_x + 1) * d_x
            y_c = y_0 + 0.5 * (n_y + 1) * d_y
            lonc, latc = pyproj.Transformer.from_crs(
                proj, wgs84, always_xy=True
            ).transform(x_c, y_c)

            domain = {
                "nam_conf_proj": {"xlon0": lon0, "xlat0": lat0},
                "nam_conf_proj_grid": {
                    "xloncen": lonc,
                    "xlatcen": latc,
                    "nimax": n_x,
                    "njmax": n_y,
                    "xdx": d_x,
                    "xdy": d_y,
                    "ilone": 0,
                    "ilate": 0,
                },
            }
            return ConfProj(domain)
        if cgrid == "IGN":
            domain = {
                "nam_ign": {
                    "clambert": f_h["CLAMBERT"][0],
                    "xx": f_h["XX"][:],
                    "xy": f_h["XY"][:],
                    "xdx": f_h["DX"][:],
                    "xdy": f_h["DY"][:],
                }
            }
            return IGN(domain)

        if cgrid == "LONLATVAL":
            domain = {
                "nam_lonlatval": {
                    "xx": f_h["XX"][:],
                    "xy": f_h["XY"][:],
                    "xdx": f_h["DX"][:],
                    "xdy": f_h["DY"][:],
                }
            }
            return LonLatVal(domain)

        if cgrid == "LONLAT REG":
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
            return LonLatReg(domain)
        raise NotImplementedError(cgrid + " is not implemented")

    def field(self, var, validtime=None):
        """Read field.

        Args:
            var (SurfexFileVariable): Variable in surfex file.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.
            validtime (_type_, optional): _description_. Defaults to None.

        Raises:
            RuntimeError: Mismatch in times in file and the wanted time

        Returns:
            np.darray: Field, surfex.Geo in read file

        """
        f_h = netCDF4.Dataset(self.filename, "r")
        if validtime is None:
            pass
        else:
            time_in_file = None
            try:
                year = f_h["DTCUR-YEAR"][0]
                month = f_h["DTCUR-MONTH"][0]
                day = f_h["DTCUR-DAY"][0]
                time = int(f_h["DTCUR-TIME"][0])
                hour = int(time / 3600)

                # TODO minutes
                time_in_file = as_datetime_args(
                    year=year, month=month, day=day, hour=hour
                )
            except IndexError:
                logging.warning("Could not checking time")

            if time_in_file is not None and validtime != time_in_file:
                logging.error(
                    "time_in_file=%s validtime=%s %s %s",
                    time_in_file,
                    validtime,
                    type(time_in_file),
                    type(validtime),
                )
                raise RuntimeError("Mismatch in times in file and the wanted time")

        geo_in = self.get_geo()
        field = f_h[var.varname][:]
        fillvalue = f_h[var.varname].getncattr("_FillValue")
        logging.info("Set %s to nan", fillvalue)
        field = field.filled(np.nan)

        # Reshape to fortran 2D style
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
        points, interpolator = SurfexIO.interpolate_field(
            field, geo_in, geo_out, interpolation=interpolation
        )
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

        # Surfex binaries use .fa suffix for all input files no matter if they are
        # lfagmap files

        if not filename.endswith(extension_suffix):
            filename = filename + extension_suffix

        SurfexIO.__init__(self, filename, geo, extension)
        self.lfagmap = lfagmap

    def field(self, var, validtime=None):
        """Read field from FA file.

        Args:
            var (SurfexFileVariable): Variable in surfex file.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.

        Returns:
            np.darray: Field, surfex.Geo in read file

        """
        file_handler = Fa(self.filename)

        field, geo_in = file_handler.field(var.varname, validtime)

        # Reshape to fortran 2D style
        logging.debug("field=%s, field.shape=%s", field, field.shape)
        field = np.reshape(field, [geo_in.nlons, geo_in.nlats], order="F")
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

        points, interpolator = SurfexIO.interpolate_field(
            field, geo_in, geo_out, interpolation=interpolation
        )
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

        Raises:
            ValueError: times must be list or tuple
            ValueError: patches must be list or tuple
            ValueError: patches must be list or tuple
            RuntimeError: Variable not found!
            NotImplementedError: Dimension not implemented
            RuntimeError: Mismatch in points

        Returns:
            (tuple): (np.array. surfex.Geometry)

        """
        layers = var.layers
        patches = var.patches

        if not isinstance(times, (list, tuple)):
            raise ValueError("times must be list or tuple")
        if not isinstance(layers, (list, tuple)):
            raise ValueError("patches must be list or tuple")
        if not isinstance(patches, (list, tuple)):
            raise ValueError("patches must be list or tuple")

        values = np.array([])
        times_read = []
        ndims = 0
        dim_indices = []
        mapping = {}
        npatch = 1

        if self.file_handler.variables[var.varname].shape[0] > 0:
            for dim in self.file_handler.variables[var.varname].dimensions:
                dimlen = self.file_handler.variables[var.varname].shape[ndims]
                this_dim = []

                if dim == "time":
                    mapping[0] = ndims
                    times_for_var = self.file_handler.variables["time"]
                    units = times_for_var.units
                    try:
                        t_cal = times_for_var.calendar
                    except AttributeError:  # Attribute doesn't exist
                        t_cal = "gregorian"  # or standard

                    indices = list(range(dimlen))
                    times_for_var = netCDF4.num2date(
                        times_for_var[indices], units=units, calendar=t_cal
                    )
                    if len(times) > 0:
                        for __, t_to_find_val in enumerate(times):
                            for tstep in range(len(indices)):
                                if times_for_var[tstep] == t_to_find_val:
                                    this_dim.append(tstep)
                                    times_read.append(t_to_find_val)
                    else:
                        times_read = times_for_var
                        this_dim = list(range(dimlen))

                elif dim in ("Number_of_points", "xx"):
                    mapping[1] = ndims
                    this_dim = list(range(dimlen))
                elif dim == "yy":
                    mapping[2] = ndims
                    this_dim = list(range(dimlen))
                elif dim == "Number_of_Patches":
                    mapping[3] = ndims
                    npatch = dimlen
                    if len(patches) > 0:
                        npatch = len(patches)
                        this_dim = patches
                    else:
                        this_dim = list(range(dimlen))
                elif dim == "Number_of_Layers":
                    mapping[4] = ndims
                    npatch = dimlen
                    this_dim = layers if len(layers) > 0 else list(range(dimlen))
                elif dim == "lon":
                    mapping[1] = ndims
                    this_dim = list(range(dimlen))
                elif dim == "lat":
                    mapping[2] = ndims
                    this_dim = list(range(dimlen))
                else:
                    raise NotImplementedError("Not implemented for: " + dim)

                dim_indices.append(this_dim)
                ndims = ndims + 1

            logging.debug("dim_indices=%s", dim_indices)
            field = self.file_handler.variables[var.varname][dim_indices]

            # Add extra dimensions
            i = 0
            reverse_mapping = []
            for dim in range(5):
                if dim not in mapping:
                    logging.debug("Adding dimension %s", dim)
                    field = np.expand_dims(field, len(dim_indices) + i)
                    reverse_mapping.append(len(dim_indices) + i)
                    i = i + 1
                else:
                    reverse_mapping.append(mapping[dim])

            # Transpose to 5D array
            field = np.transpose(field, reverse_mapping)
            npoints = self.geo.npoints * npatch

            # Create 2-D array with times and points as for the other formats
            for tstep in range(field.shape[0]):
                field2d = np.empty(npoints)
                i = 0
                for (patch,) in range(npatch):
                    if self.geo.mask is not None:
                        iii = 0
                        j = 0
                        # For some reason the NetCDF files have one less dimension in all
                        # dimensions than the PGD dimension and mask needs x first.
                        for xxx in range(-1, field.shape[1] + 1):
                            for yyy in range(-1, field.shape[2] + 1):
                                if (
                                    xxx in range(field.shape[1])
                                    and yyy in range(field.shape[2])
                                    and self.geo.mask[j] == iii
                                ):
                                    field2d[i] = np.nan
                                    if field[tstep, xxx, yyy, patch] != np.nan:
                                        field2d[i] = field[tstep, xxx, yyy, patch]
                                    i = i + 1
                                    j = j + 1
                                iii = iii + 1
                    else:
                        for yyy in range(field.shape[2]):
                            for xxx in range(field.shape[1]):
                                field2d[i] = np.nan
                                if field[tstep, xxx, yyy, patch] != np.nan:
                                    field2d[i] = field[tstep, xxx, yyy, patch]
                                i = i + 1
                if i != npoints:
                    raise RuntimeError(
                        "Mismatch in points " + str(i) + "!=" + str(npoints)
                    )

                values = np.append(values, field2d)
            # Re-shape to proper format
            values = np.reshape(values, [field.shape[0], npoints])

        else:
            raise RuntimeError("Variable " + var.varname + " not found!")

        return values, self.geo

    def field(self, var, validtime=None):
        """Read field.

        Args:
            var (SurfexFileVariable): Variable in surfex file.
            validtime (datetime.datetime, optional): Valid time. Defaults to None.

        Returns:
            tuple: (np.array, surfex.Geometry)

        """
        validtime = [] if validtime is None else [validtime]

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

        points, interpolator = SurfexIO.interpolate_field(
            field, geo_in, geo_out, interpolation=interpolation
        )
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
            variable (SurfexFileVariable): Variable in surfex file.
            times (list): List of datetime.datetime to read.

        Raises:
            RuntimeError: Basetime must be set for TEXTE
            RuntimeError: Interval must be set for TEXTE
            RuntimeError: times must be list or tuple
            RuntimeError: Dimension of domain does not match end of line

        Returns:
            tuple: (np.array, surfex.Geometry)

        """
        self.file = open(self.filename, mode="r", encoding="utf-8")  # noqa SIM115

        base_time = variable.basetime
        interval = variable.interval
        npatch = variable.patches

        if base_time is None:
            raise RuntimeError("Basetime must be set for TEXTE")
        if interval is None:
            raise RuntimeError("Interval must be set for TEXTE")

        if not isinstance(times, (list, tuple)):
            raise RuntimeError("times must be list or tuple")

        values = np.array([])
        times_read = np.array([])
        end_of_line = self.geo.npoints * npatch
        this_time = np.empty(self.geo.npoints * npatch)

        tstep = 0
        col = 0
        for line in self.file.read().splitlines():
            words = line.split()
            if len(words) > 0:
                for i, word in enumerate(words):
                    val = float(word.replace("D", "E"))
                    if val == 1e20:
                        val = np.nan
                    this_time[col] = val

                    col = col + 1
                    if col == end_of_line:
                        if (
                            times is None
                            or (base_time + as_timedelta(seconds=(tstep * interval)))
                            in times
                        ):
                            values = np.append(values, this_time)
                            times_read = np.append(
                                times_read,
                                base_time + as_timedelta(seconds=(tstep * interval)),
                            )

                        tstep = tstep + 1
                        col = 0
                        this_time[:] = np.nan
                        if i != len(words) - 1:
                            raise RuntimeError(
                                "Dimension of domain does not match end of line! "
                                + str(i)
                                + " != "
                                + str(len(words) - 1)
                            )

        if times_read.shape[0] > 0:
            values = np.asarray(values)
            values = np.reshape(values, [times_read.shape[0], this_time.shape[0]])
        else:
            logging.info("No data found!")

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
        if validtime is not None:
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
        points, interpolator = SurfexIO.interpolate_field(
            field, geo_in, geo_out, interpolation=interpolation
        )
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
        SurfexIO.__init__(self, fname, geo, "nc")

    def read_field(self, variable, times):
        """Read file.

        Args:
            variable (_type_): _description_
            times (_type_): _description_

        Raises:
            RuntimeError: You must set time!
            RuntimeError: No points found
            RuntimeError: Valid time not found in file!

        Returns:
            tuple: field, geo

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
                        raise RuntimeError("You must set time!")

                    times_read = []
                    ndims = 0
                    npoints = 0
                    for dim in self.file_handler.variables[var].dimensions:
                        dimlen = self.file_handler.variables[var].shape[ndims]

                        if dim == "time":
                            times_for_var = self.file_handler.variables["time"]
                            units = times_for_var.units
                            try:
                                t_cal = times_for_var.calendar
                            except AttributeError:  # Attribute doesn't exist
                                t_cal = "gregorian"  # or standard

                            indices = list(range(dimlen))
                            times_for_var = netCDF4.num2date(
                                times_for_var[indices], units=units, calendar=t_cal
                            )
                            for times_to_read_val in times:
                                for tstep, times_for_var_val in enumerate(times_for_var):
                                    test_time = times_for_var_val.strftime("%Y%m%d%H")
                                    test_time = as_datetime(test_time)
                                    if test_time == times_to_read_val:
                                        times_read.append(tstep)
                                        logging.debug("%s %s", tstep, times_to_read_val)
                        else:
                            npoints = dimlen

                        ndims = ndims + 1

                    if npoints == 0:
                        raise RuntimeError("No points found")

                    if len(times_read) == 0 and len(times) > 0:
                        logging.error("%s", times)
                        raise RuntimeError("Valid time not found in file!")

                    field = self.file_handler.variables[var][times_read, 0:npoints]
        else:
            logging.warning("Variable %s not found!", var)
        return field, self.geo

    def field(self, var, validtime=None):
        """Read field.

        Args:
            var (_type_): _description_
            validtime (list, optional): Validtime. Defaults to None.

        Returns:
            tuple: field, geo

        """
        validtime = [] if validtime is None else [validtime]

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

        points, interpolator = SurfexIO.interpolate_field(
            field, geo_in, geo_out, interpolation=interpolation
        )
        return points, interpolator


def read_surfex_field(
    varname,
    filename,
    validtime=None,
    basetime=None,
    patches=-1,
    layers=-1,
    fileformat=None,
    filetype=None,
    geo=None,
    datatype=None,
    interval=None,
    tiletype="FULL",
):
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
        tiletype(str, optional): Tiletype. Defaults to "FULL".

    Raises:
        RuntimeError: Not implemented and geo is None
        RuntimeError: You need to provide a geo object.

    Returns:
        field (np.ndarray): Field

    """
    if fileformat is None:
        fileformat, filetype = guess_file_format(filename, filetype)

    if filetype == "surf":
        if fileformat.lower() == "ascii":
            geo = AsciiSurfexFile(filename).geo
        elif fileformat.lower() == "nc":
            geo = NCSurfexFile(filename).geo
        elif geo is None:
            raise RuntimeError("Not implemented and geo is None")
    elif geo is None:
        raise RuntimeError(
            "You need to provide a geo object. Filetype is: " + str(filetype)
        )

    sfx_io = get_surfex_io_object(
        filename, filetype=filetype, fileformat=fileformat, geo=geo
    )
    var = SurfexFileVariable(
        varname,
        validtime=validtime,
        patches=patches,
        layers=layers,
        basetime=basetime,
        interval=interval,
        datatype=datatype,
        tiletype=tiletype,
    )
    field, __ = sfx_io.field(var, validtime=validtime)
    return field


def read_surfex_points(
    varname,
    filename,
    geo_out,
    validtime=None,
    basetime=None,
    patches=-1,
    layers=-1,
    fileformat=None,
    filetype=None,
    geo=None,
    datatype=None,
    interval=None,
    interpolation="nearest",
    tiletype="FULL",
):
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
        tiletype(str, optional): Tiletype. Defaults to "FULL".

    Raises:
        NotImplementedError: _description_
        RuntimeError: _description_

    Returns:
        np.darray: Field

    """
    if fileformat is None:
        fileformat, filetype = guess_file_format(filename, filetype)

    if filetype == "surf":
        if fileformat.lower() == "ascii":
            geo = AsciiSurfexFile(filename).geo
        elif fileformat.lower() == "nc":
            geo = NCSurfexFile(filename).geo
        elif geo is None:
            raise NotImplementedError(f"{fileformat} is not implemented and geo is None")
    elif geo is None:
        raise RuntimeError(
            "You need to provide a geo object. Filetype is: " + str(filetype)
        )

    sfx_io = get_surfex_io_object(
        filename, filetype=filetype, fileformat=fileformat, geo=geo
    )
    var = SurfexFileVariable(
        varname,
        validtime=validtime,
        patches=patches,
        layers=layers,
        basetime=basetime,
        interval=interval,
        datatype=datatype,
        tiletype=tiletype,
    )
    field, geo_out = sfx_io.points(
        var, geo_out, validtime=validtime, interpolation=interpolation
    )
    return field

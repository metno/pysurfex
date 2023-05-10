"""Input data for surfex binaries."""
import json
import logging
import os
import subprocess
from abc import ABC, abstractmethod


class InputDataToSurfexBinaries(ABC):
    """Abstract input data."""

    @abstractmethod
    def __init__(self):
        """Construct."""
        raise NotImplementedError

    @abstractmethod
    def prepare_input(self):
        """Prepare input."""
        raise NotImplementedError


class OutputDataFromSurfexBinaries(ABC):
    """Abstract output data."""

    @abstractmethod
    def __init__(self):
        """Construct."""
        raise NotImplementedError

    @abstractmethod
    def archive_files(self):
        """Archive files."""
        raise NotImplementedError


class JsonOutputData(OutputDataFromSurfexBinaries):
    """Output data."""

    def __init__(self, data):
        """Output data from dict.

        Args:
            data (dict): Output data.

        """
        self.data = data

    def archive_files(self):
        """Archive files."""
        for output_file, target in self.data.items():

            logging.info("%s -> %s", output_file, target)
            command = "mv"
            if isinstance(target, dict):
                for key in target:
                    logging.debug("%s %s %s", output_file, key, target[key])
                    command = target[key]
                    target = key

            cmd = command + " " + output_file + " " + target
            try:
                logging.info(cmd)
                subprocess.check_call(cmd, shell=True)  # noqaS602
            except IOError:
                logging.error("%s failed", cmd)
                raise RuntimeError(cmd + " failed") from IOError


class JsonOutputDataFromFile(JsonOutputData):
    """JSON output data."""

    def __init__(self, file):
        """Construct from json file."""
        with open(file, mode="r", encoding="utf-8") as file_handler:
            data = json.load(file_handler)
        JsonOutputData.__init__(self, data)

    def archive_files(self):
        """Archive files."""
        JsonOutputData.archive_files(self)


class JsonInputData(InputDataToSurfexBinaries):
    """JSON input data."""

    def __init__(self, data):
        """Construct input data.

        Args:
            data (dict): Input data.
        """
        self.data = data

    def prepare_input(self):
        """Prepare input."""
        for target, input_file in self.data.items():

            logging.info("%s -> %s", target, input_file)
            logging.debug(os.path.realpath(target))
            command = None
            if isinstance(input_file, dict):
                for key in input_file:
                    logging.debug(key)
                    logging.debug(input_file[key])
                    command = str(input_file[key])
                    input_file = str(key)
                    command = command.replace("@INPUT@", input_file)
                    command = command.replace("@TARGET@", target)

            if os.path.realpath(target) == os.path.realpath(input_file):
                logging.info("Target and input file is the same file")
            else:
                if command is None:
                    cmd = "ln -sf " + input_file + " " + target
                else:
                    cmd = command
                try:
                    logging.info(cmd)
                    subprocess.check_call(cmd, shell=True)  # noqaS602
                except IOError:
                    raise (cmd + " failed") from IOError

    def add_data(self, data):
        """Add data.

        Args:
            data (dict): Data to add
        """
        for key in data:
            value = data[key]
            self.data.update({key: value})


class JsonInputDataFromFile(JsonInputData):
    """JSON input data."""

    def __init__(self, file):
        """Construct JSON input data.

        Args:
            file (str): JSON file name

        """
        with open(file, mode="r", encoding="utf-8") as file_handler:
            data = json.load(file_handler)
        JsonInputData.__init__(self, data)

    def prepare_input(self):
        """Prepare input."""
        JsonInputData.prepare_input(self)


class InputDataFromNamelist(JsonInputData):
    """Binary input data for offline executables."""

    def __init__(self, nml, input_data, program, platform, basetime=None, validtime=None):
        self.nml = nml
        self.platform = platform
        self.basetime = basetime
        self.validtime = validtime
        try:
            self.data = input_data[program]
        except KeyError:
            raise RuntimeError(f"Could not find program {program}") from KeyError
        data = self.process_data()
        JsonInputData.__init__(self, data)

    @staticmethod
    def get_nml_value(nml, block, key, indices=None):
        logging.debug("Checking block=%s key=%s", block, key)
        if block in nml:
            logging.debug(nml[block])
            if key in nml[block]:
                if indices is not None:
                    if len(indices) == 2:
                        val = nml[block][key][indices[1]][indices[0]]
                    else:
                        val = nml[block][key][indices[0]]
                else:
                    val = nml[block][key]

                logging.debug("Found: %s Indices=%s", val, indices)
                return val
        return None

    @staticmethod
    def get_nml_value_from_string(nml, string, sep="#", indices=None):
        nam_section = string.split(sep)[0]
        nam_key = string.split(sep)[1]
        return InputDataFromNamelist.get_nml_value(
            nml, nam_section, nam_key, indices=indices
        )

    def substitute(self, key, val, macros=None, micro="@", check_parsing=False):
        """Substitute patterns.

        Args:
            key (str): _description_
            val (str): _description_
            macros (dict, optional): Macros. Defaults to None.
            micro (str, optional): Micro character. Defaults to "@".
            check_parsing (bool, optional): Caheck if values were substituted.

        Returns:
            dict: Substituted key=value

        """
        logging.debug(
            "Substitute key=%s and val=%s %s %s", key, val, self.basetime, self.validtime
        )
        pkey = key
        pval = val
        for spath_key, spath_val in self.platform.system_file_paths.items():
            pkey = pkey.replace(f"{micro}{spath_key}{micro}", spath_val)
            pval = pval.replace(f"{micro}{spath_key}{micro}", spath_val)
        if macros is not None:
            for macro_key, macro_val in macros.items():
                pkey = pkey.replace(f"{micro}{macro_key}{micro}", macro_val)
                pval = pval.replace(f"{micro}{macro_key}{micro}", macro_val)

        pkey = self.platform.parse_setting(
            pkey,
            validtime=self.validtime,
            basedtg=self.basetime,
            check_parsing=check_parsing,
        )
        pval = self.platform.parse_setting(
            pval,
            validtime=self.validtime,
            basedtg=self.basetime,
            check_parsing=check_parsing,
        )
        return {pkey: pval}

    def read_macro_setting(self, macro_defs, key, default=None, sep="#"):
        """Read a macro setting.

        Args:
            macro_defs (dict): Macro definition
            key (str): Macro setting to get.
            default (str, optional): Default value. Defaults to None.
            sep (str, optional): Namelist key separator. Defaults to "#".
        """
        try:
            setting = macro_defs[key]
            if isinstance(setting, str):
                if setting.find(sep) > 0:
                    setting = self.get_nml_value_from_string(self.nml, setting)
            return setting
        except KeyError:
            return default

    def extend_macro(self, key, val, macros, sep="#"):
        """Extend entries from macro.

        Args:
            key (_type_): _description_
            val (_type_): _description_
            macros (dict): Macros
            sep (str, optional): Namelist key separator. Defaults to "#".

        Raises:
            NotImplementedError: _description_
            NotImplementedError: _description_
            TypeError: _description_

        Returns:
            dict: Key, value dictionary
        """
        logging.debug("macros=%s", macros)
        if macros is None:
            return {key: val}

        processed_data = {}
        for macro, macro_types in macros.items():
            loop = {}
            for macro_type, macro_defs in macro_types.items():
                logging.debug("macro_defs=%s", macro_defs)

                if macro_type == "ekfpert":
                    nncvs = self.read_macro_setting(macro_defs, "list", sep=sep)
                    nncvs = nncvs.copy()
                    duplicate = self.read_macro_setting(macro_defs, "duplicate", sep=sep)
                    if duplicate:
                        nncvs += nncvs
                    loop.update({"0": "0"})
                    icounter1 = 1
                    icounter2 = 1
                    for nncv in nncvs:
                        if nncv == 1:
                            loop.update({str(icounter1): str(icounter2)})
                            icounter1 += 1
                        icounter2 += 1
                elif macro_type == "dict":
                    values = self.get_nml_value_from_string(self.nml, macro_defs)
                    counter = 0
                    for key, val in values.items():
                        loop.update({str(key): str(val)})
                        counter += 1

                elif macro_type == "iterator":
                    start = self.read_macro_setting(macro_defs, "start", sep=sep)
                    end = self.read_macro_setting(macro_defs, "end", sep=sep)
                    fmt = self.read_macro_setting(
                        macro_defs, "fmt", sep=sep, default=None
                    )
                    if fmt is None:
                        fmt = "{:d}"
                    for lval in range(start, end):
                        lval = fmt.format(lval)
                        loop.update({str(lval): str(lval)})
                else:
                    raise NotImplementedError

            # Loop normal macros not being nml arrays
            unprocessed_data = processed_data.copy()
            if processed_data:
                unprocessed_data = processed_data
            else:
                unprocessed_data = {key: val}

            for key, val in unprocessed_data.items():
                for vmacro1, vmacro2 in loop.items():
                    logging.debug(
                        "key=%s val=%s macro=%s vmacro1=%s vmacro2=%s",
                        key,
                        val,
                        macro,
                        vmacro1,
                        vmacro2,
                    )
                    if key.find("#") > 0:
                        key = self.get_nml_value_from_string(self.nml, key, sep=sep)
                    pkey = key.replace(f"@{macro}@", vmacro1)
                    pval = val.replace(f"@{macro}@", vmacro2)
                    processed_data.update({pkey: pval})

        logging.debug("Processed data=%s", processed_data)
        return processed_data

    def process_macro(self, key, val, macros, sep="#", indices=None):
        """Process macro.

        Args:
            key (_type_): _description_
            val (_type_): _description_
            macros (dict): Macros
            sep (str, optional): Namelist key separator. Defaults to "#".
            indices (list, optional): Process macro from namelist indices.

        Raises:
            NotImplementedError: _description_
            NotImplementedError: _description_
            TypeError: _description_

        Returns:
            dict: Key, value dictionary
        """
        logging.debug("macros=%s", macros)
        if macros is None:
            return {key: val}

        logging.debug("indices=%s", indices)
        if indices is None:
            return {key: val}

        pkey = key
        pval = val
        for macro in macros:
            lindex = None
            if len(indices) == 2:
                if macro == "DECADE":
                    lindex = indices[1]
                else:
                    lindex = indices[0]
            elif len(indices) == 1:
                lindex = indices[0]
            elif len(indices) > 2:
                raise NotImplementedError

            vmacro = None
            if lindex is not None:
                try:
                    macro_defs = macros[macro]
                    logging.debug("macro_defs=%s", macro_defs)
                except KeyError:
                    logging.warning(
                        "Macro %s not defined. Use index value %s", macro, lindex
                    )
                    vmacro = str(lindex + 1)

                if macro == "VTYPE":
                    vmacro = str(lindex + 1)
                elif "DECADE" in macros:
                    ntime = self.read_macro_setting(macro_defs, "ntime", sep=sep)
                    dec_days = int(360 / float(ntime))
                    dec_start = int(dec_days / 2)
                    dec_end = 360 + dec_start
                    dec = 0
                    for day in range(dec_start, dec_end, dec_days):
                        logging.debug("day=%s, dec=%s lindex=%s", day, dec, lindex)
                        month = int(day / 30) + 1
                        mday = int(day % 30)
                        if dec == lindex:
                            vmacro = f"{month:02d}{mday:02d}"
                        dec += 1

                logging.debug("Substitute @%s@ with %s", macro, vmacro)
                pkey = pkey.replace(f"@{macro}@", vmacro)
                pval = pval.replace(f"@{macro}@", vmacro)
        return {pkey: pval}

    def matching_value(self, data, val, sep="#", indices=None):
        """Match the value. Possibly also read namelist value.

        Args:
            data (dict): Data to check keys for
            val (str): Key to find
            sep (str, optional): Namelist separator. Defaults to "#".
            indices(list, optional): Indices in namelist

        Raises:
            TypeError: Data must be a dict or string

        Returns:
            dict: Matching entry in data.
        """
        if val is not None:
            val = str(val)
        if isinstance(data, dict):
            skeys = list(data.keys())
        elif isinstance(data, str):
            skeys = [data]
        else:
            raise TypeError

        logging.debug("Check for val=%s in skeys=%s", val, skeys)
        for skey in skeys:
            skey_ms = [skey]
            if skey.find(sep) > 0:
                logging.debug("skey=%s is a namelist variable", skey)
                skey_m = self.get_nml_value_from_string(self.nml, skey, indices=indices)

                if isinstance(skey_m, list):
                    skey_ms = skey_m
                else:
                    skey_ms = [skey_m]

            found_data = []
            for skey_m in skey_ms:
                logging.debug("matching val=%s skey=%s skey_m=%s", val, skey, skey_m)
                if val is not None and val == skey_m:
                    logging.debug("Found %s. data=%s", val, data)
                    if skey.find(sep) > 0:
                        if isinstance(data[skey], dict):
                            found_data.append(data[skey][skey_m])
                        elif isinstance(data[skey], str):
                            found_data.append(data[skey])
                        else:
                            found_data.append({skey_m: data[skey]})
                    else:
                        found_data.append(data[skey])

            logging.debug("found_data=%s", found_data)
            if len(found_data) > 0:
                if len(found_data) == 1:
                    return found_data[0]
                return found_data
        logging.warning("Value=%s not found in data", val)
        return None

    def process_data(self, sep="#"):
        """Process input definitions on files to map."""
        data = {}
        logging.debug("Process data: %s", self.data)
        for key, value in self.data.items():
            logging.debug("key=%s", key)
            # Required namelist variable
            if key.find(sep) > 0:
                val = self.get_nml_value_from_string(self.nml, key)
                setting = self.matching_value(value, val, sep=sep)
                if setting is not None:

                    unprocessed_data = {}
                    macros = None
                    if "macros" in setting:
                        macros = setting["macros"]
                    extenders = None
                    if "extenders" in setting:
                        extenders = setting["extenders"]
                    for key1, value1 in setting.items():
                        logging.debug("key1 %s value1 %s", key1, value1)
                        # Macro definition
                        if key1 == "macros" or key1 == "extenders":
                            pass
                        # Extra namelist level
                        elif key1.find(sep) > 0:
                            logging.debug("Key1=%s is a namelist variable", key1)
                            val1 = self.get_nml_value_from_string(self.nml, key1)
                            indices = None
                            if isinstance(val1, list):
                                logging.debug("Got a namelist list variable")
                                indices = []
                                vals1 = []
                                for i, vals in enumerate(val1):
                                    if isinstance(vals, list):
                                        for j in range(0, len(vals)):
                                            ind = [j, i]
                                            vals1.append(val1[i][j])
                                            indices.append(ind)
                                    else:
                                        vals1.append(val1[i])
                                        indices.append([i])
                            else:
                                vals1 = [val1]
                            for ind, val1 in enumerate(vals1):
                                logging.debug(
                                    "ind=%s Checking for val1=%s in setting=%s",
                                    ind,
                                    val1,
                                    setting,
                                )
                                if indices is not None:
                                    lindices = indices[ind]
                                else:
                                    lindices = None
                                setting1 = self.matching_value(
                                    setting, val1, sep=sep, indices=lindices
                                )
                                if setting1 is not None:
                                    logging.debug(
                                        "key1=%s val1=%s Setting1=%s",
                                        key1,
                                        val1,
                                        setting1,
                                    )
                                    if isinstance(setting1, str):
                                        unprocessed_data.update({val1: setting1})
                                    else:
                                        for key2, value2 in setting1.items():
                                            if key2.find(sep) > 0:
                                                key2 = self.get_nml_value_from_string(
                                                    self.nml, key2, indices=lindices
                                                )
                                            sub_macros = self.process_macro(
                                                key2, value2, macros, indices=lindices
                                            )
                                            logging.debug(
                                                "Substituted macros %s", sub_macros
                                            )
                                            unprocessed_data.update(sub_macros)
                                else:
                                    logging.warning(
                                        "Could not match namelist variable %s", key1
                                    )
                        # Direct defintions
                        else:
                            if isinstance(value1, str):
                                logging.debug("key1=%s value1=%s", key1, value1)
                                unprocessed_data.update({key1: value1})
                            else:
                                logging.warning("Did not find a string %s", type(value1))

                    logging.debug(
                        "Manipulate unprocesessed values with general"
                        " or user-defined macros %s",
                        unprocessed_data,
                    )
                    for key, value in unprocessed_data.items():
                        logging.debug("key=%s value=%s", key, value)
                        processed_values = self.extend_macro(key, value, extenders)
                        for key, val in processed_values.items():
                            sub_values = self.substitute(key, val)
                            data.update(sub_values)
                else:
                    logging.warning("Could not find the namelist value=%s", val)
            else:
                logging.warning("Expected a namelist variable")
        return data

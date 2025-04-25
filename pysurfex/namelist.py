"""Namelist."""
import logging
import re
import os
import yaml

import f90nml

from .binary_input import InputDataFromNamelist


class NamelistGenerator(object):
    """Namelist class."""

    def __init__(self, program, nml, macros=None, micro="@"):
        """Construct a base namelists class.

        Args:
            program (str): Which surfex binary you want to run ["pgd", "prep", "offline", "soda"]
            definitions (dict): Namelist definitions
            assemble(dict): Assembly order. Defines the configuration
            macros(dict, optional): Macros
        """
        self.program = program
        self.nml = nml
        if macros is None:
            macros = {}
        self.macros = macros
        self.micro = micro

    def get_namelist(self):
        """Get namelist."""
        for bkey, block in self.nml.items():
            items = {}
            for key, val in block.items():
                items.update({key: val})
                for mkey, mval in self.macros.items():
                    if isinstance(val, str):
                        val = val.replace(f"{self.micro}{mkey}{self.micro}", f"{mval}")
                        if val == f"{mval}":
                            if isinstance(mval, int) or isinstance(mval, float):
                                val = mval
                    items.update({key: val})
            self.nml[bkey].update(items)
        return self.nml

    def input_data_from_namelist(
        self, input_data, platform, basetime=None, validtime=None, check_existence=True
    ):
        """Construct a base namelists class to be implemented by namelist implementations.

        Args:
            input_data (dict): Input data definitions
            platform (SystemFilePaths): Platform specific settings
            basetime (as_datetime, optional): Base time
            validtime (as_datetime, optional): Valid time

        Returns:
            data_obj (InputDataFromNamelist): Input data from namelist

        """
        logging.info("Set input data from namelist for program: %s", self.program)

        def process_macros(data):
            for k, v in data.copy().items():
                if isinstance(v, dict):     # For DICT
                    data[k] = process_macros(v)
                elif isinstance(v, list):   # For LIST
                    data[k] = [process_macros(i) for i in v]
                else:
                    for macro, mval in self.macros.items():
                        if isinstance(v, str) and isinstance(mval, str):
                            v = v.replace(f"{self.micro}{macro}{self.micro}", mval)
                    data[k] = v
            return data

        # Substitute macros in binary input data
        if self.macros is not None:
            input_data = process_macros(input_data)

        data_obj = InputDataFromNamelist(
            self.nml,
            input_data,
            self.program,
            platform,
            basetime=basetime,
            validtime=validtime,
        )

        if check_existence:
            for __, val in data_obj.data.items():
                if not os.path.exists(val):
                    raise FileNotFoundError()
        return data_obj

    def write(self, output_file, uppercase=True, true_repr=".TRUE.", false_repr=".FALSE."):
        """Generate the namelists for 'target'.

        Args:
            output_file (str): where to write the result (OPTIONS.nam, fort.4 or EXSEG1.nam typically)

        """
        self.nml.uppercase = uppercase
        self.nml.true_repr = true_repr
        self.nml.false_repr = false_repr
        self.nml.write(output_file, force=True)
        logging.debug("Wrote: %s", output_file)

    @staticmethod
    def lower_case_namelist_dict(dict_in):
        """Lower case namelist.

        Args:
            dict_in (dict): Namelist dictionary to lower case

        Returns:
            dict: Namelist in lower case

        """
        new_dict = {}
        for key in dict_in:
            lower_case_dict = {}
            for key2 in dict_in[key]:
                lower_case_dict.update({key2.lower(): dict_in[key][key2]})
            new_dict.update({key.lower(): lower_case_dict})
        return new_dict


class NamelistGeneratorFromNamelistFile(NamelistGenerator):
    """Namelist class."""

    def __init__(self, program, nml_file, macros=None, micro="@"):
        parser = f90nml.Parser()
        nml = parser.read(nml_file)
        NamelistGenerator.__init__(self, program, nml, macros=macros, micro=micro)


class NamelistGeneratorAssemble(NamelistGenerator):
    """Namelist class."""

    def __init__(self, program, definitions, assemble, macros=None, micro="@"):
        """Construct a base namelists class.

        Args:
            program (str): Which surfex binary you want to run ["pgd", "prep", "offline", "soda"]
            definitions (dict): Namelist definitions
            assemble(dict): Assembly order. Defines the configuration
            macros(dict, optional): Macros
        """
        self.nldict = definitions
        if macros is None:
            macros = {}
        self.macros = macros

        self.assemble = assemble
        logging.info("Namelist blocks for program %s: %s", program, self.assemble)
        nlres = self.assemble_namelist(program)
        nml = f90nml.Namelist(nlres)
        NamelistGenerator.__init__(self, program, nml, macros=macros, micro=micro)


    def assemble_namelist(self, program):
        """Generate the namelists for 'target'.

        Raises:
            KeyError: Key not found

        Returns:
            nlres (dict): Assembled namelist

        """
        # Read namelist file with all the categories

        # Check target is valid
        cndict = self.assemble[program]
        nldict = self.nldict

        # Start with empty result dictionary
        nlres = {}

        # Assemble the target namelists based on the given category order
        for item in self.flatten_list(cndict):
            catg = item
            # variable substitution removed at this level (may be resurrected)
            # assemble namelists for this category
            if catg in nldict:
                for nl in nldict[catg]:
                    if nl not in nlres:
                        # create the result namelist dict
                        nlres[nl] = {}
                    if catg == "rm{" + nl + "}":
                        # clear/remove the given namelist (but not used for now)
                        nlres[nl].clear()
                    else:
                        for key in nldict[catg][nl]:
                            val = nldict[catg][nl][key]
                            finval = val
                            # Replace ${var-def} with value from config, possibly macro-expanded
                            # For now assumes only one subst. per line, could be generalized if needed
                            if str(finval).find("$") >= 0:
                                m = re.search(
                                    r"^([^\$]*)\$\{([\w\#]+)\-?([^}]*)\}(.*)", str(val)
                                )
                                if m:
                                    pre = m.group(1)
                                    nam = m.group(2)
                                    defval = m.group(3)
                                    post = m.group(4)
                                    logging.debug("macros=%s", self.macros)
                                    logging.debug("look for nam=%s", nam)
                                    try:
                                        repval = self.macros[nam]
                                    except KeyError:
                                        repval = None
                                    if repval is None:
                                        if defval != "":
                                            logging.debug(
                                                "Using default value %s for '%s'",
                                                defval,
                                                nam,
                                            )
                                            repval = self.find_num(defval)
                                        else:
                                            logging.debug("No value found for: '%s'", nam)
                                    else:
                                        logging.debug(
                                            "Replaced %s with: %s", nam, str(repval)
                                        )
                                    if isinstance(repval, str):
                                        finval = str(pre) + str(repval) + str(post)
                                    else:
                                        finval = repval
                                else:
                                    raise KeyError(val)
                            nlres[nl][key] = finval
            else:
                logging.info("Category %s not found in definitions", catg)
        return nlres

    @staticmethod
    def flatten_list(li):
        """Recursively flatten a list of lists (of lists)."""
        if li == []:
            return li
        if isinstance(li[0], list):
            return NamelistGeneratorAssemble.flatten_list(li[0]) + NamelistGeneratorAssemble.flatten_list(
                li[1:]
            )
        return li[:1] + NamelistGeneratorAssemble.flatten_list(li[1:])

    @staticmethod
    def find_num(s):
        """Purpose: un-quote numbers."""
        try:
            i = int(s)
            return i
        except ValueError:
            pass
        try:
            f = float(s)
            return f
        except ValueError:
            return s


class NamelistGeneratorAssembleFromFiles(NamelistGeneratorAssemble):

    """Namelist class."""

    def __init__(self, program, definitions, assemble, macros=None, micro="@"):
        """Construct a base namelists class.

        Args:
            program (str): Which surfex binary you want to run ["pgd", "prep", "offline", "soda"]
            definitions (str): Namelist definitions
            assemble(str)): Assembly order. Defines the configuration
            macros(dict, optional): Macros
        """
        with open(definitions, mode="r", encoding="utf8") as file_handler:
            definitions = yaml.safe_load(file_handler)
        with open(assemble, mode="r", encoding="utf8") as file_handler:
            assemble = yaml.safe_load(file_handler)
        NamelistGeneratorAssemble.__init__(self, program, definitions, assemble, macros=macros, micro=micro)

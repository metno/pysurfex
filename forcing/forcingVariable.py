import abc

"""
Top level container for all surfex variables

"""

class ForcingVariable(object):

    def __init__(self,varname):
        self.varname=varname
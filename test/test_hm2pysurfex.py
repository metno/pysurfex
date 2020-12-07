import unittest
import surfex


class Hm2PysurfexTest(unittest.TestCase):

    @staticmethod
    def test_hm2pysurfex_client():
        argv = [
            "-c", "surfex/cfg/config_exp_surfex.toml",
            "-o", "unittest_config_from_hm.toml",
            "-e", "test/settings/hm_env.json"
        ]
        kwargs = surfex.parse_args_hm2pysurfex(argv)
        surfex.hm2pysurfex(**kwargs)

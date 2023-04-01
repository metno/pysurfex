"""Harmonie environment to pysurfex."""
import unittest
import logging


from surfex.cli import hm2pysurfex


logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                    level=logging.DEBUG)


class Hm2PysurfexTest(unittest.TestCase):
    """Harmonie environment to pysurfex."""

    @staticmethod
    def test_hm2pysurfex_client():
        """Test harmonie to pysurfex client."""
        argv = [
            "-c", "surfex/cfg/config_exp_surfex.toml",
            "-o", "unittest_config_from_hm.toml",
            "-e", "test/settings/hm_env.json"
        ]
        hm2pysurfex(argv=argv)

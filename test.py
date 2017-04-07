import unittest


class MyTest(unittest.TestCase):
   def test_something(self):
       self.assertEqual(1, 1)
   def test_other(self):
       self.assertEqual(1, 2)

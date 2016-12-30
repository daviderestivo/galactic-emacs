# Copyright (C) 2012-  Takafumi Arakaki

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


from ..utils import ThreadedIterator, LockingDict

from .utils import BaseTestCase


class TestThreadedIterator(BaseTestCase):

    def check_identity(self, iterable):
        lst = list(iterable)
        self.assertEqual(list(ThreadedIterator(lst)), lst)

    def test_empty(self):
        self.check_identity([])

    def test_range_1(self):
        self.check_identity(range(1))

    def test_range_7(self):
        self.check_identity(range(7))


class TestLockingDict(BaseTestCase):

    def setUp(self):
        self.ld = LockingDict()

    def check_set_items(self, items):
        for (k, v) in items:
            self.ld[k] = v
        self.assertEqual(dict(**self.ld), dict(items))

    def test_simple_set_items(self):
        self.check_set_items(dict(a=1, b=2, c=3).items())

    def test_simple_del_items(self):
        self.test_simple_set_items()
        ld = self.ld
        del ld['a']
        del ld['b']
        self.assertEqual(dict(**self.ld), dict(c=3))

    def test_simple_pop_items(self):
        self.test_simple_set_items()
        ld = self.ld
        self.assertEqual(ld.pop('a'), 1)
        self.assertEqual(ld.pop('b'), 2)
        self.assertEqual(dict(**self.ld), dict(c=3))

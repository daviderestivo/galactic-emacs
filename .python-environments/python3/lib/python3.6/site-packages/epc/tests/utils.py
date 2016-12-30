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


import os
import sys
import functools
import io

try:
    import unittest
    unittest.TestCase.assertIs
except AttributeError:
    import unittest2 as unittest
from contextlib import contextmanager

from ..py3compat import Queue, PY3
from ..utils import newthread


@contextmanager
def mockedattr(object, name, replace):
    """
    Mock `object.name` attribute using `replace`.
    """
    original = getattr(object, name)
    try:
        setattr(object, name, replace)
        yield
    finally:
        setattr(object, name, original)


def logging_to_stdout(logger):
    # it assumes that 0-th hander is the only one stream handler...
    return mockedattr(logger.handlers[0], 'stream', sys.stdout)


def streamio():
    """
    Return `io.StringIO` for Python 3, otherwise `io.BytesIO`.
    """
    if PY3:
        return io.StringIO()
    else:
        return io.BytesIO()


class CaptureStdIO(object):

    def __enter__(self):
        self._orig_stdin = sys.stdin
        self._orig_stdout = sys.stdout
        self._orig_stderr = sys.stderr

        self.stdin = sys.stdin = streamio()
        self.stdout = sys.stdout = streamio()
        self.stderr = sys.stderr = streamio()
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        sys.stdin = self._orig_stdin
        sys.stdout = self._orig_stdout
        sys.stderr = self._orig_stderr

    def read_stdout(self):
        self.stdout.seek(0)
        return self.stdout.read()

    def read_stderr(self):
        self.stderr.seek(0)
        return self.stderr.read()


class BaseTestCase(unittest.TestCase):

    TRAVIS = os.getenv('TRAVIS')

    if TRAVIS:
        timeout = 10
    else:
        timeout = 1


def skip(reason):
    from nose import SkipTest

    def decorator(func):
        @functools.wraps(func)
        def wrapper(*args, **kwds):
            raise SkipTest("Skipping {0} because: {1}"
                           .format(func.__name__, reason))
        return wrapper
    return decorator


def post_mortem_in_thread(traceback):
    """
    `pdb.post_mortem` that can be used in a daemon thread.

    Put the following in the `except`-block::

        import sys
        from epc.tests.utils import post_mortem_in_thread
        exc_info = sys.exc_info()
        post_mortem_in_thread(exc_info[2])

    """
    import pdb
    blocker = Queue.Queue()
    thread = newthread(target=blocker.get)
    thread.daemon = False
    thread.start()
    pdb.post_mortem(traceback)
    blocker.put(None)

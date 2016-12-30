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
import nose

from ..client import EPCClient
from ..server import ThreadingEPCServer
from ..handler import ReturnError
from ..utils import newthread, callwith
from ..py3compat import Queue
from .utils import BaseTestCase, logging_to_stdout


def next_fib(x, fib):
    if x < 2:
        return x
    return fib(x - 1) + fib(x - 2)


def fib(x):
    return next_fib(x, fib)


class ThreadingPy2Py(object):

    """
    A class to setup connected EPC server and client in one process.

    This class is useful to use as a mix-in for test cases.

    """

    def setup_connection(self, **kwds):
        self.server = ThreadingEPCServer(('localhost', 0), **kwds)
        self.server.daemon_threads = True
        self.server_thread = newthread(self, target=self.server.serve_forever)
        self.server_thread.start()

        self.client_queue = q = Queue.Queue()
        self.server.handle_client_connect = q.put

        self.client = EPCClient(self.server.server_address, **kwds)

    def teardown_connection(self):
        self.client.close()
        self.server.shutdown()
        self.server.server_close()

    def wait_until_client_is_connected(self):
        if not self.client_ready:
            self.client_queue.get(timeout=1)
            self.client_ready = True

    client_ready = False


class TestEPCPy2Py(ThreadingPy2Py, BaseTestCase):

    def setUp(self):
        ThreadingEPCServer.allow_reuse_address = True
        self.setup_connection()

        @self.client.register_function
        @self.server.register_function
        def echo(*a):
            """Return argument unchanged."""
            return a

        @self.client.register_function
        @self.server.register_function
        def bad_method(*_):
            """This is a bad method.  Don't call!"""
            raise ValueError("This is a bad method!")

        @self.server.register_function
        def ping_server(x):
            return self.server.clients[0].call_sync('pong_client', [x])

        @self.client.register_function
        def pong_client(x):
            return self.client.call_sync('echo', [x])

        @self.client.register_function
        def ping_client(x):
            return self.client.call_sync('pong_server', [x])

        @self.server.register_function
        def pong_server(x):
            return self.server.clients[0].call_sync('echo', [x])

        @self.server.register_function
        def fib_server(x):
            c = self.server.clients[0].call_sync
            return next_fib(x, lambda x: c('fib_client', [x]))

        @self.client.register_function
        def fib_client(x):
            c = self.client.call_sync
            return next_fib(x, lambda x: c('fib_server', [x]))

    def tearDown(self):
        self.teardown_connection()

    def assert_call_return(self, call, method, args, reply, **kwds):
        timeout = kwds.get('timeout', self.timeout)
        self.assertEqual(call(method, args, timeout=timeout), reply)

    def assert_client_return(self, method, args, reply, **kwds):
        self.assert_call_return(self.client.call_sync,
                                method, args, reply, **kwds)

    def assert_server_return(self, method, args, reply, **kwds):
        self.wait_until_client_is_connected()
        self.assert_call_return(self.server.clients[0].call_sync,
                                method, args, reply, **kwds)

    def check_bad_method(self, call_sync):
        cm = logging_to_stdout(self.server.logger)
        call_sync = callwith(cm)(call_sync)
        self.assertRaises(ReturnError, call_sync, 'bad_method', [55])

    def test_client_calls_server_echo(self):
        self.assert_client_return('echo', [55], [55])

    def test_client_calls_server_bad_method(self):
        self.check_bad_method(self.client.call_sync)

    def test_server_calls_client_echo(self):
        self.assert_server_return('echo', [55], [55])

    def test_server_calls_client_bad_method(self):
        self.wait_until_client_is_connected()
        self.check_bad_method(self.server.clients[0].call_sync)

    max_message_limit = int('f' * 6, 16) + 1  # 16MB
    large_data_limit = max_message_limit \
        / float(os.getenv('PYEPC_TEST_LARGE_DATA_DISCOUNT', '128'))
    large_data_limit = int(large_data_limit)
    """
    Environment variable PYEPC_TEST_LARGE_DATA_DISCOUNT controls
    how large "large data" must be.  Default is ``2 ** 7`` times
    smaller than the maximum message length (16 MB).  Setting
    it to 1 must *not* fail.  However, it takes long time to finish
    the test (typically 100 sec when I tried).  Setting this value
    to less than one (e.g., 0.9) *must* fail the tests.
    """

    def check_large_data(self, assert_return):
        margin = 100  # for parenthesis, "call", uid, etc.
        data = "x" * (self.large_data_limit - margin)
        timeout = self.timeout * 100
        assert_return('echo', [data], [data], timeout=timeout)

    def test_client_sends_large_data(self):
        self.check_large_data(self.assert_client_return)

    def test_server_sends_large_data(self):
        self.check_large_data(self.assert_server_return)

    def test_client_ping_pong(self):
        self.assert_client_return('ping_server', [55], [55])

    def test_server_ping_pong(self):
        self.assert_server_return('ping_client', [55], [55])

    def test_client_close_should_not_fail_even_if_not_used(self):
        pass

    fibonacci = list(map(fib, range(12)))
    fibonacci_min = 2
    """
    The Fibonacci test must succeeds at least until this index.
    """

    def check_fib(self, assert_return, method):
        try:
            for (i, f) in enumerate(self.fibonacci):
                assert_return(method, [i], f)
        except Queue.Empty:
            if i > self.fibonacci_min:
                raise nose.SkipTest(
                    "Test for {0} fails at {1} (> {2}), but it's OK."
                    .format(method, i, self.fibonacci_min))
            else:
                raise   # not OK

    def test_client_fib(self):
        self.check_fib(self.assert_client_return, 'fib_server')

    def test_server_fib(self):
        self.check_fib(self.assert_server_return, 'fib_client')

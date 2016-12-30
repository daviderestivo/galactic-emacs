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


import io

from sexpdata import Symbol

from ..client import EPCClient
from ..handler import encode_message, unpack_message, BlockingCallback, \
    ReturnError, EPCError
from ..py3compat import Queue
from .utils import BaseTestCase


class FakeFile(object):
    pass


class FakeSocket(object):

    def __init__(self):
        self._queue = Queue.Queue()
        self._buffer = io.BytesIO()
        self.sent_message = Queue.Queue()
        self._alive = True

    def makefile(self, mode, *_):
        ff = FakeFile()
        ff.closed = False
        ff.close = lambda: None
        ff.flush = lambda: None
        if 'r' in mode:
            ff.read = self.recv
            return ff
        elif 'w' in mode:
            ff.write = self.sendall
            return ff

    def append(self, byte):
        self._queue.put(byte)

    def _pull(self):
        byte = self._queue.get()
        pos = self._buffer.tell()
        self._buffer.write(byte)
        self._buffer.seek(pos)

    def recv(self, bufsize):
        while True:
            if not self._alive:
                return ''
            got = self._buffer.read(bufsize)
            if got:
                return got
            # Do not go to the next loop until some byte is appended
            # to the queue:
            self._pull()

    def sendall(self, string):
        self.sent_message.put(string)

    def close(self):
        self._alive = False
        self.append(''.encode('ascii'))


class TestClient(BaseTestCase):

    def setUp(self):
        self.fsock = FakeSocket()
        self.next_reply = []
        self.client = EPCClient(self.fsock)

        @self.client.register_function
        def echo(*a):
            """Return argument unchanged."""
            return a

    def tearDown(self):
        self.client.socket.close()  # connection is closed by server

    def set_next_reply(self, *args):
        self.next_reply.append(encode_message(*args))

    def request(self, name, *args):
        bc = BlockingCallback()
        getattr(self.client, name)(*args, **bc.cbs)
        self.fsock.append(self.next_reply.pop(0))  # reply comes after call!
        return bc.result(timeout=self.timeout)

    def sent_message(self):
        raw = self.fsock.sent_message.get(timeout=self.timeout)
        (name, uid, rest) = unpack_message(raw[6:])
        if name == 'call':
            rest[0] = rest[0].value()
        return [name, uid] + rest

    def check_sent_message(self, name, uid, args):
        sent = self.sent_message()
        self.assertEqual(sent, [name, uid] + list(args))

    def check_return(self, desired_return, name, *args):
        uid = 1
        self.set_next_reply('return', uid, desired_return)
        got = self.request(name, *args)
        self.assertEqual(got, desired_return)
        self.check_sent_message(name, uid, args)

    def test_call_return(self):
        self.check_return('some value', 'call', 'dummy', [1, 2, 3])

    def test_methods_return(self):
        self.check_return([[Symbol('dummy'), [], "document"]], 'methods')

    def check_return_error(self, reply_name, name, *args):
        uid = 1
        reply = 'error value'
        eclass = ReturnError if reply_name == 'return-error' else EPCError
        error = eclass(reply)
        self.set_next_reply(reply_name, uid, reply)
        try:
            self.request(name, *args)
            assert False, 'self.client.{0}({1}) should raise an error' \
                .format(name, args)
        except Exception as got:
            self.assertIsInstance(got, type(error))
            self.assertEqual(got.args, error.args)
        self.check_sent_message(name, uid, args)

    def test_call_return_error(self):
        self.check_return_error('return-error', 'call', 'dummy', [1, 2, 3])

    def test_call_epc_error(self):
        self.check_return_error('epc-error', 'call', 'dummy', [1, 2, 3])

    def test_methods_return_error(self):
        self.check_return_error('return-error', 'methods')

    def test_methods_epc_error(self):
        self.check_return_error('epc-error', 'methods')

    def test_echo(self):
        uid = 1
        self.fsock.append(encode_message('call', uid, Symbol('echo'), [55]))
        self.check_sent_message('return', uid, [[55]])


class TestClientClosedByClient(TestClient):

    def tearDown(self):
        self.client.close()

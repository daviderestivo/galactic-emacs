# -*- coding: utf-8 -*-

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


import socket

from sexpdata import Symbol, loads

from ..server import ThreadingEPCServer
from ..utils import newthread
from ..handler import encode_string, encode_object, BlockingCallback, \
    ReturnError, EPCError, ReturnErrorCallerUnknown, EPCErrorCallerUnknown, \
    CallerUnknown
from ..py3compat import utf8, Queue, nested
from .utils import mockedattr, logging_to_stdout, CaptureStdIO, BaseTestCase, \
    streamio


class TestEPCServerMisc(BaseTestCase):

    """
    Test that can be done without client.
    """

    def setUp(self):
        # See: http://stackoverflow.com/questions/7720953
        ThreadingEPCServer.allow_reuse_address = True
        self.server = ThreadingEPCServer(('localhost', 0))
        self.server_thread = newthread(self, target=self.server.serve_forever)
        self.server_thread.start()

    def tearDown(self):
        self.server.shutdown()
        self.server.server_close()

    def test_print_port(self):
        stream = streamio()
        self.server.print_port(stream)
        self.assertEqual(stream.getvalue(),
                         '{0}\n'.format(self.server.server_address[1]))


class BaseEPCServerTestCase(BaseTestCase):

    def setUp(self):
        # See: http://stackoverflow.com/questions/7720953
        ThreadingEPCServer.allow_reuse_address = True
        self.server = ThreadingEPCServer(('localhost', 0))
        self.server_thread = newthread(self, target=self.server.serve_forever)
        self.server_thread.start()

        def echo(*a):
            """Return argument unchanged."""
            return a

        def bad_method(*_):
            """This is a bad method.  Don't call!"""
            raise self.error_to_throw
        self.error_to_throw = ValueError("This is a bad method!")

        self.server.register_function(echo)
        self.server.register_function(bad_method)

        self.client = socket.create_connection(self.server.server_address)
        self.client.settimeout(self.timeout)

    def tearDown(self):
        self.client.shutdown(socket.SHUT_RDWR)
        self.client.close()
        self.server.shutdown()
        self.server.server_close()

    def receive_message(self):
        result = self.client.recv(1024)
        self.assertEqual(int(result[:6], 16), len(result[6:]))
        return loads(result[6:].decode())  # skip the length part

    def client_send(self, string):
        self.client.send(encode_string(string))

    def check_echo(self):
        self.client_send('(call 1 echo (55))')
        result = self.client.recv(1024)
        self.assertEqual(encode_string('(return 1 (55))'), result)


class TestEPCServerRequestHandling(BaseEPCServerTestCase):

    """
    Test that EPCServer handles request from client properly.
    """

    def test_echo(self):
        self.check_echo()

    def test_error_in_method(self):
        with logging_to_stdout(self.server.logger):
            self.client_send('(call 2 bad_method nil)')
            result = self.client.recv(1024)
        expected = encode_object([
            Symbol('return-error'), 2, repr(self.error_to_throw)])
        self.assertEqual(result, expected)

    def test_no_such_method(self):
        with logging_to_stdout(self.server.logger):
            self.client_send('(call 3 no_such_method nil)')
            reply = self.receive_message()
        self.assertEqual(reply[0], Symbol('epc-error'))
        self.assertEqual(reply[1], 3)
        assert 'No such method' in reply[2]

    def test_methods(self):
        self.client_send('(methods 4)')
        reply = self.receive_message()
        self.assertEqual(reply[0], Symbol('return'))
        self.assertEqual(reply[1], 4)
        method = dict((m[0].value(), m[1:]) for m in reply[2])
        self.assertEqual(set(method), set(['echo', 'bad_method']))

        actual_docs = dict(
            (n, doc) for (n, (_, doc)) in method.items())
        desired_docs = dict(
            (n, f.__doc__) for (n, f) in self.server.funcs.items())
        self.assertEqual(actual_docs, desired_docs)

    def test_unicode_message(self):
        s = "日本語能力!!ソﾊﾝｶｸ"
        self.client_send(utf8('(call 1 echo ("{0}"))'.format(s)))
        result = self.client.recv(1024)
        self.assertEqual(encode_string(utf8('(return 1 ("{0}"))'.format(s))),
                         result)

    def test_invalid_sexp(self):
        with logging_to_stdout(self.server.logger):
            self.client_send('(((invalid sexp!')
            reply = self.receive_message()
        self.assertEqual(reply[0].value(), Symbol('epc-error').value())
        self.assertEqual(reply[1], [])  # uid
        assert 'Not enough closing brackets.' in reply[2]

    def check_caller_unkown(self, message, eclass, eargs):
        self.check_echo()  # to establish connection to client
        called_with = Queue.Queue()
        with nested(mockedattr(self.server.clients[0],
                               'handle_error', called_with.put),
                    logging_to_stdout(self.server.logger)):
            self.client_send(message)
            error = called_with.get(True, 1)
        self.assertIsInstance(error, eclass)
        self.assertEqual(error.args, eargs)

    def test_return_caller_unkown(self):
        self.check_caller_unkown(
            '(return 0 ("some" "value"))',  # uid=0 is always unkown
            CallerUnknown, (['some', 'value'],))

    def test_return_error_caller_unkown(self):
        self.check_caller_unkown(
            '(return-error nil "message")',
            ReturnErrorCallerUnknown, ('message',))

    def test_epc_error_caller_unkown(self):
        self.check_caller_unkown(
            '(epc-error nil "message")',
            EPCErrorCallerUnknown, ('message',))

    def check_invalid_call(self, make_call):

        # These are not necessary for the actual test, but rather
        # to make sure that the server stays in the context of
        # `logging_to_stdout` until the error is handled.  See
        # `called_with.get` below.
        def handle_error(err):
            self.assertTrue(orig_handle_error(err))
            called_with.put(err)
            return True
        self.check_echo()  # to fetch handler
        handler = self.server.clients[0]
        orig_handle_error = handler.handle_error
        called_with = Queue.Queue()

        # Here comes the actual test:
        uid = 1
        with nested(logging_to_stdout(self.server.logger),
                    mockedattr(handler, 'handle_error', handle_error)):
            self.client_send(make_call(uid))
            reply = self.receive_message()
            called_with.get(timeout=1)  # wait until the error got handled
        self.assertEqual(reply[0], Symbol('epc-error'))
        self.assertEqual(reply[1], uid)

    def test_invalid_call_not_enough_arguments(self):
        self.check_invalid_call('(call {0} echo)'.format)

    def test_invalid_call_too_many_arguments(self):
        self.check_invalid_call(
            '(call {0} echo "value" "extra" "value")'.format)

    def test_invalid_methods_too_many_arguments(self):
        self.check_invalid_call('(methods {0} "extra value")'.format)

    def test_log_traceback(self):
        stdio = CaptureStdIO()
        with nested(stdio, mockedattr(self.server, 'log_traceback', True)):
            self.test_error_in_method()
        log = stdio.read_stdout()
        self.assertIn('ValueError: This is a bad method!', log)
        self.assertIn('raise self.error_to_throw', log)


class TestEPCServerCallClient(BaseEPCServerTestCase):

    def setUp(self):
        super(TestEPCServerCallClient, self).setUp()
        self.check_echo()  # to start connection, client must send something
        self.handler = self.server.clients[0]

        self.callback_called_with = Queue.Queue()
        self.callback = self.callback_called_with.put

        self.errback_called_with = Queue.Queue()
        self.errback = self.errback_called_with.put

    def check_call_client_dummy_method(self):
        (call, uid, meth, args) = self.receive_message()
        self.assertIsInstance(uid, int)
        self.assertEqual([call, uid, meth, args],
                         [Symbol('call'), uid, Symbol('dummy'), [55]])
        return uid

    def test_call_client_dummy_method(self):
        self.handler.call('dummy', [55], self.callback, self.errback)
        uid = self.check_call_client_dummy_method()
        self.client_send('(return {0} 123)'.format(uid))
        reply = self.callback_called_with.get(True, 1)
        self.assertEqual(reply, 123)

    def test_call_client_methods_info(self):
        self.handler.methods(self.callback)
        (methods, uid) = self.receive_message()
        self.assertEqual(methods.value(), 'methods')
        self.client_send('(return {0} ((dummy () "")))'.format(uid))
        reply = self.callback_called_with.get(True, 1)
        self.assertEqual(reply, [[Symbol('dummy'), [], ""]])

    def client_send_error(self, ename, uid, message):
        self.client_send('({0} {1} "{2}")'.format(ename, uid, message))

    def check_call_client_error(self, ename, eclass, message=utf8("message")):
        self.handler.call('dummy', [55], self.callback, self.errback)
        uid = self.check_call_client_dummy_method()
        self.client_send_error(ename, uid, message)
        reply = self.errback_called_with.get(True, 1)
        self.assertIsInstance(reply, eclass)
        self.assertEqual(reply.args, (message,))

    def test_call_client_return_error(self):
        self.check_call_client_error('return-error', ReturnError)

    def test_call_client_epc_error(self):
        self.check_call_client_error('epc-error', EPCError)

    def check_dont_send_error_back(self, ename, eclass,
                                   message=utf8("message")):
        self.handler.call('dummy', [55])  # no callbacks!
        uid = self.check_call_client_dummy_method()
        with logging_to_stdout(self.server.logger):
            self.client_send_error(ename, uid, message)
            try:
                result = self.client.recv(1024)
                self.assertEqual(result, '')  # nothing goes to client
            except socket.timeout:
                pass

    def test_dont_send_return_error_back(self):
        self.check_dont_send_error_back('return-error', ReturnError)

    def test_dont_send_epc_error_back(self):
        self.check_dont_send_error_back('epc-error', EPCError)

    def check_invalid_reply(self, make_reply, should_raise=EPCError):
        bc = BlockingCallback()
        self.handler.call('dummy', [55], **bc.cbs)
        uid = self.check_call_client_dummy_method()
        with logging_to_stdout(self.server.logger):
            self.client_send(make_reply(uid))
            self.assertRaises(should_raise, bc.result, timeout=self.timeout)

    def test_invalid_return_not_enough_arguments(self):
        self.check_invalid_reply('(return {0})'.format)

    def test_invalid_return_too_many_arguments(self):
        self.check_invalid_reply(
            '(return {0} "value" "extra" "value")'.format)

    def test_invalid_return_error_not_enough_arguments(self):
        self.check_invalid_reply('(return-error {0})'.format, ReturnError)

    def test_invalid_return_error_too_many_arguments(self):
        self.check_invalid_reply(
            '(return-error {0} "value" "extra" "value")'.format,
            ReturnError)

    def test_invalid_epc_error_not_enough_arguments(self):
        self.check_invalid_reply('(epc-error {0})'.format)

    def test_invalid_epc_error_too_many_arguments(self):
        self.check_invalid_reply(
            '(epc-error {0} "value" "extra" "value")'.format)

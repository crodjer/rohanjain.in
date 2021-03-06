+++
title = "Testing tornado websockets without third party clients"
slug = "ws-test"
date = "2015-08-22T00:00:00Z"
publishdate = "2015-08-22T00:00:00Z"
+++

Recently, I built an encrypted [chat service][qotr], which was based on
[tornado][] and [ember.js][]. The project itself had grave security issues, so I
shut it down, but while working on it I learned a few new things and testing
websockets with tornado is one of them.

Most of the material out there for this suggests developing separate client
based tests, which I didn't want to do. Eventually, I figured out that tornado
already provides all the utilities to do unit/integration tests for websockets.

First, we will need a websockets based echo server to test, lets call it
`ws.py`. The websocket handler would be:

```python
from tornado import web, websocket

class Echo(websocket.WebSocketHandler):

    # Open allows for any number arguments, unlike what pylint thinks.
    # pylint: disable=W0221
    def open(self):
        self.write_message('hello')

    def on_message(self, message):
        self.write_message(message)

    def on_close(self):
        self.write_message('bye')
```

Lets define an application which uses the above handler:

```python
APP = web.Application([
    (r"/", Echo),
])

if __name__ == "__main__":
    APP.listen(5000)
```

Now, we will test the application out. Create a file, say `test_ws.py`:

```python
from tornado import testing, httpserver, gen, websocket
from ws import APP

class TestChatHandler(testing.AsyncTestCase):
    pass
```

We use tornado's testing wrapper for the integration it provides with the event
loop. Lets tell unittest how to setup the tests:

```python
class TestChatHandler(testing.AsyncTestCase):

    def setUp(self):
        super(TestChatHandler, self).setUp()
        server = httpserver.HTTPServer(APP)
        socket, self.port = testing.bind_unused_port()
        server.add_socket(socket)
```

We create a http server out of our application and get a socket bound to an
unused port. We then ask the server to accept on the created socket. Don't
forget the `super` call, it ensures that the ioloop gets created. `unittest`
will now ensure that a server and an ioloop is up and running before running
tests.

Moving forward, we need to define a helper for creating a websocket connection
to the server. Tornado websocket provides a handly websocket client. It can be
created with `websocket.websocket_connect`.

```python
    def _mk_connection(self):
        return websocket.websocket_connect(
            'ws://localhost:{}/'.format(self.port)
        )
```

We can write a simple test for this:

```python
    @testing.gen_test
    def test_hello(self):
        c = yield self._mk_connection()
        # Get the initial hello from the server.
        response = yield c.read_message()
        # Make sure that we got a 'hello' not 'bye'
        self.assertEqual('hello', response)
```

`testing.gen_test` is a wrapper over tornado's `gen.coroutine`. It runs the test
synchronously under the ioloop that `testing.AsyncTestCase` creates in
`setUp`. The test checks for the 'hello' message that we expect from the server
on connection.  `yield` makes sure that we for the response from the
server. Note that if you write a `yield c.read_message()` when a message from
server isn't expected, the coroutine will keep waiting, eventually raising
`tornado.ioloop.TimeoutError` (5 seconds by default). Great, we can write lot of
tests using just what we have now.

The tests can be run via:

```
python -m tornado.testing discover
```

This could still be further improved. We need to yield and ignore the 'hello'
message in every test, for every client. And in your application, it may be a
more complicated handshake - possibly a few initial messages. Once you write a
test for that handshake, it needn't be re-written in every test. To avoid that,
we will write an an abstraction over this:

```python
    @gen.coroutine
    def _mk_client(self):
        c = yield self._mk_connection()

        # Discard the hello
        # This could be any initial handshake, which needs to be generalized
        # for most of the tests.
        _ = yield c.read_message()

        raise gen.Return(c)
```

`_mk_client` here is a method in which you could place all the boilerplate. The
key point here is the exception `gen.Return(c)` we raise in the end. `return`
with a value is allowed only after Python `3.3`, so `tornado.gen` uses the value
associated with this exception as the coroutine's result.

With `_mk_client` available, we can write tests which only include the relevant
code:

```python
    @testing.gen_test
    def test_echo(self):
        # A client with the hello taken care of.
        c = yield self._mk_client()

        # Send a 'foo' to the server.
        c.write_message("foo")
        # Get the 'foo' back.
        response = yield c.read_message()
        # Make sure that we got a 'foo' back and not 'bar'.
        self.assertEqual('foo', response)
```

The application built out of this post is available [as a gist][example].

[qotr]: http://github.com/crodjer/qotr/
[tornado]: http://www.tornadoweb.org/
[ember.js]: http://emberjs.com/
[example]: https://gist.github.com/crodjer/1e9989ab30fdc32db926

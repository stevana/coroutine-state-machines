# coroutine-state-machines

State machines of the type `Input -> State -> (Output, State)` are great. They
are easy to reason about, and if run on a separate thread with access to a queue
of `Input`s they perform well too.

Sometimes the state machine might need to do some blocking I/O before producing
the output though, this slows down the processing of inputs.

This repo is an experiment in how we can write the state machine as if the I/O
is blocking, but actually it's non-blocking and inputs can continue to be
processes while we wait for the I/O action to complete.

## Usage

To make things more concrete we will be implementing a key-value store as a
state machine.

To start the key-value store in a terminal issue:

```bash
cabal run app
```

Then interact with the key-value store from another terminal using `Write` and
`Read` commands as follows:

```bash
$ http POST :8080 --raw 'Write "x" 1'
HTTP/1.1 200 OK
Date: Thu, 05 Jan 2023 08:47:03 GMT
Server: Warp/3.3.23
Transfer-Encoding: chunked

Ok

$ http POST :8080 --raw 'Read "x"'
HTTP/1.1 200 OK
Date: Thu, 05 Jan 2023 08:47:04 GMT
Server: Warp/3.3.23
Transfer-Encoding: chunked

Result 1
```

## How it works

The state machine for the key-value store example looks like this:

```haskell
data Input = Write String Int | Read String
  deriving stock (Show, Read)

data Output = Ok | Result Int
  deriving stock Show

sm :: SM (Map String Int) Input Output
sm = do
  i <- ask
  case i of
    Write k v -> do
      fsAppend k v
      modify (Map.insert k v)
      return Ok
    Read k -> do
      m <- get
      return (Result (m Map.! k))
```

Where `fsAppend` appends the key-value pair to a file, so that we can recover in
in-memory state in case of a crash.

The program looks sequential, but once the state machine hits the `fsAppend` it
will suspend using a coroutine monad, yielding control back to the event loop
which feeds it inputs, the event loop will enqueue the I/O action to a separate
thread that deals with I/O and continue feeding the state machine new inputs,
until the I/O thread processes a result, at which point the state machine will
be resumed.

There's a question of which state should we resume with? The old one, which we
suspended with? Or the new one, which was produced by processing more recent
inputs? Neither choice seems to work in the general case. Perhaps we might want
to give the application developer the choice? Another option, which we've
implemented currently, is to require the state to be a monoid, resume with the
old state but then `mappend` the resulting state with the new state. More
testing is still needed to ensure that this is sound.

Another solution might be to suspend and let another state machine run, but
don't process any new inputs for the state machine that is waiting for the I/O.
This solution doesn't have the dilemma about which state to resume with, but
it's also not as parallel...

## Contributing

Any feedback, comments or suggestions are most welcome!

In particular if you know how to solve this problem in general, or for some
interesting special case.

## See also

* *Development and Deployment of Multiplayer Online Games, Vol. II* by Sergey
  Ignatchenko (2020), especially chapter 5.

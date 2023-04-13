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

data Output = Ok | Result (Maybe Int)
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
      return (Result (m Map.!? k))
```

Where `fsAppend` appends the key-value pair to a file, so that we can recover in
in-memory state in case of a crash.

The program looks sequential, but once the state machine hits the `fsAppend` it
will suspend using a coroutine monad, yielding control back to the event loop
which feeds it inputs, the event loop will enqueue the I/O action to a separate
thread that deals with I/O and continue feeding the state machine new inputs,
until the I/O thread completes the write to disk, at which point the state
machine will be resumed with the latest state.

## Contributing

Any feedback, comments or suggestions are most welcome!

In particular if you know how to solve this problem in a different or better
way.

A potential source of confusion and bugs might be the fact that once we resume
the state might not be the same as it was before we suspended. It's not clear to
me how big of a problem this is in practice, or if anything can be done about it
without sacrificing either the "sequential feel" or the parallelism?

One possible generalisation that seems feasible is to not suspend immediately
upon the I/O action, but rather merely return a "future" which we later can
`await` for. This would allow us to do suspend and do multiple I/O actions
before resuming, something like:

```haskell
  a1 <- fsAppend k v
  a2 <- someOtherIOAction
  awaitBoth a1 a2 -- or awaitEither a1 a2
```

Arguably the await makes it more clear where the suspension and resumption
happen, which could help against the confusion regarding that the state might
change.

## See also

* *Development and Deployment of Multiplayer Online Games, Vol. II* by Sergey
  Ignatchenko (2020), especially chapter 5;
* [*Implementing Co, a Small Language With Coroutines #3: Adding
  Coroutines*](https://abhinavsarkar.net/posts/implementing-co-3/);
* [*A Lambda Calculus With Coroutines and Heapless, Directly-Called
  Closures*](https://ayazhafiz.com/articles/23/a-lambda-calculus-with-coroutines-and-heapless-closures);
* [Small VMs & Coroutines](https://blog.dziban.net/coroutines/);
* [Tina is a teeny tiny, header only, coroutine and job
  library](https://github.com/slembcke/Tina);
* [Protothreads](http://dunkels.com/adam/pt/);
* [Proactor pattern](https://en.wikipedia.org/wiki/Proactor_pattern);
* [WebAssembly
  Reactors](https://github.com/bytecodealliance/wasmtime/blob/main/docs/WASI-rationale.md#why-not-async).

# Concurrency in QCL

QCL now supports Go-style concurrency with channels and goroutines, enabling asynchronous and parallel programming patterns.

## Goroutines

Goroutines allow you to run code concurrently. Use the `go` statement to spawn a new goroutine:

```qcl
// Spawn a goroutine with a block statement
go {
    let result = some_computation();
    // Work happens in parallel
};

// Spawn a goroutine with a function call
go some_function();
```

## Channels

Channels provide communication between goroutines and the main program. QCL supports both unbuffered and buffered channels.

### Channel Types

- **Unbuffered channels**: Block on send until received
- **Buffered channels**: Allow sending up to capacity without blocking

### Channel Operations

#### Creating Channels

```qcl
// Unbuffered channel (capacity 0)
let ch = make_chan();

// Buffered channel with capacity 5
let ch = make_chan(5);
```

#### Sending to Channels

```qcl
// Send value to channel (blocking)
ch <- 42;

// In a goroutine context
go {
    ch <- "hello";
};
```

#### Receiving from Channels

```qcl
// Receive from channel (blocking)
let value = <-ch;

// Receive without assignment
<-ch;

// In a goroutine context
go {
    let received = <-ch;
    // Process received value
};
```

## Select Statements

Select statements allow you to handle multiple channel operations:

```qcl
select {
    case value := <-ch1:
        // Handle receive from ch1
        break;
    case ch2 <- "data":
        // Handle send to ch2
        break;
    default:
        // Handle when no operations are ready
        break;
}
```

## Complete Example

Here's a complete example demonstrating producer-consumer pattern:

```qcl
// Create a buffered channel
let ch = make_chan(3);

// Producer goroutine
go {
    let i = 0;
    while i < 5 {
        ch <- i;
        i = i + 1;
    }
};

// Consumer goroutine
go {
    let count = 0;
    while count < 5 {
        let value = <-ch;
        // Process value
        count = count + 1;
    }
};

// Main program continues
let main_work = 42;
```

## Channel Functions

The standard library provides additional channel functions:

### `make_chan(capacity?)`
Create a new channel with optional capacity.

### `send(channel, value)`
Send a value to a channel (blocking).

### `recv(channel)`
Receive a value from a channel (blocking).

### `try_send(channel, value)`
Try to send a value (non-blocking). Returns boolean success.

### `try_recv(channel)`
Try to receive a value (non-blocking). Returns [value, success] array.

### `close(channel)`
Close a channel.

## Implementation Notes

- Goroutines are implemented using OS threads
- Channels use Rust's `mpsc` (multiple producer, single consumer) channels
- Channel operations are thread-safe
- Goroutines share the execution environment and context

## Best Practices

1. **Use buffered channels** for decoupling producers and consumers
2. **Close channels** when done sending to signal completion
3. **Use select statements** for handling multiple channels
4. **Avoid sharing mutable state** between goroutines; prefer communication via channels
5. **Handle channel blocking** appropriately in your program design

## Syntax Summary

```qcl
// Goroutines
go statement
go { block }

// Channels
let ch = make_chan();        // unbuffered
let ch = make_chan(size);    // buffered
ch <- value;                 // send
let val = <-ch;             // receive
<-ch;                       // receive (discard)

// Select
select {
    case var := <-ch: stmt
    case ch <- val: stmt
    default: stmt
}
```

This concurrency model brings powerful parallel programming capabilities to QCL while maintaining the language's simplicity and safety.
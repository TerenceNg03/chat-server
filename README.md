# chat-server

A simple chat server in haskell with STM (software transactional memory).

## Commands
The following command is supported:
```
-- login to server
/login <name>

-- logout
/logout

-- disconnect
/disconnect

-- Send message to someone
/tell <name> <message>

-- kick someone from server
/kick <name> <message>

-- Broadcast message across server
<message>
```

## Output Example

```
➜  ~ nc localhost 44444
/login aaa
Notice: aaa has logged in!
Notice: bbb has logged in!
/logout
Notice: aaa has logged out!
/logout asdf
Invalid command!
/login asdf
Notice: asdf has logged in!
Notice: asdf is kicked by bbb for spamming
/login aaa
Notice: aaa has logged in!
Tell from bbb: hello
Notice: ccc has logged in!
/tell ccc aaa is bad
Broadcast form bbb: hello everyone
Notice: ccc has logged out!
Notice: bbb has logged out!
/disconnect
➜  ~
```

## How does it work

### Client interaction
When server accepted a new socket connection a new client is made.

A client utilizes three worker threads:
- Receiver: Parse user input and write to channel
- Forwarder: Forward server messages to channel
- Processor: Process messages read from channel

and contains two channels:
- Send Channel: The channel that is used to receive messages
- Broadcast Channel: This channel is duplicated from server's broadcast channel so that everyone gets a message whenever someone write a message

### Concurrency model
Shared variables are synchronized via STM and thus this server does not require any lock and consequently does not suffer from any deadlock or race condition. A shared channel is used to broadcast message in order to reduce STM run-time burden. By default the server is single-threaded but can be made to utilize multiple cores with command-line arguments `+RTS -N`.

### Safety
All resources are properly handle with `bracket`.

# chat-server

Implement a simple chat server in haskell with STM (software transactional memory).

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

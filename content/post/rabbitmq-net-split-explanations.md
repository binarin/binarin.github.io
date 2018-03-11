+++
title = "RabbitMQ net-split messages explanation"
tags = ["rabbitmq"]
draft = false
+++

After experiencing a network problem RabbitMQ writes a record to  
logs that looks like this:  

```text
=INFO REPORT==== 30-Jan-2017::19:04:04 ===
node 'rabbit@192.168.155.11' down: connection_closed
```

In this case the reason is <kbd>connection_closed</kbd>. But sometimes it  
may not be evident what this actually means or what could have  
caused this error. Especially in some outright bizzare situations.  
Here I'm trying to document all the reasons that I've seen and how  
you can reproduce them.  

<!--more-->


## <kbd>connection_closed</kbd> {#kbd-connection-closed-kbd}

This happens any time when a connection is closed using "normal"  
mechanisms. Some ways to reproduce it:  

-   Stop a remote RabbitMQ node
-   Send RST from a remote node, e.g. using <kbd>iptables</kbd>
-   Attach to a running ErlangVM with <kbd>gdb</kbd> and do <kbd>call
          close(some-fd)</kbd> here


## <kbd>net_tick_timeout</kbd> {#kbd-net-tick-timeout-kbd}

Any time when a remote node stops responding - for sender it looks  
like blackholing. Some reasons:  

-   Loss of network connectivity between 2 machines
-   Death of a remote machine
-   Firewall rule that drops packets
-   Somebody is sending a very big chunk of data through RabbitMQ  
    cluster channel. E.g. such a big AMQP messages that it's enough  
    to saturate network for at least <kbd>net_tick_timeout</kbd>.


## <kbd>disconnect</kbd> {#kbd-disconnect-kbd}

Explicit disconnect performed using  
<kbd>erlang:disconnect_node/1</kbd>. Either by some internal RabbitMQ  
mechanism or by somebody messing with <kbd>rabbitmqctl eval</kbd>.  


## <kbd>etimedout</kbd> {#kbd-etimedout-kbd}

Another quite interesting reason. I believe that this can happen  
only when OS TCP stack is tuned in a such way that TCP timeout is  
less than <kbd>net_tick_timeout</kbd>. On Linux this can be reproduced with  
some extreme tuning:  

```text
cd /proc/sys/net/ipv4
echo 2 > tcp_keepalive_intvl
echo 1 > tcp_keepalive_probes
echo 2 > tcp_keepalive_time
echo 1 > tcp_retries1
echo 2 > tcp_retries1
```


## <kbd>econnreset</kbd> {#kbd-econnreset-kbd}

This is the most strange of all reasons which I've seen only in  
production logs but can't reproduce myself. One very probable  
explanation is that RST packet has arrived with an exceptionally  
bad timing - just after a socket was returned from <kbd>epoll</kbd> as a  
ready one, but before read/write operation on it actually started.

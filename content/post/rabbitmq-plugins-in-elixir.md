+++
title = "RabbitMQ plugins in Elixir"
tags = ["rabbitmq"]
draft = false
+++

As RabbitMQ is gradually adopting Elixir (i.e. for [the next generation  
of its CLI tools](https://github.com/rabbitmq/rabbitmq-cli)), it's natural that one would also want to use Elixir  
for writing plugins. There is a [blog post](https://www.rabbitmq.com/blog/2013/06/03/using-elixir-to-write-rabbitmq-plugins/) from 2013 on that topic and  
most things there are still relevant - except the build instructions,  
as there was a complete revamp of build system in RabbitMQ. And I'm  
going to fill that gap.  

<!--more-->

RabbitMQ plugin development guide suggests that the easiest way to  
start a new plugin is to copy the simplest existing plugin  
<kbd>rabbitmq_metronome</kbd>. I've re-implemented it in Elixir at  
<https://github.com/binarin/rabbitmq_metronome_elixir>. You can just  
fork it and start hacking (note that it only works with what is  
going to be a <kbd>3.7.0</kbd> release of rabbit). Or read further down  
about some details that make this possible.  

Suppose we've created an elixir scaffolding using <kbd>mix
  new</kbd>. RabbitMQ uses <kbd>erlang.mk</kbd> as its build system, so our first  
task is to integrate some <kbd>mix</kbd> commands into <kbd>Makefile</kbd>. Here is  
the snippet that hooks <kbd>mix</kbd> into build process and which needs to  
be added to [Makefile of the original metronome plugin](https://github.com/rabbitmq/rabbitmq-metronome/blob/master/Makefile):  

```makefile
elixir_srcs := mix.exs \
               $(shell find config lib -name "*.ex" -o -name "*.exs")

app:: $(elixir_srcs) deps
     $(MIX) deps.get
     $(MIX) deps.compile
     $(MIX) compile
```

Running <kbd>mix</kbd> 3 times in a row is a bit expensive, so it's  
advisable to add some [aliases](https://hexdocs.pm/mix/Mix.html#module-aliases) to <kbd>mix.exs</kbd>:  

```elixir
[
  make_all: [
    "deps.get",
    "deps.compile",
    "compile",
  ],
]
```

Then we can replace 3 <kbd>mix</kbd> calls with a single one in our <kbd>Makefile</kbd>:  

```makefile
app:: $(elixir_srcs) deps
     $(MIX) make_all
```

Another thing is that we can drop <kbd>PROJECT_DESCRIPTION</kbd>,  
<kbd>PROJECT_MOD</kbd> and <kbd>PROJECT_ENV</kbd> variables from <kbd>Makefile</kbd>, as  
<kbd>erlang.mk</kbd> uses them only to generate an <kbd>.app</kbd> file, and <kbd>mix</kbd> is  
already handling this task.  

<kbd>erlang.mk</kbd> is the primary build system for RabbitMQ, so we need to  
tell <kbd>mix</kbd> that it shouldn't fetch or build dependencies that are  
managed by <kbd>erlang.mk</kbd>. For <kbd>rabbit</kbd> and <kbd>rabbit_common</kbd>  
which are always the direct dependencies we add this:  

```elixir
[
  {
    :rabbit_common,
    # `deps` is erlang.mk directory with dependencies
    path: "deps/rabbit_common",
    compile: "true",
    override: true
  },
  {
    :rabbit,
    path: "deps/rabbit",
    compile: "true",
    override: true
  },
]
```

There can be an additional trouble when we use some libraries that  
have transient dependencies on RabbitMQ sub-projects. E.g. this is  
the case with <kbd>amqp</kbd> elixir library, which depends on <kbd>amqp_client</kbd>  
and in turn on <kbd>rabbit_common</kbd>. If we don't explicitly specify that  
this depency is managed by <kbd>erlang.mk</kbd>, <kbd>mix</kbd> will try to fetch  
it - and it can fetch a version incompatible with our  
<kbd>rabbit_common</kbd>.  

And that's it. There are some thing that I haven't figured out yet,  
like writing CT suites in Elixir; or that sometimes I need to  
delete <kbd>plugins</kbd> and <kbd>_build</kbd> folders to make my  
changes active. But other than that everything is fine.

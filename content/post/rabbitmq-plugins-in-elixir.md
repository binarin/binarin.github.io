+++
title = "RabbitMQ plugins in Elixir"
author = ["Alexey Lebedeff"]
publishDate = 2017-04-19
tags = ["rabbitmq"]
draft = false
+++

As RabbitMQ is gradually adopting Elixir (i.e. for [the next
generation of its CLI tools](https://github.com/rabbitmq/rabbitmq-cli)), it's natural that one would also want
to use Elixir for writing plugins. There is a [blog post](https://www.rabbitmq.com/blog/2013/06/03/using-elixir-to-write-rabbitmq-plugins/) from 2013
on that topic and most things there are still relevant - except the
build instructions, as there was a complete revamp of build system
in RabbitMQ. And I'm going to fill that gap.

<!--more-->

RabbitMQ plugin development guide suggests that the easiest way to
start a new plugin is to copy the simplest existing plugin
`rabbitmq_metronome`. I've re-implemented it in Elixir at
<https://github.com/binarin/rabbitmq_metronome_elixir>. You can just
fork it and start hacking (note that it only works with what is
going to be a `3.7.0` release of rabbit). Or read further down
about some details that make this possible.

Suppose we've created an elixir scaffolding using `mix
   new`. RabbitMQ uses `erlang.mk` as its build system, so our first
task is to integrate some `mix` commands into `Makefile`. Here is
the snippet that hooks `mix` into build process and which needs to
be added to [Makefile of the original metronome plugin](https://github.com/rabbitmq/rabbitmq-metronome/blob/master/Makefile):

{{< highlight makefile>}}
elixir_srcs := mix.exs \
               $(shell find config lib -name "*.ex" -o -name "*.exs")

app:: $(elixir_srcs) deps
     $(MIX) deps.get
     $(MIX) deps.compile
     $(MIX) compile
{{< /highlight >}}

Running `mix` 3 times in a row is a bit expensive, so it's
advisable to add some [aliases](https://hexdocs.pm/mix/Mix.html#module-aliases) to `mix.exs`:

{{< highlight elixir>}}
[
  make_all: [
    "deps.get",
    "deps.compile",
    "compile",
  ],
]
{{< /highlight >}}

Then we can replace 3 `mix` calls with a single one in our `Makefile`:

{{< highlight makefile>}}
app:: $(elixir_srcs) deps
     $(MIX) make_all
{{< /highlight >}}

Another thing is that we can drop `PROJECT_DESCRIPTION`,
`PROJECT_MOD` and `PROJECT_ENV` variables from `Makefile`, as
`erlang.mk` uses them only to generate an `.app` file, and `mix` is
already handling this task.

`erlang.mk` is the primary build system for RabbitMQ, so we need to
tell `mix` that it shouldn't fetch or build dependencies that are
managed by `erlang.mk`. For `rabbit` and `rabbit_common`
which are always the direct dependencies we add this:

{{< highlight elixir>}}
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
{{< /highlight >}}

There can be an additional trouble when we use some libraries that
have transient dependencies on RabbitMQ sub-projects. E.g. this is
the case with `amqp` elixir library, which depends on `amqp_client`
and in turn on `rabbit_common`. If we don't explicitly specify that
this depency is managed by `erlang.mk`, `mix` will try to fetch
it - and it can fetch a version incompatible with our
`rabbit_common`.

And that's it. There are some thing that I haven't figured out yet,
like writing CT suites in Elixir; or that sometimes I need to
delete `plugins` and `_build` folders to make my
changes active. But other than that everything is fine.

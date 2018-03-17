+++
title = "Perl `JSON::XS` can produce un-canonical JSON"
author = ["Alexey Lebedeff"]
publishDate = 2018-03-14
tags = ["perl", "sadness"]
draft = false
+++

Sometimes you want to produce a canonical version of a data
structure in JSON - with keys sorted and no additional
whitespace. [`JSON::XS`](https://metacpan.org/pod/JSON::XS) promises us that we can achieve such result
with a `canonical` option. It only notes that canonicalization
doesn't work for tied hashes. But I've just spent half a day
debugging to learn that this is a bit more complicated.

<!--more-->

To my complete surprise the following script fails most of the
time:

{{< highlight perl >}}
use Data::Printer;
use JSON::XS;
use Test::More (tests => 1);
my $data = { date => "2018-03-13", prev_period  => "2018-03-12", profile => "Desktop"};
my $pre = JSON::XS->new->utf8->canonical(1)->encode($data);
p $data;
my $post = JSON::XS->new->utf8->canonical(1)->encode($data);
is $post, $pre;
{{< /highlight >}}

Turns out this is a problem known for 3 years with both
[Data::Printer](https://github.com/garu/Data-Printer/issues/75) and `JSON::XS` - both unfixed, except for a fork at
[Cpanel-JSON-XS](https://github.com/rurban/Cpanel-JSON-XS/pull/42).

The thing is that even when you aren't using `Data::Printer`,
somebody can still break your canonical JSON representation.

I think I'll just switch to `Cpanel::JSON::XS`, there is [too much
drama](https://metacpan.org/source/MLEHMANN/JSON-XS-3.04/XS.xs#L658) about hash key randomization in `JSON::XS`.

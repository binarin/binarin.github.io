+++
title = "Fully reproducible NixOS builds"
author = ["Alexey Lebedeff"]
date = 2018-09-28T00:00:00+02:00
tags = ["nix", "nixos"]
draft = false
+++

One of big strengths of NixOS is reproducible builds. Yet achieving
full reproducibility requires a bit of effort. The minimal set of
things needed is a `configuration.nix` and a reference to a
`nixpkgs` commit. And the easiest way to achieve this is to have a
git-repo with configuration, which also references `nixpkgs` as a
submodule.

<!--more-->

Let's change an existing configuration that way.

{{< highlight bash>}}
cd /etc/nixos
git init
git commit -m Initial --allow-empty
git add *.nix
git commit -m "Import config"
git submodule add https://github.com/NixOS/nixpkgs

# Or a branch like nixos-18.03 from
# https://github.com/nixos/nixpkgs-channels to use stable version,
# with most of the things available from binary cache.
{{< /highlight >}}

Now we need to tell `nix` that we want to use our own checkout of
`nixpkgs` instead of channels. This can be achieved by adding an
option to `configuration.nix`:

{{< highlight nix>}}
nix.nixPath = [
  "nixpkgs=/etc/nixos/nixpkgs"
  "nixos-config=/etc/nixos/configuration.nix"
];
{{< /highlight >}}

After issuing `nixos-rebuild switch` twice&nbsp;[^fn:1]
your own checkout of `nixpkgs` will be finally used.

But what if you want to build exactly the same configuration on
some other machine, like when doing CI? The manpage (and
source-code) of `nixos-rebuild` suggests that we can do it that
way:

{{< highlight bash>}}
nix build -f '<nixpkgs/nixos>' system \
    -I nixpkgs=$(pwd)/nixpkgs \
    -I nixos-config=$(pwd)/configuration.nix
{{< /highlight >}}

And it will indeed work if we'll build it exactly from the same
directory on disk (i.e. `/etc/nixos`). Sadly trying to build the same
thing from another directory will produce different path in `/nix/store/`.

After a bit of digging I found out that difference is in how full
nixpkgs version is determined (it includes a git commit, and is a
bit flaky because it tries to parse a commit in pure nix). This is
later used both as a part of derivation name, and inside some files
like `/etc/os-release`.

It's possible to override those auto-detected values with a
following configuration snippet:

{{< highlight nix>}}
{pkgs, lib, ...}:

let
  gitRepo      = "${toString pkgs.path}/../.git";
  gitCommitId  = lib.substring 0 7 (lib.commitIdFromGitRepo gitRepo);
in {
  system.nixos.versionSuffix = "-my-name-${gitCommitId}";
  system.nixos.label = "my-name-${gitCommitId}";
}
{{< /highlight >}}

It's 2 options that need to be set, and it's even more useful when
done as shown above: derivation name and `/etc/os-release` will
contain reference both to an exact version of `configuration.nix` and
of an `nixpkgs` commit (as they're both taken into account for
configuration repository commit id calculation).

So now we can commit everything, do `nixos-rebuild switch` and take
a note of what exactly was built:

{{< highlight bash>}}
readlink -f /run/current-system
{{< /highlight >}}

If we checkout our configuration repo somewhere else (at least in
another directory, and maybe even on another server) and do `nix
   build` command from above, we should finally see exactly the same
store path:

{{< highlight bash>}}
nix build ... # as above
ls -la result
{{< /highlight >}}

[^fn:1]: first time just to change the path, second time to really use your own `nixpkgs`

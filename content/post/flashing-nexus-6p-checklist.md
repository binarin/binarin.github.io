+++
title = "Notes about installing Lineage OS on Nexus 6P"
author = ["Alexey Lebedeff"]
date = 2018-03-15T18:44:00+01:00
tags = ["android", "notes"]
draft = false
+++

Checklist for myself for the next time I'll be flashing my Nexus 6P
(because official [Lineage OS instructions](https://wiki.lineageos.org/devices/angler/install) are missing some
important parts).
<!--more-->

-   Install latest TWRP (from within itself if present, using
    fastboot otherwise)
-   Download proper version (look on XDA) of [factory image](https://developers.google.com/android/images). We'll
    need vendor, radio and bootloader images from there
-   Download and put on the device
    -   Lineage OS .zip
    -   gapps for arm64 .zip
    -   some su .zip
-   Reboot into the bootloader (Power + VolumeDown), attach USB cable
    and use `fastboot` to flash vendor, radio, bootloader. And maybe
    TWRP, if not already latest. You need to reboot bootloader
    between each step. Running `fastboot` can require `sudo`. Exact
    commands can be found in shell files inside a factory image. Pay
    attention to a `fastboot` version check here (if present)
-   Follow official instructions from that point
    -   Wipe System, Cache, Data
    -   Queue 3 .zips
    -   Flash

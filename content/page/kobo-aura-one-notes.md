+++
title = "Kobo Aura One"
author = ["Alexey Lebedeff"]
draft = false
+++

## Activating and using telnet {#activating-and-using-telnet}

-   Get zip [here](https://www.mobileread.com/forums/showpost.php?p=2681553&postcount=3)
-   Put `KoboRoot.tgz` into `.kobo` folder
-   Install update from KSM
-   Enable usbnet, do smth like `sudo ifconfig enp0s20u1u1
          192.168.2.100/29 up`
-   `telnet 192.168.2.101` and use `root` for login


## Known problems {#known-problems}

-   [Dictionary lookup not working](https://github.com/koreader/koreader/issues/3505#issuecomment-345888042) - use `sdcv` from that issue
-   [Evernote plugin crashing](https://github.com/koreader/koreader/issues/3600#issuecomment-370093639) - copy .so-file per instruction
-   I needed to delete `.adds/koreader` folder before upgrade,
    despite being said otherwise in readme. It started to crash
    while turning wi-fi on/off.

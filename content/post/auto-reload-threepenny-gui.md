+++
title = "Reloading threepenny-gui apps while developing"
author = ["Alexey Lebedeff"]
date = 2018-03-17T00:00:00+01:00
tags = ["haskell", "gui"]
draft = false
+++

When you are developing a [threepenny](https://hackage.haskell.org/package/threepenny-gui-0.8.0.0) app, you need to perform the
same dance after every change: recompile and restart the app, and
then manually reload a page in a browser. This becomes tiresome
very quickly. But with the help of `ghcid` and with small changes
to your Haskell code you can completely automate this process.

<!--more-->

All commands assume the simplest project layout with all the code
in `Main.hs`. It's a direct copy from [threepenny-gui documentation](https://hackage.haskell.org/package/threepenny-gui-0.8.2.2/docs/Graphics-UI-Threepenny.html),
and you can clone my [example repository](https://github.com/binarin/threepenny-reloading-example) which also adds all the
necessary boilerplate.


## Using `ghcid` for reloading {#using-ghcid-for-reloading}

Almost all of the problems can be solved by using [`ghcid`](https://github.com/ndmitchell/ghcid), which
will give us a freshly recompiled and started application every
time we'll change the source code. Here is an example of a command
that is suitable for my example:

{{< highlight shell "linenos=table, linenostart=1" >}}
ghcid -c 'cabal new-repl' \
      --reload=./Main.hs \
      -T $':reload Main\nMain.main' \
      --restart=./threepenny-reloading-example.cabal
{{< /highlight >}}

1.  `cabal new-repl` can be replaced with whatever you use to get a
    ghci REPL (`stack ghci`, `cabal repl`, etc.)
2.  `--reload` switches (which can be specified multiple times)
    tell what files or directories should be watched for
    changes. You may want to monitor not only Haskell sources, but
    also some web assets like CSS and JavaScript files - so that
    reload will happen on all relevant changes
3.  This is a command that is being sent to `ghci` after every
    successful compilation (`$''` is just a bash syntax that allows
    us to encode literal newline using `\n`)
4.  And we want to restart the whole `ghci` process when there are
    some changes to a cabal file, like new dependencies added or
    flags changed


## Reloading browser page {#reloading-browser-page}

That's better, yet we still need to refresh our browser
manually. With small changes to Haskell code we can automate this
part also. Our `main` is usually looks like this:

{{< highlight haskell >}}
main = do
  appInit -- can take indeterminate amount of time
  startGUI defaultConfig setup
{{< /highlight >}}

To achieve automatic reloading we can introduce alternative `main`
which will be only used by `ghcid` (with the `-T` switch):

{{< highlight haskell >}}
mainDevel = do
  appInit -- can take indeterminate amount of time
  forkIO $ do
      threadDelay 500000 -- small delay so startGUI can start listening
      refreshBrowserPage
  startGUI defaultConfig setup
{{< /highlight >}}

Now you need to come up with `refreshBrowserPage` for the system
where you do development. Below you can see the dumbest
implementation which will work only for Firefox on Linux, and only
when `xdotool` is installed. But it's OK, as this is
development-only code that doesn't even need to be robust or
universal.

My Linux/Firefox implementation uses the fact that threepenny
browser-side code tries to reload a page as soon as it looses
connection to a server, which usually results in `Problem loading
    page` error (because recompiling and restarting application is not
as fast as page reload). The code just searches for all windows
which has `Problem loading page` in their title and sends reload
hotkey (`Ctrl-R`) to each of them.

{{< highlight haskell >}}
import Control.Monad (forM_)
import Control.Monad.Catch
import System.Process (readProcess, callProcess)
import Text.Read (readMaybe)

refreshBrowserPage :: IO ()
refreshBrowserPage = do
    maybeWindows :: Either SomeException String <- try
      (readProcess "xdotool"
        ["search", "--all", "--name", "Problem loading"] "")
    case maybeWindows of
      Left _ -> return ()
      Right idStr -> forM_ (lines idStr) $ \windowId ->
        case readMaybe windowId of
          Nothing -> return ()
          Just (n :: Integer) -> do
              putStrLn $ show n
              callProcess "xdotool" ["key", "--window", show n, "CTRL+R"]
              return ()
{{< /highlight >}}

And this is all that's needed to see your changes in browser
almost immediately after you save an edited file.

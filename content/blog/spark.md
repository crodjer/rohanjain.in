+++
title = "Window Management in OSX with Spark"
date = "2017-12-06T12:44:55Z"
slug = "spark"
+++

I am a big fan of deterministic access to applications windows. That is, to be
able to open them without needing any visual feedback from the screen. Tiling
window managers do this well. On OSX, this can be tricky.

For a long time, I used a [two windows](/two-windows/) workflow i.e. all the
applications I need should run in just two windows (applications): the browser
(mail, slack etc.) or the terminal (emacs, tmux, bash).  This works, but is
restrictive.

I have switched to an alternative approach now. I have a global shortcut for
each application I use frequently. For example: `Cmd-Ctrl-E` is `emacs`,
`Cmd-Ctrl-C` is `chrome`. Now, wherever I may be, I can deterministically open
Emacs. I do this with [Spark](https://www.shadowlab.org/softwares/spark.php) and
an `AppleScript` per application.

I had to write `AppleScript`s instead of simply pointing to the application
because I need the window to be resized to the maximum as well. This is what
the `AppleScript` to open (and resize) `Emacs` looks like:

```applescript
tell application "Emacs"
	-- Get bounds from the desktop.
	tell application "Finder" to bounds of window of desktop
	-- Set the window bounds to the ones we just fetched.
	set bounds of window 1 to result
	-- Focus the editor window.
	activate
end tell
```

> This way of resizing cannot be used with multiple screens because the bounds
> provided by `Finder` will span all of them and so will the application window.

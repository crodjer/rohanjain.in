+++
title = "Automating Window Management in OSX with Spark"
date = "2017-12-06T12:44:55Z"
slug = "spark"
+++

I am a big fan of deterministic access to application. By that, I mean I should
be able to reach a window/application without needing visual feedback from the
screen. Doing this on OSX can be tricky.

For a long time, I used a [two windows](/two-windows/) workflow i.e. all the
applications I need should fit in just two windows: the browser (mail, slack
etc.) or the terminal (emacs, tmux, bash).  This works, but is restrictive.

An alternate approach I have been trying out now is to have a global shortcut
to reach each application. For example, in my setup: Cmd-Ctrl-E is `emacs`,
Cmd-Ctrl-C is `chrome` and so on. Now, wherever I may be, I can
deterministically open a desired window/application. I do this with
[Spark](https://www.shadowlab.org/softwares/spark.php) and an `AppleScript` per
application.

I wrote `AppleScript`s instead of simply pointing to the application the
so that the window is resized to the maximum before being focused. This is what
the `AppleScript` to open (and resize) `Emacs` looks like.

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

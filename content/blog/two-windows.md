+++
date = "2017-02-27T06:15:12Z"
title = "The two windows workflow"
slug = "two-windows"
+++

I was introduced to OSX for the first time in September, 2015. Coming from a
XMonad/Linux based setup, I was accustomed to powerful and fast window
management. Most available solutions for OSX did not have these qualities. I
tried:

 - *Amethyst*, a window manager with basic XMonad bindings.
 - Just type the application name in *Alfred*. Initially this was slow, but
   quickly became habitual.
 - *Mjolinr*, which is configured in Lua and can actually be quite powerful.
 - Numerous other programs out there which try to solve the same problem in
   similar fashion.

These programs sort of worked, but they are slow and unresponsive. Alfred based
switching works on application level and not window level. So, I needed some
simpler alternatives.

Eventually, I noticed that `Cmd-Tab` based window switching is fast. So, for
last 6 months, I have been using a two window strategy. That is:

**The primary workflow must be dependent on at max two windows.**

This means that any application I want to have in my workflow must run in either
of those windows.

Those windows in my case are:

 - _iTerm2_  
   This includes a `tmux` session, with multiple windows. The first window will
   run a in-terminal emacs client and the rest all windows will various shell
   sessions and split configurations of their own. This way, I'll spend most of
   my time within a tmux session.
 - _Browser_  
   Apart from functioning as a browser, it serves as an host for applications
   such as: Mail, Slack, Calender.

There are multiple benefits of this setup:

 - Quick window switching.
 - Universal workflow. Use it on RaspberryPi, remote machines, Linux TTY.
 - Keystrokes are deterministic. Just queue in the keystrokes and let the
   computer execute things at its own pace. This also is my major complaint
   against touch phones (compared to say, an old Nokia QWERTY phone).
 - I use Vim as a text editor for anything non Clojure. With tmux splits panes,
   all of the context stays in the same tmux window.

There are drawbacks too:

 - Not every application can run in the browser/terminal. Because of this, you
   end up negotiating for alternates.
 - Although most of Emacs works well in terminal, some bindings at times won't
   work. This isn't a issue for me particularly.

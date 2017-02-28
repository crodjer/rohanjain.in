+++
date = "2017-02-27T06:15:12Z"
title = "The two windows workflow"
+++

I was introduced to OSX for the first time in September, 2015. Coming from a
XMonad/Linux based setup, I was accustomed to powerful and fast window
management. Any OSX based solution for window management system I tried, lacked
these quality. I tried:

 - Amethyst which follows most of the basic XMonad operations.
 - Alfred for deterministic window switching for a while. Initially there was
   the lag for me in typing the program name, but that went away pretty quickly
   with habits.
 - Mjolinr, which is configured in Lua and can actually be quite powerful.

These programs sort of worked, but they are appallingly slow and unresponsive.
OSX is full of unnecessary animations and bloat which causes me to lose context
while window switching (say between Emacs and the terminal). Alfred based
switching doesn't work because OSX doesn't work on window level, but application
level. So, good luck tracking multiple terminal/browser windows with this
method.

Eventually, I noticed that `Cmd-Tab` based window switching is very fast. So,
for last 6 months, I have been using a two window strategy. That is:

**The primary workflow should be dependent only two windows.**

This means that any application I want to have in my workflow must run in either
of these two windows.

In my case, those windows are:

 - _iTerm2_  
   This includes a `tmux` session, with multiple windows. The first window will
   run a in-terminal emacs client and the rest all windows will various shell
   sessions and split configurations of their own. This way, I'll spend most of
   my time within a tmux session.
 - _Browser_  
   The browser here works like an OS i.e. place to have various applications:
   Mail/Slack/Calender run. Plus of course, to function as a browser.

There are multiple benefits of this setup that I notice:

 - Quick window switching. It is fast enough so that the continuity is
   maintained.
 - Keystrokes are deterministic. This, actually is very important. At times,
   computers may get unresponsive. Meanwhile, you don't want to be blocked
   yourself. So, with this system, just queue in the keystrokes (given you know
   well as to what will happen) and let the computer execute things at its own
   pace. This also is my major complaint against touch phones (compared to say,
   an old Nokia QWERTY phone).
 - This flow is much more universal and works on most systems: RaspberryPi,
   remote machines, Linux TTY.

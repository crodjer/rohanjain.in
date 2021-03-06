+++
title = "The ubiquitous shell"
slug = "bash"
date = "2015-03-05T00:00:00Z"
publishdate = "2015-03-05T00:00:00Z"
+++

Nothing makes a terminal feel more like home as one's own shell
configuration. My shell is bash, configured through a [.bashrc][] and
[.profile][]. Bash is likely to be present in any Unix machine one
uses.

Bash is also my primary IDE. This allows for keeping a relatively simple set of
utilities and tools to internalize. Coupled with a reliable window manager,
workflows can be quickly scripted in the head. Shell scripts are an option as
well for more complex flows. This can be done at multiple levels: using uniquely
identifiable comments in commands , bash functions, complete bash script.

Apart from the standard utilities (sed, cut, cat, tr etc.), I use a few tools as
an aid:

 - **[ag][]** for amazingly fast lookup in files.
 - **[rsync][]**, to keep the files synchronized with remote servers.
 - **[entr][]** to run build/test commands on a file change. Or, with
   rsync, synchronize on modifications.
 - **[autojump][]** to jump directly to frequently used directories.
 - **[jq][]** to process JSON output of a command.

Tools are designed for the console first, and then bound to editors or
IDEs. Familiarizing oneself to the command line interface makes the maximum
amount of functionality accessible. For example, [magit][] is a powerful git
interface for Emacs, but can be limiting for complex git operations such as an
interactive rebase.

Composibility of the various commands can be useful as well, allowing one to
build complex, repeatable commands. For example, the following uses `ip` to
change a system's default route, given the interface:

```bash
ip route del default &> /dev/null
ip route replace default via $(ip route
     | grep ppp0 # Change this filter as per need.
     | sed -r 's/\.0\/[[:digit:]]{2,3} /.1 /'
     | cut -d ' ' -f -4)
```

A great piece of literature on the Unix Philosophy is
[The Art of Unix Programming][taoup] by Eric S. Raymond. It also talks about the
Unix interface design.

[.bashrc]: https://github.com/crodjer/configs/blob/master/.bashrc
[.profile]: https://github.com/crodjer/configs/blob/master/.profile
[ag]: https://github.com/ggreer/the_silver_searcher
[rsync]: https://wiki.archlinux.org/index.php/rsync
[entr]: https://github.com/ggreer/the_silver_searcher
[autojump]: https://www.archlinux.org/packages/community/any/autojump/
[jq]: https://stedolan.github.io/jq/
[magit]: https://github.com/magit/magit
[taoup]: http://amzn.to/1qFlKCz

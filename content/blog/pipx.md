+++
title = "Bootstrapping pipx for the obsessed"
slug = "pipx"
date = "2021-09-04T19:41:19+05:30"
draft = false
+++

Managing Python based applications is made easy with
[pipx](https://github.com/pypa/pipx). But I dislike installing pipx
via a pip user install, as it results in two python utilities
managing packages. So, instead I let pipx manage it's own updates.

Here's how we can do it on a Debian system:

1. Install `python3-pip`, with:
   ```bash
   $ sudo apt install python3-pip
   ```
2. Install pipx:
   ```bash
   $ pip install --user pipx
   ```
   Didn't we want avoid that? Yes! We'll clean this up later.

   Note the installation output for the packages that were installed.
   In my case, they were: _pyparsing, click, userpath, packaging,
   argcomplete and pipx_.
3. Ask pipx to install itself:
   ```bash
   $ pipx install pipx
   ```
4. Now, we can clean up pip:
   ```bash
   $ pip uninstall -y pyparsing click userpath packaging argcomplete pipx
   $ sudo apt uninstall python3-pip --autoremove --purge
   ```
   If you are obsessed like me, you can also clean up the site
   packages directory. Only do this _after verifying_ that it is
   empty.
   ```bash
   $ rm -r ~/.local/lib/python3.9/site-packages
   ```
   In my case, it was safe to remove `~/.local/lib`.
5. As there already was a pipx in `~/.local/bin` from the pip
   install, pipx doesn't create a symlink in `~/.local/bin` when
   we did step 3. We need to manually do this:
   ```
   $ ln -sf ~/.local/pipx/venvs/pipx/bin/pipx ~/.local/bin/
   ```

That's it! Now we can manage packages with pipx, like:
```
$ pipx install neovim-remote
```
Upgrading all packages is trivial:
```
$ pipx upgrade-all
```

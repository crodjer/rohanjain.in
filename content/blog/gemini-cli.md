+++
title = "Run Gemini CLI Safely with Firejail"
slug = "gemini-cli"
date = "2025-12-13T14:49:04+05:30"
lastmod = "2025-12-14T15:32:04+05:30"
description = "Firejail can be used to safely run JavaScript applications like gemini-cli by restricting their environment."
draft = false
+++

I have always had a strict policy against running JS applications. But
`gemini-cli` looked promising, and I wanted to use it outside of a throwaway VM.

A friend introduced me to [firejail](https://github.com/netblue30/firejail),
which runs programs in a (configurable) restricted environment. This meant I
could safely use tools like `gemini-cli` and reduce my exposure to the
ever-present supply chain attacks in the JavaScript world.

## Running Gemini CLI
We don't even have to build the `gemini-cli` client. We can directly download
`gemini.js` from the `gemini-cli`
[release page](https://github.com/google-gemini/gemini-cli/releases/latest):

```bash
curl -fsLo $HOME/.gemini/gemini.js \
    https://github.com/google-gemini/gemini-cli/releases/latest/download/gemini.js
```

Now, we can run this in a firejail sandbox. I personally prefer to use `deno`:
```bash
DENO_COMPAT=1 firejail --whitelist=~/.gemini --whitelist=$PWD \
    $(which deno) \
    --allow-env --allow-sys --allow-read --allow-write --allow-run \
    ~/.gemini/gemini.js
```

This runs `deno` in the node compatible mode under `firejail` while allowing
access to `~/.gemini` and the current directories.

## My Setup
I have a more complex script which takes care of keeping `gemini.js` up to date
and runs it with stricter permissions.
I'd recommend [checking it out here](https://github.com/crodjer/configs/blob/master/neovim/.local/bin/gemini.sh).

In addition to the basic file system restrictions, this script also leverages
`deno`'s permission system. It also grants access to the
[mise](https://github.com/jdx/mise) directory.

A generalized `firejail` profile for `deno` may be configured in
`~/.config/firejail/deno.profile`. Check out `man firejail-profile` to learn how
to do this. For example:
```
quiet
include default.profile

whitelist ${HOME}/.local/share/mise
```

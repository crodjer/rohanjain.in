+++
title = "A productive workflow with vim sessions and servers"
slug = "yet-another-vim-productivity-post-server-client"
date = "2012-04-15T00:00:00Z"
publishdate = "2012-04-15T00:00:00Z"
+++

You can find lot of posts on the internet which try to tell you how to improve
the ways in which Vim is used. Well, here is another one.

## Vim Server, ZSH and Tiles

A Vim instance behaves as a server in which files can be opened through remote
applications. Read `:help client-server` of Vim to know more about this. I
generally keep multiple Vim sessions running, described by task they are related
to.

A typical workspace has one instance of Vim running and multiple terminals
around it.  Lets call this workspace for project *foo*. So the name of Vim
instance server will be *foo*:

    vim --servername foo

And any file will be open in this server, using this command:

    vim --servername foo --remote-silent bar.hs


    +-----------------------+
    |           |~$         |
    |           |           |
    |           |-----------+
    |    Vim    |~$         |
    |   Server  |           |
    |           |-----------+
    |           |~$         |
    |           |           |
    +-----------------------+


How this helps:

 - Lets me keep a well defined separation on various projects I am working on.
   The list of files in the buffer per instance remains short and manageable.

 - Also each instance can have its own project specific environment - current
   directory, python virtual environments etc.

For easing all this up, I use a set of helper functions in my *zshrc*:

```bash
# Set the name of vim session the terminal is tied up to
eset(){
    export VI_SERVER=$1
}

# Fire up a new server according to the argument supplied
vs(){
    eset $1
    vim --servername $VI_SERVER
}

# Open up the files in the environment Vim server.
es(){
    vim --servername $VI_SERVER --remote-silent $*
}

# Reuse Vim ZSH completions for vim completions
compdef _vim es

```

## Vim Sessions

Having multiple instances is great, but I don't want to set each up every time I
need resume working. For this I use *Vim Sessions*, which allow the current Vim
state to be stored over the disk.

[vim-session][vim-session] plugin will help you manage sessions easily. It is
well integrated with Vim client-server.

From docs:

> When you start Vim with a custom server name that matches one of the existing
> session names then the matching session will be automatically restored.

[vim-session]: https://github.com/xolox/vim-session
[zshrc]: https://github.com/crodjer/vimfiles
[vimconfig]: https://github.com/crodjer/configs/blob/master/zshrc

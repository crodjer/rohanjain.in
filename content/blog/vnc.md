+++
title = "VNC over reverse SSH"
slug = "vnc"
date = "2015-05-10T00:00:00Z"
publishdate = "2015-05-10T00:00:00Z"
+++

> Note: My [recent article](/jumper), talking about WireGuard is a
> better and more ergonomic approach to achive this, compared to using
> reverse SSH.

About a year ago, I switched my parents computer (lets call it `P`) from the
pre-installed OS (Windows) to Ubuntu. The primary reason for this switch was it
hard for me to debug and fix issues. Things have been smooth after the switch
with no issues which I found hard to fix remotely. What specially has worked is
my ability to log in to the computer when its on through my VPS and service it.

## Reverse SSH

Whenever `P` starts, it makes a reverse SSH connection to my VPS. I use
[autossh][autossh] to monitor and make sure that it is eventually always
connected.

```bash
$ autossh -M 10001 -f -NXYR 10000:localhost:22 vps
```

Here `vps` is a SSH alias for my personal VPS.

I can tunnel through the VPS and login to `P` via VPS using the following SSH
configuration:

```
Host home
     Hostname localhost
     User username
     Port 10000
     ProxyCommand ssh -e none -W %h:%p vps
```
To log in to `P` transparently, I can simply do:

```bash
$ ssh home
```

I use this to resolve most of the issues. Of course it does have the requirement
that the system be booted up and connected to a network.

## VNC

There also can be issues which have to do with the GUI (say browser maintenance)
and cannot be done easily over a text SSH session. For those cases, VNC is to
rescue. I create a VNC server on `P` using [x11vnc][x11vnc] and tunnel the VNC
port to my local through SSH. Now, I can access `P`'s graphical session remotely
via a vnc client (mine is [tigervnc][tigervnc]).


```bash
$ ssh home -L 5900:localhost:5900 "x11vnc -rfbauth ~/.vnc/passwd -display :0 -noxdamage"
$ vncviewer localhost::5900 -QualityLevel 3 -CompressLevel 6
```

This method is much better compared to the more common solutions such has
Google's VNC chrome extension or other proprietary services. But they are slow
and untrustable 3rd parties. With this way of setting up VNC, everything is
protected under SSH connections.

[x11vnc]: https://wiki.archlinux.org/index.php/X11vnc
[autossh]: https://www.archlinux.org/packages/community/x86_64/autossh/
[tigervnc]: https://www.archlinux.org/packages/community/x86_64/tigervnc/

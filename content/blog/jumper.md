---
title: "Hopping across the country with WireGuard"
slug: "jumper"
date: 2021-02-21T19:11:38+05:30
---

I need to be able to access all the computers over the
internet as they are spread across the country:

- `descartes`: My dad's Debian desktop (1800 km away).
- `kant`: RPi 3 running Arch (a NAS, printer, a wormhole).
- `camus`: RPi 4 running Manjaro (my personal work machine).
- `nietzsche`: My wife's Manjaro notebook.
- Phones: `newton`, `feyman`, `einstine` and `sagan`.
- `kepler`: a tiny Debian on Google Cloud.
- `webb`: a tiny Ubuntu Server on Oracle Cloud.

> All the computers actually have different astronomy inspired names.

Additionally, I found my lost RPi 2 hidden in a bag's pocket, so it'll
also find a place among all the others (likely a second wormhole at
my parens').

For a long time, I was using [reverse SSH based tunnels](/vnc/), which
worked reliably, but was slow (a US based VM) and not the cleanest. I
couldn't easily add machines and each machine would need to SSH in
opening a reverse SSH tunnel which I'd need to note somewhere.

For the last year or so though, I have been using
[ZeroTier](https://www.zerotier.com/). It works relibably and is easy
to administer through the web interface. Being peer-to-peer, it is
very low latency and sloved all my problems. So, the reverse SSH
tunnels was now a redundant setup for me.

That is, until now. I finally got around to setup a [WireGuard](https://www.wireguard.com) server.
WireGuard is surprisingly simple to setup and works reliably. There
are multiple guides on how to set it up (hence I didn't yet another),
like [this one](https://wiki.debian.org/SimplePrivateTunnelVPNWithWireGuard).
The way to administor it is a bit more involved than ZeroTier, but
once setup, it works transparently. Interestingly, the hop latency
doesn't take much of a hit because the cloud VM sits near the line
connecting me and my parents'.

I am happy with the setup now with ZeroTier and WireGuard serving as
each other's redundancy.

Interestingly, the VPNs aren't even the primary network tool that I
use. All these devices share most of their data with each other via
[syncthing](https://syncthing.net/). Syncthing is an awesome piece of technology allowing
me to have redundancy and local access to files. Run [a relay](https://docs.syncthing.net/users/strelaysrv.html) or
[donate](https://syncthing.net/donations/) if you can.

+++
title = "Switch between networks based on availability"
slug = "route-to"
date = "2015-08-19T00:00:00Z"
publishdate = "2015-08-19T00:00:00Z"
+++

Having been blessed with an unreliable internet, I always have had the need to
change the default route when one goes down. [ip][] provides you the required
tooling to do that. I had been doing it so often, that I ended up writing a
function to do this:

```bash
route_to () {
    interface=$1
    route=$(ip route | grep $interface | sed -r 's/\.0\/[[:digit:]]{2,3} /.1 /' | cut -d ' ' -f -4)
    if [ -n "$route" ]; then
        sudo ip route del default &> /dev/null
        sudo ip route replace default via $route
    else
        echo "No routes match: $interface" >&2
    fi
}
```

With this in available in my shell, I can simply do `route_to wlan0` and my
system will start using `wlan0` as the default interface.

[ip]: http://linux.die.net/man/8/ip

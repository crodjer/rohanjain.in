+++
title = "Configure ssh to handle proxies and tunnels seamlessly"
slug = "configure-ssh-to-handle-proxies-and-tunnels-seamlessly"
date = "2011-08-27T00:00:00Z"
publishdate = "2011-08-27T00:00:00Z"
+++

Recently I opened up on the port *443* over my EC2 instance (more or less like
a VPS), so that I can access it through any firewalled proxy too. Apart from
getting SSH running to a machine from outside world, I did some cool
configuration to have ssh deal with with various remote hosts automatically. It
uses *the right* proxy settings according the host being accessed.

All the stuff which runs over ssh (like *scp*, *git* etc.) also work the way
they are supposed to, following the ssh configuration.

To get this to work you need to have *corkscrew* and *netcat* (the swiss army
knife) already installed.

Here is how it goes:  
`File ~/.ssh/config:`

```apache
# Let me access local remotes directly.
Host 127.0.0.1, 10.*
    ProxyCommand none

# Github lets you ssh over port 443 too, utilize that instead of tunneling
# through remote computer
Host github.com
    Hostname ssh.github.com
    User git
    ProxyCommand corkscrew 10.3.100.211 8080 %h %p
    Port 443
    Hostname ssh.github.com

# Let me ssh onto my ec2 instance, which has full network access, through a
# http proxy.
Host ecc.rohanjain.in
    ProxyCommand corkscrew 10.3.100.211 8080 %h %p
    Port 443
    ServerAliveInterval 600

# All the rest should connect through an ssh connection over the ec2 instance.
Host *
    ProxyCommand ssh -q -a -x -p 443 ecc.rohanjain.in nc %h %p
```

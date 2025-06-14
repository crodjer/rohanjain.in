+++
date = "2017-03-18T05:28:10Z"
publishdate = "2017-03-18T05:28:10Z"
title = "Dynamic DNS with Cloudflare"
slug = "cloudflare-ddns"
+++

It is hard to maintain a regular internet uptime with home connections. 1/2
router re-connections per week can be fairly common. When that happens, the
public IP changes with most ISPs. This can be annoying if you want access your
computer remotely (possibly to SSH home). Dynamic DNS is a solution that exists
to solve just that.

I maintain DNS entries for my domain at
[Clouflare](https://www.cloudflare.com/). The latest version of
[`ddclient`](https://sourceforge.net/p/ddclient/wiki/Home/) supports Cloudflare
API, which works well with my ArchLinux box. But ddclient bundled with Raspbian,
is yet to get that update. Cloudflare's API is fairly straight forward, so I
decided to use a curl/systemd based solution on my RaspberryPi.

# Configuration information
If you too want to set Dynamic DNS with Cloudflare, you need to acquire some
configuration information first:

## Cloudflare configuration details
- `DNS Zone`  
  You can get it by visiting the overview page of Cloudflare dashboard (called
  Zone ID):
  `https://www.cloudflare.com/a/overview/<your-domain>`
- `Auth Email`: This is your cloudflare account email.
- `Auth Key`: This is your *Global API Key* under Cloudflare account settings:
  <https://www.cloudflare.com/a/account/my-account>
- `IDENTIFIER`  
  To get the identifier, you need to first set an A record for the subdomain you
  want to manage with Dynamic DNS. Then, plug in the subdomain in this curl
  query to obtain the identifier:

  ```bash
  curl "https://api.cloudflare.com/client/v4/zones/<your-dns-zone>/dns_records?name=<subdomain>.<your-domain>" \
       --silent -X GET \
       -H "X-Auth-Email: <cloudflare-auth-email>" \
       -H "X-Auth-Key: <cloudflare-auth-key>" \
       -H "Content-Type: application/json" 
  ```

## IP Address
I use `dig` with OpenDNS to get the current public IP and
<https://www.ipify.org/> as a fallback if `dig` isn't installed. Using `dig` is
faster though and you can install it via `dnsutils` (available as an official
package on most systems).


# The script
All the above information can now be plugged in this bash script to update the
DNS entry on Cloudflare. I placed it at `/usr/local/bin/cloudflare-ddns.sh`
```bash
#/usr/bin/env sh

# Get the Zone ID from: https://www.cloudflare.com/a/overview/<your-domain>
DNS_ZONE=<your-dns-zone>

# Get the existing identifier for DNS entry:
# https://api.cloudflare.com/#dns-records-for-a-zone-list-dns-records
IDENTIFIER=<your-domain-dns-entry-identifier>

# Get these from: https://www.cloudflare.com/a/account/my-account
AUTH_EMAIL=<cloudflare-auth-email>
AUTH_KEY=<cloudflare-auth-key>

# Desired domain name
DOMAIN_NAME="<subdomain>.<your-domain>"

# Get previous IP address
_PREV_IP_FILE="/tmp/public-ip.txt"
_PREV_IP=$(cat $_PREV_IP_FILE &> /dev/null)

# Install `dig` via `dnsutils` for faster IP lookup.
command -v dig &> /dev/null && {
    _IP=$(dig +short myip.opendns.com @resolver1.opendns.com)
} || {
    _IP=$(curl --silent https://api.ipify.org)
} || {
    exit 1
}

# If new/previous IPs match, no need for an update.
if [ "$_IP" = "$_PREV_IP" ]; then
    exit 0
fi

_UPDATE=$(cat <<EOF
{ "type": "A",
  "name": "$DOMAIN_NAME",
  "content": "$_IP",
  "ttl": 120,
  "proxied": false }
EOF
)

curl "https://api.cloudflare.com/client/v4/zones/$DNS_ZONE/dns_records/$IDENTIFIER" \
     --silent \
     -X PUT \
     -H "Content-Type: application/json" \
     -H "X-Auth-Email: $AUTH_EMAIL" \
     -H "X-Auth-Key: $AUTH_KEY" \
     -d "$_UPDATE" > /tmp/cloudflare-ddns-update.json && \
     echo $_IP > $_PREV_IP_FILE
```

Try running `cloudflare-ddns.sh` to see if DNS records get updated on
Cloudflare. For debugging purposes, I store the Cloudflare response at
`/tmp/cloudflare-ddns-update.json`.

# Systemd
We'll automate this execution by writing a systemd service and a timer.

## Configuring the service
My systemd service unit looks like this:
`/etc/systemd/system/cloudflare-ddns.service`
```
[Unit]
Description=Update DNS entry for this host to current IP

[Service]
Type=oneshot
ExecStart=/bin/sh /usr/local/bin/cloudflare-ddns.sh
```

Test the service by running:
```bash
sudo systemctl start cloudflare-ddns.service
# Check the results:
sudo journalctl -u cloudflare-ddns
```

## Setting up a timer
Set up a timer, so that DNS update is attempted every 2 minutes:
`/etc/systemd/system/cloudflare-ddns.timer`
```
[Unit]
Description=Update DNS entry in cloudflare every 2 minutes

[Timer]
OnBootSec=1min
OnCalendar=*:0/2
Unit=cloudflare-ddns.service

[Install]
WantedBy=basic.target
```

To enable the timer, run:
```bash
sudo systemctl enable cloudflare-ddns.timer
```

And that's it, now the system's IP will be kept up to date (within 2 minutes)
for remote access.

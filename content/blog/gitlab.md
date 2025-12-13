+++
title = "Self Hosting Gitlab using Quadlets"
slug = "gitlab"
date = "2025-10-26T09:27:21+05:30"
draft = false
+++

### What are Quadlets?
I self-host most of my services important to me personally. Lately, I have been
using [Quadlets](https://www.redhat.com/en/blog/quadlet-podman) for this. I came
across the concept of Quadlets from Immich.

For git repositories, I have been just using `git init --bare` in a server and
plain SSH to work them: `git clone git@<user>:sources/<repo>`.

Having used Quadlets with Immich, I thought of trying using them with Gitlab.
And this post talks about how to do that.

## Podman Setup
Before setting up the quadlet, there are some packages required for `podman` to
function fine with networking. I installed: `aardvark-dns, dbus-user-session,
passt, podman, uidmap`. But some packages from this list may not be needed for
`gitlab` as it’s just a single container.

## Gitlab Setup
Basically, all that we need to do is define:
`.config/containers/systemd/gitlab.container`

```systemd
[Unit]
Description=GitLab Container
After=network-online.target
Wants=network-online.target
# This ensures that the container won't start unless the directories are
# available.
# I need this since my file system is double encrypted - FDE with LUKS for
# system) and file-level encryption, managed by `ansible`. You might not need
# this if this path is guaranteed to be available.
ConditionPathExists=<path-to-data-directory>/repositories

[Container]
Image=docker.io/gitlab/gitlab-ce:latest
ContainerName=gitlab
ShmSize=256m

PublishPort=3939:80
PublishPort=2222:22

Volume=<path-to-data-directory>/repositories/config:/etc/gitlab:Z
Volume=<path-to-data-directory>/repositories/logs:/var/log/gitlab:Z
Volume=<path-to-data-directory>/repositories/data:/var/opt/gitlab:Z

# Explaining, GITLAB_OMNIBUS_CONFIG
# `external_url`: I have an external reverse proxy managed by my Ansible roles.
# `nginx['listen_port']: Just listen on `80`, so that I can manage the actual
# SSL from my reverse proxy outside.
# `nginx['listen_https']: Ask `nginx` to not do SSL inside the container.
# `gitlab_rails['gitlab_shell_ssh_port']: The port I use for listening for SSH.
Environment=GITLAB_OMNIBUS_CONFIG="external_url 'https://<my-hostname>'; nginx['listen_port'] = 80; nginx['listen_https'] = false; gitlab_rails['gitlab_shell_ssh_port'] = 2222;"

[Service]
Restart=always
TimeoutStartSec=900

[Install]
WantedBy=default.target
```

### Gitlab Omnibus Config
There's quite a bit going on in the `GITLAB_OMNIBUS_CONFIG`. My Ansible setup
takes care of setting up SSL and exposing it over `443`, so I don't forward that
to `gitlab`. That's what I configure `external_url` to. The `nginx` specific
configuration asks nginx to not do SSL and listen on `80` which I forward to
3939 outside. For SSH, the port I forward is `2222` as my node itself does its
ssh on `22`.

### Starting the Service
Once this unit file is set up, you should be able to start it with:

```bash
$ systemctl --user daemon-reload
$ systemctl --user enable --now gitlab.service
```

For the first launch, getting the image will take some time. You can look at the
status using:
`systemctl status --user gitlab`.


Once it is up and running, use `journalctl --user -fu gitlab` to look at the
service logs.

Now, ideally if you set up your reverse proxy right, you should be able to access
`gitlab` over your hostname.

### Gitlab Configuration
We do need to take care of one more thing, that is the root password. You can
find it under your config directory in the file `initial_root_password`. Use this
password with the username `root` for further configuration as per
[next steps documentation](https://docs.gitlab.com/install/next_steps/).

## Why Quadlets?
Podman and Quadlets can manage a significantly more complex configuration (for
instance, Immich via
[this handbook](https://github.com/linux-universe/immich-podman-quadlets/blob/main/README.md)).
I find this much easier to manage than `docker-compose.yaml`, which isn't
ergonomic with Ansible. Additionally, `podman` doesn't need any daemon running
like Docker for it to be useful.

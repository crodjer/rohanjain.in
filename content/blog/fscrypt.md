---
title: "Encrypting a home in a Raspberry Pi"
slug: fscrypt
date: 2021-05-24T11:22:27+05:30
draft: false
---

> ;tldr
> Use [fscrypt](https://wiki.archlinux.org/title/Fscrypt#Encrypt_a_home_directory) to encrypt your
> home partition, with a few simple steps.

## Why encryption?
I prefer to encrypt all my (and my family's) storage at rest.
Encryption is useful in scenarios where someone gains physical access
to your device. This may save you from any losses other than the
material cost of the device. Specially with a Raspberry Pi, it is more
of a worry as how tiny the device is (the SD card is even smaller).
It is much less likely that you _lose_ your desktop tower. Its
worrying how many people carry around their portable computing devices
(phones/laptops/tables) around without encryption at rest.

This allowed me to safely RMA a failed SD card. If I hadn't encrypted
the data, I would have had to destroy an expensive SD card. Could as
easily happen with any other portable device.

## Encrypting the Pi
Both Manjaro or Raspbian do not encrypt by default. To get around
that, I have been relying on [ecryptfs](https://wiki.archlinux.org/title/ECryptfs#Encrypting_a_home_directory)
to encrypt my sensitive directories. Recently though, I switched to
using [fscrypt](https://wiki.archlinux.org/title/Fscrypt#Encrypt_a_home_directory),
which `ext4` and `f2fs` support natively in most recent kernels. The
goal of this post is to outline the key steps to get usable encryption
working with the Pi.

### Enable `encrypt` feature flag
If you are already on `ext4` / `f2fs`, you can simply enable the
`encrypt` feature flag:

```bash
tune2fs -O encrypt /dev/mmcblk0p2
```

`/dev/mmcblk0p2` here is the mount point for my root file system.

### Install `fscrypt`
To be able to use `fscrypt`, you'll need to install the userspace
tools. On Arch/Manjaro:

```bash
$ pacman -S fscrypt
```

On Debian/Raspbian:

```bash
$ apt install fscrypt libpam-fscrypt
```

### Setup `fscrypt`
To setup fscrypt, I'd recommend you to login as `root`, without any
session running as your target user (say `penguin`).

```bash
root@pi:/home# fscrypt setup
Defaulting to policy_version 2 because kernel supports it.
Customizing passphrase hashing difficulty for this system...
Created global config file at "/etc/fscrypt.conf".
Metadata directories created at "/.fscrypt".
```

### Configure the new home directory
```bash
root@pi:/home# mkdir /home/penguin-new-home
root@pi:/home# fscrypt encrypt penguin-new-home --user=penguin
```

When prompted, choose `1` (pam_passphrase) to decrypt using the login
password:

```bash
The following protector sources are available:
1 - Your login passphrase (pam_passphrase)
2 - A custom passphrase (custom_passphrase)
3 - A raw 256-bit key (raw_key)
Enter the source number for the new protector [2 - custom_passphrase]: 1

IMPORTANT: Before continuing, ensure you have properly set up your system for
           login protectors.  See
           https://github.com/google/fscrypt#setting-up-for-login-protectors

Enter login passphrase for penguin:
"penguin-new-home" is now encrypted, unlocked, and ready for use.
```

Copy over the contents to the new encrypted directory:

```bash
root@pi:/home# cp -a -T /home/penguin /home/penguin-new-home
```
Test the encryption 🤞:

```bash
root@pi:/home# echo foo > penguine-new-home/bar
root@pi:/home# fscrypt lock penguin-new-home --user=penguin
"penguin-new-home" is now locked.
root@pi:/home# ls penguin-new-home/bar
ls: cannot access 'penguin-new-home/bar': No such file or directory
root@pi:/home# fscrypt unlock penguin-new-home
Enter login passphrase for penguin:
"penguin-new-home" is now unlocked and ready for use.
root@pi:/home# cat penguin-new-home/bar
foo
```

Great, encryption works!

> If you also plan to login directly to your Pi (without SSH), would
> recommend using the [PAM Module](https://wiki.archlinux.org/title/Fscrypt#PAM_module)
> on **Manjaro/Arch** to automatically unlock the directory. It also
> will allow you to keep your home directory's passphrase in sync with
> changes to the login passphrase.
> On **Debian**, `libpam-fscrypt` should have already configured PAM.
>
> You can test this by logging in as `penguin` on a reboot. Observe
> that the home directory should be decrypted.

Now you may move the encrypted directory as the home.

```bash
root@pi:/home# mv penguin penguin.bk
root@pi:/home# mv penguin-new-home penguin
```

Test if everything in the system works as per your liking and then
remove `/home/penguin.bk`.

> Although, if the existing home had data, the content may still be
> recoverable. Would recommend
> [wiping the free space](https://wiki.archlinux.org/title/Securely_wipe_disk/Tips_and_tricks#Wipe_free_space).

### Remote Access
Because the `ssh` daemon needs access to
`/home/penguin/.ssh/authorized_keys`, you will not be able to login
remotely. To get around this, we have to move our `authorized_keys`
file on the unencrypted space. Create a file
`/etc/ssh/sshd_config.d/overrides.conf` with the following content:

```sshdconfig
AuthorizedKeysFile      .ssh/authorized_keys  /etc/ssh/authorized_keys/%u
PasswordAuthentication  no
```
This adds `/etc/ssh/authorized_keys/<user>` as a file to look for a
user's authorized keys. You can maintain the authorized keys in
`/etc/ssh/authorized_keys/penguin`:

```bash
root@pi:/# mkdir /etc/ssh/authorized_keys
root@pi:/# cp /home/penguin/.ssh/authorized_keys /etc/ssh/authorized_keys/penguin
root@pi:/# chown penguin /etc/ssh/authorized_keys/penguin
root@pi:/# chmod 600 /etc/ssh/authorized_keys/penguin
root@pi:/# ln -sf /etc/ssh/authorized_keys/penguin /home/penguin/.ssh/authorized_keys
root@pi:/# systemctl restart sshd
```

SSH should now work again! If you work with `tmux`, you can
automatically decrypt before logging in:

```bash
ssh penguin@pi -t -- 'fscrypt unlock /home/penguin; tmux -u new -As pi'
```

## Concerns
1. Encrypting of files on a Pi can impact the system performance, as
   encryption/decryption needs the CPU. If you need to do a lot of
   disk IO (eg: build files), I'd recommend moving those operations to
   an unencrypted directory or the memory.
2. Only the home directory is encrypted, so rest is still vulnerable.
   Someone could get an hold of your SSL certificates, ZeroTier token,
   WireGuard token. Ensure these are on an encrypted directory as much
   as possible. In case of a disk loss, review your backups to
   rotate all the keys.
3. This doesn't protect you from an attack in a running machine, so
   use a firewall. I'd recommend
   [ufw](https://wiki.archlinux.org/title/Uncomplicated_Firewall).

> Ads:
>
> Learn: [How Linux Works](https://amzn.to/3kxb1d0), [The Linux
Command Line](https://amzn.to/3ijmWZ9)  
> Get a [Raspberry Pi 4](https://amzn.to/3ijRwlA), A fast [SD Card](https://amzn.to/3z9Cjdu)

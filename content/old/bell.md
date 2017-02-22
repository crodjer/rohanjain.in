+++
title = "Using pulseaudio to play system beeps"
slug = "bell"
date = "2015-08-20T00:00:00Z"
+++

Recently, I built a new desktop. After receiving everything, I realized that I
was missing the onboard speaker (which creates the annoying beep sound).
Ideally, the computer case should have had it, but they don't come with a buzzer
anymore. My various applications rely on the system beep, hence I needed to find
a software alternative. Turns out it is fairly trivial to configure it to
capture alerts with pulseaudio:

```bash
start-pulseaudio-x11
pactl upload-sample /usr/share/sounds/freedesktop/stereo/message.oga beep
pactl load-module module-x11-bell sample=beep
xset b 100
```

To automatically do it on X11 start, I placed the above in my
[xsessionrc](https://github.com/crodjer/configs/blob/121caa22d4b7c6324fa9a5b22e2d2fcc334afc96/.xsessionrc#L31).

+++
title = "Cooling the Raspberry Pi 4 with Argon Neo"
slug = "pi-cooling"
date = "2021-01-08T10:21:02+05:30"
+++

The Raspberry Pi 4 runs hot, specially when overclocked. As I use it
as my programming machine for my personal projects, I wanted to get
the most out of its CPU.

The [first case](https://twitter.com/__crodjer__/status/1265526804808372224/photo/1) I ordered for my Pi came with a fan which worked on
3.3V and 5V (too noisy). I setup a circuit which would allow me to
turn it on/off via a GPIO pin controlled by
[this python based service](https://github.com/crodjer/configs/blob/master/scripts/rpi-fan.py),
which turns the fan on/off based on certain threshold temperatures.
This did work but the idling Pi would always go beyond the threshold
temperature turning the fan on when. With the threshold at 65°C. I was
getting 1:10 on:off ratio, which meant fan noise every 10 minutes of
idle time.

I spent some time trying to figure out the best cooling setup for the
Pi with a target of staying cool with a sustained load. To test the
cooling setup, I wrote [this script](https://gist.github.com/crodjer/8a79078a058db4afe0d74527efd29e30).
The key benchmarking command that the script run is:
```bash
sysbench cpu --threads=4 --events=200000 --time=0 --cpu-max-prime=100000 run
```

I did not get satisfactory results with my plastic case + small heat
sink + fan setup. Eventually, I found a way to purchase a fantastic
passively cooled case: [Argon Neo](https://twitter.com/__crodjer__/status/1346864027473825793).
It looks great and works as a heat sink itself. With the Neo, I now
have a silent setup which would sustain long running CPU bound tasks
with an even higher overclock of 2100 MHz without throttling.

A few of the results from the numerous benchmarks I did with
[the script](https://gist.github.com/crodjer/8a79078a058db4afe0d74527efd29e30):

|Events|Configuration             |Overclock|Fan |Temp (°C)|Events/Sec|
|------|--------------------------|---------|----|---------|----------|
|50000 |Plastic Case + Heat Sink  |2000 MHz |No  |83.7 (T) |310.89    |
|50000 |Plastic Case + Heat Sink  |2000 MHz |3.3V|75.0     |340.84    |
|50000 |Plastic Case + Heat Sink  |2000 MHz |5V  |63.3     |339.36    |
|50000 |Plastic Case + Heat Sink  |2147 MHz |5V  |77.9     |366.46    |
|100000|Plastic Case + Heat Sink  |2000 MHz |3.3V|79.8     |337.47    |
|100000|Neo + Thermal Paste       |2000 MHz |No  |81.3 (T) |342.85    |
|100000|Neo without cover + Paste |2000 MHz |No  |75.4     |342.93    |
|100000|Neo + Thermal Paste       |2000 MHz |5V  |76.4     |340.66    |
|100000|Neo + Thermal Paste       |2000 MHz |5V  |76.4     |340.66    |
|100000|Neo + Thermal Pad         |2100 MHz |No  |75.0     |360.07    |
|200000|Neo + Thermal Pad         |2100 MHz |No  |78.8     |360.18    |

A few observations I made with the Neo:

- Neo performs much better without the cover on because of
  insufficient thermal connectivity between the cover and the case.
- Adding a fan made not much of a difference. Specially with the cover
  when there is no ventilation.
- The case works better when the room fan is on which is likely when I
  am working on the Pi. I observe 3-4°C lower temperatures.
- The included thermal pads performed much better than the thermal
  paste I had with me.
- Although the case is Aluminium, I didn't observe much impact on the
  wireless signal. Likely because of the plastic base.

# UHDAPI
This package provides an interface to the UHD C API to interact with USRP Software defined Radio hardware.

It provides a relatively thin wrapper around the C API, but hides all the low level pointers from the user.

A simple program to receive some data could look like this:

```julia
using UHDAPI

channel=0;
freq=433.1e6
rate=1e5;
gain=40;

# Get the USRP object.
usrp=Uhd_usrp("");

# Set the frequency
set_rx_freq(usrp,freq,channel);

# Set the sampling rate
set_rx_rate(usrp,rate,channel);

# Set the RX gain
set_rx_gain(usrp,gain,channel,"PGA");

# Specify stream args: Complex floats on the computer, and Complex Int16 over the wire.
sargs=Uhd_stream_args("fc32","sc16","",[channel]);

# Get the RX streamer
stream=get_rx_stream(usrp,sargs)

# Specify streaming command: Stream finite number of samples, start immediately.
start_cmd=Uhd_stream_cmd(UHD_STREAM_MODE_NUM_SAMPS_AND_DONE,1e5,true,0,0);

# Define buffer for the data
buffer=Vector{Complex{Float32}}(undef,100000);

# Issue the command to start streaming
issue_stream_cmd(stream,start_cmd);

# Receive the samples
(n,md)=recv!([buffer],stream,2.0)
```

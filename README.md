This is a fairly simple program which receives the data stream from a Mastech MS2115B meter and outputs it to the console as a decoded value. The data rate is determined by the meter, but there will typically be an update a couple of times a second.

This tries- occasionally successfully- to work out which USB/serial port is connected to the meter. Because this uses a lot of OS-specific stuff it is not anticipated that it will be easily portable to anything except Linux, but the remainder of the program should be more tolerant. If the port can't be determined automagically, then it must be specified on the command line as the final parameter.

The output is basically a timestamped line showing the meter's current range and value. Alternatively, the -F or --format parameter can take a printf-style string, in which case the value (and only the value) is output formatted as specified. If the -F parameter is @, then raw hex-formatted packets are output.

In the general case, use %x@yBzd or %x@yLzd for offset x and y bytes of big/little-endian data with C format %zd etc. There's lots more possibilities to allow the reading to be scaled and offset using (postfix, RPN) operations, please refer to the source for details.

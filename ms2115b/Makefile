# This is intended to build a text-only variant of the program in this directory, based on
# http://troydm.github.io/blog/2014/01/26/making-30-years-old-pascal-code-run-again/
# It probably won't attempt anything fancy like working out what revision the sources are
# which might result in warnings during compilation and possibly linkage, to do the
# whole job properly use lazbuild or the Lazarus IDE. MarkMLl.

# PREREQUISITE: FPC (Free Pascal Compiler), v2.6.0 but preferably v3.0.2 or above.

############

# FPCFLAGS can usefully be transcribed from the Lazarus IDE's "Show Options" output.

FPC=/usr/local/bin/fpc
FPCFLAGS=-O3 -CX -XX -k--build-id 
DEFINES=
INIFILES=../inifiles/trunk
CPU=$(shell uname -m | sed 's/i686/i386/' | sed 's/armv7l/arm/')
OPSYS=$(shell uname -o | sed 's/GNU\/Linux/linux/')
FPCSrcDir=/usr/local/src/fpc/$(shell $(FPC) -iV)/compiler

# Note that the CPU identifier above loses potentially-useful information, but
# this was done in order to generate the same filenames emitted by Lazarus.

############

all: ms2115b-$(CPU)-$(OPSYS)

# NOTE THAT THIS MIGHT ONLY BE A PARTIAL DEPENDENCY LIST.

ms2115b-$(CPU)-$(OPSYS): Makefile ../common/inifilesabout.pas ../common/locatecp210xport.pas consoleapp.pas ms2115b.lpr 
	$(FPC) $(FPCFLAGS) $(DEFINES) -Fu$(INIFILES) -Fu../common -oms2115b-$(CPU)-$(OPSYS) ms2115b.lpr 

############

clean:
	rm -f ms2115b*.ppu ms2115b*.o ms2115b.or
	rm -f $(INIFILES)/inifilesabout.o $(INIFILES)/inifilesabout.ppu
                
distclean: clean
	rm -f ms2115b-$(CPU)-$(OPSYS)


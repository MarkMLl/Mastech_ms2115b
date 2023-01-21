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
DEFINES=-dHAS_SVN -dHAS_FPC_SVN
INIFILES=../inifiles/trunk
SERIALCOMMS=../serialcomms/trunk
SCPISRV=../scpisrv/trunk
CPU=$(shell uname -m | sed 's/i686/i386/' | sed 's/armv7l/arm/')
OPSYS=$(shell uname -o | sed 's/GNU\/Linux/linux/')
FPCSrcDir=/usr/local/src.fpc/$(shell $(FPC) -iV)/compiler

# Note that the CPU identifier above loses potentially-useful information, but
# this was done in order to generate the same filenames emitted by Lazarus.

############

_=ms2115b
TARGET=$_-$(CPU)-$(OPSYS)

all: $(TARGET)

any: Makefile project_svnrevision.inc fpc_svnrevision.inc

project_svnrevision.inc:
	echo -n C`svnversion -n`C |tr A-G %-+ >project_svnrevision.inc

fpc_svnrevision.inc:
	echo -n C`svnversion -n $(FPCSrcDir)`C |tr A-G %-+>fpc_svnrevision.inc

# NOTE THAT THIS MIGHT ONLY BE A PARTIAL DEPENDENCY LIST.

$(INIFILES)/inifilesabout.o: any $(INIFILES)/inifilesabout.pas
$(SERIALCOMMS)/locateports.o: any $(SERIALCOMMS)/locateports.pas
$(SERIALCOMMS)/locatecp210xport.o: any $(SERIALCOMMS)/locatecp210xport.pas
$(SERIALCOMMS)/netlink.o: any $(SERIALCOMMS)/netlink.pas
$(SCPISRV)/scpiserver.o: any $(SCPISRV)/scpiserver.pas
$(SCPISRV)/scpiparser.o: any $(SCPISRV)/scpiparser.pas

$(TARGET): any $(INIFILES)/inifilesabout.pas \
  $(SERIALCOMMS)/locatecp210xport.pas $(SERIALCOMMS)/locateports.pas \
  $(SERIALCOMMS)/netlink.pas $(SCPISRV)/scpiserver.pas $(SCPISRV)/scpiparser.pas \
  consoleapp.pas $_.lpr 
	$(FPC) $(FPCFLAGS) $(DEFINES) -Fu$(INIFILES) -Fu$(SERIALCOMMS) \
  -Fu$(SCPISRV) -o$(TARGET) $_.lpr 

test: $(TARGET)
	./$(TARGET) --version

############

clean:
	rm -f *.ppu *.o $_.or project_svnrevision.inc fpc_svnrevision.inc
	rm -f $(INIFILES)/inifilesabout.o $(INIFILES)/inifilesabout.ppu
	rm -f $(SERIALCOMMS)/locatecp210xport.o $(SERIALCOMMS)/locatecp210xport.ppu
	rm -f $(SERIALCOMMS)/netlink.o $(SERIALCOMMS)/netlink.ppu
	rm -f $(SCPISRV)/scpiserver.o $(SCPISRV)/scpiserver.ppu
	rm -f $(SCPISRV)/scpiparser.o $(SCPISRV)/scpiparser.ppu
        
distclean: clean
	rm -f $(TARGET)* *lps $_.tar $_.tar.gz $_.tgz $_.zip

pubclean: distclean
	rm -rf backup lib *~

PUBFILES=../$_/* ../common/* $(INIFILES)/inifilesabout.pas \
  $(SERIALCOMMS)/locateports.pas $(SERIALCOMMS)/locatecp210xport.pas \
  $(SERIALCOMMS)/netlink.pas $(SCPISRV)/scpiserver.pas $(SCPISRV)/scpiparser.pas

tar: pubclean
	tar chf $_.tar $(PUBFILES)

tar.gz: pubclean
	tar chzf $_.tar.gz $(PUBFILES)

tgz: tar.gz
	mv $_.tar.gz $_.tgz

zip: pubclean
	zip $_.zip $(PUBFILES)

publish: zip

PUBDIR:=$(shell mktemp -d /tmp/$_-XXX)

pubtest: zip
	- unzip -d $(PUBDIR) $_.zip | grep -v warning:
	make -C $(PUBDIR)/$_ test
	rm -rf $(PUBDIR)


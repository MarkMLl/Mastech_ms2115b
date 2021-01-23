(* Lazarus+FPC 2.1.0+3.0.4 on Linux Lazarus+FPC 2.1.0+3.0.4 on Linux Lazarus+FP *)

unit LocateCp210Port;

(* Find a piece of test equipment (specifically, a Masteck MS2115B) with an     *)
(* embedded Silicon Labs CP210 chip. The bulk of this is unchanged from DSOcat  *)
(* which interfaces to a DSO112A, and before that from a program to detect and  *)
(* interface to a Logitech LG600 gamer's mouse. MarkMLl.                        *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Serial;

const
  InvalidSerialHandle= TSerialHandle(-1);

(* A numbered list of ports is prepared at startup, or any time this is called
  and no port is open. The list is cached as necessary and also returned as a
  StringList, which should be freed by the caller.
*)
FUNCTION ListPorts(currentPort: TSerialHandle= InvalidSerialHandle): TStringList;

(* Assuming that the OS is Linux, walk the available serial ports looking for
  a CP210 which is what's in the Mastech 2115B. Note that this is also used by
  the DSO112A, so it's not entirely foolproof... if it were an FTDI chip it
  could be branded with the device serial number (or model number etc.) as a
  one-time operation which would make it more reliable.
*)
function FindMs2115bPort(portScan: boolean= false): string;

(* Assuming that the cached list has been properly populated, dump its contents.
*)
procedure DumpCachedPorts;


implementation

uses
  StrUtils, BaseUnix, Regexpr;


(* Read manufacturer name etc. This is a standard file operation handling data
  generated by the kernel, so I'm being fairly relaxed about the risk of
  overrun etc.
*)
function cat(const fn: string): string;

var
  handle: THandle;
  buffer: array[0..255] of byte;

begin
  handle := FileOpen(fn, fmOpenRead);
  if handle >= 0 then
    try
      FillByte(buffer, SizeOf(buffer), 0);
      if FileRead(handle, buffer, SizeOf(buffer) - 1) > 0 then
        result := Trim(StrPas(Addr(buffer)))  (* Discard trailing EOL etc.    *)
      else
        result := ''
    finally
      FileClose(handle)
    end
  else
    result := ''
end { cat } ;


type
  TWalkTestPredicate= function(const dir: string; const name: string;
                        const content: string; const extra: string;
                                                hint: integer= -1): boolean;


(* Look in the directory specified as the first parameter (terminated by /) and
  either return the canonical path if a directory matching the second parameter
  is encountered or recurse into any other directories. If the third parameter
  exists then the directory that is found must contain it, however it is not
  appended to the result and the result is not terminated by /, in other cases
  the result is terminated by /. The final parameter is an optional test which
  is applied after all naming requirements have been met.

  We know that this is being used on the /sys filesystem which contains numerous
  recursive symlinks, so need to look at real directories *only*. The format of
  the directory names appears to have been stable from Linux 2.6 to at least 4,
  however the position in the tree that the hidraw directory is located depends
  on e.g. whether the mouse device is connected by USB or Bluetooth:

  USB: /sys/devices/pci0000:00/0000:00:03.1/usb4/4-3/4-3:1.0/0003:1BCF:0053.0009/hidraw

  Bt:  /sys/devices/virtual/misc/uhid/0005:248A:8266.000D/hidraw

  The hidraw directory (note: no appended number) contains an hidrawN directory
  (note: one or more decimal digits appended) which should match something in
  /dev giving us a file name which can be handled by open(), ioctl() and so on.
*)
function WalkDirs(dir: string; pattern: string; content: string= '';
                        wtp: TWalkTestPredicate= nil; extra: string= '';
                                                hint: integer= -1): string;

(* This is lifted from my Logitech G600 interface program.                      *)

var
  searchRec: TUnicodeSearchRec;
  sorter: TStringList;
  i: integer;
  candidate: string;


  function wtp2(const dir: string; const name: string; const content: string;
                        const extra: string; hint: integer= -1): boolean; inline;

  begin
    if assigned(wtp) then
      result := wtp(dir, name, content, extra, hint)
    else
      result := true
  end { wtp2 } ;


begin
  result := '';
  if FindFirst(dir + '*'{%H-}, faDirectory, searchRec) = 0 then begin
    sorter := TStringList.Create;
    sorter.Sorted := true;
    try

(* It is not safe to assume that the kernel returns names in a natural or       *)
(* reproducible order, particularly when looking at e.g. /sys, so handle it in  *)
(* two stages.                                                                  *)

      repeat
        if searchRec.Name[1] = '.' then (* Always ignore . and .. completely    *)
          continue;

(* For everything in the directory, make sure it's a subdirectory and not a     *)
(* symlink. Don't try to use searchRec.Attr for this.                           *)

        if FileGetAttr({%H-}dir + searchRec.Name) and (faDirectory + faSymLink{%H-}) = faDirectory then
          sorter.Add(searchRec.Name{%H-})
      until FindNext(searchRec) <> 0;
      for i := 0 to sorter.Count - 1 do begin
        candidate := sorter[i];         (* Make visible for debugging           *)

(* Does the subdirectory name match the pattern?                                *)

        if IsWild(candidate, pattern, false) then

(* If there's content specified does it exist in the subdirectory, a symlink    *)
(* being acceptable in this case? Note that this also determines whether / is   *)
(* appended to the result, the rationale being that if we are explicitly        *)
(* looking for some content then we're likely to be continuing to process the   *)
(* tree in some form. If all conditions are satisfactory then apply one final   *)
(* test passed as a function, which can be used to distinguish between e.g.     *)
(* multiple endpoints associated with the same USB device.                      *)

          if content = '' then
            if wtp2(dir, candidate, content, extra, hint) then
              exit(dir + candidate)
            else begin end              (* Watch for dangling else here         *)
          else
            if FileExists(dir + candidate + '/' + content) then
              if wtp2(dir, candidate, content, extra, hint) then
                exit(dir + candidate + '/');

(* We've got a subdirectory but either it doesn't match the pattern or it lacks *)
(* the expected content: recurse.                                               *)

        result := WalkDirs(dir + candidate + '/', pattern, content, wtp, extra, hint);

(* If recursion found a hit then return it.                                     *)

        if result <> '' then
          exit
      end
    finally
      Sorter.Free;
      FindClose(searchRec)
    end
  end
end { WalkDirs } ;


const
  cachedDriver= 0;
  cachedProduct= 1;
  cachedSerial= 2;
  cachedTop= cachedSerial;

var
  portsListCache: TStringList= nil;


(* With the naming requirements met, make one final check that the serial
  device has a plausible description.
*)
function testDriver(const dir: string; const name: string; const content: string;
                                                const extra: string; hint: integer= -1): boolean;

var
  scratch: string;

begin

(* The directory parameter will look something like /sys/devices/pci0000:00/0000:00:1d.0/usb2/2-2/2-2:1.0/ttyUSB2/tty *)
(* ...ttyUSB2/driver should be a symlink to /sys/bus/usb-serial/drivers/cp210x  *)
(* but might be distressingly relative.                                         *)

  scratch := dir + name + '/' + content;
  scratch := fpReadLink(scratch);
  if hint >= 0 then begin
    Assert(portsListCache.Objects[hint] is TStringList, 'Internal error: bad cached port list.');
    TStringList(portsListCache.Objects[hint])[cachedDriver] := ExtractFileName(scratch)
  end;
  result := pos('/' + extra, scratch) > 0
end { testDriver } ;


(* Return true if the device named in the form /dev/ttyUSBn appears to be using
  a plausible device driver module.
*)
function usingDriver(const device, driver: string; hint: integer= -1): boolean;

var
  name, path: string;

begin
  result := false;
  name := ExtractFilename(device);
  path := WalkDirs('/sys/devices/', name, 'driver', @testDriver, driver, hint); (* Appends /  *)
  result := path <> ''
end { usingDriver } ;


(* With the naming requirements met, make one final check that the serial
  device has a plausible description.
*)
function testProduct(const dir: string; const name: string; const content: string;
                                                const extra: string; hint: integer= -1): boolean;

var
  scratch: string;

begin

(* The directory parameter will look something like /sys/devices/pci0000:00/0000:00:1d.0/usb2/2-2/2-2:1.0/ttyUSB2/tty *)
(* ...ttyUSB2/driver should be a symlink to /sys/bus/usb-serial/drivers/cp210x  *)
(* but might be distressingly relative.                                         *)

  scratch := dir + name + '/' + content;
  if not FileExists(scratch) then
    scratch := ''
  else
    scratch := cat(scratch);
  if hint >= 0 then begin
    Assert(portsListCache.Objects[hint] is TStringList, 'Internal error: bad cached port list.');
    TStringList(portsListCache.Objects[hint])[cachedProduct] := scratch
  end;
  result := extra = scratch
end { testProduct } ;


(* Return true if the device named in the form /dev/ttyUSBn has a plausible
  product description.
*)
function usingProduct(const device, product: string; hint: integer= -1): boolean;

var
  name, path: string;

begin
  result := false;
  name := ExtractFilename(device);
  path := WalkDirs('/sys/devices/', name, '../../product', @testProduct, product, hint); (* Appends /  *)
  result := path <> ''
end { usingProduct } ;


(* With the naming requirements met, make one final check that the serial
  device has a plausible description.
*)
function testSerial(const dir: string; const name: string; const content: string;
                                                const extra: string; hint: integer= -1): boolean;

var
  scratch: string;

begin

(* The directory parameter will look something like /sys/devices/pci0000:00/0000:00:1d.0/usb2/2-2/2-2:1.0/ttyUSB2/tty *)
(* ...ttyUSB2/driver should be a symlink to /sys/bus/usb-serial/drivers/cp210x  *)
(* but might be distressingly relative.                                         *)

  scratch := dir + name + '/' + content;
  if not FileExists(scratch) then
    scratch := ''
  else
    scratch := cat(scratch);
  if hint >= 0 then begin
    Assert(portsListCache.Objects[hint] is TStringList, 'Internal error: bad cached port list.');
    TStringList(portsListCache.Objects[hint])[cachedSerial] := scratch
  end;
  result := extra = scratch
end { testSerial } ;


(* Return true if the device named in the form /dev/ttyUSBn has a plausible
  serial number.
*)
function usingSerial(const device, serial: string; hint: integer= -1): boolean;

var
  name, path: string;

begin
  result := false;
  name := ExtractFilename(device);
  path := WalkDirs('/sys/devices/', name, '../../serial', @testSerial, serial, hint); (* Appends /  *)
  result := path <> ''
end { usingSerial } ;


(* Assume that the list contains name=value pairs, and order by value.
*)
function compareValues(List: TStringList; Index1: Integer; Index2: Integer):Integer;

var
  v1, v2: integer;

begin
  v1 := StrToIntDef(ExtractDelimited(2, List[Index1], ['=']), -99999);
  v2 := StrToIntDef(ExtractDelimited(2, List[Index2], ['=']), +99999);
  if v1 = v2 then
    result := 0
  else
    if v1 < v2 then
      result := -1
    else
      result := +1
end { compareValues } ;


(* A numbered list of ports is prepared at startup, or any time this is called
  and no port is open. The list is cached as necessary and also returned as a
  StringList, which should be freed by the caller.
*)
FUNCTION ListPorts(currentPort: TSerialHandle= InvalidSerialHandle): TStringList;

(* This is lifted from a port I'm working on of some very old DOS/Windows code  *)
(* implementing a proprietary protocol.                                         *)

var
  searchRec: TSearchRec;
  majorList, minorList: TStringList;
  r: TRegExpr;
  i, j: integer;


  (* The parameter is a number, extract as implemented in the clib macros.
  *)
  function major(dev: qword): dword;

//  __NTH (gnu_dev_major (unsigned long long int __dev))
//  {
//    return ((__dev >> 8) & 0xfff) | ((unsigned int) (__dev >> 32) & ~0xfff);
//  }

  begin
    result := ((dev shr 8) and $0fff) or ((dev shr 32) and $fffff000)
  end { major } ;


  (* The parameter is a number, extract as implemented in the clib macros.
  *)
  function minor(dev: qword): dword;

//  __NTH (gnu_dev_minor (unsigned long long int __dev))
//  {
//    return (__dev & 0xff) | ((unsigned int) (__dev >> 12) & ~0xff);
//  }

  begin
    result := (dev and $00ff) or ((dev shr 12) and $ffffff00)
  end { minor } ;


  (* The parameter is a number as documented in the inode structure.
  *)
  function isChr(mode: dword): boolean;

  begin
    result := mode and &0120000 = &0020000
  end { isChr } ;


  (* The parameter is the entire device name.
  *)
  function major(const devName: string): dword;

  var
    devStat: Stat;

  begin
    FillByte(devStat{%H-}, SizeOf(devStat), 0);
    if FpStat(devName, devStat) <> 0 then
      result := 0
    else
      result := major(devStat.st_rdev)
  end { major } ;


  (* The parameter is the entire device name.
  *)
  function minor(const devName: string): dword;

  var
    devStat: Stat;

  begin
    FillByte(devStat{%H-}, SizeOf(devStat), 0);
    if FpStat(devName, devStat) <> 0 then
      result := 0
    else
      result := minor(devStat.st_rdev)
  end { minor } ;


  (* The parameter is the entire device name.
  *)
  function isChr(const devName: string): boolean;

  var
    devStat: Stat;

  begin
    FillByte(devStat{%H-}, SizeOf(devStat), 0);
    if FpStat(devName, devStat) <> 0 then
      result := false
    else
      result := isChr(devStat.st_mode)
  end { isChr } ;


  (* The parameter is the first part of the device name, i.e. with no appended
    digits.
  *)
  function blacklisted(const {%H-}devName: string): boolean;

  begin
    result := false
  end { blacklisted } ;


begin
  if currentPort <> InvalidSerialHandle then begin
    if portsListCache = nil then
      result := nil
    else begin
      result := TStringList.Create;
      result.Assign(portsListCache)
    end;
    exit
  end;
  if portsListCache = nil then
    portsListCache := TStringList.Create
  else begin
    for i := 0 to portsListCache.Count - 1 do
      if portsListCache.Objects[i] <> nil then begin
        Assert(portsListCache.Objects[i] is TStringList, 'Internal error: bad cached port list.');
        TStringList(portsListCache.Objects[i]).Free
      end;
    portsListCache.Clear
  end;
  majorList := TStringList.Create;
  minorList := TStringList.Create;
  r := TRegExpr.Create;
  r.Expression := '(tty\D+)\d+';
  try

(* Run through all devices named like /dev/tty[[:alpha:]]+[[:digit:]]+ checking *)
(* that they're character-mode and not blacklisted and saving the major device  *)
(* number. The final result is a list of devices without appended digits, plus  *)
(* the associated major device number.                                          *)

    majorList.Sorted := true;
    majorList.Duplicates := dupIgnore;
    IF FindFirst('/dev/tty*', faSysFile{%H-}, searchRec) = 0 THEN
      REPEAT
        if r.Exec(searchRec.Name) and isChr('/dev/' + searchRec.Name) then
          if not blacklisted('/dev/' + r.Match[1]) then
            majorList.Add('/dev/' + r.Match[1] + '=' + IntToStr(major('/dev/' + searchRec.Name)))
      UNTIL FindNext(searchRec) <> 0;
    FindClose(searchRec);

(* Sort by major device number. Enumeration order might be determined by the    *)
(* order that kernel modules are loaded, in practice it's probably best to make *)
(* no assumption in which case the default Quicksort is probably appropriate.   *)

    majorList.Sorted := false;
    majorList.CustomSort(@compareValues);

(* For each major device get the actually-available physical devices. This      *)
(* assumes that udev or equivalent is being used to create devices dynamically, *)
(* i.e. won't work with Linux kernels older than 2.6 (circa 2003), in fact it   *)
(* doesn't even bother trying to check for statically-allocated systems since   *)
(* this would end up in a minefield of heuristics looking at the age of the     *)
(* kernel, whether it was possible to decide whether udev support was compiled  *)
(* into the kernel, whether systemd or equivalent was running and so on: it's   *)
(* quite simply not worth it for a change made 15 years ago particularly since  *)
(* this code "fails safe" by possibly listing more devices than actually exist  *)
(* rather than by hiding some which can't be opened as a result.                *)

    for i := 0 to majorList.Count - 1 do begin
      minorList.Clear;
      IF FindFirst(majorList.Names[i] + '*', faSysFile{%H-}, searchRec) = 0 THEN
        REPEAT
          minorList.Add('/dev/' + searchRec.Name + '=' + IntToStr(minor('/dev/' + searchRec.Name)))
        UNTIL FindNext(searchRec) <> 0;
      FindClose(searchRec);

(* In the context of the current major device, sort by minor device number. The *)
(* enumeration order might be reversed, which suggests that something like a    *)
(* comb sort would be appropriate; however this can't be relied on so again use *)
(* the default Quicksort.                                                       *)

      minorList.CustomSort(@compareValues);

(* Discarding major and minor device number, append the name to the cache.      *)

      for j := 0 to MinorList.Count - 1 do
        portsListCache.Append(minorList.Names[j])
    end;

(* The cached result should first have traditional serial devices /dev/ttySx,   *)
(* ISDN devices /dev/ttyIx, USB devices /dev/ttyUSBx with additional support    *)
(* for devices implemented by multiport cards etc. inserted as appropriate      *)
(* based on the major device numbers which were allocated approximately         *)
(* chronologically. It should also have any "Low-density serial ports" found    *)
(* to be present, hopefully at the end of the list, where those are e.g. on-    *)
(* chip console ports and are distinguished by minor rather than major device   *)
(* number:                                                                      *)
(*                                                                              *)
(*   4 /dev/ttySx                                                               *)
(*  43 /dev/ttyIx                                                               *)
(* 188 /dev/ttyUSBx                                                             *)
(* 204 /dev/ttyAMAx etc.                                                        *)
(*                                                                              *)
(* The overall result will hopefully be "correct" both from the system and user *)
(* POV. See Documentation/devices.txt or Documentation/admin-guide/devices.txt  *)
(* in the Linux source tree.                                                    *)

  finally
    r.Free;
    minorList.Free;
    majorList.Free
  end;

(* Add extra storage to the cache so that as we're checking the ports we can    *)
(* accumulate driver names etc.                                                 *)

  for i := 0 to portsListCache.Count - 1 do begin
    portsListCache.Objects[i] := TStringList.Create;
    for j := 0 to cachedTop do
      TStringList(portsListCache.Objects[i]).Append('');
  end;
  result := TStringList.Create;
  result.Assign(portsListCache)
end { ListPorts } ;


(* We know what kind of serial device is in the DSO, so look for that specific
  driver (Linux kernel module). The hint parameter, if >= 0, indicates a cache
  line that can be updated once we know a bit about a port (this was grafted
  on as an afterthought and could be done better).
*)
function selectAsDefault(const testPortName: string; hint: integer= -1): boolean;

const
  portName= 'ttyUSB';
  portDriverName= 'usb-serial/drivers/cp210x';
  portProductDescription= 'CP2102 USB to UART Bridge Controller';
  portSerialDescription= '0001';

begin
  result := true;

(* At least one of the tests is non-blank, so we can probably make some useful  *)
(* decisions. For example, we probably know something about the name of the     *)
(* port, e.g. it's going to be one of the /dev/ttyUSBn ports for a DSO112A so   *)
(* we don't have to waste time checking /dev/ttySn. If the port name is         *)
(* plausible then walk part of the /sys tree to try to find the driver name, in *)
(* the case of the DSO112A we know that this will be a Silicon Labs cp210x      *)
(* since it's internal to the device, and possibly continue by looking at the   *)
(* product description and serial number.                                       *)

(* This is the original implementation, it is fairly fast but since it doesn't  *)
(* investigate every port in depth it doesn't accumulate serial numbers etc.    *)

  if hint < 0 then begin
  //  DebugWriteF('Testing port %s against pattern %s\n', [portName, Backend.PortName()]);
    if Pos(portName, testPortName) = 0 then
      exit(false);
  //  DebugWriteF('Testing against driver name %s\n', [Backend.PortDriverName()]);
    if not usingDriver(testPortName, portDriverName, hint) then
      exit(false);
  //  DebugWriteF('Testing against product description %s\n', [Backend.PortProductDescription()]);
    if not usingProduct(testPortName, portProductDescription, hint) then
      exit(false);
  //  DebugWriteF('Testing against serial description %s\n', [Backend.PortSerialDescription()]);
    if not usingSerial(testPortName, portSerialDescription, hint) then
      exit(false)
  end else begin
    result := Pos(portName, testPortName) > 0;
    result := usingDriver(testPortName, portDriverName, hint) and result;
    result := usingProduct(testPortName, portProductDescription, hint) and result;
    result := usingSerial(testPortName, portSerialDescription, hint) and result
  end;

(* If either there's no useful tests we can make or all tests have been         *)
(* successful, return true. The result of this will be that either the first    *)
(* device in the list or the last device that matches will have its radio       *)
(* button set.                                                                  *)

end { selectAsDefault } ;


(* Assuming that the OS is Linux, walk the available serial ports looking for
  a CP210 which is what's in the Mastech 2115B. Note that this is also used by
  the DSO112A, so it's not entirely foolproof... if it were an FTDI chip it
  could be branded with the device serial number (or model number etc.) as a
  one-time operation which would make it more reliable.
*)
function FindMs2115bPort(portScan: boolean= false): string;

var
  i: integer;
  portNames: TStringList;

begin
  result := '';

(* Initialise the menu from the known ports.                                    *)

  portNames := ListPorts();
  try
    for i := 0 to portNames.Count - 1 do begin

(* Let's assume that because we're adding devices in sequence, that the most    *)
(* recent that the OS has seen plugged in appears last. Select this as the      *)
(* device to be used if it matches certain critera.                             *)
(*                                                                              *)
(* If the portScan parameter is not set then we don't waste (lots of) time      *)
(* scanning device descriptions in the /sys tree.                               *)

      if not portScan then
        if selectAsDefault(portnames[i]) then
          result := portnames[i]
        else begin end
      else
        if selectAsDefault(portnames[i], i) then
          result := portnames[i]
    end
  finally
    FreeAndNil(portNames)
  end
end { FindMs2115bPort } ;


(* Assuming that the cached list has been properly populated, dump its contents.
*)
procedure DumpCachedPorts;

var
  i: integer;

begin
  for i := 0 to portsListCache.Count - 1 do begin
    WriteLn(portsListCache[i]);
    if portsListCache.Objects[i] <> nil then begin
      Assert(portsListCache.Objects[i] is TStringList, 'Internal error: bad cached port list.');
      with TStringList(portsListCache.Objects[i]) do begin
        Assert(Count = cachedTop + 1, 'Internal error: bad cached port list.');
        WriteLn('  Driver: ', Strings[cachedDriver]);
        WriteLn('  Product: ', Strings[cachedProduct]);
        WriteLn('  Serial: ', Strings[cachedSerial])
      end
    end
  end;
  WriteLn
end { DumpCachedPorts } ;


end.


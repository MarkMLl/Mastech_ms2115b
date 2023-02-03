(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

unit ConsoleApp;

(* This is the greater part of a console program which reads data from a        *)
(* Mastech MS2115B meter and sends it to stdout.                MarkMLl.        *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Serial;

type
  TWriteLn= procedure(const s: string);

(* GNU-mandated support for --version and --help.
*)
procedure DoVersion(const projName: string);

(* GNU-mandated support for --version and --help.
*)
procedure DoHelp(const portName: string; portScan: boolean= false);

(* This is the inner loop of the main function. If the second parameter is nil
  then output each decoded line using WriteLn(), otherwise call the indicated
  writer procedure which will typically send the output to the GUI.
*)
function RunConsoleApp2(portHandle: TSerialHandle; var pleaseStop: boolean;
                                        writer: TWriteLn= nil): integer;

(* Main function, return 0 if no error.
*)
function RunConsoleApp(portName: string): integer;


implementation

uses
  StrUtils, LocatePorts, IniFilesAbout, ScpiServer, SyncObjs;

var
  oddity: boolean= false;
  scpiPort: integer= -2;
  scpi: TScpiServer= nil;
  scpiLock: TCriticalSection= nil;
  scpiFunc1, scpiFunc2, scpiValue1, scpiValue2: string;


(* GNU-mandated support for --version and --help.
*)
procedure DoVersion(const projName: string);

begin
  WriteLn();
  WriteLn(projName + ' ' + AboutText());
  WriteLn()
end { DoVersion } ;


(* GNU-mandated support for --version and --help.
*)
procedure DoHelp(const portName: string; portScan: boolean= false);

begin
  WriteLn();
  WriteLn('Usage: ms2115b [OPTIONS]... [DEVICE]');
  WriteLn();
  WriteLn('Output readings from a Mastech 2115B meter.');
  WriteLn();
  WriteLn('DEVICE is the name of a serial device such as /dev/ttyUSB0.');
  WriteLn();
{$ifdef LCL }
  WriteLn('If there is no explicit option or device an interactive GUI screen will');
  WriteLn('be presented. Supported options are as below:');
{$else      }
  WriteLn('Supported options are as below:');
{$endif LCL }
  WriteLn();
  WriteLn('  --version      Version information.');
  WriteLn();
  WriteLn('  --help         This help text, also reports default device.');
  WriteLn();
  WriteLn('  --portScan     Extends --help output with device list (might be slow).');
  WriteLn();
  WriteLn('  -F --format p  Output format for main value is defined by pattern p, which');
  WriteLn('                 is e.g. %5.2f to specify a total of five characters with two');
  WriteLn('                 digits after the decimal point. Alternatively use %x@yBzd or');
  WriteLn('                 %x@yLzd for offset x and y bytes of big/little-endian data');
  WriteLn('                 with C format %zd etc., @ by itself dumps raw data as read.');
  WriteLn();
  WriteLn('  --scpi [p]     Listen for SCPI commands on port p, or standard input.');
  WriteLn();
{$ifdef LCL }
  WriteLn('  -              Dummy option, ignored.');
{$else      }
  WriteLn('  - --           Dummy options, ignored.');
{$endif LCL }
  WriteLn();
  WriteLn('Exit status:');
  WriteLn();
  WriteLn(' 0  Normal termination');
  WriteLn(' 1  Cannot parse device identifier');
  WriteLn(' 2  Named device cannot be opened');
  WriteLn(' 3  Named device is unresponsive');
  WriteLn(' 4  Data access error');
  WriteLn(' 5  Data format error');
  WriteLn(' 9  Bad command-line parameters');
  WriteLn('10  Unable to create or start SCPI server');
  WriteLn();
  if portScan then
    DumpCachedPorts;
  WriteLn('Default port: ', portName);
  WriteLn()
end { DoHelp } ;


(* Wait until there is a gap in the data to make sure we don't try to treat data
  as a sync byte;
*)
procedure waitDataGap(port: TSerialHandle);

var
  scratch: byte;

begin
  while SerReadTimeout(port, scratch, 10) > 0 do        (* 10 mSec timeout      *)
    Sleep(1)
end { waitDataGap } ;


type
  byteArray= array of byte;


operator + (const a: byteArray; const b: byte): byteArray;

begin
  result := a;
  SetLength(result, Length(result) + 1);
  result[High(result)] := b
end { + } ;


(* Output data that we've got from a feature request or a query. This is either
  a block of hex/ASCII data in the conventional layout, or is formatted
  according to a layout string where

         %      Indicates start of layout string
         n @    Buffer is zero-based, advance to this index
  either n L    n bytes of little-endian data follow
  or     n B    n bytes of big-endian data follow
  or     n H    n bytes of host-endian data follow
         xxx    C-style format string, leading % assumed

  Assume that this requires three components required in that order, and with n
  being a decimal number. This is intended as a general aid to investigating
  HID behaviour, I'm writing it primarily to support a TEMPer sensor.
*)
procedure diagOutput(const rawBlock: byteArray; l: integer; const diagOutputFormat: string);

var
  i, j, scratchInt: integer;
  expectMaths: boolean;
  pattern, numAsString, savedStr: string;
  savedChar: char;
  accumulator: double;


  function hex(b: byte): string; inline;

  begin
    result := ' ' + LowerCase(IntToHex(b, 2))
  end { hex } ;


  (* Parse off anything that looks like a signed or unsigned integer or real
    plus the # character and space.
  *)
  function parseNum(var str: string): string;

  var
    valid: set of char;

  begin
    result := '';
    valid := [' ', '#', '.', '0'..'9'];
    if not expectMaths then
      valid += ['+', '-'];
    while (str <> '') and (str[1] in valid) do begin
      result += str[1];
      Delete(str, 1, 1)
    end
  end { parseNum } ;


  procedure accumulateData(littleEndian: boolean);

  begin
    if littleEndian then begin
      if numAsString <> '' then
        j := StrToInt(numAsString);
      case j of
        1: scratchInt := rawBlock[i];
        2: scratchInt := (rawBlock[i + 1] shl 8) + rawBlock[i];
        3: scratchInt := (rawBlock[i + 2] shl 16) + (rawBlock[i + 1] shl 8) +
                                rawBlock[i];
        4: scratchInt := (rawBlock[i + 3] shl 24) + (rawBlock[i + 2] shl 16) +
                                (rawBlock[i + 1] shl 8) + rawBlock[i]
      otherwise
      end;
      savedChar := Chr(scratchInt);
      SetLength(savedStr, j);     (* UNTESTED             *)
      Move(rawBlock[i], savedStr[1], j); (* Natural order *)
      accumulator := scratchInt
    end else begin
      if numAsString <> '' then
        j := StrToInt(numAsString);
      case j of
        1: scratchInt := rawBlock[i];
        2: scratchInt := (rawBlock[i] shl 8) + rawBlock[i + 1];
        3: scratchInt := (rawBlock[i] shl 16) + (rawBlock[i + 1] shl 8) +
                                rawBlock[i + 2];
        4: scratchInt := (rawBlock[i] shl 24) + (rawBlock[i + 1] shl 16) +
                                (rawBlock[i + 2] shl 8) + rawBlock[i + 3]
      otherwise
      end;
      savedChar := Chr(scratchInt);
      SetLength(savedStr, j);     (* UNTESTED             *)
      Move(rawBlock[i], savedStr[1], j); (* Natural order *)
      accumulator := scratchInt
    end
  end { accumulateData } ;


begin
  if diagOutputFormat = '' then begin
    for i := 0 to SizeOf(rawBlock) div 16 do
      if i * 16 < l then begin
        Write(LowerCase(IntToHex(I * 16, 4)), ' ');
        for j := 0 to 15 do
          if (i * 16) + j < l then
            Write(hex(rawBlock[(i * 16) + j]));
        Write('  ');
        for j := 0 to 15 do
          if (i * 16) + j < l then
            if (rawBlock[(i * 16) + j] >= $20) and (rawBlock[(i * 16) + j] < $7f) then
              Write(Chr(rawBlock[(i * 16) + j]))
            else
              Write('·');               (* Dot                                  *)
        WriteLn
      end
  end else begin
    pattern := diagOutputFormat;
    while pattern <> '' do
      case pattern[1] of
        '\': begin                      (* Control characters, and literal %    *)
               case pattern[2] of
                 'b': Write(#$08);
                 'n': WriteLn;
                 'r': Write(#$0d);
                 't': Write(#$09)
               otherwise                (* Second character verbatim            *)
                 Write(pattern[2])
               end;
               Delete(pattern, 1, 2)
             end;
        '%': begin                      (* Formatted output, uses a nested loop *)
               i := 0;                  (* Index into array, default zero       *)
               j := 2;                  (* Number of bytes to read, default 2   *)
               accumulator := 0.0;
               savedChar := #$00;       (* These bypass any arithmetic operations *)
               savedStr := '';
               expectMaths := false;
               Delete(pattern, 1, 1);   (* Leading %                            *)
               while pattern <> '' do begin
                 numAsString := parseNum(pattern); (* Deletes chars parsed off  *)
                 if pattern = '' then begin (* Treat this as literal output,    *)
                   Write(numAsString);  (* we'll have lost an earlier %.        *)
                   exit
                 end;
                 try
                   case pattern[1] of
                     '(': expectMaths := true; (* Are + and - maths operators   *)
                     ')': expectMaths := false; (* or part of the format string? *)
                     '@': if numAsString <> '' then
                            i := StrToInt(numAsString);
                     'L': accumulateData(true);
                     'B': accumulateData(false);
(*$IFDEF ENDIAN_LITTLE *)
                     'H': accumulateData(true);         (* Host-endian          *)
(*$ELSE                *)
                     'H': accumulateData(false);
(*$ENDIF ENDIAN_LITTLE *)
                     '+': begin
                            if numAsString <> '' then
                              scratchInt := StrToInt(numAsString)
                            else
                              scratchInt := 0;
                            accumulator += scratchInt
                          end;
                     '-': begin
                            if numAsString <> '' then
                              scratchInt := StrToInt(numAsString)
                            else
                              scratchInt := 0;
                            accumulator -= scratchInt
                          end;
                     '*': begin
                            if numAsString <> '' then
                              scratchInt := StrToInt(numAsString)
                            else
                              scratchInt := 1;
                            accumulator *= scratchInt
                          end;
                     '/': begin
                            if numAsString <> '' then
                              scratchInt := StrToInt(numAsString)
                            else
                              scratchInt := 1;
                            accumulator /= scratchInt
                          end;
                     '%': begin         (* Not a modulus: round to precision    *)
                            if numAsString <> '' then
                              scratchInt := StrToInt(numAsString)
                            else
                              scratchInt := 1;
                            accumulator /= scratchInt;
                            accumulator := Round(accumulator);
                            accumulator *= scratchInt
                          end

(* Assume that anything else is a printf()-style type character, except that    *)
(* this does not have provision for a prefix character since that's been        *)
(* handled explicitly by the L or B command.                                    *)
(*                                                                              *)
(* I am resisting the temptation of putting in any additional operators here,   *)
(* for example some of the sexy APL ones, not so much because the sort of       *)
(* manipulation they imply is well outside the original scope of this program   *)
(* (which was, after all, a GUI to configure a mouse) but because most of them  *)
(* imply the availability of variable or array support which is quite simply    *)
(* impractical without using a completely different notation.                   *)

                   otherwise
                     case pattern[1] of
                       'c':      Write(Format('%' + numAsString + pattern[1], [savedChar]));
                       's':      Write(Format('%' + numAsString + pattern[1], [savedStr]));
                       'e', 'E', 'f',
                       'g', 'G': Write(Format('%' + numAsString + pattern[1], [accumulator]));
                       'x':      Write(LowerCase(ReplaceStr(
                                        Format('%' + numAsString + pattern[1], [Round(accumulator)]), ' ', '0')));
                       'X':      Write(UpperCase(ReplaceStr(
                                        Format('%' + numAsString + pattern[1], [Round(accumulator)]), ' ', '0')))
                     otherwise
                       Write(FormatEx('%' + numAsString + pattern[1], [Round(accumulator)], IsoTimeOnly, 2))
                     end;
                     Delete(pattern, 1, 1);
                     Break              (* Back to outer loop                   *)
                   end;
                   Delete(pattern, 1, 1) (* Stay in inner loop                  *)
                 except
                   Write(#$0d);         (* Start of line                        *)
                   diagOutput(rawBlock, l, '') (* Something wrong               *)
                 end
               end                      (* Inner loop (handles % format)        *)
             end
      otherwise                         (* Anything else                        *)
        Write(pattern[1]);
        Delete(pattern, 1, 1)
      end;
      WriteLn                           (* Outer loop ends immediately after this *)
  end
end { diagOutput } ;


type
  Tmessage= array[0..8] of byte;


(* Output either as a data dump (format is @ by itself) or with C-style
  formatting applied to each byte or word.
*)
function outputFormattedRaw(message: TMessage; const formatString: string): boolean;

begin
  result := true;
  if Length(formatString) <= 2 then
    diagOutput(message, SizeOf(message), '')
  else
    diagOutput(message, SizeOf(message), formatString)
end { outputFormattedRaw } ;


(* Output using an explicit C-style format string. If this ends with whitespace
  or a control character (tab or newline) then don't append an implicit newline.
*)
function outputFormattedRescaled(value: double; const formatString: string): boolean;

var
  ln: boolean= true;

begin
  result := true;
  try
    if Length(formatString) > 2 then begin
      if formatString[Length(formatString)] = ' ' then
        ln := true;
      if formatString[Length(formatString) - 1] = '\' then
        ln := true
    end;
    Write(FormatEx(formatString, [value]));
    if ln then
      WriteLn;
  except
    result := false
  end
end { outputFormattedRescaled } ;


(* The parameter is a nine-byte message, if it makes sense then return true. The
  other results are text strings of consistent length representing the current
  range, scale/units, and a multiplier to be applied to the (16-bit) analogue
  value.
*)
function unpack(const message: Tmessage; out range, scale: string;
                                        out multiplier: double; out units: string): boolean;

const
  longestRange: integer= -1;
  longestScale: integer= -1;

var
  i1, i2, i3: integer;
  scratch: double;
  scratch2: string;
  myScpiFunc1, myScpiFunc2, myScpiValue1, myScpiValue2: string;
  value: smallint;


  (* Convert (part of) a byte to a meter range. This will be called at least one
    extra time in order to determine the maximum result length for padding.
  *)
  function numTorange(m: integer): string;

  begin
    case m of
      $00: result := '600A AC';         (* Will be padded later                 *)
      $01: result := '60A AC';
      $02: result := 'V AC';
      $03: result := 'Beep';
      $04: result := 'Restnce';
      $05: result := 'Captnce';
      $06: result := 'Freqncy';
      $07: result := 'Mode 7';
      $08: result := '600A DC';
      $09: result := '60A DC';
      $0a: result := 'V DC';
      $0b: result := 'Diode';
      $0c: result := 'Mode 12';
      $0d: result := 'Mode 13';
      $0e: result := 'Mode 14';
      $0f: result := 'Mode 15'
    otherwise
      result := ''                      (* Can't happen                         *)
    end
  end { numToRange } ;


  (* Convert portions of a message to range scaling etc. This will be called at
    least one extra time in order to determine the maximum result length for
    padding, things that don't have an indeterminate length are returned as
    function parameters.
  *)
  function numToScale(m1, m2, m3: integer; out mult: double; out units: string): string;

  begin
    result := '';
    mult := 1.0;                        (* = 1.0e0 scientific notation (below)  *)
    units := '   ';                     (* Enough space for kHz as a unit       *)
    case m1 and $07 of
      $00: begin
             result := 'A';
             mult := 1.0e-1;
             units := '  A'
           end;
      $01: begin
             result := 'A';
             mult := 1.0e-2;
             units := '  A'
           end;
      $02: case message[2] of
             $00: begin
                    result :=   '600mV';        (* Will be padded later         *)
//                    result += ' (x0.1)';
                    mult := 1.0e-1;
                    units := ' mV'
                  end;
             $01: begin
                    result := '6V';
//                    result += ' (x0.001)';
                    mult := 1.0e-3;
                    units := '  V'
                  end;
             $02: begin
                    result := '60V';
//                    result += ' (x0.01)';
                    mult := 1.0e-2;
                    units := '  V'
                  end;
             $03: begin
                    result := '600V';
//                    result += ' (x0.1)';
                    mult := 1.0e-1;
                    units := '  V'
                  end;
             $04: begin
                    result := '1000V';
//                    result += ' (x1)';
                    mult := 1.0e-0;
                    units := '  V'
                  end;
           otherwise                    (* Shouldn't happen, probably will      *)
             oddity := true
           end;
      $03: if m1 = 3 then begin         (* Beep                                 *)
             result := 'Ω';
             mult := 1.0e0;
             units := '  Ω'
           end else begin               (* Diode                                *)
             result := 'V';
             mult := 1.0e-3;
             units := '  V'
           end;
      $04: case m2 of
             $00: begin
                    result := '600Ω';
//                    result += ' (x0.1)';
                    mult := 1.0e-1;
                    units := '  Ω'
                  end;
             $01: begin
                    result := '6kΩ';
//                    result += ' (x0.001)';
                    mult := 1.0e-3;
                    units := ' kΩ'
                  end;
             $02: begin
                    result := '60kΩ';
//                    result += ' (x0.01)';
                    mult := 1.0e-2;
                    units := ' kΩ'
                  end;
             $03: begin
                    result := '600kΩ';
//                    result += ' (x0.1)';
                    mult := 1.0e-1;
                    units := ' kΩ'
                  end;
             $04: begin
                    result := '6MΩ';
//                    result += ' (x0.001)';
                    mult := 1.0e-3;
                    units := ' MΩ'
                  end;
             $05: begin
                    result := '60MΩ';
//                    result += ' (x0.01)';
                    mult := 1.0e-2;
                    units := ' MΩ'
                  end;
           otherwise                    (* Shouldn't happen, probably will      *)
             oddity := true
           end;
      $05: case m2 of
             $00: begin
                    result := '6nF';
//                    result += ' (x0.001)';
                    mult := 1.0e-3;
                    units := ' nF'
                  end;
             $01: begin
                    result := '60nF';
//                    result += ' (x0.01)';
                    mult := 1.0e-2;
                    units := ' nF'
                  end;
             $02: begin
                    result := '600nF';
//                    result += ' (x0.1)';
                    mult := 1.0e-1;
                    units := ' nF'
                  end;
             $03: begin
                    result := '6µF';
//                    result += ' (x0.001)';
                    mult := 1.0e-3;
                    units := ' µF'
                  end;
             $04: begin
                    result := '60µF';
//                    result += ' (x0.01)';
                    mult := 1.0e-2;
                    units := ' µF'
                  end;
             $05: begin
                    result := '600µF';
//                    result += ' (x0.1)';
                    mult := 1.0e-1;
                    units := ' µF'
                  end;
             $06: begin
                    result := '6mF';
//                    result += ' (x0.001)';
                    mult := 1.0e-3;
                    units := ' mF'
                  end;
             $07: begin
                    result := '60mF';
//                    result += ' (x0.01)';
                    mult := 1.0e-2;
                    units := ' mF'
                  end;
           otherwise                    (* Shouldn't happen, probably will      *)
             oddity := true
           end;

// Below is largely guesswork, I've not got a frequency source that I'm even
// half-confident in.

      $06: case m3 of                   // TODO : Hz range inadequately tested
             $00: begin
                    result := '60 Hz';
                    mult := 1.0e3;
                    units := ' Hz'
                  end;
             $01: begin
                    result := '600 Hz';
                    mult := 1.0e0;
                    units := ' Hz'
                  end;
             $02: begin
                    result := '6 kHz';
                    mult := 1.0e-1;
                    units := 'kHz'
                  end;
             $03: begin
                    result := '60 kHz';
                    mult := 1.0e-2;
                    units := 'kHz'
                  end;
             $04: begin
                    result := '600 kHz';
                    mult := 1.0e-3;
                    units := 'kHz'
                  end;
             $05: begin
                    result := '6 MHz';
                    mult := 1.0e-0;
                    units := 'MHz'
                  end;
             $06: begin
                    result := '60 MHz';
                    mult := 1.0e-1;
                    units := 'MHz'
                  end
           otherwise                    (* Shouldn't happen, probably will      *)
             oddity := true
           end
    otherwise                           (* Shouldn't happen, probably will      *)
      oddity := true
    end
  end { numToScale } ;


  (* As of FPC 3.0.4 I'm having problems with the length of strings containing
    UTF-8, hack it for the time being.
  *)
  function length(const s: string): integer;

  var
    i, k: integer;

  begin
    result := System.length(s);
    k := 0;
    for i := 1 to result do
      if s[i] > #$7f then
        k += 1;
    result -= k div 2
  end { length } ;


  (* As of FPC 3.0.4 I'm having problems with the length of strings containing
    UTF-8, hack it for the time being.
  *)
  function padRight(const s: string; l: integer): string;

  begin
    result := s;
    while length(result) < l do
      result += ' '
  end { padRight } ;


begin
  range := '';
  scale := '';
  multiplier := 0.0;
  myScpiFunc1 := '';
  myScpiFunc2 := '';
  myScpiValue1 := '';
  myScpiValue2 := '';
  result := true;

(* I've never done this before and I'm not very happy with it. A typed constant *)
(* in a function is static, so the block below should only be run once after    *)
(* which we'll know how much padding to apply.                                  *)

  if longestRange < 0 then
    for i1 := 0 to 15 do
      if Length(numToRange(i1)) > longestRange then
        longestRange := Length(numToRange(i1));

(* Do the same for the scale, except in this case we have to look at two bytes  *)
(* plus one bit from a third.                                                   *)

(* There's something wrong that I've not sorted out yet: if the scale contains  *)
(* a UTF-8 character (Ω or µ) the length/padding is screwed up.                 *)

// TODO : Sort UTF-8

    if longestScale < 0 then
      for i1 := 0 to 7 do
        for i2 := 0 to 7 do
          if Length(numToScale(i1, i2, 0, scratch, scratch2)) > longestScale then
            longestScale := Length(numToScale(i1, i2, 0, scratch, scratch2));

(* Now do the decode for real, including the multipler.                         *)

  if message[0] <> $55 then
    exit(false);                        (* Bad sync byte                        *)
  if message[1] and $f0 <> 0 then
    exit(false);                        (* Expected to be zero                  *)
  range := PadRight(numToRange(message[1]), longestRange);
  if message[8] and $01 = 0 then
    range += ' Ⓐ|'                      (* Auto-scale marker                    *)
  else
    range += '  |';
  if message[2] and $f0 <> 0 then
    exit(false);                        (* Expected to be zero                  *)
  scale := PadRight(numToScale(message[1], message[2], message[3], multiplier, units), longestScale) + '|';

// TODO : Distribute SCPI value collection better to reduce the amount of non-ASCII.
// It was originally in autoFormatted() below, but should probably be in
// numToRange() and numToScale() above: strictly speaking all of the micros and
// Ohms should be removed.

  myScpiFunc1 := range + scale;
  myScpiFunc1 := StringReplace(myScpiFunc1, 'Ⓐ', '', []);
  myScpiFunc1 := Trim(DelSpace1(StringReplace(myScpiFunc1, '|', '', [rfReplaceAll])));
  value := smallint(message[4] or (message[5] shl 8));
  Str(multiplier * value:8:3, myScpiValue1);
  myScpiValue1 := Trim(DelSpace1(StringReplace(myScpiValue1, '|', '', [rfReplaceAll])));
  value := smallint(message[6] or (message[7] shl 8));
  Str(0.01 * value:5:2, myScpiValue2);
  case message[1] of
    $00,
    $01,
    $02: begin
           myScpiValue2 := Trim(DelSpace1(StringReplace(myScpiValue2, '|', '', [rfReplaceAll])));
           myScpiFunc2 := 'Hz'
         end;
    $06: begin
           Str(0.1 * value:4:1, myScpiValue2);
           myScpiFunc2 := '%'
         end
  otherwise
  end;

(* This lock isn't strictly necessary, since the main thread is being used to   *)
(* both update the (global) variables showing the meter's state, and dispatch   *)
(* SCPI sommands.                                                               *)

  if Assigned(scpi) then begin
    scpiLock.Enter;
    try
      scpiFunc1 := myScpiFunc1;
      scpiFunc2 := myScpiFunc2;
      scpiValue1 := myScpiValue1;
      scpiValue2 := myScpiValue2
    finally
      scpiLock.Leave
    end
  end
end { unpack } ;


(* If the format string contains an @ then process the raw buffer. Otherwise
  extract the main value and scale to a double, then format it.
*)
function outputFormatted(message: Tmessage; const formatString: string): boolean;

var
  range, scale, units: string;
  multiplier, value: double;

begin
  if (formatString = '') or (Pos('@', formatString) > 0) then
    result := outputFormattedRaw(message, formatString)
  else begin
    multiplier := 1.0;
    if not unpack(message, range, scale, multiplier, units) then
      exit(false);
    value := smallint(message[4] or (message[5] shl 8)) * multiplier;
    result := outputFormattedRescaled(value, formatString)
  end
end { outputFormatted } ;


(* Format a 9-byte message into text, using the scaling etc. provided by the
  meter. This is based on the format described in ms2115b.c in the Sigrok
  sources.
*)
function autoFormatted(message: Tmessage): string;

var
  range, scale, units, scratch: string;
  multiplier: double= 1.0;
  value: smallint;

begin
  if message[0] <> $55 then
    exit('');                           (* Bad sync byte                        *)
  result := IsoFormatDateTime(Now(), IsoTimeOnly, 2) + '|'; (* Date, time       *)
  if not unpack(message, range, scale, multiplier, units) then
    exit('');
  result += range + scale;              (* Range, scale                         *)
  value := smallint(message[4] or (message[5] shl 8));
  Str(multiplier * value:8:3, scratch);
  result += scratch + '|';              (* Main value                           *)
  result += units + '|';
  result += HexStr(message[3], 2) + '|'; (* Frequency range?                    *)
  value := smallint(message[6] or (message[7] shl 8));
  case message[1] of
    $00,
    $01,
    $02: begin
           Str(0.01 * value:5:2, scratch);
           result += scratch + 'Hz'     (* V-AC's secondary value is Hz * 100   *)
         end;
    $06: begin
           Str(0.1 * value:4:1, scratch);
           result += scratch + '%'      (* Freq's secondary is duty cycle * 10  *)
         end
  otherwise
    result += IntToStr(value)           (* Other secondary values               *)
  end
end { autoFormatted } ;


var
  debugLevel: integer= 0;
  formatString: string= '';
  onceOnly: boolean= false;


(* This is the inner loop of the main function. If the second parameter is nil
  then output each decoded line using WriteLn(), otherwise call the indicated
  writer procedure which will typically send the output to the GUI.
*)
function RunConsoleApp2(portHandle: TSerialHandle; var pleaseStop: boolean;
                                        writer: TWriteLn= nil): integer;
const
  scpiPrompt= false;

var
  i, lastI: integer;
  waitingFirstSync: boolean;
  resync: jmp_buf;
  message: Tmessage;
  scratch: string;

begin
  try
    SerSetParams(portHandle, 1200, 8, NoneParity, 1, []);
    lastI := SetJmp(resync);
    waitDataGap(portHandle);
    waitingFirstSync := true;
    repeat
      i := -1;

(* There's two possibilities here. If we're not confident that we're seeing a   *)
(* stream of contiguous data then be prepared to wait several seconds for a     *)
(* sync byte, otherwise be less tolerant.                                       *)
(*                                                                              *)
(* The real issue is that when the front panel switch position is changed there *)
(* will be an interruption of >1.7secs, and I don't want to make any assumption *)
(* about whether this is between or in the middle of a message. It looks as     *)
(* though autoranging will emit messages as it happens, but again I don't want  *)
(* to make any assumptions. The lastI variable is zero if data has never been   *)
(* seen, 1 if waiting for a sync byte, and > 1 if waiting for the remainder of  *)
(* the message, this is used mainly to try to keep error messages sensible.     *)

      if waitingFirstSync then
        if SerReadTimeout(portHandle, message[0], 5000) = 0 then
          case lastI of
            0,
            1: exit(3);                 (* Unresponsive                         *)
          otherwise
            exit(4)                     (* Access error, message not intact     *)
          end
        else begin end                  (* Continue with check that it's $55    *)
      else
        if SerReadTimeout(portHandle, message[0], 1000) = 0 then
          LongJmp(resync, 1);
      waitingFirstSync := false;
      if message[0] <> $55 then
        continue;

(* Anything that isn't a sync byte has been discarded. Read the remainder of    *)
(* the message.                                                                 *)

      for i := 1 to 8 do
        if SerReadTimeout(portHandle, message[i], 100) = 0 then
          LongJmp(resync, i + 1);
      if debugLevel > 1 then begin
        Write(stderr, '#');
        for i := 0 to 8 do
          Write(stderr, ' ' + HexStr(message[i], 2));
        WriteLn(stderr)
      end;
      if Assigned(writer) then begin
        scratch := autoFormatted(message);
        if scratch = '' then
          exit(5);                      (* Format error                         *)
        writer(scratch)
      end else

(* We have a 9-byte message. If there is an explicit format string then apply   *)
(* it, otherwise just try to do the right thing.                                *)

        if formatString <> '' then
          if not outputFormatted(message, formatString) then
            exit(5)                     (* Format error                         *)
          else begin end
        else begin
          scratch := autoFormatted(message);
          if scratch = '' then
            exit(5);                    (* Format error                         *)
          WriteLn(scratch)
        end;

(* Only output a single line if an SCPI server has been requested. Even this    *)
(* might be excessive.                                                          *)

      if Assigned(scpi) and not scpi.Finished then

(* If Run() hasn't been called to activate the thread, which depends on the     *)
(* thread manager being imported into the main unit at compilation, then it's   *)
(* necessary to call Poll() regularly. Don't expect this to perform well.       *)

        if scpi.Suspended then
          scpi.Poll(NonBlocking);

(* This lock isn't strictly necessary, since the main thread is being used to   *)
(* both update the (global) variables showing the meter's state, and dispatch   *)
(* SCPI sommands.                                                               *)

        if scpi.CommandsAvailable() > 0 then begin
          scpiLock.Enter;
          try
            while scpi.Dispatch() do
              Sleep(10)
          finally
            scpiLock.Leave
          end
        end;
      if onceOnly then                  (* Debugging option, -ve level          *)
        break
    until pleaseStop { Dave }           (* Or signal from keyboard              *)
  finally
    SerClose(portHandle)
  end;
  result := 0
end { RunConsoleApp2 } ;


{$macro on  }
{$define IS_SCPI_SYNTAX__:= Pos(' SYNTAX', command) = Length(command) - Length(' SYNTAX') + 1 }
{$define SCPI_COMMAND_NO_SYNTAX__:= Copy(command, 1, Length(command) - Length(' SYNTAX')) }

function scpiDoNothing(scpi: TScpiServer; const {%H-}command: AnsiString): boolean;

begin
  result := true;
  if not IS_SCPI_SYNTAX__ then
    scpi.Respond('Do not understand "' + command + '"', true)
end { scpiDoNothing } ;


function scpiDoHalt(scpi: TScpiServer; const {%H-}command: AnsiString): boolean;

begin
  result := true;
  if IS_SCPI_SYNTAX__ then begin
    if scpi.Prompt then
      scpi.Respond('  ', false);
    scpi.Respond(SCPI_COMMAND_NO_SYNTAX__, true)
  end else begin
    scpi.Destroy;
    Halt
  end
end { scpiDoHalt } ;


(* Response should be four fields with manufacturer, model (without "Model "
  text etc.), serial number, and firmware/revision info of all subsystems.

  Assume that response fields may be empty but must remain comma-delimited, and
  that trailing (but not embedded) spurious commas may be suppressed.
*)
function scpiDoIdentify(scpi: TScpiServer; const {%H-}command: AnsiString): boolean;

begin
  result := true;
  if IS_SCPI_SYNTAX__ then begin
    if scpi.Prompt then
      scpi.Respond('  ', false);
    scpi.Respond(SCPI_COMMAND_NO_SYNTAX__, true)
  end else
    scpi.Respond('Mastech,MS2115B')
end { scpiDoIdentify } ;


{
https://github.com/andrey-nakin/v7-28-arduino/blob/master/doc/v7-28-arduino.tex
suggests that  SENSe:FUNCtion  would be a good start, indicating the current
switch setting on the meter. Also see the similar commands in
https://scdn.rohde-schwarz.com/ur/pws/dl_downloads/dl_common_library/dl_manuals/gb_1/h/hmc8012_1/HMC8012_SCPI_ProgrammersManual_en_01.pdf

Also https://raw.githubusercontent.com/TheHWcave/OWON-XDM1041/main/SCPI/XDM1041-SCPI.pdf
which appears to put FUNCtion? at the top level. I suspect this is also used
for a similar meter sold by Farnell.
}


function scpiDoReportFunction1(scpi: TScpiServer; const {%H-}command: AnsiString): boolean;

begin
  result := true;
  if IS_SCPI_SYNTAX__ then begin
    if scpi.Prompt then
      scpi.Respond('  ', false);
    scpi.Respond(SCPI_COMMAND_NO_SYNTAX__ + ' (No instrument attached.)', true)
  end else begin
    scpi.Respond(scpiFunc1, true)
  end
end { scpiDoReportFunction1 } ;


function scpiDoReportFunction2(scpi: TScpiServer; const {%H-}command: AnsiString): boolean;

begin
  result := true;
  if IS_SCPI_SYNTAX__ then begin
    if scpi.Prompt then
      scpi.Respond('  ', false);
    scpi.Respond(SCPI_COMMAND_NO_SYNTAX__ + ' (No instrument attached.)', true)
  end else begin
    scpi.Respond(scpiFunc2, true)
  end
end { scpiDoReportFunction2 } ;


function scpiDoReportValue1(scpi: TScpiServer; const {%H-}command: AnsiString): boolean;

begin
  result := true;
  if IS_SCPI_SYNTAX__ then begin
    if scpi.Prompt then
      scpi.Respond('  ', false);
    scpi.Respond(SCPI_COMMAND_NO_SYNTAX__ + ' (No instrument attached.)', true)
  end else begin
    scpi.Respond(scpiValue1, true)
  end
end { scpiDoReportValue1 } ;


function scpiDoReportValue2(scpi: TScpiServer; const {%H-}command: AnsiString): boolean;

begin
  result := true;
  if IS_SCPI_SYNTAX__ then begin
    if scpi.Prompt then
      scpi.Respond('  ', false);
    scpi.Respond(SCPI_COMMAND_NO_SYNTAX__ + ' (No instrument attached.)', true)
  end else begin
    scpi.Respond(scpiValue2, true)
  end
end { scpiDoReportValue2 } ;


(* Main function, return 0 if no error.
*)
function RunConsoleApp(portName: string): integer;

var
  portHandle: TSerialHandle= InvalidSerialHandle;
  dontStop: boolean= false;
  i: integer;
  ports: TStringList;

begin
  result := 3;                          (* Unresponsive is a good default       *)
  i := 1;
  while i <= ParamCount() do begin
    case ParamStr(i) of
      '-',                              (* Placeholder only                     *)
      '--',                             (* This doesn't work with GUI/LCL       *)
      '--ports',                        (* Used as --help modifier only         *)
      '--portscan',
      '--portsScan':  ;
      '--debug':      if i = ParamCount() then begin
                        WriteLn(stderr, 'Debug level has no parameter');
                        exit(9)         (* Missing debug level                  *)
                      end else begin
                        i += 1;
                        try
                          debugLevel := Abs(StrToInt(ParamStr(i)));
                          onceOnly := StrToInt(ParamStr(i)) < 0
                        except
                          WriteLn(stderr, 'Debug level not numeric');
                          exit(9)       (* Bad debug level                      *)
                        end
                      end;
      '-F',
      '--format':     if i = ParamCount() then begin
                        WriteLn(stderr, 'Format string has no parameter');
                        exit(9)         (* Missing format string                *)
                      end else begin
                        i += 1;
                        formatString := ParamStr(i)
                      end;

      '--scpi':       if i = ParamCount() then
                        scpiPort := -1
                      else begin
                        i += 1;
                        if Lowercase(ParamStr(i)) = 'scpi-telnet' then
                          scpiPort := 5024      (* Gospel according to NMap     *)
                        else
                          if Lowercase(ParamStr(i)) = 'scpi-raw' then
                            scpiPort := 5025    (* Gospel according to NMap     *)
                          else
                            if (not TryStrToInt(ParamStr(i), scpiPort)) or
                                        (scpiPort < 0) or (scpiPort > 65535) then begin
                              WriteLn(stderr, 'Bad SCPI port number');
                              exit(1)   (* Bad SCPI port                        *)
                            end
                      end
    otherwise
      if i <> ParamCount() then begin
        WriteLn(stderr, 'Bad device name');
        exit(1)                         (* Bad device name                      *)
      end else
        portName := ParamStr(i)
    end;
    i += 1
  end;

(* In principle, if the debugging level were appropriate I could list the auto- *)
(* detected serial ports here. In practice telling the user anything useful     *)
(* would be quite a lot of work, since the standard code doesn't actually save  *)
(* manufacturer and driver names as it's scanning the /sys tree trying to find  *)
(* a satisfactory match.                                                        *)

  if debugLevel > 1 then begin
    ports := ListPorts;
    try
      for i := 0 to ports.Count - 1 do
        WriteLn(stderr, '# ' + ports[i])
    finally
      FreeAndNil(ports)
    end
  end;

// OK, so I did a bit but it doesn't show very much that's useful. What I'm
// inclined to do next is hang a secondary stringlist onto each line that
// represents a port, and then as properties (e.g. kernel driver) are being
// checked update indexed lines which can be subsequently walked.

  portHandle := SerOpenLocked(portName);
  if portHandle = InvalidSerialHandle then begin
    WriteLn(stderr, 'Device ' + portName + ' cannot be opened');
    exit(2)                             (* Cannot be opened                     *)
  end;
  result := 0;
  if debugLevel > 0 then
    WriteLn(stderr, '# Using port ', portName);

(* If requested, start an SCPI server.                                          *)

  if scpiPort >= -1 then begin
    if debugLevel > 0 then
      if scpiPort = -1 then
        WriteLn(stderr, '# Starting SCPI daemon on standard I/O')
      else
        WriteLn(stderr, '# Starting SCPI daemon on port ', scpiPort);
    scpi := TScpiServer.Create(scpiPort);
    if Assigned(scpi) then begin
      scpiLock := TCriticalSection.Create;
      scpi.BlankIsHelp := true;
      scpi.HelpIsHelp := true;
      scpi.HelpQIsHelp := true;
      scpi.Register('', @scpiDoNothing); (* Default does nothing              *)
      scpi.Register('*HALT', @scpiDoHalt);
      scpi.Register('SYSTem:HELP:HEADers?', nil);
      scpi.Register('*IDN', @scpiDoIdentify);
      scpi.Register('SENSe:FUNCtion?', @scpiDoReportFunction1);
      scpi.Register('SENSe:FUNCtion1?', @scpiDoReportFunction1);
      scpi.Register('SENSe:FUNCtion2?', @scpiDoReportFunction2);
      scpi.Register('SENSe:VALUe?', @scpiDoReportValue1);
      scpi.Register('SENSe:VALUe1?', @scpiDoReportValue1);
      scpi.Register('SENSe:VALUe2?', @scpiDoReportValue2);

(* As an alternative, we could use e.g.                                         *)
(*                                                                              *)
(*      scpi.Register('SENSe:VALUe#?', @scpiDoReportValue);                     *)
(*                                                                              *)
(* and rely on the handler looking at the command.                              *)

(* If Run() hasn't been called to activate the thread, which depends on the     *)
(* thread manager being imported into the main unit at compilation, then it's   *)
(* necessary to call Poll() regularly. Don't expect this to perform well.       *)

     if not scpi.Run() then begin
        result := 10;
        if scpiPort < 0 then
          WriteLn(stderr, 'Unable to run SCPI server on stdin')
        else
          WriteLn(stderr, 'Unable to run SCPI server on port ', scpiPort)
      end
    end else begin
      result := 10;
      WriteLn(stderr, 'Unable to create SCPI server')
    end
  end;
  if result = 0 then
    result := RunConsoleApp2(portHandle, dontStop);
  case result of
    3: WriteLn(stderr, 'No data waiting for sync byte');
    4: WriteLn(stderr, 'No data reading message');
    5: WriteLn(stderr, 'Error formatting message')
  otherwise

(* Assume that whatever command we specified on the commandline, defaulting to  *)
(* a display of the model number, has been successful.                          *)

  end
end { RunConsoleApp };


initialization
  Assert(SizeOf(smallint) = 2)
end.

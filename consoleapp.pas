(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

unit ConsoleApp;

(* This is the greater part of a console program which reads data from a        *)
(* Mastech MS2115B meter and sends it to stdout.                MarkMLl.        *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

(* GNU-mandated support for --version and --help.
*)
procedure DoVersion(const projName: string);

(* GNU-mandated support for --version and --help.
*)
procedure DoHelp(const portName: string; portScan: boolean= false);

(* Main function, return 0 if no error.
*)
function RunConsoleApp(portName: string): integer;


implementation

uses
  StrUtils, LocateCp210Port, IniFilesAbout, Serial, BaseUnix, Termio;

var
  oddity: boolean= false;


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
  scale := PadRight(numToScale(message[1], message[2], message[3], multiplier, units), longestScale) + '|'
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
function outputAutoFormatted(message: Tmessage): boolean;

var
  range, scale, units: string;
  multiplier: double= 1.0;
  value: smallint;

begin
  if message[0] <> $55 then
    exit(false);                        (* Bad sync byte                        *)
  Write(IsoFormatDateTime(Now(), IsoTimeOnly, 2) + '|'); (* Date, time          *)
  if not unpack(message, range, scale, multiplier, units) then
    exit(false);
  Write(range, scale);                  (* Range, scale                         *)
  value := smallint(message[4] or (message[5] shl 8));
  Write(multiplier * value:8:3, '|');   (* Main value                           *)
  Write(units + '|');
  Write(HexStr(message[3], 2) + '|');   (* Frequency range?                     *)
  value := smallint(message[6] or (message[7] shl 8));
  case message[1] of
    $00,
    $01,
    $02: WriteLn(0.01 * value:5:2, 'Hz'); (* V-AC's secondary value is Hz * 100 *)
    $06: WriteLn(0.1 * value:4:1, '%')  (* Freq's secondary is duty cycle * 10  *)
  otherwise
    WriteLn(value)                      (* Other secondary values               *)
  end;
  result := true
end { outputAutoFormatted } ;


(* Main function, return 0 if no error.
*)
function RunConsoleApp(portName: string): integer;

var
  portHandle: TSerialHandle= InvalidSerialHandle;
  i, lastI: integer;
  formatString: string= '';
  debugLevel: integer= 0;
  onceOnly: boolean= false;
  ports: TStringList;
  message: Tmessage;
  resync: jmp_buf;
  waitingFirstSync: boolean;

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

  portHandle := SerOpen(portName);
{$ifdef UNIX }
  if portHandle > 0 then
    if fpIoctl(portHandle, TIOCEXCL, nil) <> 0 then begin (* Mandatory lock,    *)
      SerClose(portHandle);             (* unlike flock() (if it even works in  *)
      portHandle := -1                  (* this context) or a lock file as used *)
    end;                                (* by various gettys etc.               *)
{$endif UNIX }
  if portHandle = InvalidSerialHandle then begin
    WriteLn(stderr, 'Device ' + portName + ' cannot be opened');
    exit(2)                             (* Cannot be opened                     *)
  end;
  if debugLevel > 0 then
    WriteLn(stderr, '# Using port ', portName);
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
            1: begin
                 WriteLn(stderr, 'No data waiting for sync byte');
                 exit(3)                (* Unresponsive                         *)
               end
          otherwise
            WriteLn(stderr, 'No data reading message');
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

(* We have a 9-byte message. If there is an explicit format string then apply   *)
(* it, otherwise just try to do the right thing.                                *)

      if formatString <> '' then
        if not outputFormatted(message, formatString) then begin
          WriteLn(stderr, 'Error formatting message');
          exit(5)                       (* Format error                         *)
        end else begin end
      else
        outputAutoFormatted(message);
      if onceOnly then                  (* Debugging option, -ve level          *)
        break
    until false                         (* Or signal from keyboard              *)
  finally
    SerClose(portHandle)
  end;
  result := 0
end { RunConsoleApp };


initialization
  Assert(SizeOf(smallint) = 2)
end.


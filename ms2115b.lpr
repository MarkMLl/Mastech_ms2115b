(* Lazarus+FPC 2.1.0+3.0.4 on Linux Lazarus+FPC 2.1.0+3.0.4 on Linux Lazarus+FP *)

program ms2115b;

(* Read data from a Mastech MS2115B, either as a console program or with a GUI. *)
(*                                                              MarkMLl.        *)

{$mode objfpc}{$H+}

uses
  ConsoleApp, LocateCp210Port;

var
  Ms2115bPort: string= '';
  scanPorts: boolean= false;
  i: integer;

begin
  for i := 1 to ParamCount() do
    if Pos('-ports', LowerCase(ParamStr(i))) <> 0 then
      scanPorts := true;
  for i := 1 to ParamCount() do
    if LowerCase(ParamStr(i)) = '--version' then begin
      DoVersion('Ms2115b');
      Halt(0)
    end;
  Ms2115bPort := FindMs2115bPort(scanPorts); (* Builds cached ports list        *)
  for i := 1 to ParamCount() do
    if LowerCase(ParamStr(i)) = '--help' then begin
      DoHelp(Ms2115bPort, scanPorts);
      Halt(0)
    end;
    Halt(RunConsoleApp(Ms2115bPort));

(* The objective here is to minimise the amount of manually-inserted text so as *)
(* to give the IDE the best chance of managing form names etc. automatically. I *)
(* try, I don't always succeed...                                               *)

end.


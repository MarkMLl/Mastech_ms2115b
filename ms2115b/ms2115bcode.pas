(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

unit Ms2115bCode;

(* GUI program to read data from a Mastech MS2115B meter, styled on their       *)
(* supplied Windows-only application.                           MarkMLl         *)

// TODO : Save-to-file etc.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, PairSplitter,
  ComCtrls, Grids, StdCtrls, ExtCtrls, TAGraph, TAIntervalSources, TASeries;

type
  String15= string[15];

  { TMs2115bForm }

  TMs2115bForm = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    GroupBoxReading: TGroupBox;
    ImageList1: TImageList;
    LabelChartSize: TLabel;
    LabelReadingTop: TLabel;
    LabelReadingBottom: TLabel;
    LabelReadingMiddle: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItemExportImageGif: TMenuItem;
    MenuItemExportImagePng: TMenuItem;
    MenuItemExportImage: TMenuItem;
    MenuItemStopLog: TMenuItem;
    MenuItemDump: TMenuItem;
    MenuItemLog: TMenuItem;
    MenuItemRunStop: TMenuItem;
    MenuItemRunStart: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemSetport: TMenuItem;
    MenuItemRun: TMenuItem;
    MenuItemFileExit: TMenuItem;
    MenuItemFile: TMenuItem;
    PairSplitterLR: TPairSplitter;
    PairSplitterLTB: TPairSplitter;
    PairSplitterLRLeft: TPairSplitterSide;
    PairSplitterLRRight: TPairSplitterSide;
    PairSplitterLTBTop: TPairSplitterSide;
    PairSplitterLTBBottom: TPairSplitterSide;
    SaveDialog1: TSaveDialog;
    StringGridLog: TStringGrid;
    ToolBar1: TToolBar;
    ToolButtonFileSave: TToolButton;
    ToolButtonFilePrint: TToolButton;
    ToolButtonRunStart: TToolButton;
    ToolButtonRunStop: TToolButton;
    ToolButtonWhatsThis: TToolButton;
    ToolButtonFileSeparator1: TToolButton;
    ToolButtonFileSeparator2: TToolButton;
    ToolButtonFileNew: TToolButton;
    ToolButtonFileOpen: TToolButton;
    procedure Chart1Resize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure LabelChartSizeDblClick(Sender: TObject);
    procedure MenuItemExportImagePngClick(Sender: TObject);
    procedure MenuItemFileExitClick(Sender: TObject);
    procedure MenuItemFileNewClick(Sender: TObject);
    procedure MenuItemFileOpenClick(Sender: TObject);
    procedure MenuItemRunStartClick(Sender: TObject);
    procedure MenuItemRunStopClick(Sender: TObject);
  strict private
    widthTweak, heightTweak: integer;

    (* This is called when the wm_After_Show message is dequeued, which is
      guaranteed to be after form creation has completed.
    *)
    procedure OnAfterShow(afterShowParam: PtrInt);
  protected
    fcommsThread: TThread;

    (* Output the values read from the meter in text form. This is called, via
      Synchronize(), by the background communications thread.
    *)
    procedure ReadingTxt(const func, mainDisp, mainUnit, subDisp, subUnit,
                                                        date, time, extra: string15);

    (* Output the values read from the meter in binary form for the chart etc.
      This is called, via Synchronize(), by the background communications thread.
    *)
    procedure ReadingBin(timestamp: TDateTime; value: double);
  public

    (* This is the default port name, generated by looking for a serial device
      using the Linux cp210x kernel module.

      This illustrates one of my gripes about Object Pascal: I want this to be
      settable from the main unit (project file) only, but there is insufficient
      protection available to enforce this. The best that could be done would be
      to set up a property which detects if it has already been set, but that
      would need an extra field (or reserved string) to detect that it had been
      set to a "no port" value... all in all the hassle isn't worth it.
    *)
    DefaultPort: string;

    (* Return the apparent size of the specified control, as a formatted string.
    *)
    function GraphicSize(ctrl: TWinControl): string;

    (* Message box optimised for position.
    *)
    FUNCTION MessageDlgOpt(CONST aMsg: STRING; dlgType: TMsgDlgType;
                            buttons: TMsgDlgButtons; helpCtx: LONGINT): INTEGER;

  end;

var
  Ms2115bForm: TMs2115bForm;


implementation

{$R *.lfm}

uses
  Serial, BaseUnix, Termio, ConsoleApp, LocatePorts, IniFilesAbout, StrUtils;

const
  ProjName= 'MS2115B';
  Ms2115bCodeMagicNumber= 2020012719;

(********************************************************************************)

type
  TcommsThread= class(TThread)
  strict private
    serialHandle: TSerialHandle;
    messageStr: string;
    procedure msgShim;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; mySerialHandle: TSerialHandle);
    procedure writerShim;
  end;

  TMs2115bFormH=                        (* Avoid circular definition            *)
    class helper for TMs2115bForm
    protected
      function CommsThread(): TCommsThread; inline;
    end;


(* This helper avoids problems with circular references, in particular many
  occurrences of things like TCommsThread(Ms2115bForm.CommsThread).Synchronize()
  in favour of the somewhat more concise Ms2115bForm.CommsThread.Synchronize().
*)
function TMs2115bFormH.CommsThread(): TCommsThread; inline;

begin
  result := fCommsThread as TcommsThread
end { TMs2115bFormH.CommsThread } ;


(* These are used to pass values between the background thread and the main     *)
(* (GUI) thread. They'd normally be local to the thread object, but can't be in *)
(* this case since the RunConsoleApp2() function hence the writer callback are  *)
(* "traditional" Pascal rather than being "methods" in an object.               *)

var
  func, mainDisp, mainUnit, subDisp, subUnit, date, time, extra: string15;
  mainBin: double;
  stopLoop: boolean= false;


constructor TcommsThread.Create(CreateSuspended: Boolean; mySerialHandle: TSerialHandle);

begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := true;
  serialHandle := mySerialHandle
end { TcommsThread.Create } ;


(* Provided that it is called via Synchronize(), this executes in the context of
  the main (GUI) thread.
*)
procedure TcommsThread.msgShim;

begin
  Ms2115bForm.MessageDlgOpt(messageStr, mtError, [mbOk], 0);
  Ms2115bForm.MenuItemRunStopClick(nil) (* Help GUI recover from error          *)
end { TcommsThread.msgShim } ;


(* Provided that it is called via Synchronize(), this executes in the context of
  the main (GUI) thread.
*)
procedure TcommsThread.writerShim;

begin
  Ms2115bForm.ReadingTxt(func, mainDisp, mainUnit, subDisp, subUnit, date, time, extra);
  Ms2115bForm.ReadingBin(Now(), mainBin)
end { TcommsThread.writerShim } ;


(* This is called by the datacomms loop in the datacomms thread roughly every
  half second, and passes the unpacked data to the main (GUI) thread via
  Synchronize() and a shim procedure (immediately above).
*)
procedure writer(const s: string);

// This isn't very efficient since it is basically processing text output which
// has been parsed and scaled from the binary data received from meter, but
// since this is a background thread I'm not particularly bothered by that.

begin
  date := IsoNow(IsoDateOnly);
  time := Trim(ExtractWord(1, s, ['|']));
  func := Trim(ExtractWord(2, s, ['|']));
  mainUnit := Trim(ExtractWord(3, s, ['|']));
  mainDisp := Trim(ExtractWord(4, s, ['|']));
  subUnit := Trim(ExtractWord(5, s, ['|']));
  subDisp := Trim(ExtractWord(6, s, ['|']));
  extra := Trim(ExtractWord(7, s, ['|']));
  mainBin := StrToFloat(mainDisp);
  Ms2115bForm.CommsThread.Synchronize(@Ms2115bForm.CommsThread.writerShim)
end { writer } ;


(* Main datacomms code. Because RunConsoleApp2() may run either in the context
  of this background thread or a simple console program, its writer parameter
  is a top-level procedure rather than being a method in the thread.
*)
procedure TcommsThread.Execute;

begin
  case RunConsoleApp2(serialHandle, stopLoop, @writer) of
    3: messageStr := 'No data waiting for sync byte';
    4: messageStr := 'No data reading message';
    5: messageStr := 'Error formatting message'
  otherwise
    messageStr := ''
  end;
  if (messageStr <> '') and not stopLoop then
    Synchronize(@msgShim);
  messageStr := ''
end { TcommsThread.Execute } ;


(********************************************************************************)

const
  topStandardSize= 8;

type
  TstandardSizeUnit= (ssPx, ssIn, ssMm);

type
  TstandardSize= record
                   name: string[62];
                   ssUnit: TstandardSizeUnit;
                   width, height: integer;
                   widthPx, heightPx: integer
                 end;

var
  standardSizes: array[0..topStandardSize] of TStandardSize;


(* Initialise the array which defines plausible page sizes for exported
  graphical output.
*)
procedure initPageSizes;

begin
    with standardSizes[0] do begin
    name := '4x3"';
    ssUnit := ssIn;
    width := 4;
    height := 3
  end;
  with standardSizes[1] do begin
    name := '5x4"';
    ssUnit := ssIn;
    width := 5;
    height := 4
  end;
  with standardSizes[2] do begin
    name := '7x5"';
    ssUnit := ssIn;
    width := 7;
    height := 5
  end;
  with standardSizes[3] do begin
    name := '640x480';
    ssUnit := ssPx;
    width := 640;
    height := 480
  end;
  with standardSizes[4] do begin
    name := '800x600';
    ssUnit := ssPx;
    width := 800;
    height := 600
  end;
  with standardSizes[5] do begin
    name := '1024x768';
    ssUnit := ssPx;
    width := 1024;
    height := 768
  end;
  with standardSizes[6] do begin
    name := 'ISO A5';
    ssUnit := ssMm;
    width := 210;
    height := 148
  end;
  with standardSizes[7] do begin
    name := 'ISO A6';
    ssUnit := ssIn;
    width := 148;
    height := 105
  end;
  with standardSizes[8] do begin
    name := 'ISO A7';
    ssUnit := ssIn;
    width := 105;
    height := 74
  end
end { initPageSizes } ;


(* Immediately before the page size array is referred to in a resize operation,
  make sure that sizes are updated using the current DPI setting where needed.
*)
procedure scalePageSizes;

var
  i: integer;

begin
  for i := 0 to topStandardSize do
    with standardSizes[i] do
      case ssUnit of
        ssIn: begin
                widthPx := width * Screen.PrimaryMonitor.PixelsPerInch;
                heightPx := height * Screen.PrimaryMonitor.PixelsPerInch
              end;
        ssMm: begin
                widthPx := Round((width * Screen.PrimaryMonitor.PixelsPerInch) / 25.4);
                heightPx := Round((height * Screen.PrimaryMonitor.PixelsPerInch) / 25.4)
              end
      otherwise
        widthPx := width;
        heightPx := height
      end
end { scalePageSizes } ;


(********************************************************************************)


{ TMs2115bForm }


procedure TMs2115bForm.MenuItemFileNewClick(Sender: TObject);
begin

end { TMs2115bForm.MenuItemFileNewClick } ;


procedure TMs2115bForm.MenuItemFileExitClick(Sender: TObject);

begin
  Close
end { TMs2115bForm.MenuItemFileExitClick } ;


procedure TMs2115bForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

var
  temp: string;

begin
  temp:= Application.MainForm.Caption;
  IF Pos(' ', temp) > 0 THEN
    SetLength(temp, Pos(' ', temp) - 1);
  temp:= 'Terminate ' + temp + '?';
  CanClose:= MessageDlgOpt(temp, mtWarning, [mbYes, mbNo], 0) = mrYes
end { TMs2115bForm.FormCloseQuery } ;


(* Return the apparent size of the specified control, as a formatted string.
*)
function TMs2115bForm.GraphicSize(ctrl: TWinControl): string;

var
  nearestDistance, currentDistance: single;
  nearestIndex, i: integer;

begin
  scalePageSizes;                       (* DPI might change during program execution *)

(* Find the standard-size corner nearest to the current control corner.         *)

  nearestIndex := 0;
  nearestDistance := 1.0E99;
  for i := 0 to topStandardSize do begin
    currentDistance := Sqrt(Sqr(ctrl.Width - standardSizes[i].widthPx) + Sqr(ctrl.Height - standardSizes[i].heightPx));
    if currentDistance < nearestDistance then begin
      nearestIndex := i;
      nearestDistance := currentDistance
    end
  end;

(* I'd prefer to use Format() right through here but I don't trust the FPC      *)
(* implementation (or for that matter my own extensions) to reliably give me    *)
(* signed +ve integers where I want them. The tweaks are saved since they will  *)
(* be needed if the user wants to snap the control to a standard size before    *)
(* exporting it, they are used here to indicate how much bigger the control is  *)
(* than a standard size so if used to adjust the size they are subtracted.      *)

  widthTweak := ctrl.Width - standardSizes[nearestIndex].widthPx;
  heightTweak := ctrl.Height - standardSizes[nearestIndex].heightPx;
  result := Format('%d×%d (%s', [ctrl.Width, ctrl.Height, standardSizes[nearestIndex].name]);
  if (widthTweak <> 0) or (heightTweak <> 0) then begin
    result += ' ';
    if ctrl.Width >= standardSizes[nearestIndex].widthPx then
      result += '+';
    result += IntToStr(widthTweak) + '×';
    if ctrl.Height >= standardSizes[nearestIndex].heightPx then
      result += '+';
    result += IntToStr(heightTweak)
  end;
  result += ')'
end { TMs2115bForm.GraphicSize } ;


procedure TMs2115bForm.Chart1Resize(Sender: TObject);

begin
  LabelChartSize.Caption := GraphicSize(Chart1);
  LabelChartSize.Top := Chart1.Top;
  LabelChartSize.Left := Chart1.Left + (Chart1.Width - Chart1.AxisList[2].LabelSize) -
                                                        LabelChartSize.Width
end { TMs2115bForm.Chart1Resize } ;


(* This is called when the wm_After_Show message is dequeued, which is
  guaranteed to be after form creation has completed.
*)
procedure TMs2115bForm.OnAfterShow(afterShowParam: PtrInt);

var
  i: integer;
  portNames: TStringList;

begin
  Assert(afterShowParam = Ms2115bCodeMagicNumber, 'Internal error: TMs2115bForm bad magic number');

// TODO : Use updatePortList from jds6600 project here.
// Also add idle timer to detect and action hotplug events.

(* Initialise the menu from the known ports. The first port in the list is      *)
(* selected as the initial default, then this is changed to the "best" one e.g. *)
(* /dev/ttyUSB0.                                                                *)

  portNames := ListPorts();
  try
    for i := 0 to portNames.Count - 1 do begin
      MenuItemSetPort.Add(TMenuItem.Create(nil));
      with MenuItemSetPort.Items[i] do begin
        Caption := portnames[i];
        GroupIndex := 1;
        RadioItem := true;
        Checked := (i = 0) or (Caption = DefaultPort);
        AutoCheck := true
      end
    end;
    MenuItemRun.Enabled := portNames.Count > 0;
    MenuItemRunStart.Enabled := MenuItemRun.Enabled
  finally
    FreeAndNil(portNames)
  end;
  LabelChartSize.Parent := Chart1;      (* Dont change in resize event, assume  *)
  LabelChartSize.Color := Chart1.BackColor (* it will be called imminently.     *)
end { TMs2115bForm.OnAfterShow } ;


procedure TMs2115bForm.FormCreate(Sender: TObject);

begin

  Application.QueueAsyncCall(@OnAfterShow, Ms2115bCodeMagicNumber) (* Keep at end *)
end { TMs2115bForm.FormCreate } ;


(* Double-click on the label which displays the graphic size to resize the main
  form in order to make the graphic size standard for easy insertion in a
  notenook etc.
*)
procedure TMs2115bForm.LabelChartSizeDblClick(Sender: TObject);

begin

(* The tweak values are set by a TAChart resize operation, of which there will  *)
(* always be at least one (at program start).                                   *)

  Application.MainForm.Width := Application.MainForm.Width - widthTweak;
  widthTweak := 0;
  Application.MainForm.Height := Application.MainForm.Height - HeightTweak;
  heightTweak := 0
end { TMs2115bForm.LabelChartSizeDblClick } ;


procedure TMs2115bForm.MenuItemExportImagePngClick(Sender: TObject);

var
  nameBlank: boolean;
  png: TPortableNetworkGraphic;
  everything: TRect;

begin
  nameBlank := Trim(SaveDialog1.Filename) = '';
  if nameBlank then
    SaveDialog1.Filename := projName + '_' + StringReplace(IsoNow(), ' ', 'T', [rfReplaceAll]);
  SaveDialog1.DefaultExt := 'png';

(* Possibly replace this later with a custom setup form to select compression   *)
(* etc.                                                                         *)

  if SaveDialog1.Execute then begin
    png := TPortableNetworkGraphic.Create;
    try
      png.Width := Chart1.Width;
      png.Height := Chart1.Height;
      everything.Left := 0;
      everything.Top := 0;
      everything.Right := Chart1.Width - 1;
      everything.bottom := Chart1.Height - 1;
      png.Canvas.CopyRect(everything, Chart1.Canvas, everything);
      png.SaveToFile(SaveDialog1.Filename)
    finally
      png.Free
    end
  end;
  if nameBlank then
    SaveDialog1.Filename := ''
end { TMs2115bForm.MenuItemExportImagePngClick } ;


procedure TMs2115bForm.MenuItemFileOpenClick(Sender: TObject);

begin

end { TMs2115bForm.MenuItemFileOpenClick } ;


procedure TMs2115bForm.MenuItemRunStartClick(Sender: TObject);

var
  i: integer;
  portName: string= '';
  portHandle: TSerialHandle= InvalidSerialHandle;

begin
  for i := 0 to MenuItemSetPort.Count - 1 do
    if MenuItemSetPort.Items[i].Checked then
      portName := MenuItemSetPort.Items[i].Caption;
  if portName = '' then
    exit;
  portHandle := SerOpen(portName);      (* Configured and closed by thread      *)
{$ifdef UNIX }
  if portHandle > 0 then
    if fpIoctl(portHandle, TIOCEXCL, nil) <> 0 then begin (* Mandatory lock,    *)
      SerClose(portHandle);             (* unlike flock() (if it even works in  *)
      portHandle := InvalidSerialHandle (* this context) or a lock file as used *)
    end;                                (* by various gettys etc.               *)
{$endif UNIX }
  stopLoop := false;
  if portHandle <> InvalidSerialHandle then begin
    GroupBoxReading.Caption := 'Reading';
    MenuItemRunStart.Enabled := false;
    StringGridLog.RowCount := 1;

(* The thread should free itself on termination. I'm going to be somewhat       *)
(* naughty and assume this is done correctly.                                   *)

    fcommsThread := TcommsThread.Create(false, portHandle);
    MenuItemRunStop.Enabled := true
  end
end { TMs2115bForm.MenuItemRunStartClick } ;


procedure TMs2115bForm.MenuItemRunStopClick(Sender: TObject);

begin
  MenuItemRunStop.Enabled := false;
  fcommsThread.Terminate;               (* Leaves field containing rubbish      *)
  stopLoop := true;                     (* Makes sure thread loop exits promptly *)
  fcommsThread := nil;
  GroupBoxReading.Caption := 'Paused';
  MenuItemRunStart.Enabled := true
end { TMs2115bForm.MenuItemRunStopClick } ;


(* Output the values read from the meter in text form. This is called, via
  Synchronize(), by the background communications thread.
*)
procedure TMs2115bForm.ReadingTxt(const func, mainDisp, mainUnit, subDisp, subUnit,
                                                        date, time, extra: string15);

var
  currentRow: integer;


  function spaceBeforeUnit(const s: string): string;

  var
    i: integer;

  begin
    result := s;
    if Pos(' ', result) <= 0 then
      for i := 2 to Length(s) do
        if (s[i - 1] in ['.', '0'..'9']) and not (s[i] in ['.', '0'..'9']) then begin
          insert(' ', result, i);
          break
        end
  end { spaceBeforeUnit } ;


begin
  if StringGridLog.RowCount >= 1000 + 125 then
    while StringGridLog.RowCount > 1000 - 125 do
      StringGridLog.DeleteRow(1);
  currentRow := StringGridLog.RowCount;
  StringGridLog.RowCount := currentRow + 1;
  StringGridLog.Row := currentRow + 1;
  with StringGridLog do begin
    Cells[0, currentRow] := DelSpace1(func);
    Cells[1, currentRow] := mainDisp;
    Cells[2, currentRow] := mainUnit;
    Cells[3, currentRow] := subDisp;
    Cells[4, currentRow] := subUnit;
    Cells[5, currentRow] := date;
    Cells[6, currentRow] := time
  end;
  LabelReadingTop.Caption := spaceBeforeUnit(extra);
  LabelReadingMiddle.Caption := mainDisp + ' ' + subunit;
  LabelReadingBottom.Caption := DelSpace1(func)
end { TMs2115bForm.ReadingTxt } ;


(* Output the values read from the meter in binary form for the chart etc.
  This is called, via Synchronize(), by the background communications thread.
*)
procedure TMs2115bForm.ReadingBin(timestamp: TDateTime; value: double);

var
  i: integer;

begin
  if Chart1LineSeries1.Count = 0 then   (* Make startup a bit more predictable  *)
    for i := -99 to 0 do
      Chart1LineSeries1.AddXY(Now() + (0.5 * i / SecsPerDay), value);
  while Chart1LineSeries1.Count >= 100 + 4 do
    while Chart1LineSeries1.Count > 100 - 4 do
      Chart1LineSeries1.Delete(0);
  Chart1LineSeries1.AddXY(timestamp, value)
end { TMs2115bForm.ReadingBin } ;


(* Message box optimised for position.
*)
FUNCTION TMs2115bForm.MessageDlgOpt(CONST aMsg: STRING; dlgType: TMsgDlgType;
                        buttons: TMsgDlgButtons; helpCtx: LONGINT): INTEGER;

VAR     x, y: INTEGER;

BEGIN
  x:= (Left + Width DIV 2 + Screen.Width DIV 2) DIV 2;
  y:= (Top + Height DIV 2 + Screen.Height DIV 2) DIV 2;
  RESULT:= MessageDlgPos(aMsg, dlgType, buttons, helpCtx, x, y)
END { TMs2115bForm.MessageDlgOpt } ;


initialization
  initPageSizes
end.


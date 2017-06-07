unit formMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActiveX, ExtCtrls, DirectShow9 ,DSUtil, StdCtrls, DCDSPPluginRenderer,
  DCVISPluginRenderer, ComCtrls, dmoConst, dspConst, DCDSPFilterInterfaces;

type
  TfrmMain = class(TForm,IDCDSPFilterPCMCallBack)
    pnlVideo: TPanel;
    pnlControls: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    OpenDialog: TOpenDialog;
    Button5: TButton;
    Button6: TButton;
    TrackBar1: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnlVideoResize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure pnlVideoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlVideoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlVideoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    iMC : IMediaControl;
    iVW : IVideoWindow;
    iMS : IMediaSeeking;
    dsGraph : IGraphBuilder;
    fMouseDown : Boolean;
    cx, cy : integer;

    fROT : integer;

    function PCMDataCB(Buffer : Pointer; Length : integer; out NewSize : integer; Stream : PDSStream) : HRESULT; stdcall;
    function MediaTypeChanged(Stream : PDSStream) : HRESULT; stdcall;
    function Flush : HRESULT; stdcall;

    function RenderMediaFileW32(Filename : String) : integer;
    procedure ClearDirectShow;
  public
    fDCDSPFilter : IBaseFilter;
    fDSPPlugins : TDCDSPPluginList;
    fVISPlugins : TDCVISPluginList;
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses formPlugins;

{$R *.dfm}
{*** Filter Callbacks *********************************************************}
function TfrmMain.PCMDataCB(Buffer : Pointer; Length : integer; out NewSize : integer; Stream : PDSStream) : HRESULT;
begin
  Result := S_OK;
  NewSize := fDSPPlugins.Process(Buffer,Length);
end;

function TfrmMain.MediaTypeChanged(Stream : PDSStream) : HRESULT;
begin
  fDSPPlugins.Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  Result := S_OK;
end;

function TfrmMain.Flush : HRESULT;
begin
  fDSPPlugins.Flush;
  Result := S_OK;
end;
{******************************************************************************}
procedure TfrmMain.ClearDirectShow;
var
  EnumFilters : IEnumFilters;
  BaseFilter : IBaseFilter;
begin
  if Assigned(iMC) then
  begin
    iMC.Stop;
    iMC := nil;
  end;
  fVISPlugins.SetAudiofilter(nil);
  if Assigned(dsGraph) then
  begin
    RemoveGraphFromRot(fROT);
    if dsGraph.EnumFilters(EnumFilters) = S_OK then
    begin
      while EnumFilters.Next(1,BaseFilter,nil) = S_OK do
      begin
        dsGraph.RemoveFilter(BaseFilter);
        EnumFilters.Reset;
      end;
    end;
    EnumFilters := nil;
    BaseFilter := nil;
    dsGraph := nil;
  end;
  if Assigned(iMC) then iMC := nil;
  if Assigned(iVW) then iVW := nil;
  if Assigned(iMS) then iMS := nil;
  fDCDSPFilter := nil;
end;

function TfrmMain.RenderMediaFileW32(Filename : String) : integer;
var
  hr : HRESULT;
  iFG : IFilterGraph2;
  Dur : Int64;
begin
  ClearDirectShow;
  hr := CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER, IID_IGraphBuilder, dsGraph);
  if (hr <> S_OK) then
  begin
    showmessage(GetErrorString(hr) + ' (Error Code : ' + inttohex(hr,8) + ')');
    Result := hr;
    Exit;
  end;
  hr := dsGraph.QueryInterface(IID_IFilterGraph2,iFG);
  if (hr <> S_OK) then
  begin
    showmessage(GetErrorString(hr) + ' (Error Code : ' + inttohex(hr,8) + ')');
    Result := hr;
    Exit;
  end;
  AddGraphToRot(iFG,fROT);

  hr := CoCreateInstance(CLSID_DCDSPFilter, nil, CLSCTX_INPROC, IID_IBaseFilter, fDCDSPFilter);
  if (hr <> S_OK) then
  begin
    showmessage(GetErrorString(hr) + ' (Error Code : ' + inttohex(hr,8) + ')');
  end else
  begin
    hr := iFG.AddFilter(fDCDSPFilter, StringToOleStr('DC-DSP Filter'));
    if (hr <> S_OK) then
    begin
      showmessage(GetErrorString(hr) + ' (Error Code : ' + inttohex(hr,8) + ')');
    end else
    begin
      // Set the Form to retrieve Events of the Filter
      (fDCDSPFilter as IDCDSPFilterInterface).set_CallbackPCM(Self);
      fVISPlugins.SetAudiofilter(fDCDSPFilter);
      fVISPlugins.SetMediaName(ExtractFileName(Filename));
    end;
  end;

  hr := dsgraph.RenderFile(StringToOleStr(FileName),nil);
  if (hr <> S_OK) then
  begin
    case hr of
      VFW_S_PARTIAL_RENDER:
      begin
        showmessage(GetErrorString(hr) + ' (Error Code : ' + inttohex(hr,8) + ')');
      end else
      begin
        dsGraph := nil;
        showmessage(GetErrorString(hr) + ' (Error Code : ' + inttohex(hr,8) + ')');
        Result := hr;
        Exit;
      end;
    end;
  end;

  dsGraph.QueryInterface(IID_IMediaControl,iMC);
  dsGraph.QueryInterface(IID_IVideoWindow,iVW);
  dsGraph.QueryInterface(IID_IMediaSeeking, iMS);

  if Assigned(iMS) then
  begin
    iMS.GetDuration(Dur);
    TrackBar1.Max := Dur div 100000;
  end;

  if Assigned(iVW) then
  begin
    iVW.put_Owner(pnlVideo.Handle);
    iVW.put_WindowStyle(WS_CHILD);
    iVW.put_WindowStyleEx(0);
    iVW.put_MessageDrain(Handle);
    pnlVideoResize(Self);
    iVW.put_Visible(True);
  end;
  iFG := nil;
  iMC.Run;
  Result := S_OK;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  CoInitialize(nil);
  fDSPPlugins := TDCDSPPluginList.Create;
  fDSPPlugins.Directory := ExtractFilePath(Application.ExeName) + '\plugins';
  fVISPlugins := TDCVISPluginList.Create;
  fVISPlugins.Directory := ExtractFilePath(Application.ExeName) + '\plugins';
  pnlVideo.Align := alClient;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  ClearDirectShow;
  fVISPlugins.Free;
  fDSPPlugins.Free;
  CoUninitialize;
end;

procedure TfrmMain.pnlVideoResize(Sender: TObject);
var
  rcWnd : TRect;
begin
  if Assigned(iVW) then
  begin
    Windows.GetClientRect(pnlVideo.Handle, rcWnd);
    iVW.SetWindowPosition(rcwnd.Left,rcwnd.Top,rcwnd.Right - rcwnd.Left,rcwnd.Bottom - rcwnd.Top);
  end;
  fVISPlugins.Resize(pnlVideo.Width,pnlVideo.Height)
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  if Assigned(iMC) then iMC.Run;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  if Assigned(iMC) then iMC.Pause;
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
  if Assigned(iMC) then iMC.Stop;
end;

procedure TfrmMain.Button5Click(Sender: TObject);
begin
  if OpenDialog.Execute then RenderMediaFileW32(OpenDialog.FileName);
end;

procedure TfrmMain.Button6Click(Sender: TObject);
begin
  frmPlugins.Show;
  frmPlugins.BringToFront;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  fDSPPlugins.OwnerWindow := frmPlugins.pnlPlugins.Handle;
  fVISPlugins.OwnerWindow := frmPlugins.pnlPlugins.Handle;
  fVISPlugins.VISOwnerWindow := pnlVideo.Handle;
  frmPlugins.UpdateDSPPlugins;
end;

procedure TfrmMain.TrackBar1Change(Sender: TObject);
var
  Pos : Int64;
begin
  if Assigned(iMS) then
  begin
    Pos := TrackBar1.Position;
    Pos := Pos * 100000;
    iMS.SetPositions(Pos,AM_SEEKING_AbsolutePositioning,Pos,AM_SEEKING_NoPositioning);
  end;
end;

procedure TfrmMain.pnlVideoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    cx := x;
    cy := y;
    fMouseDown := True;
  end;  
end;

procedure TfrmMain.pnlVideoMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if fMouseDown then
  begin
    Left := left + (x - cx);
    top := top + (y - cy);
  end;
end;

procedure TfrmMain.pnlVideoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fMouseDown := False;
end;

end.

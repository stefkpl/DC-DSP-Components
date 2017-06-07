
    (*********************************************************************
     *  DynamicFilterList.pas                                            *
     *                                                                   *
     *  This unit is Part of the DC-DSP Audio Filter v1.0                *
     *                                                                   *
     *  author    : Milenko Mitrovic                                     *
     *  email     : dcoder@dsp-worx.de                                   *
     *  web       : http://dsp-worx.de                                   *
     *  date      : 24-07-2003                                           *
     *                                                                   *
     *  The contents of this file are used with permission, subject to   *
     *  the Mozilla Public License Version 1.1 (the "License"); you may  *
     *  not use this file except in compliance with the License. You may *
     *  obtain a copy of the License at                                  *
     *  http://www.mozilla.org/MPL/MPL-1.1.html                          *
     *                                                                   *
     *  Software distributed under the License is distributed on an      *
     *  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   *
     *  implied. See the License for the specific language governing     *
     *  rights and limitations under the License.                        *
     *                                                                   *
     *  (C) 2003, 2004 Milenko Mitrovic <dcoder@dsp-worx.de>             *
     *                                                                   *
     *********************************************************************)

unit DynamicFilterList;

interface

uses
  Windows, Classes, ActiveX, SyncObjs,

  dspAmplify, dspBandPass, dspChannelOrder, dspCompressor, dspDownMix, dspDynamicAmplify,
  dspEchoDelay, dspEqualizer, dspFlanger, dspHighPass, dspLowPass, dspNotch, dspPhaseInvert,
  dspPhaser, dspPitchScale, dspPitchShift, dspSound3D, dspTempo, dspTrebleEnhancer, dspTrueBass,
  dmoChorus, dmoCompressor, dmoDistortion, dmoEcho, dmoFlanger, dmoGargle, dmoI3DL2Reverb,
  dmoParamEQ, dmoWavesReverb, dspConst, dspUtils, dmoConst, dmoUtils, dspInterfaces,
  dmoInterfaces, dspParametricEQ;

const
  NumEQBands = 10;
  EQFreq: array[0..NumEQBands] of Word = (
    0, 3, 9, 16, 29, 48, 100, 141, 280, 559, 1024
  );

type
  TDCFilterType = (
    ftNone, ftAmplify, ftBandPass, ftChannelOrder, ftCompressor, ftDownMix, ftDynamicAmplify,
    ftEchoDelay, ftEqualizer, ftFlanger, ftHighPass, ftLowPass, ftNotch, ftPhaseInvert,
    ftPhaser, ftPitchScale, ftPitchShift, ftSound3D, ftTempo, ftTrebleEnhancer, ftTrueBass,
    ftDMOChorus, ftDMOCompressor, ftDMODistortion, ftDMOEcho, ftDMOFlanger, ftDMOGargle,
    ftDMOI3DL2Reverb, ftDMOParamEQ, ftDMOWavesReverb, ftParametricEQ
  );

  TEQSavings = record
    ChannelPos: integer;
    DB: integer;
    Band: array[0..MaxChannels -1,0..9] of ShortInt;
    Preset: array[0..255] of Char;
  end;
  PEQSavings = ^TEQSavings;

  TDCFilterList = class;

  TDCFilterItem = class(TCollectionItem)
  private
    fExtraBuffer: PChar;
    fWindowShown: Boolean;
    fFilterType: TDCFilterType;
    fFilter: TComponent;
    fSettingsSize: integer;
    fName: String;
    fOwner: TDCFilterList;
    procedure SetEnabled(aEnabled: Boolean);
    function GetEnabled: Boolean;
    procedure SetFilterType(aFilterType: TDCFilterType);
    procedure FreeFilter;
  public
    destructor Destroy; override;
    procedure Init(Stream: PDSStream);
    function Process(Buffer: Pointer; Size: integer; Stream: PDSStream): integer;
    procedure Flush;
    procedure SaveSettings(var Buffer: PChar);
    procedure LoadSettings(Buffer: PChar);
    function ExtraBuffer: PChar;
  published
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property FilterType: TDCFilterType read fFilterType write SetFilterType;
    property Owner: TDCFilterList read fOwner write fOwner;
    property Name: String read fName;
    property SettingsSize: integer read fSettingsSize;
    property Filter: TComponent read fFilter write fFilter;
    property WindowShown: Boolean read fWindowShown write fWindowShown;
  end;

  TDCFilterList = class(TCollection)
  private
    fLock: TCriticalSection;
    fInitialized: Boolean;
    fSaveBuffer: PChar;
    fStream: TDSStream;
    function  GetItem(Index: Integer): TDCFilterItem;
    procedure SetItem(Index: Integer; Value: TDCFilterItem);
    function GetName(Index: integer): String;
    function GetFilterType(Index: integer): TDCFilterType;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Delete(Index: Integer);
    function Add(FilterType: TDCFilterType): TDCFilterItem;
    function Insert(Index: integer; FilterType: TDCFilterType): TDCFilterItem;
    property Items[Index: Integer]: TDCFilterItem read GetItem write SetItem; default;
    property Name[Index: integer]: String read GetName;
    property FilterType[Index: integer]: TDCFilterType read GetFilterType;
    procedure Init(Stream: PDSStream);
    function Process(Buffer: Pointer; Size: integer): integer;
    procedure Flush;
    function ExportSettings(out Buffer: PChar; out Size: integer): Boolean;
    function ImportSettings(Buffer: PChar): Boolean;
    procedure ResetShownWindows;
    function GetDSPInterface(Index: integer): IUnknown;
    property Stream: TDSStream read fStream;
  end;

  function MakeNdbEQ(Value: integer; N: integer): integer;
  function GetNdbEQ(Value: integer; N: integer): integer;

implementation

uses SysUtils, ComObj;

function MakeNdbEQ(Value: integer; N: integer): integer;
begin
  Result := Value * N div 20;
end;

function GetNdbEQ(Value: integer; N: integer): integer;
begin
  Result := Value * 20 div N;
end;

destructor TDCFilterItem.Destroy;
begin
  FreeFilter;
  inherited Destroy;
end;

procedure TDCFilterItem.SetEnabled(aEnabled: Boolean);
begin
  if (fFilter = nil) then Exit;
  case fFilterType of
    ftAmplify:        TDCAmplify(fFilter).Enabled := aEnabled;
    ftBandPass:       TDCBandpass(fFilter).Enabled := aEnabled;
    ftChannelOrder:   TDCChannelOrder(fFilter).Enabled := aEnabled;
    ftCompressor:     TDCCompressor(fFilter).Enabled := aEnabled;
    ftDownMix:        TDCDownMix(fFilter).Enabled := aEnabled;
    ftDynamicAmplify: TDCDynamicAmplify(fFilter).Enabled := aEnabled;
    ftEchoDelay:      TDCEchoDelay(fFilter).Enabled := aEnabled;
    ftEqualizer:      TDCEqualizer(fFilter).Enabled := aEnabled;
    ftFlanger:        TDCFlanger(fFilter).Enabled := aEnabled;
    ftHighPass:       TDCHighpass(fFilter).Enabled := aEnabled;
    ftLowPass:        TDCLowpass(fFilter).Enabled := aEnabled;
    ftNotch:          TDCNotch(fFilter).Enabled := aEnabled;
    ftParametricEQ:   TDCParametricEQ(fFilter).Enabled := aEnabled;
    ftPhaseInvert:    TDCPhaseInvert(fFilter).Enabled := aEnabled;
    ftPhaser:         TDCPhaser(fFilter).Enabled := aEnabled;
    ftPitchScale:     TDCPitchScale(fFilter).Enabled := aEnabled;
    ftPitchShift:     TDCPitchShift(fFilter).Enabled := aEnabled;
    ftSound3D:        TDCSound3D(fFilter).Enabled := aEnabled;
    ftTempo:          TDCTempo(fFilter).Enabled := aEnabled;
    ftTrebleEnhancer: TDCTrebleEnhancer(fFilter).Enabled := aEnabled;
    ftTrueBass:       TDCTrueBass(fFilter).Enabled := aEnabled;
    ftDMOChorus:      TDCDMOChorus(fFilter).Enabled := aEnabled;
    ftDMOCompressor:  TDCDMOCompressor(fFilter).Enabled := aEnabled;
    ftDMODistortion:  TDCDMODistortion(fFilter).Enabled := aEnabled;
    ftDMOEcho:        TDCDMOEcho(fFilter).Enabled := aEnabled;
    ftDMOFlanger:     TDCDMOFlanger(fFilter).Enabled := aEnabled;
    ftDMOGargle:      TDCDMOGargle(fFilter).Enabled := aEnabled;
    ftDMOI3DL2Reverb: TDCDMOI3DL2Reverb(fFilter).Enabled := aEnabled;
    ftDMOParamEQ:     TDCDMOParamEQ(fFilter).Enabled := aEnabled;
    ftDMOWavesReverb: TDCDMOWavesReverb(fFilter).Enabled := aEnabled;
  end;
end;

function TDCFilterItem.GetEnabled: Boolean;
begin
  Result := False;
  if (fFilter = nil) then Exit;
  case fFilterType of
    ftAmplify:        Result := TDCAmplify(fFilter).Enabled;
    ftBandPass:       Result := TDCBandpass(fFilter).Enabled;
    ftChannelOrder:   Result := TDCChannelOrder(fFilter).Enabled;
    ftCompressor:     Result := TDCCompressor(fFilter).Enabled;
    ftDownMix:        Result := TDCDownMix(fFilter).Enabled;
    ftDynamicAmplify: Result := TDCDynamicAmplify(fFilter).Enabled;
    ftEchoDelay:      Result := TDCEchoDelay(fFilter).Enabled;
    ftEqualizer:      Result := TDCEqualizer(fFilter).Enabled;
    ftFlanger:        Result := TDCFlanger(fFilter).Enabled;
    ftHighPass:       Result := TDCHighpass(fFilter).Enabled;
    ftLowPass:        Result := TDCLowpass(fFilter).Enabled;
    ftNotch:          Result := TDCNotch(fFilter).Enabled;
    ftParametricEQ:   Result := TDCParametricEQ(fFilter).Enabled;
    ftPhaseInvert:    Result := TDCPhaseInvert(fFilter).Enabled;
    ftPhaser:         Result := TDCPhaser(fFilter).Enabled;
    ftPitchScale:     Result := TDCPitchScale(fFilter).Enabled;
    ftPitchShift:     Result := TDCPitchShift(fFilter).Enabled;
    ftSound3D:        Result := TDCSound3D(fFilter).Enabled;
    ftTempo:          Result := TDCTempo(fFilter).Enabled;
    ftTrebleEnhancer: Result := TDCTrebleEnhancer(fFilter).Enabled;
    ftTrueBass:       Result := TDCTrueBass(fFilter).Enabled;
    ftDMOChorus:      Result := TDCDMOChorus(fFilter).Enabled;
    ftDMOCompressor:  Result := TDCDMOCompressor(fFilter).Enabled;
    ftDMODistortion:  Result := TDCDMODistortion(fFilter).Enabled;
    ftDMOEcho:        Result := TDCDMOEcho(fFilter).Enabled;
    ftDMOFlanger:     Result := TDCDMOFlanger(fFilter).Enabled;
    ftDMOGargle:      Result := TDCDMOGargle(fFilter).Enabled;
    ftDMOI3DL2Reverb: Result := TDCDMOI3DL2Reverb(fFilter).Enabled;
    ftDMOParamEQ:     Result := TDCDMOParamEQ(fFilter).Enabled;
    ftDMOWavesReverb: Result := TDCDMOWavesReverb(fFilter).Enabled;
  end;
end;

procedure TDCFilterItem.FreeFilter;
begin
  if (fFilter <> nil) then
  begin
    fFilter.Free;
    fFilter := nil;
  end;
  if (fExtraBuffer <> nil) then
  begin
    FreeMem(fExtraBuffer);
    fExtraBuffer := nil;
  end;
  fFilterType := ftNone;
  fName := 'Error';
end;

procedure TDCFilterItem.SetFilterType(aFilterType: TDCFilterType);
begin
  FreeFilter;
  fFilterType := aFilterType;
  case fFilterType of
    ftAmplify:
    begin
      fFilter := TDCAmplify.Create(nil);
      fName := 'Amplify';
      fSettingsSize := (MaxChannels * SizeOf(Cardinal)) + (2 * SizeOf(Boolean));
      fExtraBuffer := AllocMem(8);
      ZeroMemory(fExtraBuffer,8);
      fSettingsSize := fSettingsSize + 8;
    end;
    ftBandPass:
    begin
      fFilter := TDCBandpass.Create(nil);
      fName := 'Bandpass';
      fSettingsSize := (MaxChannels * SizeOf(Single) * 2) + (2 * SizeOf(Boolean));
      fExtraBuffer := AllocMem(4);
      ZeroMemory(fExtraBuffer,4);
      fSettingsSize := fSettingsSize + 4;
    end;
    ftChannelOrder:
    begin
      fFilter := TDCChannelOrder.Create(nil);
      fName := 'Channel Order';
      fSettingsSize := (MaxChannels * SizeOf(Byte)) + (SizeOf(Boolean));
    end;
    ftCompressor:
    begin
      fFilter := TDCCompressor.Create(nil);
      fName := 'Compressor';
      fSettingsSize := 21;
    end;
    ftDownMix:
    begin
      fFilter := TDCDownMix.Create(nil);
      fName := 'Down Mix';
      fSettingsSize := 1;
    end;
    ftDynamicAmplify:
    begin
      fFilter := TDCDynamicAmplify.Create(nil);
      fName := 'Dynamic Amplify';
      fSettingsSize := 13;
    end;
    ftEchoDelay:
    begin
      fFilter := TDCEchoDelay.Create(nil);
      fName := 'Echo/Delay';
      fSettingsSize := 8;
    end;
    ftEqualizer:
    begin
      fFilter := TDCEqualizer.Create(nil);
      fName := 'Equalizer';
      TDCEqualizer(fFilter).FFTSize := fts2048;
      fExtraBuffer := AllocMem(364);
      ZeroMemory(fExtraBuffer,364);
      fSettingsSize := 366;
      FillChar(PEQSavings(fExtraBuffer).Preset,256,' ');
      CopyMemory(@PEQSavings(fExtraBuffer).Preset,PChar('(Default)'),Length('(Default)'));
    end;
    ftFlanger:
    begin
      fFilter := TDCFlanger.Create(nil);
      fName := 'Flanger';
      fSettingsSize := 92;
      fExtraBuffer := AllocMem(4);
      ZeroMemory(fExtraBuffer,4);
      fSettingsSize := fSettingsSize + 4;
    end;
    ftHighPass:
    begin
      fFilter := TDCHighpass.Create(nil);
      fName := 'High Pass';
      fSettingsSize := 42;
      fExtraBuffer := AllocMem(4);
      ZeroMemory(fExtraBuffer,4);
      fSettingsSize := fSettingsSize + 4;
    end;
    ftLowPass:
    begin
      fFilter := TDCLowpass.Create(nil);
      fName := 'Low Pass';
      fSettingsSize := 42;
      fExtraBuffer := AllocMem(4);
      ZeroMemory(fExtraBuffer,4);
      fSettingsSize := fSettingsSize + 4;
    end;
    ftNotch:
    begin
      fFilter := TDCNotch.Create(nil);
      fName := 'Notch';
      fSettingsSize := 42;
      fExtraBuffer := AllocMem(4);
      ZeroMemory(fExtraBuffer,4);
      fSettingsSize := fSettingsSize + 4;
    end;
    ftParametricEQ:
    begin
      fFilter := TDCParametricEQ.Create(nil);
      fName := 'Parametric EQ';
      fSettingsSize := 122;
      fExtraBuffer := AllocMem(4);
      ZeroMemory(fExtraBuffer,4);
      fSettingsSize := fSettingsSize + 4;
    end;
    ftPhaseInvert:
    begin
      fFilter := TDCPhaseInvert.Create(nil);
      TDCPhaseInvert(fFilter).Seperate := True;
      fName := 'Phase Invert';
      fSettingsSize := 11;
    end;
    ftPhaser:
    begin
      fFilter := TDCPhaser.Create(nil);
      fName := 'Phaser';
      fSettingsSize := 122;
      fExtraBuffer := AllocMem(4);
      ZeroMemory(fExtraBuffer,4);
      fSettingsSize := fSettingsSize + 4;
    end;
    ftPitchScale:
    begin
      fFilter := TDCPitchScale.Create(nil);
      fName := 'Pitch Scale';
      fSettingsSize := 32;
    end;
    ftPitchShift:
    begin
      fFilter := TDCPitchShift.Create(nil);
      fName := 'Pitch Shift';
      fSettingsSize := 5;
    end;
    ftSound3D:
    begin
      fFilter := TDCSound3D.Create(nil);
      fName := 'Sound 3D';
      fSettingsSize := 3;
    end;
    ftTempo:
    begin
      fFilter := TDCTempo.Create(nil);
      fName := 'Tempo';
      fSettingsSize := 5;
    end;
    ftTrebleEnhancer:
    begin
      fFilter := TDCTrebleEnhancer.Create(nil);
      fName := 'Treble Enhancer';
      fSettingsSize := 22;
      fExtraBuffer := AllocMem(4);
      ZeroMemory(fExtraBuffer,4);
      fSettingsSize := fSettingsSize + 4;
    end;
    ftTrueBass:
    begin
      fFilter := TDCTrueBass.Create(nil);
      fName := 'True Bass';
      fSettingsSize := 22;
      fExtraBuffer := AllocMem(4);
      ZeroMemory(fExtraBuffer,4);
      fSettingsSize := fSettingsSize + 4;
    end;
    ftDMOChorus:
    begin
      fFilter := TDCDMOChorus.Create(nil);
      fName := 'Chorus DMO';
      fSettingsSize := 29;
    end;
    ftDMOCompressor:
    begin
      fFilter := TDCDMOCompressor.Create(nil);
      fName := 'Compressor DMO';
      fSettingsSize := 25;
    end;
    ftDMODistortion:
    begin
      fFilter := TDCDMODistortion.Create(nil);
      fName := 'Distortion DMO';
      fSettingsSize := 21;
    end;
    ftDMOEcho:
    begin
      fFilter := TDCDMOEcho.Create(nil);
      fName := 'Echo DMO';
      fSettingsSize := 21;
    end;
    ftDMOFlanger:
    begin
      fFilter := TDCDMOFlanger.Create(nil);
      fName := 'Flanger DMO';
      fSettingsSize := 29;
    end;
    ftDMOGargle:
    begin
      fFilter := TDCDMOGargle.Create(nil);
      fName := 'Gargle DMO';
      fSettingsSize := 9;
    end;
    ftDMOI3DL2Reverb:
    begin
      fFilter := TDCDMOI3DL2Reverb.Create(nil);
      fName := 'I3DL2 Reverb DMO';
      fSettingsSize := 54;
      fExtraBuffer := AllocMem(4);
      ZeroMemory(fExtraBuffer,4);
      fSettingsSize := fSettingsSize + 4;
    end;
    ftDMOParamEQ:
    begin
      fFilter := TDCDMOParamEQ.Create(nil);
      fName := 'Param EQ DMO';
      fSettingsSize := 13;
    end;
    ftDMOWavesReverb:
    begin
      fFilter := TDCDMOWavesReverb.Create(nil);
      fName := 'Waves Reverb DMO';
      fSettingsSize := 17;
    end;
  end;
end;

procedure TDCFilterItem.Init(Stream: PDSStream);
begin
  if (fFilter = nil) then Exit;
  case fFilterType of
    ftEchoDelay:      TDCEchoDelay(fFilter).Init(Stream.Frequency,Stream.Bits,Stream.Channels,True);
    ftFlanger:        TDCFlanger(fFilter).Init(Stream.Channels,Stream.Frequency,0.1);
    ftTempo:          TDCTempo(fFilter).Init(Stream.Frequency,Stream.Bits,Stream.Channels);
    ftTrebleEnhancer: TDCTrebleEnhancer(fFilter).Init(Stream.Frequency);
    ftTrueBass:       TDCTrueBass(fFilter).Init(Stream.Frequency);
    ftDMOChorus:      TDCDMOChorus(fFilter).Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftDMOCompressor:  TDCDMOCompressor(fFilter).Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftDMODistortion:  TDCDMODistortion(fFilter).Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftDMOEcho:        TDCDMOEcho(fFilter).Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftDMOFlanger:     TDCDMOFlanger(fFilter).Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftDMOGargle:      TDCDMOGargle(fFilter).Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftDMOI3DL2Reverb: TDCDMOI3DL2Reverb(fFilter).Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftDMOParamEQ:     TDCDMOParamEQ(fFilter).Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftDMOWavesReverb: TDCDMOWavesReverb(fFilter).Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  end;
end;

function TDCFilterItem.Process(Buffer: Pointer; Size: integer; Stream: PDSStream): integer;
begin
  Result := Size;
  if (fFilter = nil) then Exit;
  case fFilterType of
    ftAmplify:        TDCAmplify(fFilter).Process(Buffer,Size,Stream.Bits,Stream.Channels,Stream.Float);
    ftBandPass:       TDCBandpass(fFilter).Process(Buffer,Size,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftChannelOrder:   TDCChannelOrder(fFilter).Process(Buffer,Size,Stream.Bits,Stream.Channels,Stream.Float);
    ftCompressor:     TDCCompressor(fFilter).Process(Buffer,Size,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftDownMix:        TDCDownMix(fFilter).Process(Buffer,Size,Stream.Bits,Stream.Channels,Stream.Float);
    ftDynamicAmplify: TDCDynamicAmplify(fFilter).Process(Buffer,Size,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftEchoDelay:      TDCEchoDelay(fFilter).Process(Buffer,Size,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftEqualizer:      TDCEqualizer(fFilter).Process(Buffer,Size,Stream.Bits,Stream.Channels,Stream.Float);
    ftFlanger:        TDCFlanger(fFilter).Process(Buffer,Size,Stream.Bits,Stream.Float);
    ftHighPass:       TDCHighpass(fFilter).Process(Buffer,Size,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftLowPass:        TDCLowpass(fFilter).Process(Buffer,Size,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftNotch:          TDCNotch(fFilter).Process(Buffer,Size,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftParametricEQ:   TDCParametricEQ(fFilter).Process(Buffer,Size,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftPhaseInvert:    TDCPhaseInvert(fFilter).Process(Buffer,Size,Stream.Bits,Stream.Channels,Stream.Float);
    ftPhaser:         TDCPhaser(fFilter).Process(Buffer,Size,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftPitchScale:     TDCPitchScale(fFilter).Process(Buffer,Size,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftPitchShift:     Result := TDCPitchShift(fFilter).Process(Buffer,Size,Stream.Bits,Stream.Channels,Stream.Float);
    ftSound3D:        TDCSound3D(fFilter).Process(Buffer,Size,Stream.Bits,Stream.Channels,Stream.Float);
    ftTempo:          Result := TDCTempo(fFilter).Process(Buffer,Size,Stream.Bits,Stream.Channels,Stream.Float);
    ftTrebleEnhancer: TDCTrebleEnhancer(fFilter).Process(Buffer,Size,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftTrueBass:       TDCTrueBass(fFilter).Process(Buffer,Size,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
    ftDMOChorus:      TDCDMOChorus(fFilter).Process(Buffer,Size);
    ftDMOCompressor:  TDCDMOCompressor(fFilter).Process(Buffer,Size);
    ftDMODistortion:  TDCDMODistortion(fFilter).Process(Buffer,Size);
    ftDMOEcho:        TDCDMOEcho(fFilter).Process(Buffer,Size);
    ftDMOFlanger:     TDCDMOFlanger(fFilter).Process(Buffer,Size);
    ftDMOGargle:      TDCDMOGargle(fFilter).Process(Buffer,Size);
    ftDMOI3DL2Reverb: TDCDMOI3DL2Reverb(fFilter).Process(Buffer,Size);
    ftDMOParamEQ:     TDCDMOParamEQ(fFilter).Process(Buffer,Size);
    ftDMOWavesReverb: TDCDMOWavesReverb(fFilter).Process(Buffer,Size);
  end;
end;

procedure TDCFilterItem.Flush;
begin
  if (fFilter = nil) then Exit;
  case fFilterType of
    ftBandPass:       TDCBandpass(fFilter).Flush;
    ftEchoDelay:      TDCEchoDelay(fFilter).Flush;
    ftEqualizer:      TDCEqualizer(fFilter).Flush;
    ftFlanger:        TDCFlanger(fFilter).Flush;
    ftHighPass:       TDCHighpass(fFilter).Flush;
    ftLowPass:        TDCLowpass(fFilter).Flush;
    ftNotch:          TDCNotch(fFilter).Flush;
    ftParametricEQ:   TDCParametricEQ(fFilter).Flush;
    ftPhaser:         TDCPhaser(fFilter).Flush;
    ftPitchScale:     TDCPitchScale(fFilter).Flush;
    ftTempo:          TDCTempo(fFilter).Flush;
    ftTrebleEnhancer: TDCTrebleEnhancer(fFilter).Flush;
    ftTrueBass:       TDCTrueBass(fFilter).Flush;
    ftDMOChorus:      TDCDMOChorus(fFilter).Flush;
    ftDMOCompressor:  TDCDMOCompressor(fFilter).Flush;
    ftDMODistortion:  TDCDMODistortion(fFilter).Flush;
    ftDMOEcho:        TDCDMOEcho(fFilter).Flush;
    ftDMOFlanger:     TDCDMOFlanger(fFilter).Flush;
    ftDMOGargle:      TDCDMOGargle(fFilter).Flush;
    ftDMOI3DL2Reverb: TDCDMOI3DL2Reverb(fFilter).Flush;
    ftDMOParamEQ:     TDCDMOParamEQ(fFilter).Flush;
    ftDMOWavesReverb: TDCDMOWavesReverb(fFilter).Flush;
  end;
end;

procedure TDCFilterItem.SaveSettings(var Buffer: PChar);
var
  aCardinal: array[0..MaxChannels] of Cardinal;
  aInteger: array[0..MaxChannels] of Integer;
  aWORD: array[0..MaxChannels] of WORD;
  aSingle: array[0..MaxChannels] of Single;
  aByte: array[0..MaxChannels] of Byte;
  i: integer;
begin
  case fFilterType of
    ftAmplify:
    begin
      Buffer[0] := Char(TDCAmplify(fFilter).Enabled);
      Buffer[1] := Char(TDCAmplify(fFilter).Seperate);
      for i := 0 to MaxChannels -1 do aCardinal[i] := TDCAmplify(fFilter).Volume[i];
      CopyMemory(@Buffer[2], @aCardinal, MaxChannels * SizeOf(Cardinal));
      CopyMemory(@Buffer[(MaxChannels * SizeOf(Cardinal)) + (2 * SizeOf(Boolean))],fExtraBuffer,8);
    end;
    ftBandPass:
    begin
      Buffer[0] := Char(TDCBandPass(fFilter).Enabled);
      Buffer[1] := Char(TDCBandPass(fFilter).Seperate);
      for i := 0 to MaxChannels -1 do aSingle[i] := TDCBandPass(fFilter).CutoffLow[i];
      CopyMemory(@Buffer[2], @aSingle, MaxChannels * SizeOf(Single));
      for i := 0 to MaxChannels -1 do aSingle[i] := TDCBandPass(fFilter).CutoffHigh[i];
      CopyMemory(@Buffer[MaxChannels * SizeOf(Single) + 2], @aSingle, MaxChannels * SizeOf(Single));
      CopyMemory(@Buffer[MaxChannels * SizeOf(Single) * 2 + 2], fExtraBuffer,4);
    end;
    ftChannelOrder:
    begin
      Buffer[0] := Char(TDCChannelOrder(fFilter).Enabled);
      for i := 0 to MaxChannels -1 do aByte[i] := TDCChannelOrder(fFilter).Order[i];
      CopyMemory(@Buffer[1], @aByte, MaxChannels * SizeOf(Byte));
    end;
    ftCompressor:
    begin
      Buffer[0] := Char(TDCCompressor(fFilter).Enabled);
      aSingle[0] := TDCCompressor(fFilter).AttackTime;
      aSingle[1] := TDCCompressor(fFilter).DecayTime;
      aSingle[2] := TDCCompressor(fFilter).ThresholdDB;
      aSingle[3] := TDCCompressor(fFilter).Ratio;
      aSingle[4] := TDCCompressor(fFilter).GainDB;
      CopyMemory(@Buffer[1], @aSingle, 20);
    end;
    ftDownMix:
    begin
      Buffer[0] := Char(TDCDownMix(fFilter).Enabled);
    end;
    ftDynamicAmplify:
    begin
      Buffer[0] := Char(TDCDynamicAmplify(fFilter).Enabled);
      aCardinal[0] := TDCDynamicAmplify(fFilter).AttackTime;
      aCardinal[1] := TDCDynamicAmplify(fFilter).ReleaseTime;
      aCardinal[2] := TDCDynamicAmplify(fFilter).MaxAmplification;
      CopyMemory(@Buffer[1], @aCardinal, 12);
    end;
    ftEchoDelay:
    begin
      Buffer[0] := Char(TDCEchoDelay(fFilter).Enabled);
      Buffer[1] := Char(TDCEchoDelay(fFilter).KillMain);
      Buffer[2] := Char(TDCEchoDelay(fFilter).NumDelays);
      Buffer[3] := Char(TDCEchoDelay(fFilter).Highpass);
      aWORD[0] := TDCEchoDelay(fFilter).DelayAmp;
      aWORD[1] := TDCEchoDelay(fFilter).Delay;
      CopyMemory(@Buffer[4], @aWORD, 4);
    end;
    ftEqualizer:
    begin
      Buffer[0] := Char(TDCEqualizer(fFilter).Enabled);
      Buffer[1] := Char(TDCEqualizer(fFilter).Seperate);
      CopyMemory(@Buffer[2], fExtraBuffer,364);
    end;
    ftFlanger:
    begin
      Buffer[0] := Char(TDCFlanger(fFilter).Enabled);
      Buffer[1] := Char(TDCFlanger(fFilter).Seperate);
      for i := 0 to MaxChannels -1 do aSingle[i] := TDCFlanger(fFilter).Frequency[i];
      CopyMemory(@Buffer[2], @aSingle, MaxChannels * SizeOf(Single));
      for i := 0 to MaxChannels -1 do aSingle[i] := TDCFlanger(fFilter).Delay[i];
      CopyMemory(@Buffer[42], @aSingle, MaxChannels * SizeOf(Single));
      for i := 0 to MaxChannels -1 do Buffer[82 + i] := Char(TDCFlanger(fFilter).PhaseInvert[i]);
      CopyMemory(@Buffer[92], fExtraBuffer,4);
    end;
    ftHighPass:
    begin
      Buffer[0] := Char(TDCHighPass(fFilter).Enabled);
      Buffer[1] := Char(TDCHighPass(fFilter).Seperate);
      for i := 0 to MaxChannels -1 do aCardinal[i] := TDCHighPass(fFilter).Cutoff[i];
      CopyMemory(@Buffer[2],@aCardinal,40);
      CopyMemory(@Buffer[42], fExtraBuffer,4);
    end;
    ftLowPass:
    begin
      Buffer[0] := Char(TDCLowpass(fFilter).Enabled);
      Buffer[1] := Char(TDCLowpass(fFilter).Seperate);
      for i := 0 to MaxChannels -1 do aCardinal[i] := TDCLowpass(fFilter).Cutoff[i];
      CopyMemory(@Buffer[2],@aCardinal,40);
      CopyMemory(@Buffer[42], fExtraBuffer,4);
    end;
    ftNotch:
    begin
      Buffer[0] := Char(TDCNotch(fFilter).Enabled);
      Buffer[1] := Char(TDCNotch(fFilter).Seperate);
      for i := 0 to MaxChannels -1 do aCardinal[i] := TDCNotch(fFilter).Cutoff[i];
      CopyMemory(@Buffer[2],@aCardinal,40);
      CopyMemory(@Buffer[42], fExtraBuffer,4);
    end;
    ftParametricEQ:
    begin
      Buffer[0] := Char(TDCParametricEQ(fFilter).Enabled);
      Buffer[1] := Char(TDCParametricEQ(fFilter).Seperate);
      for i := 0 to MaxChannels -1 do aSingle[i] := TDCParametricEQ(fFilter).Frequency[i];
      CopyMemory(@Buffer[2],@aSingle,40);
      for i := 0 to MaxChannels -1 do aSingle[i] := TDCParametricEQ(fFilter).GainDB[i];
      CopyMemory(@Buffer[42],@aSingle,40);
      for i := 0 to MaxChannels -1 do aSingle[i] := TDCParametricEQ(fFilter).Q[i];
      CopyMemory(@Buffer[82],@aSingle,40);
      CopyMemory(@Buffer[122], fExtraBuffer,4);
    end;
    ftPhaseInvert:
    begin
      Buffer[0] := Char(TDCPhaseInvert(fFilter).Enabled);
      for i := 0 to MaxChannels -1 do Buffer[i+1] := Char(TDCPhaseInvert(fFilter).Invert[i]);
    end;
    ftPhaser:
    begin
      Buffer[0] := Char(TDCPhaser(fFilter).Enabled);
      Buffer[1] := Char(TDCPhaser(fFilter).Seperate);
      for i := 0 to MaxChannels -1 do
      begin
        Buffer[i+MaxChannels*0+2] := Char(TDCPhaser(fFilter).DryWetRatio[i]);
        Buffer[i+MaxChannels*1+2] := Char(TDCPhaser(fFilter).Feedback[i]);
        Buffer[i+MaxChannels*2+2] := Char(TDCPhaser(fFilter).Stages[i]);
        Buffer[i+MaxChannels*3+2] := Char(TDCPhaser(fFilter).Depth[i]);
      end;
      for i := 0 to MaxChannels -1 do aSingle[i] := TDCPhaser(fFilter).StartPhase[i];
      CopyMemory(@Buffer[42], @aSingle, MaxChannels * SizeOf(Single));
      for i := 0 to MaxChannels -1 do aSingle[i] := TDCPhaser(fFilter).Frequency[i];
      CopyMemory(@Buffer[82], @aSingle, MaxChannels * SizeOf(Single));
      CopyMemory(@Buffer[122], fExtraBuffer,4);
    end;
    ftPitchScale:
    begin
      Buffer[0] := Char(TDCPitchScale(fFilter).Enabled);
      Buffer[1] := Char(TDCPitchScale(fFilter).FFTSize);
      for i := 0 to MaxChannels -1 do Buffer[i+2] := Char(TDCPitchScale(fFilter).Quality[i]);
      for i := 0 to MaxChannels -1 do aWORD[i] := TDCPitchScale(fFilter).Pitch[i];
      CopyMemory(@Buffer[12],@aWORD,20);
    end;
    ftPitchShift:
    begin
      Buffer[0] := Char(TDCPitchShift(fFilter).Enabled);
      aCardinal[0] := TDCPitchShift(fFilter).Pitch;
      CopyMemory(@Buffer[1],@aCardinal,4);
    end;
    ftSound3D:
    begin
      Buffer[0] := Char(TDCSound3D(fFilter).Enabled);
      aWORD[0] := TDCSound3D(fFilter).Volume;
      CopyMemory(@Buffer[1],@aWORD,2);
    end;
    ftTempo:
    begin
      Buffer[0] := Char(TDCTempo(fFilter).Enabled);
      ainteger[0] := TDCTempo(fFilter).Tempo;
      CopyMemory(@Buffer[1],@aInteger,4);
    end;
    ftTrebleEnhancer:
    begin
      Buffer[0] := Char(TDCTrebleEnhancer(fFilter).Enabled);
      Buffer[1] := Char(TDCTrebleEnhancer(fFilter).Seperate);
      for i := 0 to MaxChannels -1 do aWORD[i] := TDCTrebleEnhancer(fFilter).Volume[i];
      CopyMemory(@Buffer[2], @aWORD,20);
      CopyMemory(@Buffer[22], fExtraBuffer,4);
    end;
    ftTrueBass:
    begin
      Buffer[0] := Char(TDCTrueBass(fFilter).Enabled);
      Buffer[1] := Char(TDCTrueBass(fFilter).Seperate);
      for i := 0 to MaxChannels -1 do aWORD[i] := TDCTrueBass(fFilter).Volume[i];
      CopyMemory(@Buffer[2], @aWORD,20);
      CopyMemory(@Buffer[22], fExtraBuffer,4);
    end;
    ftDMOChorus:
    begin
      Buffer[0] := Char(TDCDMOChorus(fFilter).Enabled);
      aSingle[0] := TDCDMOChorus(fFilter).WetDryMix;
      aSingle[1] := TDCDMOChorus(fFilter).Depth;
      aSingle[2] := TDCDMOChorus(fFilter).Feedback;
      aSingle[3] := TDCDMOChorus(fFilter).Frequency;
      aSingle[4] := TDCDMOChorus(fFilter).Delay;
      CopyMemory(@Buffer[1], @aSingle,20);
      aInteger[0] := TDCDMOChorus(fFilter).Waveform;
      aInteger[1] := TDCDMOChorus(fFilter).Phase;
      CopyMemory(@Buffer[21], @aInteger,8);
    end;
    ftDMOCompressor:
    begin
      Buffer[0] := Char(TDCDMOCompressor(fFilter).Enabled);
      aSingle[0] := TDCDMOCompressor(fFilter).Gain;
      aSingle[1] := TDCDMOCompressor(fFilter).Attack;
      aSingle[2] := TDCDMOCompressor(fFilter).Release;
      aSingle[3] := TDCDMOCompressor(fFilter).Threshold;
      aSingle[4] := TDCDMOCompressor(fFilter).Ratio;
      aSingle[5] := TDCDMOCompressor(fFilter).PreDelay;
      CopyMemory(@Buffer[1], @aSingle,24);
    end;
    ftDMODistortion:
    begin
      Buffer[0] := Char(TDCDMODistortion(fFilter).Enabled);
      aSingle[0] := TDCDMODistortion(fFilter).Gain;
      aSingle[1] := TDCDMODistortion(fFilter).Edge;
      aSingle[2] := TDCDMODistortion(fFilter).PostEQCenterFrequency;
      aSingle[3] := TDCDMODistortion(fFilter).PostEQBandwidth;
      aSingle[4] := TDCDMODistortion(fFilter).PreLowpassCutoff;
      CopyMemory(@Buffer[1], @aSingle,20);
    end;
    ftDMOEcho:
    begin
      Buffer[0] := Char(TDCDMOEcho(fFilter).Enabled);
      aSingle[0] := TDCDMOEcho(fFilter).WetDryMix;
      aSingle[1] := TDCDMOEcho(fFilter).Feedback;
      aSingle[2] := TDCDMOEcho(fFilter).LeftDelay;
      aSingle[3] := TDCDMOEcho(fFilter).RightDelay;
      aInteger[4] := TDCDMOEcho(fFilter).PanDelay;
      CopyMemory(@Buffer[1], @aSingle,16);
      CopyMemory(@Buffer[17], @aInteger,4);
    end;
    ftDMOFlanger:
    begin
      Buffer[0] := Char(TDCDMOFlanger(fFilter).Enabled);
      aSingle[0] := TDCDMOFlanger(fFilter).WetDryMix;
      aSingle[1] := TDCDMOFlanger(fFilter).Depth;
      aSingle[2] := TDCDMOFlanger(fFilter).Feedback;
      aSingle[3] := TDCDMOFlanger(fFilter).Frequency;
      aSingle[4] := TDCDMOFlanger(fFilter).Delay;
      CopyMemory(@Buffer[1], @aSingle,20);
      aInteger[0] := TDCDMOFlanger(fFilter).Waveform;
      aInteger[1] := TDCDMOFlanger(fFilter).Phase;
      CopyMemory(@Buffer[21], @aInteger,8);
    end;
    ftDMOGargle:
    begin
      Buffer[0] := Char(TDCDMOGargle(fFilter).Enabled);
      aCardinal[0] := TDCDMOGargle(fFilter).RateHz;
      aCardinal[1] := TDCDMOGargle(fFilter).WaveShape;
      CopyMemory(@Buffer[1], @aCardinal,8);
    end;
    ftDMOI3DL2Reverb:
    begin
      Buffer[0] := Char(TDCDMOI3DL2Reverb(fFilter).Enabled);
      Buffer[1] := Char(TDCDMOI3DL2Reverb(fFilter).Preset);
      aInteger[0] := TDCDMOI3DL2Reverb(fFilter).Room;
      aInteger[1] := TDCDMOI3DL2Reverb(fFilter).RoomHF;
      aInteger[2] := TDCDMOI3DL2Reverb(fFilter).Reflections;
      aInteger[3] := TDCDMOI3DL2Reverb(fFilter).Reverb;
      aInteger[4] := TDCDMOI3DL2Reverb(fFilter).Quality;
      CopyMemory(@Buffer[2], @aInteger,20);
      aSingle[0] := TDCDMOI3DL2Reverb(fFilter).RoomRolloffFactor;
      aSingle[1] := TDCDMOI3DL2Reverb(fFilter).DecayTime;
      aSingle[2] := TDCDMOI3DL2Reverb(fFilter).DecayHFRatio;
      aSingle[3] := TDCDMOI3DL2Reverb(fFilter).ReflectionsDelay;
      aSingle[4] := TDCDMOI3DL2Reverb(fFilter).ReverbDelay;
      aSingle[5] := TDCDMOI3DL2Reverb(fFilter).Diffusion;
      aSingle[6] := TDCDMOI3DL2Reverb(fFilter).Density;
      aSingle[7] := TDCDMOI3DL2Reverb(fFilter).HFReference;
      CopyMemory(@Buffer[22], @aSingle,32);
      CopyMemory(@Buffer[54], fExtraBuffer,4);
    end;
    ftDMOParamEQ:
    begin
      Buffer[0] := Char(TDCDMOParamEQ(fFilter).Enabled);
      aSingle[0] := TDCDMOParamEQ(fFilter).Center;
      aSingle[1] := TDCDMOParamEQ(fFilter).Bandwidth;
      aSingle[2] := TDCDMOParamEQ(fFilter).Gain;
      CopyMemory(@Buffer[1], @aSingle,12);
    end;
    ftDMOWavesReverb:
    begin
      Buffer[0] := Char(TDCDMOWavesReverb(fFilter).Enabled);
      aSingle[0] := TDCDMOWavesReverb(fFilter).InGain;
      aSingle[1] := TDCDMOWavesReverb(fFilter).ReverbMix;
      aSingle[2] := TDCDMOWavesReverb(fFilter).ReverbTime;
      aSingle[3] := TDCDMOWavesReverb(fFilter).HighFreqRTRatio;
      CopyMemory(@Buffer[1], @aSingle,16);
    end;
  end;
end;

procedure TDCFilterItem.LoadSettings(Buffer: PChar);
var
  aCardinal: array[0..MaxChannels] of Cardinal;
  aWORD: array[0..MaxChannels] of WORD;
  aInteger: array[0..MaxChannels] of Integer;
  aSingle: array[0..MaxChannels] of Single;
  aByte: array[0..MaxChannels] of Byte;
  i: integer;
  c: integer;
  z: integer;
  EQ: PEQSavings;
begin
  case fFilterType of
    ftAmplify:
    begin
      TDCAmplify(fFilter).Enabled := Boolean(Buffer[0]);
      TDCAmplify(fFilter).Seperate := Boolean(Buffer[1]);
      CopyMemory(@aCardinal, @Buffer[2], MaxChannels * SizeOf(Cardinal));
      for i := 0 to MaxChannels -1 do TDCAmplify(fFilter).Volume[i] := aCardinal[i];
      CopyMemory(fExtraBuffer,@Buffer[(MaxChannels * SizeOf(Cardinal)) + (2 * SizeOf(Boolean))],8);
    end;
    ftBandPass:
    begin
      TDCBandPass(fFilter).Enabled := Boolean(Buffer[0]);
      TDCBandPass(fFilter).Seperate := Boolean(Buffer[1]);
      CopyMemory(@aSingle, @Buffer[2], MaxChannels * SizeOf(Single));
      for i := 0 to MaxChannels -1 do TDCBandPass(fFilter).CutoffLow[i] := aSingle[i];
      CopyMemory(@aSingle, @Buffer[MaxChannels * SizeOf(Single) + 2], MaxChannels * SizeOf(Single));
      for i := 0 to MaxChannels -1 do TDCBandPass(fFilter).CutoffHigh[i] := aSingle[i];
      CopyMemory(fExtraBuffer, @Buffer[MaxChannels * SizeOf(Single) * 2 + 2], 4);
    end;
    ftChannelOrder:
    begin
      TDCChannelOrder(fFilter).Enabled := Boolean(Buffer[0]);
      CopyMemory(@aByte, @Buffer[1], MaxChannels * SizeOf(Byte));
      for i := 0 to MaxChannels -1 do TDCChannelOrder(fFilter).Order[i] := aByte[i];
    end;
    ftCompressor:
    begin
      CopyMemory(@aSingle,@Buffer[1],21);
      TDCCompressor(fFilter).Enabled := Boolean(Buffer[0]);
      TDCCompressor(fFilter).AttackTime := aSingle[0];
      TDCCompressor(fFilter).DecayTime := aSingle[1];
      TDCCompressor(fFilter).ThresholdDB := aSingle[2];
      TDCCompressor(fFilter).Ratio := aSingle[3];
      TDCCompressor(fFilter).GainDB := aSingle[4];
    end;
    ftDownMix:
    begin
      TDCDownMix(fFilter).Enabled := Boolean(Buffer[0]);
    end;
    ftDynamicAmplify:
    begin
      TDCDynamicAmplify(fFilter).Enabled := Boolean(Buffer[0]);
      CopyMemory(@aCardinal,@Buffer[1],12);
      TDCDynamicAmplify(fFilter).AttackTime := aCardinal[0];
      TDCDynamicAmplify(fFilter).ReleaseTime := aCardinal[1];
      TDCDynamicAmplify(fFilter).MaxAmplification := aCardinal[2];
    end;
    ftEchoDelay:
    begin
      TDCEchoDelay(fFilter).Enabled := Boolean(Buffer[0]);
      TDCEchoDelay(fFilter).KillMain := Boolean(Buffer[1]);
      TDCEchoDelay(fFilter).NumDelays := Byte(Buffer[2]);
      TDCEchoDelay(fFilter).Highpass := Boolean(Buffer[3]);
      CopyMemory(@aWORD,@Buffer[4],4);
      TDCEchoDelay(fFilter).DelayAmp := aWORD[0];
      TDCEchoDelay(fFilter).Delay := aWORD[1];
    end;
    ftEqualizer:
    begin
      TDCEqualizer(fFilter).Enabled := Boolean(Buffer[0]);
      TDCEqualizer(fFilter).Seperate := Boolean(Buffer[1]);
      CopyMemory(fExtraBuffer,@Buffer[2],364);
      EQ := PEQSavings(fExtraBuffer);
      for i := 0 to MaxChannels -1 do
        for c := 0 to NumEQBands -1 do
          for z := EQFreq[c] to EQFreq[c+1] -1 do
            TDCEqualizer(fFilter).Band[i,z] := EQ.Band[i,c];
    end;
    ftFlanger:
    begin
      TDCFlanger(fFilter).Enabled := Boolean(Buffer[0]);
      TDCFlanger(fFilter).Seperate := Boolean(Buffer[1]);
      CopyMemory(@aSingle,@Buffer[2],MaxChannels * SizeOf(Single));
      for i := 0 to MaxChannels -1 do TDCFlanger(fFilter).Frequency[i] := aSingle[i];
      CopyMemory(@aSingle,@Buffer[42],MaxChannels * SizeOf(Single));
      for i := 0 to MaxChannels -1 do TDCFlanger(fFilter).Delay[i] := aSingle[i];
      for i := 0 to MaxChannels -1 do TDCFlanger(fFilter).PhaseInvert[i] := Boolean(Buffer[82 + i]);
      CopyMemory(fExtraBuffer,@Buffer[92],4);
    end;
    ftHighPass:
    begin
      TDCHighPass(fFilter).Enabled := Boolean(Buffer[0]);
      TDCHighPass(fFilter).Seperate := Boolean(Buffer[1]);
      CopyMemory(@aCardinal,@Buffer[2],40);
      for i := 0 to MaxChannels -1 do TDCHighPass(fFilter).Cutoff[i] := aCardinal[i];
      CopyMemory(fExtraBuffer,@Buffer[42],4);
    end;
    ftLowPass:
    begin
      TDCLowpass(fFilter).Enabled := Boolean(Buffer[0]);
      TDCLowpass(fFilter).Seperate := Boolean(Buffer[1]);
      CopyMemory(@aCardinal,@Buffer[2],40);
      for i := 0 to MaxChannels -1 do TDCLowpass(fFilter).Cutoff[i] := aCardinal[i];
      CopyMemory(fExtraBuffer,@Buffer[42],4);
    end;
    ftNotch:
    begin
      TDCNotch(fFilter).Enabled := Boolean(Buffer[0]);
      TDCNotch(fFilter).Seperate := Boolean(Buffer[1]);
      CopyMemory(@aCardinal,@Buffer[2],40);
      for i := 0 to MaxChannels -1 do TDCNotch(fFilter).Cutoff[i] := aCardinal[i];
      CopyMemory(fExtraBuffer,@Buffer[42],4);
    end;
    ftParametricEQ:
    begin
      TDCParametricEQ(fFilter).Enabled := Boolean(Buffer[0]);
      TDCParametricEQ(fFilter).Seperate := Boolean(Buffer[1]);
      CopyMemory(@aSingle,@Buffer[2],40);
      for i := 0 to MaxChannels -1 do TDCParametricEQ(fFilter).Frequency[i] := aSingle[i];
      CopyMemory(@aSingle,@Buffer[42],40);
      for i := 0 to MaxChannels -1 do TDCParametricEQ(fFilter).GainDB[i] := aSingle[i];
      CopyMemory(@aSingle,@Buffer[82],40);
      for i := 0 to MaxChannels -1 do TDCParametricEQ(fFilter).Q[i] := aSingle[i];
      CopyMemory(fExtraBuffer,@Buffer[122],4);
    end;
    ftPhaseInvert:
    begin
      TDCPhaseInvert(fFilter).Enabled := Boolean(Buffer[0]);
      for i := 0 to MaxChannels -1 do TDCPhaseInvert(fFilter).Invert[i] := Boolean(Buffer[i+1]);
    end;
    ftPhaser:
    begin
      TDCPhaser(fFilter).Enabled := Boolean(Buffer[0]);
      TDCPhaser(fFilter).Seperate := Boolean(Buffer[1]);
      for i := 0 to MaxChannels -1 do
      begin
        TDCPhaser(fFilter).DryWetRatio[i] := Byte(Buffer[i+MaxChannels*0+2]);
        TDCPhaser(fFilter).Feedback[i] := Byte(Buffer[i+MaxChannels*1+2]);
        TDCPhaser(fFilter).Stages[i] := Byte(Buffer[i+MaxChannels*2+2]);
        TDCPhaser(fFilter).Depth[i] := Byte(Buffer[i+MaxChannels*3+2]);
      end;
      CopyMemory(@aSingle,@Buffer[42],MaxChannels * SizeOf(Single));
      for i := 0 to MaxChannels -1 do TDCPhaser(fFilter).StartPhase[i] := aSingle[i];
      CopyMemory(@aSingle,@Buffer[82], MaxChannels * SizeOf(Single));
      for i := 0 to MaxChannels -1 do TDCPhaser(fFilter).Frequency[i] := aSingle[i];
      CopyMemory(fExtraBuffer,@Buffer[122],4);
    end;
    ftPitchScale:
    begin
      TDCPitchScale(fFilter).Enabled := Boolean(Buffer[0]);
      TDCPitchScale(fFilter).FFTSize := TDCFFTSize(Buffer[1]);
      for i := 0 to MaxChannels -1 do TDCPitchScale(fFilter).Quality[i] := Byte(Buffer[i+2]);
      CopyMemory(@aWORD,@Buffer[12],20);
      for i := 0 to MaxChannels -1 do TDCPitchScale(fFilter).Pitch[i] := aWORD[i];
    end;
    ftPitchShift:
    begin
      TDCPitchShift(fFilter).Enabled := Boolean(Buffer[0]);
      CopyMemory(@aCardinal,@Buffer[1],4);
      TDCPitchShift(fFilter).Pitch := aCardinal[0];
    end;
    ftSound3D:
    begin
      TDCSound3D(fFilter).Enabled := Boolean(Buffer[0]);
      CopyMemory(@aWORD,@Buffer[1],2);
      TDCSound3D(fFilter).Volume := aWORD[0];
    end;
    ftTempo:
    begin
      TDCTempo(fFilter).Enabled := Boolean(Buffer[0]);
      CopyMemory(@aInteger,@Buffer[1],4);
      TDCTempo(fFilter).Tempo := ainteger[0];
    end;
    ftTrebleEnhancer:
    begin
      TDCTrebleEnhancer(fFilter).Enabled := Boolean(Buffer[0]);
      TDCTrebleEnhancer(fFilter).Seperate := Boolean(Buffer[1]);
      CopyMemory(@aWORD,@Buffer[2],20);
      for i := 0 to MaxChannels -1 do TDCTrebleEnhancer(fFilter).Volume[i] := aWORD[i];
      CopyMemory(fExtraBuffer,@Buffer[22],4);
    end;
    ftTrueBass:
    begin
      TDCTrueBass(fFilter).Enabled := Boolean(Buffer[0]);
      TDCTrueBass(fFilter).Seperate := Boolean(Buffer[1]);
      CopyMemory(@aWORD,@Buffer[2],20);
      for i := 0 to MaxChannels -1 do TDCTrueBass(fFilter).Volume[i] := aWORD[i];
      CopyMemory(fExtraBuffer,@Buffer[22],4);
    end;
    ftDMOChorus:
    begin
      TDCDMOChorus(fFilter).Enabled := Boolean(Buffer[0]);
      CopyMemory(@aSingle,@Buffer[1],20);
      TDCDMOChorus(fFilter).WetDryMix := aSingle[0];
      TDCDMOChorus(fFilter).Depth := aSingle[1];
      TDCDMOChorus(fFilter).Feedback := aSingle[2];
      TDCDMOChorus(fFilter).Frequency := aSingle[3];
      TDCDMOChorus(fFilter).Delay := aSingle[4];
      CopyMemory(@aInteger,@Buffer[21],8);
      TDCDMOChorus(fFilter).Waveform := aInteger[0];
      TDCDMOChorus(fFilter).Phase := aInteger[1];
    end;
    ftDMOCompressor:
    begin
      TDCDMOCompressor(fFilter).Enabled := Boolean(Buffer[0]);
      CopyMemory(@aSingle,@Buffer[1],24);
      TDCDMOCompressor(fFilter).Gain := aSingle[0];
      TDCDMOCompressor(fFilter).Attack := aSingle[1];
      TDCDMOCompressor(fFilter).Release := aSingle[2];
      TDCDMOCompressor(fFilter).Threshold := aSingle[3];
      TDCDMOCompressor(fFilter).Ratio := aSingle[4];
      TDCDMOCompressor(fFilter).PreDelay := aSingle[5];
    end;
    ftDMODistortion:
    begin
      TDCDMODistortion(fFilter).Enabled := Boolean(Buffer[0]);
      CopyMemory(@aSingle,@Buffer[1],20);
      TDCDMODistortion(fFilter).Gain := aSingle[0];
      TDCDMODistortion(fFilter).Edge := aSingle[1];
      TDCDMODistortion(fFilter).PostEQCenterFrequency := aSingle[2];
      TDCDMODistortion(fFilter).PostEQBandwidth := aSingle[3];
      TDCDMODistortion(fFilter).PreLowpassCutoff := aSingle[4];
    end;
    ftDMOEcho:
    begin
      TDCDMOEcho(fFilter).Enabled := Boolean(Buffer[0]);
      CopyMemory(@aSingle,@Buffer[1],16);
      CopyMemory(@aInteger,@Buffer[17],4);
      TDCDMOEcho(fFilter).WetDryMix := aSingle[0];
      TDCDMOEcho(fFilter).Feedback := aSingle[1];
      TDCDMOEcho(fFilter).LeftDelay := aSingle[1];
      TDCDMOEcho(fFilter).RightDelay := aSingle[2];
      TDCDMOEcho(fFilter).PanDelay := aInteger[4];
    end;
    ftDMOFlanger:
    begin
      TDCDMOFlanger(fFilter).Enabled := Boolean(Buffer[0]);
      CopyMemory(@aSingle,@Buffer[1],20);
      TDCDMOFlanger(fFilter).WetDryMix := aSingle[0];
      TDCDMOFlanger(fFilter).Depth := aSingle[1];
      TDCDMOFlanger(fFilter).Feedback := aSingle[2];
      TDCDMOFlanger(fFilter).Frequency := aSingle[3];
      TDCDMOFlanger(fFilter).Delay := aSingle[4];
      CopyMemory(@aInteger,@Buffer[21],8);
      TDCDMOFlanger(fFilter).Waveform := aInteger[0];
      TDCDMOFlanger(fFilter).Phase := aInteger[1];
    end;
    ftDMOGargle:
    begin
      TDCDMOGargle(fFilter).Enabled := Boolean(Buffer[0]);
      CopyMemory(@aCardinal,@Buffer[1],8);
      TDCDMOGargle(fFilter).RateHz := aCardinal[0];
      TDCDMOGargle(fFilter).WaveShape := aCardinal[1];
    end;
    ftDMOI3DL2Reverb:
    begin
      TDCDMOI3DL2Reverb(fFilter).Enabled := Boolean(Buffer[0]);
      TDCDMOI3DL2Reverb(fFilter).Preset := TDCDMOI3DL2ReverbPreset(Buffer[1]);
      CopyMemory(@aInteger,@Buffer[2],20);
      TDCDMOI3DL2Reverb(fFilter).Room := aInteger[0];
      TDCDMOI3DL2Reverb(fFilter).RoomHF := aInteger[1];
      TDCDMOI3DL2Reverb(fFilter).Reflections := aInteger[2];
      TDCDMOI3DL2Reverb(fFilter).Reverb := aInteger[3];
      TDCDMOI3DL2Reverb(fFilter).Quality := aInteger[4];
      CopyMemory(@aSingle,@Buffer[22],32);
      TDCDMOI3DL2Reverb(fFilter).RoomRolloffFactor := aSingle[0];
      TDCDMOI3DL2Reverb(fFilter).DecayTime := aSingle[1];
      TDCDMOI3DL2Reverb(fFilter).DecayHFRatio := aSingle[2];
      TDCDMOI3DL2Reverb(fFilter).ReflectionsDelay := aSingle[3];
      TDCDMOI3DL2Reverb(fFilter).ReverbDelay := aSingle[4];
      TDCDMOI3DL2Reverb(fFilter).Diffusion := aSingle[5];
      TDCDMOI3DL2Reverb(fFilter).Density := aSingle[6];
      TDCDMOI3DL2Reverb(fFilter).HFReference := aSingle[7];
      CopyMemory(fExtraBuffer,@Buffer[54],4);
    end;
    ftDMOParamEQ:
    begin
      TDCDMOParamEQ(fFilter).Enabled := Boolean(Buffer[0]);
      CopyMemory(@aSingle,@Buffer[1],12);
      TDCDMOParamEQ(fFilter).Center := aSingle[0];
      TDCDMOParamEQ(fFilter).Bandwidth := aSingle[1];
      TDCDMOParamEQ(fFilter).Gain := aSingle[2];
    end;
    ftDMOWavesReverb:
    begin
      TDCDMOWavesReverb(fFilter).Enabled := Boolean(Buffer[0]);
      CopyMemory(@aSingle,@Buffer[1],16);
      TDCDMOWavesReverb(fFilter).InGain := aSingle[0];
      TDCDMOWavesReverb(fFilter).ReverbMix := aSingle[1];
      TDCDMOWavesReverb(fFilter).ReverbTime := aSingle[2];
      TDCDMOWavesReverb(fFilter).HighFreqRTRatio := aSingle[3];
    end;
  end;
end;

function TDCFilterItem.ExtraBuffer: PChar;
begin
  Result := fExtraBuffer;
end;
{*******************************************************************************}
constructor TDCFilterList.Create;
begin
  inherited Create(TDCFilterItem);
  fLock := TCriticalSection.Create;
end;

destructor TDCFilterList.Destroy;
begin
  Clear;
  if fSaveBuffer <> nil then FreeMem(fSaveBuffer);
  fLock.Free;
  inherited Destroy;
end;

function TDCFilterList.GetItem(Index: Integer): TDCFilterItem;
begin
  Result := TDCFilterItem(inherited GetItem(Index));
end;

procedure TDCFilterList.SetItem(Index: Integer; Value: TDCFilterItem);
begin
  inherited SetItem(Index, Value);
end;

function TDCFilterList.Insert(Index: integer; FilterType: TDCFilterType) : TDCFilterItem;
begin
  fLock.Enter;
  try
    Result := Add(FilterType);
    Result.Index := Index;
  finally
    fLock.Leave;
  end;
end;

function TDCFilterList.Add(FilterType: TDCFilterType): TDCFilterItem;
begin
  fLock.Enter;
  try
    Result := TDCFilterItem(inherited Add);
    Result.Owner := Self;
    Result.FilterType := FilterType;
    if fInitialized then
    begin
      Result.Init(@fStream);
      Result.Flush;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TDCFilterList.Delete(Index: Integer);
begin
  fLock.Enter;
  try
    inherited Delete(Index);
  finally
    FLock.Leave;
  end;
end;

procedure TDCFilterList.Init(Stream: PDSStream);
var
  i: integer;
begin
  fLock.Enter;
  try
    fStream := Stream^;
    fInitialized := True;

    for i := 0 to Count -1 do
    begin
      Items[i].Init(Stream);
      Items[i].Flush;
    end;
  finally
    FLock.Leave;
  end;
end;

function TDCFilterList.Process(Buffer: Pointer; Size: integer): integer;
var
  i: integer;
begin
  Result := Size;
  fLock.Enter;
  try
    if Count > 0 then
    begin
      for i := 0 to Count -1 do
      begin
        // Count can change during the processing,
        // so check here if we´re still in the Range
        if (i < Count) and (Items[i] <> nil) then
          Result := Items[i].Process(Buffer,Result,@fStream);
      end;
    end;
  finally
    fLock.Leave;
  end;
end;

function TDCFilterList.GetName(Index: integer): String;
begin
  Result := 'Error (Index)';
  fLock.Enter;
  try
    if (Index < 0) or (Index > Count) then Exit;
    Result := Items[Index].Name;
  finally
    fLock.Leave;
  end;
end;

function TDCFilterList.GetFilterType(Index: integer): TDCFilterType;
begin
  fLock.Enter;
  try
    Result := ftNone;
    if (Index < 0) or (Index >= Count) then Exit;
    Result := Items[Index].FilterType;
  finally
    fLock.Leave;
  end;
end;

procedure TDCFilterList.Flush;
var
 i: integer;
begin
  fLock.Enter;
  try
    for i := 0 to Count -1 do Items[i].Flush;
  finally
    fLock.Leave;
  end;
end;

function TDCFilterList.ExportSettings(out Buffer: PChar; out Size: integer): Boolean;
var
  BufSize: integer;
  i: integer;
  Header: integer;
  CurSize: integer;
  LastPos: integer;
  sBuf: PChar;
  fCount: integer;
begin
  Result := False;
  if Count < 1 then Exit;
  if fSaveBuffer <> nil then
  begin
    FreeMem(fSaveBuffer);
    fSaveBuffer := nil;
  end;  
  BufSize := 0;

  Header := 4;
  Header := Header + Count;

  for i := 0 to Count -1 do BufSize := BufSize + Items[i].SettingsSize;
  fSaveBuffer := AllocMem(Header + BufSize);

  fCount := Count;
  CopyMemory(fSaveBuffer,@fCount,4);

  LastPos := Header;
  for i := 0 to Count -1 do
  begin
    fSaveBuffer[i+4] := Char(Items[i].FilterType);
    CurSize := Items[i].SettingsSize;
    sBuf := @fSaveBuffer[LastPos];
    Items[i].SaveSettings(sBuf);
    LastPos := LastPos + CurSize;
  end;

  Buffer := fSaveBuffer;
  Size := Header + BufSize;
  Result := True;
end;

function TDCFilterList.ImportSettings(Buffer: PChar): Boolean;
var
  fCount: integer;
  i: integer;
  CurPos: integer;
begin
  Result := False;
  if Buffer = nil then Exit;
  Clear;

  CopyMemory(@fCount,Buffer,4);
  if fCount <= 0 then Exit;

  CurPos := 4 + fCount;
  for i := 0 to fCount -1 do
  begin
    Add(TDCFilterType(Buffer[i+4]));
    Items[i].LoadSettings(@Buffer[CurPos]);
    CurPos := CurPos + Items[i].SettingsSize;
  end;
  Result := True;
end;

procedure TDCFilterList.ResetShownWindows;
var
  i: integer;
begin
  if Count < 1 then Exit;
  for i := 0 to Count -1 do Items[i].WindowShown := False;
end;

function TDCFilterList.GetDSPInterface(Index: integer): IUnknown;
begin
  Result := nil;
  if (Index < 0) or (Index > Count) then Exit;

  case GetFilterType(Index) of
    ftAmplify:        Result := TDCAmplify(Items[Index].Filter) as IDCAmplify;
    ftBandPass:       Result := TDCBandpass(Items[Index].Filter) as IDCBandPass;
    ftChannelOrder:   Result := TDCChannelOrder(Items[Index].Filter) as IDCChannelOrder;
    ftCompressor:     Result := TDCCompressor(Items[Index].Filter) as IDCCompressor;
    ftDownMix:        Result := TDCDownMix(Items[Index].Filter) as IDCDownMix;
    ftDynamicAmplify: Result := TDCDynamicAmplify(Items[Index].Filter) as IDCDynamicAmplify;
    ftEchoDelay:      Result := TDCEchoDelay(Items[Index].Filter) as IDCEchoDelay;
    ftEqualizer:      Result := TDCEqualizer(Items[Index].Filter) as IDCEqualizer;
    ftFlanger:        Result := TDCFlanger(Items[Index].Filter) as IDCFlanger;
    ftHighPass:       Result := TDCHighpass(Items[Index].Filter) as IDCHighPass;
    ftLowPass:        Result := TDCLowpass(Items[Index].Filter) as IDCLowPass;
    ftNotch:          Result := TDCNotch(Items[Index].Filter) as IDCNotch;
    ftParametricEQ:   Result := TDCParametricEQ(Items[Index].Filter) as IDCParametricEQ;
    ftPhaseInvert:    Result := TDCPhaseInvert(Items[Index].Filter) as IDCPhaseInvert;
    ftPhaser:         Result := TDCPhaser(Items[Index].Filter) as IDCPhaser;
    ftPitchScale:     Result := TDCPitchScale(Items[Index].Filter) as IDCPitchScale;
    ftPitchShift:     Result := TDCPitchShift(Items[Index].Filter) as IDCPitchShift;
    ftSound3D:        Result := TDCSound3D(Items[Index].Filter) as IDCSound3D;
    ftTempo:          Result := TDCTempo(Items[Index].Filter) as IDCTempo;
    ftTrebleEnhancer: Result := TDCTrebleEnhancer(Items[Index].Filter) as IDCTrebleEnhancer;
    ftTrueBass:       Result := TDCTrueBass(Items[Index].Filter) as IDCTrueBass;
    ftDMOChorus:      Result := TDCDMOChorus(Items[Index].Filter) as IDCDMOChorus;
    ftDMOCompressor:  Result := TDCDMOCompressor(Items[Index].Filter) as IDCDMOCompressor;
    ftDMODistortion:  Result := TDCDMODistortion(Items[Index].Filter) as IDCDMODistortion;
    ftDMOEcho:        Result := TDCDMOEcho(Items[Index].Filter) as IDCDMOEcho;
    ftDMOFlanger:     Result := TDCDMOFlanger(Items[Index].Filter) as IDCDMOFlanger;
    ftDMOGargle:      Result := TDCDMOGargle(Items[Index].Filter) as IDCDMOGargle;
    ftDMOI3DL2Reverb: Result := TDCDMOI3DL2Reverb(Items[Index].Filter) as IDCDMOI3DL2Reverb;
    ftDMOParamEQ:     Result := TDCDMOParamEQ(Items[Index].Filter) as IDCDMOParamEQ;
    ftDMOWavesReverb: Result := TDCDMOWavesReverb(Items[Index].Filter) as IDCDMOWavesReverb;
  end;
end;

end.

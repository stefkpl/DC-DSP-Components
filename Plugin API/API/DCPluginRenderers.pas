
    (*********************************************************************
     *  DCPluginRenderers.pas                                            *
     *                                                                   *
     *  This unit is Part of the DC-DSP Audio Filter v1.0                *
     *                                                                   *
     *  author    : Milenko Mitrovic                                     *
     *  email     : dcoder@dsp-worx.de                                   *
     *  web       : http://dsp-worx.de                                   *
     *  date      : 04-08-2003                                           *
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

unit DCPluginRenderers;

interface

uses
  Classes, Windows, DCDSPPluginRenderer, DCVISPluginRenderer;

type
  TDCVISPluginRenderer = class(TComponent)
  private
    fPlugins : TDCVISPluginList;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Plugins : TDCVISPluginList read fPlugins write fPlugins;
  end;

  TDCDSPPluginRenderer = class(TComponent)
  private
    fPlugins : TDCDSPPluginList;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Plugins : TDCDSPPluginList read fPlugins write fPlugins;
  end;

  procedure Register;

implementation

constructor TDCVISPluginRenderer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fPlugins := TDCVISPluginList.Create;
end;

destructor TDCVISPluginRenderer.Destroy;
begin
  fPlugins.Free;
  inherited Destroy;
end;

constructor TDCDSPPluginRenderer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fPlugins := TDCDSPPluginList.Create;
end;

destructor TDCDSPPluginRenderer.Destroy;
begin
  fPlugins.Destroy;
  inherited Destroy;
end;

procedure Register;
begin
  RegisterComponents('DC-DSP Tools', [
    TDCDSPPluginRenderer, TDCVISPluginRenderer
  ]);
end;

end.

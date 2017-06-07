program DSPackSample;

uses
  FastMM4,
  FastMove,
  Forms,
  formMain in 'formMain.pas' {frmMain},
  formDSPAmplify in 'formDSPAmplify.pas' {frmDSPAmplify},
  formDMOChorus in 'formDMOChorus.pas' {frmDMOChorus},
  formDMOCompressor in 'formDMOCompressor.pas' {frmDMOCompressor},
  formDMODistortion in 'formDMODistortion.pas' {frmDMODistortion},
  formDMOEcho in 'formDMOEcho.pas' {frmDMOEcho},
  formDMOFlanger in 'formDMOFlanger.pas' {frmDMOFlanger},
  formDMOGargle in 'formDMOGargle.pas' {frmDMOGargle},
  formDMOI3DL2Reverb in 'formDMOI3DL2Reverb.pas' {frmDMOI3DL2Reverb},
  formDMOParamEQ in 'formDMOParamEQ.pas' {frmDMOParamEQ},
  formDMOWavesReverb in 'formDMOWavesReverb.pas' {frmDMOWavesReverb},
  formDSPBandpass in 'formDSPBandpass.pas' {frmDSPBandpass},
  formDSPChannelOrder in 'formDSPChannelOrder.pas' {frmDSPChannelOrder},
  formDSPCompressor in 'formDSPCompressor.pas' {frmDSPCompressor},
  formDSPDownMix in 'formDSPDownMix.pas' {frmDSPDownMix},
  formDSPDynamicAmplify in 'formDSPDynamicAmplify.pas' {frmDSPDynamicAmplify},
  formDSPEchoDelay in 'formDSPEchoDelay.pas' {frmDSPEchoDelay},
  formDSPEqualizer in 'formDSPEqualizer.pas' {frmDSPEqualizer},
  formDSPFlanger in 'formDSPFlanger.pas' {frmDSPFlanger},
  formDSPHighpass in 'formDSPHighpass.pas' {frmDSPHighpass},
  formDSPLowpass in 'formDSPLowpass.pas' {frmDSPLowpass},
  formDSPNotch in 'formDSPNotch.pas' {frmDSPNotch},
  formDSPPhaseInvert in 'formDSPPhaseInvert.pas' {frmDSPPhaseInvert},
  formDSPPhaser in 'formDSPPhaser.pas' {frmDSPPhaser},
  formDSPPitchScale in 'formDSPPitchScale.pas' {frmDSPPitchScale},
  formDSPPitchShift in 'formDSPPitchShift.pas' {frmDSPPitchShift},
  formDSPSound3D in 'formDSPSound3D.pas' {frmDSPSound3D},
  formDSPTempo in 'formDSPTempo.pas' {frmDSPTempo},
  formDSPTrueBass in 'formDSPTrueBass.pas' {frmDSPTrueBass},
  formWA2DSPPlugins in 'formWA2DSPPlugins.pas' {frmWA2DSPPlugins},
  formWA2VisualPlugins in 'formWA2VisualPlugins.pas' {frmWA2VisualPlugins},
  formDSPTrebleEnhancer in 'formDSPTrebleEnhancer.pas' {frmDSPTrebleEnhancer},
  formDSPParametricEQ in 'formDSPParametricEQ.pas' {frmDSPParametricEQ};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmDSPAmplify, frmDSPAmplify);
  Application.CreateForm(TfrmDMOChorus, frmDMOChorus);
  Application.CreateForm(TfrmDMOCompressor, frmDMOCompressor);
  Application.CreateForm(TfrmDMODistortion, frmDMODistortion);
  Application.CreateForm(TfrmDMOEcho, frmDMOEcho);
  Application.CreateForm(TfrmDMOFlanger, frmDMOFlanger);
  Application.CreateForm(TfrmDMOGargle, frmDMOGargle);
  Application.CreateForm(TfrmDMOI3DL2Reverb, frmDMOI3DL2Reverb);
  Application.CreateForm(TfrmDMOParamEQ, frmDMOParamEQ);
  Application.CreateForm(TfrmDMOWavesReverb, frmDMOWavesReverb);
  Application.CreateForm(TfrmDSPBandpass, frmDSPBandpass);
  Application.CreateForm(TfrmDSPChannelOrder, frmDSPChannelOrder);
  Application.CreateForm(TfrmDSPCompressor, frmDSPCompressor);
  Application.CreateForm(TfrmDSPDownMix, frmDSPDownMix);
  Application.CreateForm(TfrmDSPDynamicAmplify, frmDSPDynamicAmplify);
  Application.CreateForm(TfrmDSPEchoDelay, frmDSPEchoDelay);
  Application.CreateForm(TfrmDSPEqualizer, frmDSPEqualizer);
  Application.CreateForm(TfrmDSPFlanger, frmDSPFlanger);
  Application.CreateForm(TfrmDSPHighpass, frmDSPHighpass);
  Application.CreateForm(TfrmDSPLowpass, frmDSPLowpass);
  Application.CreateForm(TfrmDSPNotch, frmDSPNotch);
  Application.CreateForm(TfrmDSPPhaseInvert, frmDSPPhaseInvert);
  Application.CreateForm(TfrmDSPPhaser, frmDSPPhaser);
  Application.CreateForm(TfrmDSPPitchScale, frmDSPPitchScale);
  Application.CreateForm(TfrmDSPPitchShift, frmDSPPitchShift);
  Application.CreateForm(TfrmDSPSound3D, frmDSPSound3D);
  Application.CreateForm(TfrmDSPTempo, frmDSPTempo);
  Application.CreateForm(TfrmDSPTrueBass, frmDSPTrueBass);
  Application.CreateForm(TfrmWA2DSPPlugins, frmWA2DSPPlugins);
  Application.CreateForm(TfrmWA2VisualPlugins, frmWA2VisualPlugins);
  Application.CreateForm(TfrmDSPTrebleEnhancer, frmDSPTrebleEnhancer);
  Application.CreateForm(TfrmDSPParametricEQ, frmDSPParametricEQ);
  Application.Run;
end.

;Nullsoft Installation Script (NSIS)
;works only for v2.0 Final
;http://www.nullsoft.com/free/nsis/
;http://nsis.sourceforge.net/

SetCompressor lzma

;--------------------------------
;Include Modern UI

  !include "MUI.nsh"

;--------------------------------
;Product Info
  !define VER_FILE "1.03"
  !define VER_PRODUCT "DC-DSP Filter"
  Name "${VER_PRODUCT} ${VER_FILE}"

;--------------------------------
;Configuration

  ;General
  OutFile "DCDSPFilterSetup.exe"

  ;Folder selection page
  InstallDir "$PROGRAMFILES\DSP-worx\${VER_PRODUCT}"
  
  ;Get install folder from registry if available
  InstallDirRegKey HKCU "Software\DSP-worx\${VER_PRODUCT}" ""

;--------------------------------
;Variables

  Var MUI_TEMP
  Var STARTMENU_FOLDER

;--------------------------------
;Modern UI Configuration

  !define MUI_ABORTWARNING
  !define MUI_UI "${NSISDIR}\Contrib\UIs\modern.exe"
  !define MUI_HEADERBITMAP "${NSISDIR}\Contrib\Graphics\Header\win.bmp"
  !define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\arrow-install.ico"
  !define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\arrow-uninstall.ico"
  !define MUI_FINISHPAGE_SHOWREADME "$INSTDIR\DCDSPFilterHelp.chm"
;  !define MUI_COMPONENTSPAGE_SMALLDESC

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_WELCOME
  !insertmacro MUI_PAGE_LICENSE "License.rtf"
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY

  ;Start Menu Folder Page Configuration
  !define MUI_STARTMENUPAGE_DEFAULTFOLDER "${VER_PRODUCT}"
  !define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKCU" 
  !define MUI_STARTMENUPAGE_REGISTRY_KEY "Software\DSP-worx\${VER_PRODUCT}" 
  !define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"

  !insertmacro MUI_PAGE_STARTMENU Application $STARTMENU_FOLDER

  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH
  
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES

;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "DC-DSP Filter (binary)" SecCore

  SectionIn 1 RO

  ;Extract out Files
  SetOutPath "$INSTDIR"
  UnRegDll $INSTDIR\DCDSPFilter.ax
  File "..\binary\configure.exe"
  File "..\binary\DCDSPFilter.ax"
  File "..\..\Help\DCDSPFilterHelp.chm"
  File "..\binary\register.bat"
  File "..\binary\unregister.bat"

  ;Register the Filter to DirectShow
  UnRegDll $INSTDIR\DCDSPFilter.ax
  RegDll $INSTDIR\DCDSPFilter.ax

  ;Store install folder
  WriteRegStr HKCU "Software\DSP-worx\${VER_PRODUCT}" "" $INSTDIR

  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

  ; write uninstall strings
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${VER_PRODUCT}" "DisplayName" "${VER_PRODUCT} ${VER_FILE}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${VER_PRODUCT}" "UninstallString" '"$INSTDIR\Uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${VER_PRODUCT}" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${VER_PRODUCT}" "NoRepair" 1

  
  !insertmacro MUI_STARTMENU_WRITE_BEGIN Application
    
    ;Create shortcuts
    CreateDirectory "$SMPROGRAMS\$STARTMENU_FOLDER"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Register Filter.lnk" "$INSTDIR\register.bat"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Unregister Filter.lnk" "$INSTDIR\unregister.bat"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Configure Filter.lnk" "$INSTDIR\configure.exe"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Help.lnk" "$INSTDIR\DCDSPFilterHelp.chm"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Uninstall.lnk" "$INSTDIR\Uninstall.exe"
  
  !insertmacro MUI_STARTMENU_WRITE_END

SectionEnd

;--------------------------------
;Descriptions

  LangString DESC_SecCore ${LANG_ENGLISH} "This installs the binary of the DSP Filter onto your System."

  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecCore} $(DESC_SecCore)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END
 
;--------------------------------
;Uninstaller Section

Section "Uninstall"

  UnRegDll $INSTDIR\DCDSPFilter.ax
  
  Delete "$INSTDIR\Uninstall.exe"

  RMDir /r "$INSTDIR"
  
  !insertmacro MUI_STARTMENU_GETFOLDER Application $MUI_TEMP
    
  Delete "$SMPROGRAMS\$MUI_TEMP\Register Filter.lnk"
  Delete "$SMPROGRAMS\$MUI_TEMP\Unregister Filter.lnk"
  Delete "$SMPROGRAMS\$MUI_TEMP\Configure Filter.lnk"
  Delete "$SMPROGRAMS\$MUI_TEMP\Help.lnk"
  Delete "$SMPROGRAMS\$MUI_TEMP\Uninstall.lnk"
  
  ;Delete empty start menu parent diretories
  StrCpy $MUI_TEMP "$SMPROGRAMS\$MUI_TEMP"
 
  startMenuDeleteLoop:
    RMDir /r $MUI_TEMP
    GetFullPathName $MUI_TEMP "$MUI_TEMP\.."
    
    IfErrors startMenuDeleteLoopDone
  
    StrCmp $MUI_TEMP $SMPROGRAMS startMenuDeleteLoopDone startMenuDeleteLoop
  startMenuDeleteLoopDone:

  DeleteRegValue HKCU "Software\DSP-worx\${VER_PRODUCT}" "Start Menu Folder"
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${VER_PRODUCT}"

  MessageBox MB_YESNO|MB_ICONQUESTION \
                     "Do you want to remove all Registry Settings for this Filter?" \
                     IDNO finish

  DeleteRegKey   HKCU "Software\DSP-worx\DC-DSP Filter\EQPresets"
  DeleteRegKey   HKCU "Software\DSP-worx\DC-DSP Filter"
  DeleteRegKey /ifempty HKCU "Software\DSP-worx"

  finish:

SectionEnd

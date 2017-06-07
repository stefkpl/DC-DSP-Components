@echo off

cd "DirectShow Filter\Binary"
upx -9 DCDSPFilter.ax
regsvr32 DCDSPFilter.ax /S
cd ..\Source
del *.~*;*.dcu;*.obj;*.hpp;*.ddp
cd "Property Pages"
del *.~*;*.dcu;*.obj;*.hpp;*.ddp
cd ..\..\Interface
del *.~*;*.dcu;*.obj;*.hpp;*.ddp

cd "..\..\DSP Components"
del *.~*;*.dcu;*.obj;*.hpp;*.ddp;*.dof;*.cfg;*.res

cd "..\DSPack Filter"
del *.~*;*.dcu;*.obj;*.hpp;*.ddp;*.dof;*.cfg;*.res

cd "..\Help\Source\DSP Components"
del *.htm;*.gif;*.hhc;*.hhk;*.hhp;*.log;*.chm


cd "..\..\..\Plugin API\API"
del *.~*;*.dcu;*.obj;*.hpp;*.ddp;*.dof;*.cfg;*.res
cd ..\Samples\DirectShow
del *.~*;*.dcu;*.obj;*.hpp;*.ddp;*.res
cd ..\DSPack
del *.~*;*.dcu;*.obj;*.hpp;*.ddp;*.res
cd "..\Plugins\DSP Plugin 1"
del *.~*;*.dcu;*.obj;*.hpp;*.ddp;*.res
cd "..\DSP Plugin 2"
del *.~*;*.dcu;*.obj;*.hpp;*.ddp;*.res
cd "..\VIS Plugin 1"
del *.~*;*.dcu;*.obj;*.hpp;*.ddp;*.res
cd "..\VIS Plugin 2"
del *.~*;*.dcu;*.obj;*.hpp;*.ddp;*.res
cd "..\Winamp2 DSP Wrapper"
del *.~*;*.dcu;*.obj;*.hpp;*.ddp;*.res

cd ..\..\..\..\Samples\DirectShow
del *.~*;*.dcu;*.obj;*.hpp;*.ddp;*.dof;*.cfg;*.res;*.exe;*.dat;*.ini
cd ..\DSPack
del *.~*;*.dcu;*.obj;*.hpp;*.ddp;*.dof;*.cfg;*.res;*.exe;*.dat;*.ini
cd "..\DSPack DVDPlay"
del *.~*;*.dcu;*.obj;*.hpp;*.ddp;*.dof;*.cfg;*.res;*.exe;*.dat;*.ini

cd "..\..\MoveLibrary"
del *.~*;*.dcu;*.obj;*.hpp;*.ddp;*.dof;*.cfg;*.res;*.exe;*.dat;*.ini

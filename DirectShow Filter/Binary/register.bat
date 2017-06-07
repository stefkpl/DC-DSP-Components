@echo off
if "%OS%"=="Windows_NT" goto NT
%WINDIR%\system\regsvr32.exe DCDSPFilter.ax
goto END
:NT
regsvr32.exe DCDSPFilter.ax
:END
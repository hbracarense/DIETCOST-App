@echo off
setlocal

set "APPDIR=%~dp0"
cd /d "%APPDIR%"

set "R_HOME=%APPDIR%R"
set "PATH=%APPDIR%R\bin\x64;%APPDIR%R\bin;%PATH%"

"%APPDIR%R\bin\x64\Rscript.exe" "%APPDIR%launcher.R"
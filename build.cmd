@echo off
cls

.\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

IF NOT EXIST build.fsx (
  .\paket.exe update
  packages\FAKE\tools\FAKE.exe init.fsx
)
packages\FAKE\tools\FAKE.exe build.fsx %*

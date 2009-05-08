@for %%a in (cl.exe) do if EXIST %%~$PATH:a set vcvars_found=Y

@if "%vcvars_found%"=="Y" goto after_vcvars

@call "c:\Program Files\Microsoft Visual Studio 8\VC\bin\vcvars32.bat"
@call "c:\Program Files (x86)\Microsoft Visual Studio 8\VC\bin\vcvars32.bat"
@call "c:/Microsoft Visual Studio 8/VC/bin/vcvars32.bat"

:after_vcvars

@del c.tab.c c.tab.h c.output
@REM bison c.y
@if NOT "%ERRORLEVEL%"=="0" goto error

@REM /RTC{s,c,u} : stack frame runtime checking, convert checks, unininitialized checks
@REM /J : type char unsigned
@REM /Wall, /we : warning level 4, warning as error

@REM /J : unsigned char
@REM /ZI: debug info
@REM /O2: maximize speed
@REM /Og: global opt

set INPUTS=Kernel32.lib abscope.c locinfo.c c_parse.c strs.c abutil.c
set FLAGS=/O2
@REM set FLAGS=/RTCscu /ZI
cl /analyze /J /MTd /W4 %FLAGS% %INPUTS%


@if NOT "%ERRORLEVEL%"=="0" goto error

@echo "done"
goto end

:error
@echo "something gone wrong"

:end
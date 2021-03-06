@REM ====================
@REM find compiler
@REM ====================

@REM using vcvars32.bat instead of cl.exe because SN Systems uses a cl.exe for an exe name as well for VS-PS3 
@set vcvars_found=N
@for %%a in (vcvars32.bat) do @if EXIST %%~$PATH:a set vcvars_found=Y
@if "%vcvars_found%"=="Y" goto after_vcvars
@for %%a in ("c:\Program Files\Microsoft Visual Studio 8\VC\bin\vcvars32.bat" 
	 "c:\Program Files\Microsoft Visual Studio 8\VC\bin\vcvars32.bat" 
	 "c:\Program Files (x86)\Microsoft Visual Studio 8\VC\bin\vcvars32.bat") do @(
	@if EXIST %%a (
	   @call %%a
	   @goto after_vcvars
	)
)

:after_vcvars

@call killall abscope.exe
@call copy /Y abscope.exe abscope.exe.bak
@IF /I "%1" == "OPT" set opt=Y
@IF /I "%COMPUTERNAME%" EQU "abrady" set opt=Y
@IF /I "%1" == "DBG" set opt=N

@REM /RTC{s,c,u} : stack frame runtime checking, convert checks, unininitialized checks
@REM /Wall, /we : warning level 4, warning as error
@REM /J : unsigned char
@REM /ZI: debug info
@REM /O2: maximize speed
@REM /Og: global opt

@set LIBS= kernel32.lib pcre.lib /D PCRE_STATIC
@set INPUTS= %LIBS% abscope.c locinfo.c c_parse.c strs.c abutil.c abtree.c abhash.c abfile.c abarray.c
@REM for real speed: /MD vs. /MT ? (single threaded vs. multi crt?)
@IF /I "%opt%" EQU "Y" (
set FLAGS=/O2 /Oi /Zi /MT
) ELSE (
set FLAGS=/RTCscu /ZI  /MTd
)
@REM /link /NODEFAULTLIB:MSVCR80D
@REM /ALLOWISOLATION:NO : turn off manifest look up for some crt library
cl /MP /analyze:stacksize 38000 /analyze /J /W4 %FLAGS% %INPUTS% /link /ALLOWISOLATION:NO


@if NOT "%ERRORLEVEL%"=="0" goto error

@echo "done"

IF EXIST %HOME%\bin (
copy abscope.exe %HOME%\bin
copy abscope.pdb %HOME%\bin
) 

goto end

:error
@echo "something gone wrong"

:end
@REM -t : debug
@REM -d : make a header file
@REM -p : name prefix
@REM --locations : 
@REM -v : verbose
call "c:\Program Files\Microsoft Visual Studio 8\VC\bin\vcvars32.bat"
call "c:/Microsoft Visual Studio 8/VC/bin/vcvars32.bat"

@REM -t --locations -v
del c.tab.c c.tab.h c.output
bison -pc_ c.y
if NOT "%ERRORLEVEL%"=="0" goto error

@REM /RTC{s,c,u} : stack frame runtime checking, convert checks, unininitialized checks
@REM /J : type char unsigned
@REM /Wall, /we : warning level 4, warning as error

cl /RTCscu /W4 /ZI abscope.c locinfo.c c_parse.c c.tab.c strs.c
if NOT "%ERRORLEVEL%"=="0" goto error

echo "done"
goto end

:error
echo "something gone wrong"

:end
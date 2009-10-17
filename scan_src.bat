set SRC=c:\src
if "%1" NEQ "" set SRC=%1

pushd %SRC%
@echo "----------" >> c:\abs\abscope\scan_src_result.log
@date /t >> c:\abs\abscope\scan_src_result.log
@time /t >> c:\abs\abscope\scan_src_result.log
c:\abs\abscope\abscope.exe -R . -Dt -E AutoGen -E 3rdparty >> scan_src_result.log
@time /t >> c:\abs\abscope\scan_src_result.log
cmd /c killall.exe abscope.exe
popd

:done
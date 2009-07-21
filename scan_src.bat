pushd c:\src
echo "----------" >> c:\abs\abscope\scan_src_result.txt
date /t >> c:\abs\abscope\scan_src_result.txt
time /t >> c:\abs\abscope\scan_src_result.txt
c:\abs\abscope\abscope.exe -R . -Dt -E AutoGen -E 3rdparty >> c:\abs\abscope\scan_src_result.log
popd
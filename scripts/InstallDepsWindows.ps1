if (!(Test-Path "make.exe") -or !(Test-Path "libssp-0.dll")) {
    Invoke-WebRequest -OutFile make.zip -Uri "https://github.com/maweil/MakeForWindows/releases/download/v4.4.1/make-bin-win64.zip"
    Expand-Archive -Force make.zip .
    Copy-Item "make-4.4.1\dist\make.exe" "make.exe"
    Copy-Item "make-4.4.1\dist\libssp-0.dll" "libssp-0.dll"
    Remove-Item -Recurse -Force make
}

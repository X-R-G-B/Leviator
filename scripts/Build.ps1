stack build --copy-bins --local-bin-path .

if (Test-Path ".\koaky-exe.exe")
{
    Rename-Item ".\koaky-exe.exe" "koaky.exe"
}

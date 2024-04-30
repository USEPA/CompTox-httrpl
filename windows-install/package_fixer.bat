@echo off
setlocal enabledelayedexpansion
REM set "installer_file=%~dp0installer_fixer.R"
set "installer_file=installer_fixer.R"
set "install_dir=%~dp0RpackagesDir"
set "packages_file=..\\RPackages_fixer"

pushd "%install_dir%"

REM Delete existing installer file if it exists
if exist "%installer_file%" (
    del "%installer_file%"
)


for /f "usebackq delims=" %%i in ("%packages_file%") do (
    set "package=%%i"

    echo "https://cran-archive.r-project.org/bin/windows/contrib/3.6/!package!.zip"

    curl -o "!package!.zip" "https://cran-archive.r-project.org/bin/windows/contrib/3.6/!package!.zip"

    if not errorlevel 1 (
        echo install.packages("!package!.zip",repos=NULL,type="win.library"^) >> "%installer_file%"
    )  else (
        echo Failed to download package: !package!
    )
)

REM Run the installer script
Rscript "%installer_file%"

popd

endlocal
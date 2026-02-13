#*****************************************************************
# compile.ps1 - GnuCOBOL Build Script for Windows (PowerShell)
# Altoro Mutual Banking System - COBOL Edition
#
# Compiles all COBOL programs using GnuCOBOL (cobc).
#
# Prerequisites:
#   - GnuCOBOL installed: choco install gnucobol -y (as admin)
#   - cobc.exe available in PATH
#
# Usage:
#   cd cobol\scripts
#   .\compile.ps1
#
# Note: EXEC SQL blocks are wrapped in >>IF DB2-ENABLED
#       and will be skipped during compilation.
#*****************************************************************

$ErrorActionPreference = "Continue"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectDir = Split-Path -Parent $ScriptDir
$SrcDir = Join-Path $ProjectDir "programs"
$CpyDir = Join-Path $ProjectDir "copybooks"
$OutDir = Join-Path $ProjectDir "bin"
$DataDir = Join-Path $ProjectDir "data"

# GnuCOBOL environment setup (Chocolatey install)
$GnuRoot = "C:\ProgramData\chocolatey\lib\gnucobol\tools"
if (Test-Path $GnuRoot) {
    $env:COB_CONFIG_DIR = "$GnuRoot\config"
    $env:COB_COPY_DIR = "$GnuRoot\copy"
    if ($env:PATH -notlike "*$GnuRoot\bin*") {
        $env:PATH = "$GnuRoot\bin;$env:PATH"
    }
}

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host " Altoro Mutual Banking System"
Write-Host " GnuCOBOL Compilation Script (Windows)"
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""

# Check for GnuCOBOL
$cobc = Get-Command cobc -ErrorAction SilentlyContinue
if (-not $cobc) {
    Write-Host "ERROR: GnuCOBOL (cobc) not found in PATH." -ForegroundColor Red
    Write-Host "Install with: choco install gnucobol -y (run as Administrator)"
    Write-Host ""
    Write-Host "After installation, you may need to restart your shell"
    Write-Host "or add GnuCOBOL to PATH manually."
    exit 1
}

$cobcVersion = & cobc --version 2>&1 | Select-Object -First 1
Write-Host "GnuCOBOL: $cobcVersion"
Write-Host ""

# Create output directory
if (-not (Test-Path $OutDir)) {
    New-Item -ItemType Directory -Path $OutDir -Force | Out-Null
}

# Main program (compiled as executable)
$MainProgram = "MAINPROG"
# Sub-programs (compiled as modules/DLLs)
$SubPrograms = @("AUTHNTCN", "ACCTMGMT", "CUSTMGMT", "RPTGEN")

$Compiled = 0
$Failed = 0
$Warnings = 0

# Compile sub-programs first (as modules)
foreach ($Prog in $SubPrograms) {
    $SrcFile = Join-Path $SrcDir "$Prog.cbl"
    $OutFile = Join-Path $OutDir "$Prog.dll"

    if (-not (Test-Path $SrcFile)) {
        Write-Host "[SKIP] $Prog.cbl - file not found" -ForegroundColor Yellow
        continue
    }

    Write-Host -NoNewline "Compiling $Prog.cbl (module)... "

    $output = & cobc -m -I "$CpyDir" -o "$OutFile" "$SrcFile" 2>&1

    if ($LASTEXITCODE -eq 0) {
        Write-Host "[OK]" -ForegroundColor Green
        $Compiled++
        $warningLines = $output | Where-Object { $_ -match "warning" }
        if ($warningLines) {
            Write-Host "  Warnings:" -ForegroundColor Yellow
            $warningLines | Select-Object -First 10 | ForEach-Object {
                Write-Host "    $_" -ForegroundColor Yellow
            }
            $Warnings++
        }
    } else {
        Write-Host "[FAIL]" -ForegroundColor Red
        $Failed++
        Write-Host "  Error output:" -ForegroundColor Red
        $output | Select-Object -First 20 | ForEach-Object {
            Write-Host "    $_" -ForegroundColor Red
        }
    }
}

# Compile main program (as executable)
$SrcFile = Join-Path $SrcDir "$MainProgram.cbl"
$OutFile = Join-Path $OutDir "$MainProgram.exe"

if (Test-Path $SrcFile) {
    Write-Host -NoNewline "Compiling $MainProgram.cbl (executable)... "

    $output = & cobc -x -I "$CpyDir" -o "$OutFile" "$SrcFile" 2>&1

    if ($LASTEXITCODE -eq 0) {
        Write-Host "[OK]" -ForegroundColor Green
        $Compiled++
        $warningLines = $output | Where-Object { $_ -match "warning" }
        if ($warningLines) {
            Write-Host "  Warnings:" -ForegroundColor Yellow
            $warningLines | Select-Object -First 10 | ForEach-Object {
                Write-Host "    $_" -ForegroundColor Yellow
            }
            $Warnings++
        }
    } else {
        Write-Host "[FAIL]" -ForegroundColor Red
        $Failed++
        Write-Host "  Error output:" -ForegroundColor Red
        $output | Select-Object -First 20 | ForEach-Object {
            Write-Host "    $_" -ForegroundColor Red
        }
    }
}

Write-Host ""
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host " Compilation Summary"
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host " Compiled: $Compiled / $($SubPrograms.Count + 1)" -ForegroundColor Green
Write-Host " Failed:   $Failed" -ForegroundColor $(if ($Failed -gt 0) { "Red" } else { "Green" })
Write-Host " Warnings: $Warnings" -ForegroundColor $(if ($Warnings -gt 0) { "Yellow" } else { "Green" })
Write-Host ""

if ($Compiled -gt 0) {
    Write-Host "Output files in: $OutDir"
    Get-ChildItem "$OutDir\*" -Include "*.exe","*.dll" | ForEach-Object {
        Write-Host "  $($_.Name) ($([math]::Round($_.Length / 1KB)) KB)"
    }
    Write-Host ""
    Write-Host "To run the application:" -ForegroundColor Cyan
    Write-Host "  `$env:COB_LIBRARY_PATH = `"$OutDir`""
    Write-Host "  cd `"$DataDir`""
    Write-Host "  & `"$OutFile`""
}

Write-Host ""
Write-Host "==========================================" -ForegroundColor Cyan

exit $Failed

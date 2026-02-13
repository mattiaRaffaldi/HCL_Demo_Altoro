#!/bin/bash
#*****************************************************************
# compile.sh - GnuCOBOL Build Script
# Altoro Mutual Banking System - COBOL Edition
#
# Compiles all COBOL programs using GnuCOBOL (cobc).
# 
# Prerequisites:
#   - GnuCOBOL installed (apt install gnucobol / brew install gnucobol)
#   - cobc command available in PATH
#
# Usage:
#   chmod +x compile.sh
#   ./compile.sh
#
# Note: Programs with EXEC SQL will compile with warnings
#       since they require a DB2 precompiler. For AppScan
#       analysis, only the source code is needed.
#*****************************************************************

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
SRC_DIR="$PROJECT_DIR/programs"
CPY_DIR="$PROJECT_DIR/copybooks"
OUT_DIR="$PROJECT_DIR/bin"
DATA_DIR="$PROJECT_DIR/data"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=========================================="
echo " Altoro Mutual Banking System"
echo " GnuCOBOL Compilation Script"
echo "=========================================="
echo ""

# Check for GnuCOBOL
if ! command -v cobc &> /dev/null; then
    echo -e "${RED}ERROR: GnuCOBOL (cobc) not found.${NC}"
    echo "Install with:"
    echo "  Ubuntu/Debian: sudo apt install gnucobol"
    echo "  macOS:         brew install gnucobol"
    echo "  Windows:       Download from https://gnucobol.sourceforge.io/"
    exit 1
fi

echo "GnuCOBOL version: $(cobc --version | head -1)"
echo ""

# Create output directory
mkdir -p "$OUT_DIR"

# List of programs to compile (order matters for dependencies)
PROGRAMS="MAINPROG AUTHNTCN ACCTMGMT CUSTMGMT RPTGEN"

COMPILED=0
FAILED=0
WARNINGS=0

for PROG in $PROGRAMS; do
    SRC_FILE="$SRC_DIR/$PROG.cbl"
    
    if [ ! -f "$SRC_FILE" ]; then
        echo -e "${RED}[SKIP] $PROG.cbl - file not found${NC}"
        continue
    fi
    
    echo -n "Compiling $PROG.cbl... "
    
    # Compile with GnuCOBOL
    # -I = copybook include path
    # -x = produce executable
    # -o = output file
    # -Wall = all warnings
    # -free = free-format source (our files use fixed format but cobc handles both)
    if cobc -x -I "$CPY_DIR" -o "$OUT_DIR/$PROG" "$SRC_FILE" 2>/tmp/cobc_$PROG.log; then
        echo -e "${GREEN}[OK]${NC}"
        COMPILED=$((COMPILED + 1))
        
        # Check for warnings
        if [ -s /tmp/cobc_$PROG.log ]; then
            echo -e "  ${YELLOW}Warnings:${NC}"
            cat /tmp/cobc_$PROG.log | head -10
            WARNINGS=$((WARNINGS + 1))
        fi
    else
        echo -e "${RED}[FAIL]${NC}"
        FAILED=$((FAILED + 1))
        echo "  Error output:"
        cat /tmp/cobc_$PROG.log | head -20
    fi
done

echo ""
echo "=========================================="
echo " Compilation Summary"
echo "=========================================="
echo -e " Compiled: ${GREEN}$COMPILED${NC}"
echo -e " Failed:   ${RED}$FAILED${NC}"
echo -e " Warnings: ${YELLOW}$WARNINGS${NC}"
echo ""

if [ $COMPILED -gt 0 ]; then
    echo "Executables in: $OUT_DIR/"
    ls -la "$OUT_DIR/" 2>/dev/null
    echo ""
    echo "To run the application:"
    echo "  cd $DATA_DIR"
    echo "  $OUT_DIR/MAINPROG"
fi

echo ""
echo "=========================================="

# Cleanup temp files
rm -f /tmp/cobc_*.log

exit $FAILED

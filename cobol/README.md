# AltoroJ-COBOL: Vulnerable Banking Application

**Altoro Mutual Banking System - COBOL Edition**

> **WARNING: This application is intentionally vulnerable.**  
> It is designed for security testing and demonstration purposes only.  
> Do NOT deploy in any production environment.

## Overview

AltoroJ-COBOL is a deliberately vulnerable COBOL banking application, companion to the [AltoroJ](https://github.com/HCL-TECH-SOFTWARE/AltoroJ) Java web application. It simulates a mainframe banking system for **Altoro Mutual Bank** and is designed to be scanned by **HCL AppScan Source** to demonstrate COBOL security analysis capabilities.

The application contains **~5,000 lines of COBOL** code with **29 intentional security vulnerabilities** across **12 CWE categories**, all representative of real-world issues found in legacy mainframe banking systems.

## Architecture

```
cobol/
├── copybooks/              Shared data structures (COPY members)
│   ├── CUSTCOPY.cpy        Customer record layout (from PEOPLE table)
│   ├── ACCTCOPY.cpy        Account record layout (from ACCOUNTS table)
│   ├── TRANCOPY.cpy        Transaction record layout
│   └── SYSCOPY.cpy         System constants, DB2 config, shared areas
├── programs/               COBOL source programs
│   ├── MAINPROG.cbl        Main menu and program dispatcher
│   ├── AUTHNTCN.cbl        Authentication and security module
│   ├── ACCTMGMT.cbl        Account management and transfers
│   ├── CUSTMGMT.cbl        Customer management and admin
│   └── RPTGEN.cbl          Report generation and audit
├── jcl/                    z/OS JCL for mainframe compilation
│   ├── COMPILE.jcl         Compilation JCL (Enterprise COBOL)
│   └── RUNPROG.jcl         Execution JCL
├── scripts/
│   └── compile.sh          GnuCOBOL build script (Linux/Mac)
├── data/                   Sample test data
│   ├── CUSTFILE.dat        Customer master data
│   └── ACCTFILE.dat        Account master data
└── README.md               This file
```

## Test Data (from AltoroJ)

The application uses the same test data as AltoroJ for consistency:

| Username | Password   | Role  | Accounts                        |
|----------|------------|-------|---------------------------------|
| admin    | admin      | ADMIN | Corporate ($52M), Checking ($93K) |
| jsmith   | demo1234   | USER  | Savings ($10K), Checking ($15K), Credit Card |
| jdoe     | demo1234   | USER  | Savings ($10), Checking ($25), Credit Card |
| sspeed   | demo1234   | USER  | Savings ($59K), Checking ($150)   |
| tuser    | tuser      | USER  | (no accounts)                    |

Admin panel password: `Altoro1234`

## Building with GnuCOBOL

### Prerequisites

Install GnuCOBOL:

```bash
# Ubuntu/Debian
sudo apt install gnucobol

# macOS
brew install gnucobol

# Windows - download from:
# https://gnucobol.sourceforge.io/
```

### Compile

```bash
cd cobol/scripts
chmod +x compile.sh
./compile.sh
```

### Run

```bash
cd cobol/data
../bin/MAINPROG
```

> **Note:** Programs containing `EXEC SQL` statements require a DB2 precompiler and will generate warnings with GnuCOBOL. For AppScan analysis, only the source code is needed - compilation is not required.

## Generating .IRX File with HCL AppScan

### Option 1: AppScan Source for Analysis (Desktop)

1. Open HCL AppScan Source for Analysis
2. Create a new Application
3. Add the `cobol/programs/` and `cobol/copybooks/` directories as source
4. Set language to **COBOL**
5. Configure scan:
   - Include all `.cbl` and `.cpy` files
   - Set COBOL source format to Fixed (columns 7-72)
6. Run scan
7. Export results as `.irx` file: **File > Export > IRX**

### Option 2: AppScan CLI (Command Line)

```bash
# Navigate to the cobol directory
cd cobol

# Generate IRX using AppScan Source CLI
appscan.sh prepare \
  -s programs/ \
  -cp copybooks/ \
  -l cobol \
  -o AltoroJ-COBOL.irx

# Or with appscan.bat on Windows
appscan.bat prepare ^
  -s programs\ ^
  -cp copybooks\ ^
  -l cobol ^
  -o AltoroJ-COBOL.irx
```

### Option 3: AppScan on Cloud (ASoC)

1. Log in to [HCL AppScan on Cloud](https://cloud.appscan.com)
2. Create a new Application: "Altoro Mutual COBOL"
3. Create a new SAST scan
4. Upload the `cobol/` directory as source
5. Start the scan
6. Download the `.irx` and report from the dashboard

## Vulnerability Catalog

### Summary

| CWE | Category | Count | Programs |
|-----|----------|-------|----------|
| CWE-89  | SQL Injection | 8 | AUTHNTCN, ACCTMGMT, CUSTMGMT, RPTGEN |
| CWE-78  | OS Command Injection | 6 | CUSTMGMT, RPTGEN |
| CWE-120 | Buffer Overflow | 6 | ACCTMGMT, CUSTMGMT |
| CWE-798 | Hardcoded Credentials | 3 | AUTHNTCN, SYSCOPY |
| CWE-327 | Weak Cryptography | 1 | AUTHNTCN |
| CWE-256 | Plaintext Password Storage | 3 | AUTHNTCN, CUSTMGMT |
| CWE-200 | Information Leakage | 8 | All programs |
| CWE-22  | Path Traversal | 3 | CUSTMGMT, RPTGEN |
| CWE-367 | TOCTOU Race Condition | 2 | CUSTMGMT |
| CWE-862 | Missing Authentication | 5 | MAINPROG, ACCTMGMT, CUSTMGMT |
| CWE-190 | Numeric Overflow | 5 | ACCTMGMT, RPTGEN |
| CWE-755 | Improper Error Handling | 20+ | All programs |

### Detailed Vulnerability List

#### V01 - SQL Injection (CWE-89)
**Location:** `AUTHNTCN.cbl`, `ACCTMGMT.cbl`, `CUSTMGMT.cbl`, `RPTGEN.cbl`  
**Pattern:** User input concatenated via `STRING` into `EXEC SQL EXECUTE IMMEDIATE` without parameterized queries.  
**Example:** Login query in AUTHNTCN builds SQL by concatenating username and password directly.

#### V02 - Hardcoded Credentials (CWE-798)
**Location:** `AUTHNTCN.cbl`, `SYSCOPY.cpy`  
**Pattern:** Admin password `Altoro1234`, master key `MSTR2024`, backdoor account, and DB2 credentials stored as `VALUE` literals in source code.

#### V03 - Weak Cryptography (CWE-327)
**Location:** `AUTHNTCN.cbl`  
**Pattern:** XOR-based password "encryption" with a fixed 8-byte key. Trivially reversible.

#### V04 - Plaintext Password Storage (CWE-256)
**Location:** `AUTHNTCN.cbl`, `CUSTMGMT.cbl`  
**Pattern:** Passwords stored in clear text in VSAM customer file and password history file.

#### V05 - Information Leakage (CWE-200)
**Location:** All programs  
**Pattern:** Credentials, SSN, and PII displayed via `DISPLAY` or written to log files. Debug SQL statements shown on screen.

#### V08 - Buffer Overflow (CWE-120)
**Location:** `ACCTMGMT.cbl`, `CUSTMGMT.cbl`  
**Pattern:** `ACCEPT` into oversized buffer (PIC X(80)) then `MOVE` to shorter field (PIC X(20)) without length validation.

#### V09 - Numeric Overflow (CWE-190)
**Location:** `ACCTMGMT.cbl`, `RPTGEN.cbl`  
**Pattern:** `COMPUTE` statements on monetary amounts without `ON SIZE ERROR` clause.

#### V14 - Command Injection (CWE-78)
**Location:** `CUSTMGMT.cbl`, `RPTGEN.cbl`  
**Pattern:** User-supplied filenames/paths passed to `CALL "SYSTEM"` without sanitization.

#### V16 - Path Traversal (CWE-22)
**Location:** `CUSTMGMT.cbl`, `RPTGEN.cbl`  
**Pattern:** User-supplied file paths used directly in `OPEN` statements without validation.

#### V17 - Race Condition / TOCTOU (CWE-367)
**Location:** `CUSTMGMT.cbl`  
**Pattern:** File existence checked via `CALL "SYSTEM" USING "test -f"`, then file opened separately - gap allows file swap.

## License

This project is part of AltoroJ and is provided under the [Apache 2.0 License](../LICENSE).

## Disclaimer

This application is **intentionally insecure** and is designed for:
- HCL AppScan security scanning demonstrations
- Security training and awareness
- Static analysis tool evaluation

**Never use this code as a reference for secure coding practices.**

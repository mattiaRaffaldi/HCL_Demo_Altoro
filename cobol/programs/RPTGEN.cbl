       IDENTIFICATION DIVISION.
       PROGRAM-ID. RPTGEN.
       AUTHOR. ALTORO-MUTUAL-DEVELOPMENT.
       DATE-WRITTEN. 2024-01-15.
       DATE-COMPILED.
      ******************************************************************
      * RPTGEN.cbl - Report Generation and Audit Module
      * Altoro Mutual Banking System - COBOL Edition
      *
      * Generates various banking reports: customer listing,
      * account summary, transaction reports, audit trails,
      * and regulatory compliance reports.
      *
      * VULNERABILITIES:
      *   V21 - Information Leakage / PII Exposure: SSN, password,
      *         PAN in cleartext in reports
      *   V22 - Insecure Temp Files: predictable temp filenames
      *   V23 - Sensitive Data in Logs: PII in SYSOUT/DISPLAY
      *   V24 - Command Injection: report formatting via SYSTEM
      *   V25 - Path Traversal: output directory from user input
      *   V26 - Improper Error Handling: WRITE errors ignored
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZOS.
       OBJECT-COMPUTER. IBM-ZOS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE
               ASSIGN TO "CUSTFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-USER-ID
               FILE STATUS IS WS-CUST-FILE-STATUS.

           SELECT ACCOUNT-FILE
               ASSIGN TO "ACCTFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-ID
               FILE STATUS IS WS-ACCT-FILE-STATUS.

           SELECT TRANSACTION-FILE
               ASSIGN TO "TRANFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TRAN-ID
               FILE STATUS IS WS-TRAN-FILE-STATUS.

           SELECT REPORT-OUTPUT-FILE
               ASSIGN TO WS-RPT-OUTPUT-PATH
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RPT-OUTPUT-STATUS.

           SELECT TEMP-WORK-FILE
               ASSIGN TO WS-TEMP-FILE-PATH
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-TEMP-FILE-STATUS.

           SELECT AUDIT-TRAIL-FILE
               ASSIGN TO "AUDITLOG"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-AUDIT-TRAIL-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
           COPY CUSTREC.

       FD  ACCOUNT-FILE.
           COPY ACCTREC.

       FD  TRANSACTION-FILE.
           COPY TRANREC.

       FD  REPORT-OUTPUT-FILE.
       01  RPT-OUTPUT-RECORD           PIC X(132).

       FD  TEMP-WORK-FILE.
       01  TEMP-WORK-RECORD            PIC X(256).

       FD  AUDIT-TRAIL-FILE.
       01  AUDIT-TRAIL-RECORD          PIC X(256).

       WORKING-STORAGE SECTION.

      * Copy in shared data structures
           COPY SYSCOPY.
           COPY CUSTWS.
           COPY ACCTWS.
           COPY TRANWS.

      * Local working fields
       01  WS-PROGRAM-NAME             PIC X(08) VALUE "RPTGEN".
       01  WS-RPT-CONTINUE             PIC X(01) VALUE "Y".
           88  WS-RPT-LOOP             VALUE "Y".
           88  WS-RPT-EXIT-FLG         VALUE "N".

      * Menu
       01  WS-RPT-MENU-CHOICE          PIC X(02).

      * Report output and temp file paths
       01  WS-RPT-OUTPUT-PATH          PIC X(256) VALUE SPACES.
       01  WS-TEMP-FILE-PATH           PIC X(256) VALUE SPACES.
       01  WS-RPT-OUTPUT-STATUS        PIC X(02).
       01  WS-TEMP-FILE-STATUS         PIC X(02).
       01  WS-AUDIT-TRAIL-STATUS       PIC X(02).

      * Report header/detail/footer lines
       01  WS-RPT-HEADER-1.
           05  FILLER                  PIC X(20) VALUE SPACES.
           05  FILLER                  PIC X(40)
               VALUE "ALTORO MUTUAL BANKING SYSTEM".
           05  FILLER                  PIC X(10) VALUE SPACES.
           05  WS-RPT-HDR-DATE         PIC X(10).
           05  FILLER                  PIC X(52) VALUE SPACES.

       01  WS-RPT-HEADER-2.
           05  FILLER                  PIC X(20) VALUE SPACES.
           05  WS-RPT-HDR-TITLE        PIC X(50).
           05  FILLER                  PIC X(05) VALUE "Page ".
           05  WS-RPT-HDR-PAGE         PIC Z(03)9.
           05  FILLER                  PIC X(53) VALUE SPACES.

       01  WS-RPT-SEPARATOR            PIC X(132) VALUE ALL "=".
       01  WS-RPT-DASH-LINE            PIC X(132) VALUE ALL "-".

      * Customer report detail line
       01  WS-CUST-RPT-DETAIL.
           05  WS-CRPT-USERID          PIC X(20).
           05  FILLER                  PIC X(01) VALUE " ".
           05  WS-CRPT-NAME            PIC X(35).
           05  FILLER                  PIC X(01) VALUE " ".
           05  WS-CRPT-ROLE            PIC X(05).
           05  FILLER                  PIC X(01) VALUE " ".
      * VULNERABILITY V21: SSN in report
           05  WS-CRPT-SSN             PIC X(11).
           05  FILLER                  PIC X(01) VALUE " ".
      * VULNERABILITY V21: Password in report
           05  WS-CRPT-PASSWORD        PIC X(20).
           05  FILLER                  PIC X(01) VALUE " ".
           05  WS-CRPT-EMAIL           PIC X(35).

      * Account report detail line
       01  WS-ACCT-RPT-DETAIL.
           05  WS-ARPT-ID              PIC 9(16).
           05  FILLER                  PIC X(01) VALUE " ".
           05  WS-ARPT-OWNER           PIC X(20).
           05  FILLER                  PIC X(01) VALUE " ".
           05  WS-ARPT-NAME            PIC X(20).
           05  FILLER                  PIC X(01) VALUE " ".
           05  WS-ARPT-TYPE            PIC X(02).
           05  FILLER                  PIC X(01) VALUE " ".
           05  WS-ARPT-BALANCE         PIC Z(12)9.99-.
           05  FILLER                  PIC X(01) VALUE " ".
           05  WS-ARPT-STATUS          PIC X(01).
           05  FILLER                  PIC X(01) VALUE " ".
      * VULNERABILITY V21: PIN in report
           05  WS-ARPT-PIN             PIC X(06).
           05  FILLER                  PIC X(31) VALUE SPACES.

      * Transaction report detail line
       01  WS-TRAN-RPT-DETAIL.
           05  WS-TRPT-ID              PIC 9(10).
           05  FILLER                  PIC X(01) VALUE " ".
           05  WS-TRPT-DATE            PIC X(10).
           05  FILLER                  PIC X(01) VALUE " ".
           05  WS-TRPT-ACCT            PIC 9(16).
           05  FILLER                  PIC X(01) VALUE " ".
           05  WS-TRPT-TYPE            PIC X(02).
           05  FILLER                  PIC X(01) VALUE " ".
           05  WS-TRPT-AMOUNT          PIC Z(12)9.99-.
           05  FILLER                  PIC X(01) VALUE " ".
           05  WS-TRPT-DESC            PIC X(30).
           05  FILLER                  PIC X(01) VALUE " ".
      * VULNERABILITY V21: User ID in transaction report
           05  WS-TRPT-USER            PIC X(20).
           05  FILLER                  PIC X(09) VALUE SPACES.

      * Report counters and accumulators
       01  WS-RPT-COUNTERS.
           05  WS-RPT-PAGE-NUM         PIC 9(04) VALUE ZEROS.
           05  WS-RPT-LINE-NUM         PIC 9(02) VALUE ZEROS.
           05  WS-RPT-MAX-LINES        PIC 9(02) VALUE 55.
           05  WS-RPT-RECORD-COUNT     PIC 9(08) VALUE ZEROS.
           05  WS-RPT-TOTAL-BALANCE    PIC S9(15)V99 VALUE ZEROS.
           05  WS-RPT-TOTAL-CREDITS    PIC S9(15)V99 VALUE ZEROS.
           05  WS-RPT-TOTAL-DEBITS     PIC S9(15)V99 VALUE ZEROS.
           05  WS-RPT-CUST-COUNT       PIC 9(06) VALUE ZEROS.
           05  WS-RPT-ACCT-COUNT       PIC 9(06) VALUE ZEROS.
           05  WS-RPT-TRAN-COUNT       PIC 9(08) VALUE ZEROS.

      * Input fields
       01  WS-RPT-INPUT-FIELDS.
           05  WS-RPT-INPUT-PATH       PIC X(256).
           05  WS-RPT-INPUT-FORMAT     PIC X(10).
           05  WS-RPT-INPUT-ACCT-ID    PIC X(20).
           05  WS-RPT-INPUT-START-DATE PIC X(10).
           05  WS-RPT-INPUT-END-DATE   PIC X(10).
           05  WS-RPT-INPUT-TYPE       PIC X(02).

      * Display fields
       01  WS-DSP-BALANCE              PIC Z(14)9.99-.
       01  WS-DSP-AMOUNT               PIC Z(12)9.99-.
       01  WS-DSP-COUNT                PIC Z(07)9.

      * System command fields
       01  WS-CMD-BUFFER               PIC X(512).
       01  WS-CMD-RETURN-CODE          PIC S9(04) VALUE ZEROS.

      * Date/time fields
       01  WS-ACCEPT-DATE              PIC 9(08).
       01  WS-ACCEPT-TIME              PIC 9(08).
       01  WS-RPT-TIMESTAMP            PIC X(26).

      * Separator
       01  WS-SEPARATOR                PIC X(72) VALUE ALL "-".

      * Files open flags
       01  WS-FILES-OPEN               PIC X(01) VALUE "N".
       01  WS-RPT-FILE-OPEN            PIC X(01) VALUE "N".
       01  WS-TEMP-FILE-OPEN           PIC X(01) VALUE "N".

       LINKAGE SECTION.
       01  LS-SESSION-INFO.
           05  LS-CURRENT-USER         PIC X(20).
           05  LS-CURRENT-ROLE         PIC X(05).
           05  LS-SESSION-ACTIVE       PIC X(01).
           05  LS-SESSION-START        PIC X(26).
           05  LS-SESSION-TIMEOUT      PIC 9(04).
           05  LS-LAST-ACTIVITY        PIC X(26).
           05  LS-AUTH-TOKEN           PIC X(40).
           05  LS-LOGIN-ATTEMPTS       PIC 9(02).
           05  LS-MAX-LOGIN-ATTEMPTS   PIC 9(02).

       01  LS-DB2-CONFIG.
           05  LS-DB2-SUBSYSTEM        PIC X(04).
           05  LS-DB2-DATABASE         PIC X(08).
           05  LS-DB2-SCHEMA           PIC X(08).
           05  LS-DB2-USER             PIC X(08).
           05  LS-DB2-PASSWORD         PIC X(08).
           05  LS-DB2-PLAN             PIC X(08).
           05  LS-DB2-COLLECTION       PIC X(18).

       01  LS-SQL-FIELDS.
           05  LS-SQL-STMT             PIC X(1024).
           05  LS-SQL-STMT-LEN         PIC 9(04).
           05  LS-SQL-RETURN-CODE      PIC S9(09).
           05  LS-SQL-ROW-COUNT        PIC 9(09).
           05  LS-SQL-ERROR-MSG        PIC X(256).

       01  LS-REPORT-FIELDS.
           05  LS-REPORT-DATE          PIC X(10).
           05  LS-REPORT-TITLE         PIC X(60).
           05  LS-REPORT-PAGE          PIC 9(04).
           05  LS-REPORT-LINE          PIC 9(02).
           05  LS-REPORT-MAX-LINES     PIC 9(02).
           05  LS-REPORT-FILENAME      PIC X(256).

       01  LS-ERROR-FIELDS.
           05  LS-ERROR-CODE           PIC X(04).
           05  LS-ERROR-MESSAGE        PIC X(80).
           05  LS-ERROR-PROGRAM        PIC X(08).
           05  LS-ERROR-PARAGRAPH      PIC X(30).
           05  LS-ERROR-SEVERITY       PIC 9(01).
           05  LS-ERROR-DETAIL         PIC X(256).

       01  LS-AUDIT-FIELDS.
           05  LS-AUDIT-TIMESTAMP      PIC X(26).
           05  LS-AUDIT-USER           PIC X(20).
           05  LS-AUDIT-ACTION         PIC X(20).
           05  LS-AUDIT-DETAIL         PIC X(256).
           05  LS-AUDIT-SEVERITY       PIC X(04).
           05  LS-AUDIT-PROGRAM        PIC X(08).

       PROCEDURE DIVISION USING
           LS-SESSION-INFO
           LS-DB2-CONFIG
           LS-SQL-FIELDS
           LS-REPORT-FIELDS
           LS-ERROR-FIELDS
           LS-AUDIT-FIELDS.

      ******************************************************************
      * MAIN CONTROL
      ******************************************************************
       0000-MAIN-CONTROL.
           MOVE "RPTGEN" TO WS-PROGRAM-NAME
           PERFORM 0100-OPEN-FILES

      * Check function code from caller
           EVALUATE LS-AUDIT-ACTION
               WHEN "SLOG"
                   PERFORM 5000-SYSTEM-LOG-REPORT
               WHEN OTHER
                   PERFORM 1000-REPORT-MENU
           END-EVALUATE

           PERFORM 0900-CLOSE-FILES
           GOBACK.

      ******************************************************************
      * 0100 - OPEN FILES
      ******************************************************************
       0100-OPEN-FILES.
           OPEN INPUT CUSTOMER-FILE
      * VULNERABILITY V26: File status not checked
           OPEN INPUT ACCOUNT-FILE
      * VULNERABILITY V26: File status not checked
           OPEN INPUT TRANSACTION-FILE
      * VULNERABILITY V26: File status not checked
           MOVE "Y" TO WS-FILES-OPEN.

      ******************************************************************
      * 0900 - CLOSE FILES
      ******************************************************************
       0900-CLOSE-FILES.
           IF WS-FILES-OPEN = "Y"
               CLOSE CUSTOMER-FILE
               CLOSE ACCOUNT-FILE
               CLOSE TRANSACTION-FILE
           END-IF
           IF WS-RPT-FILE-OPEN = "Y"
               CLOSE REPORT-OUTPUT-FILE
               MOVE "N" TO WS-RPT-FILE-OPEN
           END-IF
           IF WS-TEMP-FILE-OPEN = "Y"
               CLOSE TEMP-WORK-FILE
               MOVE "N" TO WS-TEMP-FILE-OPEN
           END-IF.

      ******************************************************************
      * 1000 - REPORT MENU
      ******************************************************************
       1000-REPORT-MENU.
           MOVE "Y" TO WS-RPT-CONTINUE

           PERFORM UNTIL WS-RPT-EXIT-FLG
               DISPLAY " "
               DISPLAY WS-SEPARATOR
               DISPLAY "  REPORT GENERATOR"
               DISPLAY "  User: " LS-CURRENT-USER
               DISPLAY WS-SEPARATOR
               DISPLAY " "
               DISPLAY "  1. Customer Listing Report"
               DISPLAY "  2. Account Summary Report"
               DISPLAY "  3. Transaction Report"
               DISPLAY "  4. Full Audit Report"
               DISPLAY "  5. System Log Report"
               DISPLAY "  6. Regulatory Compliance Report"
               DISPLAY "  7. Custom SQL Report"
               DISPLAY " "
               DISPLAY "  0. Return to Main Menu"
               DISPLAY " "
               DISPLAY "Enter option: " WITH NO ADVANCING
               ACCEPT WS-RPT-MENU-CHOICE FROM CONSOLE

               EVALUATE WS-RPT-MENU-CHOICE
                   WHEN "1"
                       PERFORM 2000-CUSTOMER-LISTING
                   WHEN "2"
                       PERFORM 3000-ACCOUNT-SUMMARY
                   WHEN "3"
                       PERFORM 4000-TRANSACTION-REPORT
                   WHEN "4"
                       PERFORM 4500-FULL-AUDIT-REPORT
                   WHEN "5"
                       PERFORM 5000-SYSTEM-LOG-REPORT
                   WHEN "6"
                       PERFORM 6000-COMPLIANCE-REPORT
                   WHEN "7"
                       PERFORM 7000-CUSTOM-SQL-REPORT
                   WHEN "0"
                       MOVE "N" TO WS-RPT-CONTINUE
                   WHEN OTHER
                       DISPLAY "Invalid option."
               END-EVALUATE
           END-PERFORM.

      ******************************************************************
      * 1500 - OPEN REPORT FILE
      * VULNERABILITY V25: Path traversal in output path
      * VULNERABILITY V22: Insecure temp file
      ******************************************************************
       1500-OPEN-REPORT-FILE.
           DISPLAY "Report output path (blank for default): "
               WITH NO ADVANCING
           ACCEPT WS-RPT-INPUT-PATH FROM CONSOLE

           IF WS-RPT-INPUT-PATH = SPACES
      * VULNERABILITY V22: Predictable temp filename
      *   Uses date-based naming with fixed prefix
               ACCEPT WS-ACCEPT-DATE FROM DATE YYYYMMDD
               ACCEPT WS-ACCEPT-TIME FROM TIME
               STRING "/tmp/altoro_rpt_"
                      WS-ACCEPT-DATE
                      "_" WS-ACCEPT-TIME(1:6) ".tmp"
                      DELIMITED SIZE INTO WS-RPT-OUTPUT-PATH
           ELSE
      * VULNERABILITY V25: Path traversal
      *   User-supplied path used directly without validation
      *   Can write to arbitrary locations like ../../etc/
               MOVE WS-RPT-INPUT-PATH TO WS-RPT-OUTPUT-PATH
           END-IF

           DISPLAY "Writing report to: " WS-RPT-OUTPUT-PATH
           OPEN OUTPUT REPORT-OUTPUT-FILE
      * VULNERABILITY V26: File status not checked
           MOVE "Y" TO WS-RPT-FILE-OPEN

      * Also open temp working file
      * VULNERABILITY V22: Predictable temp filename
           STRING "/tmp/altoro_work_"
                  WS-ACCEPT-DATE ".tmp"
                  DELIMITED SIZE INTO WS-TEMP-FILE-PATH
           OPEN OUTPUT TEMP-WORK-FILE
      * VULNERABILITY V26: File status not checked
           MOVE "Y" TO WS-TEMP-FILE-OPEN

      * Initialize report counters
           MOVE ZEROS TO WS-RPT-PAGE-NUM
           MOVE ZEROS TO WS-RPT-LINE-NUM
           MOVE ZEROS TO WS-RPT-RECORD-COUNT.

      ******************************************************************
      * 1600 - WRITE REPORT HEADER
      ******************************************************************
       1600-WRITE-REPORT-HEADER.
           ADD 1 TO WS-RPT-PAGE-NUM
           ACCEPT WS-ACCEPT-DATE FROM DATE YYYYMMDD
           MOVE WS-ACCEPT-DATE TO WS-RPT-HDR-DATE
           MOVE WS-RPT-PAGE-NUM TO WS-RPT-HDR-PAGE
           MOVE LS-REPORT-TITLE TO WS-RPT-HDR-TITLE

           WRITE RPT-OUTPUT-RECORD FROM WS-RPT-SEPARATOR
      * VULNERABILITY V26: WRITE status not checked
           WRITE RPT-OUTPUT-RECORD FROM WS-RPT-HEADER-1
           WRITE RPT-OUTPUT-RECORD FROM WS-RPT-HEADER-2
           WRITE RPT-OUTPUT-RECORD FROM WS-RPT-SEPARATOR
           MOVE SPACES TO RPT-OUTPUT-RECORD
           WRITE RPT-OUTPUT-RECORD
           MOVE 5 TO WS-RPT-LINE-NUM.

      ******************************************************************
      * 1700 - CHECK PAGE BREAK
      ******************************************************************
       1700-CHECK-PAGE-BREAK.
           IF WS-RPT-LINE-NUM >= WS-RPT-MAX-LINES
               PERFORM 1600-WRITE-REPORT-HEADER
           END-IF.

      ******************************************************************
      * 2000 - CUSTOMER LISTING REPORT
      * VULNERABILITY V21: PII in report (SSN, passwords)
      * VULNERABILITY V23: Sensitive data displayed on screen
      ******************************************************************
       2000-CUSTOMER-LISTING.
           MOVE "CUSTOMER LISTING REPORT"
               TO LS-REPORT-TITLE
           PERFORM 1500-OPEN-REPORT-FILE
           PERFORM 1600-WRITE-REPORT-HEADER

      * Write column headers
           MOVE "USER ID              "
               & "NAME                                "
               & "ROLE  SSN          "
               & "PASSWORD             EMAIL"
               TO RPT-OUTPUT-RECORD
           WRITE RPT-OUTPUT-RECORD
      * VULNERABILITY V26: WRITE status not checked
           WRITE RPT-OUTPUT-RECORD FROM WS-RPT-DASH-LINE
           ADD 2 TO WS-RPT-LINE-NUM

      * Read all customers
           MOVE LOW-VALUES TO CUST-USER-ID
           START CUSTOMER-FILE KEY >= CUST-USER-ID
      * VULNERABILITY V26: START status not checked
           MOVE ZEROS TO WS-RPT-CUST-COUNT

           PERFORM UNTIL WS-CUST-FILE-STATUS NOT = "00"
               READ CUSTOMER-FILE NEXT
               IF WS-CUST-FILE-STATUS = "00"
                   ADD 1 TO WS-RPT-CUST-COUNT
                   PERFORM 1700-CHECK-PAGE-BREAK

      * VULNERABILITY V21: PII Exposure
      *   Full SSN and plaintext password written to report
                   MOVE CUST-USER-ID TO WS-CRPT-USERID
                   STRING CUST-FIRST-NAME " " CUST-LAST-NAME
                          DELIMITED SIZE INTO WS-CRPT-NAME
                   MOVE CUST-ROLE TO WS-CRPT-ROLE
      * V21: SSN in cleartext
                   MOVE CUST-SSN TO WS-CRPT-SSN
      * V21: Password in cleartext
                   MOVE CUST-PASSWORD TO WS-CRPT-PASSWORD
                   MOVE CUST-EMAIL TO WS-CRPT-EMAIL

                   WRITE RPT-OUTPUT-RECORD
                       FROM WS-CUST-RPT-DETAIL
      * VULNERABILITY V26: WRITE status not checked
                   ADD 1 TO WS-RPT-LINE-NUM

      * VULNERABILITY V23: Also display on screen
                   DISPLAY WS-CUST-RPT-DETAIL

      * VULNERABILITY V23: Write to temp file with PII
                   STRING CUST-USER-ID ","
                          CUST-PASSWORD ","
                          CUST-SSN ","
                          CUST-DOB
                          DELIMITED SIZE INTO TEMP-WORK-RECORD
                   WRITE TEMP-WORK-RECORD
      * VULNERABILITY V26: WRITE status not checked
               END-IF
           END-PERFORM

      * Write footer
           WRITE RPT-OUTPUT-RECORD FROM WS-RPT-DASH-LINE
           MOVE WS-RPT-CUST-COUNT TO WS-DSP-COUNT
           STRING "Total Customers: " WS-DSP-COUNT
                  DELIMITED SIZE INTO RPT-OUTPUT-RECORD
           WRITE RPT-OUTPUT-RECORD

           DISPLAY " "
           DISPLAY "Customer report generated. "
                   WS-RPT-CUST-COUNT " records."

      * VULNERABILITY V22: Temp file not cleaned up
      *   /tmp/altoro_work_*.tmp left behind with PII
           CLOSE REPORT-OUTPUT-FILE
           MOVE "N" TO WS-RPT-FILE-OPEN
           CLOSE TEMP-WORK-FILE
           MOVE "N" TO WS-TEMP-FILE-OPEN

      * VULNERABILITY V24: Command injection - format report
           PERFORM 8000-FORMAT-REPORT.

      ******************************************************************
      * 3000 - ACCOUNT SUMMARY REPORT
      * VULNERABILITY V21: PII (account PINs) in report
      ******************************************************************
       3000-ACCOUNT-SUMMARY.
           MOVE "ACCOUNT SUMMARY REPORT"
               TO LS-REPORT-TITLE
           PERFORM 1500-OPEN-REPORT-FILE
           PERFORM 1600-WRITE-REPORT-HEADER

      * Write column headers
           MOVE "ACCOUNT ID        "
               & "OWNER                "
               & "NAME                  "
               & "TY BALANCE          "
               & "S PIN"
               TO RPT-OUTPUT-RECORD
           WRITE RPT-OUTPUT-RECORD
           WRITE RPT-OUTPUT-RECORD FROM WS-RPT-DASH-LINE
           ADD 2 TO WS-RPT-LINE-NUM

           MOVE LOW-VALUES TO ACCT-ID
           START ACCOUNT-FILE KEY >= ACCT-ID
           MOVE ZEROS TO WS-RPT-ACCT-COUNT
           MOVE ZEROS TO WS-RPT-TOTAL-BALANCE

           PERFORM UNTIL WS-ACCT-FILE-STATUS NOT = "00"
               READ ACCOUNT-FILE NEXT
               IF WS-ACCT-FILE-STATUS = "00"
                   ADD 1 TO WS-RPT-ACCT-COUNT
                   PERFORM 1700-CHECK-PAGE-BREAK

                   MOVE ACCT-ID TO WS-ARPT-ID
                   MOVE ACCT-OWNER-ID TO WS-ARPT-OWNER
                   MOVE ACCT-NAME TO WS-ARPT-NAME
                   MOVE ACCT-TYPE TO WS-ARPT-TYPE
                   MOVE ACCT-BALANCE TO WS-ARPT-BALANCE
                   MOVE ACCT-STATUS TO WS-ARPT-STATUS
      * VULNERABILITY V21: PIN in report
                   MOVE ACCT-PIN TO WS-ARPT-PIN

                   WRITE RPT-OUTPUT-RECORD
                       FROM WS-ACCT-RPT-DETAIL
      * VULNERABILITY V26: WRITE status not checked
                   ADD 1 TO WS-RPT-LINE-NUM

      * VULNERABILITY V23: Display with PIN
                   DISPLAY WS-ACCT-RPT-DETAIL

      * Accumulate totals
      * VULNERABILITY V09: No ON SIZE ERROR
                   COMPUTE WS-RPT-TOTAL-BALANCE =
                       WS-RPT-TOTAL-BALANCE + ACCT-BALANCE
               END-IF
           END-PERFORM

      * Write footer
           WRITE RPT-OUTPUT-RECORD FROM WS-RPT-DASH-LINE
           MOVE WS-RPT-ACCT-COUNT TO WS-DSP-COUNT
           MOVE WS-RPT-TOTAL-BALANCE TO WS-DSP-BALANCE
           STRING "Total Accounts: " WS-DSP-COUNT
                  "  Total Balance: $" WS-DSP-BALANCE
                  DELIMITED SIZE INTO RPT-OUTPUT-RECORD
           WRITE RPT-OUTPUT-RECORD

           DISPLAY " "
           DISPLAY "Account report generated. "
                   WS-RPT-ACCT-COUNT " accounts."
           DISPLAY "Total balance: $" WS-DSP-BALANCE

           CLOSE REPORT-OUTPUT-FILE
           MOVE "N" TO WS-RPT-FILE-OPEN
           IF WS-TEMP-FILE-OPEN = "Y"
               CLOSE TEMP-WORK-FILE
               MOVE "N" TO WS-TEMP-FILE-OPEN
           END-IF

           PERFORM 8000-FORMAT-REPORT.

      ******************************************************************
      * 4000 - TRANSACTION REPORT
      ******************************************************************
       4000-TRANSACTION-REPORT.
           MOVE "TRANSACTION REPORT" TO LS-REPORT-TITLE
           PERFORM 1500-OPEN-REPORT-FILE
           PERFORM 1600-WRITE-REPORT-HEADER

           DISPLAY "Account ID (blank for all): "
               WITH NO ADVANCING
           ACCEPT WS-RPT-INPUT-ACCT-ID FROM CONSOLE

           DISPLAY "Start Date (YYYY-MM-DD): " WITH NO ADVANCING
           ACCEPT WS-RPT-INPUT-START-DATE FROM CONSOLE

           DISPLAY "End Date (YYYY-MM-DD): " WITH NO ADVANCING
           ACCEPT WS-RPT-INPUT-END-DATE FROM CONSOLE

      * Write column headers
           MOVE "TRAN ID     "
               & "DATE        "
               & "ACCOUNT ID        "
               & "TY AMOUNT          "
               & "DESCRIPTION                     "
               & "USER"
               TO RPT-OUTPUT-RECORD
           WRITE RPT-OUTPUT-RECORD
           WRITE RPT-OUTPUT-RECORD FROM WS-RPT-DASH-LINE
           ADD 2 TO WS-RPT-LINE-NUM

           MOVE LOW-VALUES TO TRAN-ID
           START TRANSACTION-FILE KEY >= TRAN-ID
           MOVE ZEROS TO WS-RPT-TRAN-COUNT
           MOVE ZEROS TO WS-RPT-TOTAL-CREDITS
           MOVE ZEROS TO WS-RPT-TOTAL-DEBITS

           PERFORM UNTIL WS-TRAN-FILE-STATUS NOT = "00"
               READ TRANSACTION-FILE NEXT
               IF WS-TRAN-FILE-STATUS = "00"
      * Filter by account if specified
                   IF WS-RPT-INPUT-ACCT-ID = SPACES
                       OR TRAN-ACCOUNT-ID =
                          WS-RPT-INPUT-ACCT-ID

                       ADD 1 TO WS-RPT-TRAN-COUNT
                       PERFORM 1700-CHECK-PAGE-BREAK

                       MOVE TRAN-ID TO WS-TRPT-ID
                       MOVE TRAN-DATE TO WS-TRPT-DATE
                       MOVE TRAN-ACCOUNT-ID TO WS-TRPT-ACCT
                       MOVE TRAN-TYPE TO WS-TRPT-TYPE
                       MOVE TRAN-AMOUNT TO WS-TRPT-AMOUNT
                       MOVE TRAN-DESCRIPTION TO WS-TRPT-DESC
      * VULNERABILITY V21: User ID in report
                       MOVE TRAN-USER-ID TO WS-TRPT-USER

                       WRITE RPT-OUTPUT-RECORD
                           FROM WS-TRAN-RPT-DETAIL
      * VULNERABILITY V26: WRITE status not checked
                       ADD 1 TO WS-RPT-LINE-NUM

      * Accumulate
                       IF TRAN-AMOUNT >= 0
                           COMPUTE WS-RPT-TOTAL-CREDITS =
                               WS-RPT-TOTAL-CREDITS
                               + TRAN-AMOUNT
                       ELSE
                           COMPUTE WS-RPT-TOTAL-DEBITS =
                               WS-RPT-TOTAL-DEBITS
                               + TRAN-AMOUNT
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

      * Write footer
           WRITE RPT-OUTPUT-RECORD FROM WS-RPT-DASH-LINE
           MOVE WS-RPT-TOTAL-CREDITS TO WS-DSP-AMOUNT
           STRING "Total Credits: $" WS-DSP-AMOUNT
                  DELIMITED SIZE INTO RPT-OUTPUT-RECORD
           WRITE RPT-OUTPUT-RECORD
           MOVE WS-RPT-TOTAL-DEBITS TO WS-DSP-AMOUNT
           STRING "Total Debits:  $" WS-DSP-AMOUNT
                  DELIMITED SIZE INTO RPT-OUTPUT-RECORD
           WRITE RPT-OUTPUT-RECORD

           DISPLAY " "
           DISPLAY "Transaction report generated. "
                   WS-RPT-TRAN-COUNT " transactions."

           CLOSE REPORT-OUTPUT-FILE
           MOVE "N" TO WS-RPT-FILE-OPEN
           IF WS-TEMP-FILE-OPEN = "Y"
               CLOSE TEMP-WORK-FILE
               MOVE "N" TO WS-TEMP-FILE-OPEN
           END-IF

           PERFORM 8000-FORMAT-REPORT.

      ******************************************************************
      * 4500 - FULL AUDIT REPORT
      * VULNERABILITY V21: Exposes all PII in one report
      * VULNERABILITY V23: Displays everything to screen
      ******************************************************************
       4500-FULL-AUDIT-REPORT.
           MOVE "FULL AUDIT REPORT - CONFIDENTIAL"
               TO LS-REPORT-TITLE
           PERFORM 1500-OPEN-REPORT-FILE
           PERFORM 1600-WRITE-REPORT-HEADER

      * VULNERABILITY V21/V23: Full PII dump for all customers
           MOVE "=== CUSTOMER PII DATA ===" TO RPT-OUTPUT-RECORD
           WRITE RPT-OUTPUT-RECORD

           MOVE LOW-VALUES TO CUST-USER-ID
           START CUSTOMER-FILE KEY >= CUST-USER-ID

           PERFORM UNTIL WS-CUST-FILE-STATUS NOT = "00"
               READ CUSTOMER-FILE NEXT
               IF WS-CUST-FILE-STATUS = "00"
                   ADD 1 TO WS-RPT-RECORD-COUNT
                   PERFORM 1700-CHECK-PAGE-BREAK

      * VULNERABILITY V21: ALL PII fields written to report
                   STRING "USER:" CUST-USER-ID
                          " PWD:" CUST-PASSWORD
                          " SSN:" CUST-SSN
                          " DOB:" CUST-DOB
                          " EMAIL:" CUST-EMAIL
                          DELIMITED SIZE INTO RPT-OUTPUT-RECORD
                   WRITE RPT-OUTPUT-RECORD
      * VULNERABILITY V26: WRITE status not checked
                   ADD 1 TO WS-RPT-LINE-NUM

      * VULNERABILITY V23: Display all PII on screen
                   DISPLAY RPT-OUTPUT-RECORD

      * Write security Q&A to report too
                   STRING "  SecQ:" CUST-SECURITY-QUESTION
                          DELIMITED SIZE INTO RPT-OUTPUT-RECORD
                   WRITE RPT-OUTPUT-RECORD
                   STRING "  SecA:" CUST-SECURITY-ANSWER
                          DELIMITED SIZE INTO RPT-OUTPUT-RECORD
                   WRITE RPT-OUTPUT-RECORD
                   ADD 2 TO WS-RPT-LINE-NUM
               END-IF
           END-PERFORM

      * Now add all account data with PINs
           MOVE "=== ACCOUNT DATA WITH PINS ===" TO RPT-OUTPUT-RECORD
           WRITE RPT-OUTPUT-RECORD

           MOVE LOW-VALUES TO ACCT-ID
           START ACCOUNT-FILE KEY >= ACCT-ID

           PERFORM UNTIL WS-ACCT-FILE-STATUS NOT = "00"
               READ ACCOUNT-FILE NEXT
               IF WS-ACCT-FILE-STATUS = "00"
                   ADD 1 TO WS-RPT-RECORD-COUNT
                   PERFORM 1700-CHECK-PAGE-BREAK

      * VULNERABILITY V21: PIN and balance in report
                   MOVE ACCT-BALANCE TO WS-DSP-BALANCE
                   STRING "ACCT:" ACCT-ID
                          " OWNER:" ACCT-OWNER-ID
                          " BAL:$" WS-DSP-BALANCE
                          " PIN:" ACCT-PIN
                          DELIMITED SIZE INTO RPT-OUTPUT-RECORD
                   WRITE RPT-OUTPUT-RECORD
                   ADD 1 TO WS-RPT-LINE-NUM

      * VULNERABILITY V23: Display to screen
                   DISPLAY RPT-OUTPUT-RECORD
               END-IF
           END-PERFORM

           WRITE RPT-OUTPUT-RECORD FROM WS-RPT-SEPARATOR
           STRING "Total records: " WS-RPT-RECORD-COUNT
                  DELIMITED SIZE INTO RPT-OUTPUT-RECORD
           WRITE RPT-OUTPUT-RECORD

           DISPLAY " "
           DISPLAY "Full audit report generated."
           DISPLAY "Records: " WS-RPT-RECORD-COUNT

           CLOSE REPORT-OUTPUT-FILE
           MOVE "N" TO WS-RPT-FILE-OPEN
           IF WS-TEMP-FILE-OPEN = "Y"
               CLOSE TEMP-WORK-FILE
               MOVE "N" TO WS-TEMP-FILE-OPEN
           END-IF.

      ******************************************************************
      * 5000 - SYSTEM LOG REPORT
      * VULNERABILITY V23: Displays raw log data with credentials
      ******************************************************************
       5000-SYSTEM-LOG-REPORT.
           DISPLAY " "
           DISPLAY "=== SYSTEM LOG REPORT ==="
      * VULNERABILITY V23/V27: Display system paths
           DISPLAY "Log directory: " WS-SYSTEM-LOG-PATH
           DISPLAY "Reading audit trail..."

           OPEN INPUT AUDIT-TRAIL-FILE
      * VULNERABILITY V26: File status not checked

           IF WS-AUDIT-TRAIL-STATUS = "00"
               PERFORM UNTIL WS-AUDIT-TRAIL-STATUS NOT = "00"
                   READ AUDIT-TRAIL-FILE
                   IF WS-AUDIT-TRAIL-STATUS = "00"
      * VULNERABILITY V23: Display raw log entries
      *   Logs contain passwords, SSNs, tokens
                       DISPLAY AUDIT-TRAIL-RECORD
                   END-IF
               END-PERFORM
               CLOSE AUDIT-TRAIL-FILE
           ELSE
               DISPLAY "Unable to open audit trail."
           END-IF.

      ******************************************************************
      * 6000 - REGULATORY COMPLIANCE REPORT
      * VULNERABILITY V21: Exports all PII for "compliance"
      * VULNERABILITY V25: Path traversal in export
      ******************************************************************
       6000-COMPLIANCE-REPORT.
           MOVE "REGULATORY COMPLIANCE REPORT"
               TO LS-REPORT-TITLE
           PERFORM 1500-OPEN-REPORT-FILE
           PERFORM 1600-WRITE-REPORT-HEADER

           DISPLAY " "
           DISPLAY "Generating compliance data export..."

      * VULNERABILITY V21: Full PII export justified as
      *   "compliance requirement"
           MOVE "=== PCI-DSS DATA INVENTORY ===" TO RPT-OUTPUT-RECORD
           WRITE RPT-OUTPUT-RECORD

           MOVE LOW-VALUES TO CUST-USER-ID
           START CUSTOMER-FILE KEY >= CUST-USER-ID

           PERFORM UNTIL WS-CUST-FILE-STATUS NOT = "00"
               READ CUSTOMER-FILE NEXT
               IF WS-CUST-FILE-STATUS = "00"
                   PERFORM 1700-CHECK-PAGE-BREAK

      * VULNERABILITY V21: PII in "compliance" report
                   STRING "CARDHOLDER: "
                          CUST-FIRST-NAME " " CUST-LAST-NAME
                          " SSN=" CUST-SSN
                          " DOB=" CUST-DOB
                          " PWD=" CUST-PASSWORD
                          DELIMITED SIZE INTO RPT-OUTPUT-RECORD
                   WRITE RPT-OUTPUT-RECORD
                   ADD 1 TO WS-RPT-LINE-NUM
               END-IF
           END-PERFORM

      * Now list account card numbers
           MOVE "=== PAYMENT CARD INVENTORY ===" TO RPT-OUTPUT-RECORD
           WRITE RPT-OUTPUT-RECORD

           MOVE LOW-VALUES TO ACCT-ID
           START ACCOUNT-FILE KEY >= ACCT-ID

           PERFORM UNTIL WS-ACCT-FILE-STATUS NOT = "00"
               READ ACCOUNT-FILE NEXT
               IF WS-ACCT-FILE-STATUS = "00"
                   IF ACCT-TYPE = "CC"
                       PERFORM 1700-CHECK-PAGE-BREAK
      * VULNERABILITY V21: Full PAN in report
                       MOVE ACCT-BALANCE TO WS-DSP-BALANCE
                       STRING "CARD: " ACCT-ID
                              " HOLDER: " ACCT-OWNER-ID
                              " BALANCE: $" WS-DSP-BALANCE
                              " PIN: " ACCT-PIN
                              DELIMITED SIZE
                              INTO RPT-OUTPUT-RECORD
                       WRITE RPT-OUTPUT-RECORD
                       ADD 1 TO WS-RPT-LINE-NUM

      * VULNERABILITY V23: Display PAN on screen
                       DISPLAY RPT-OUTPUT-RECORD
                   END-IF
               END-IF
           END-PERFORM

           WRITE RPT-OUTPUT-RECORD FROM WS-RPT-SEPARATOR

           DISPLAY " "
           DISPLAY "Compliance report generated."

           CLOSE REPORT-OUTPUT-FILE
           MOVE "N" TO WS-RPT-FILE-OPEN
           IF WS-TEMP-FILE-OPEN = "Y"
               CLOSE TEMP-WORK-FILE
               MOVE "N" TO WS-TEMP-FILE-OPEN
           END-IF.

      ******************************************************************
      * 7000 - CUSTOM SQL REPORT
      * VULNERABILITY V07: Direct SQL execution from user input
      ******************************************************************
       7000-CUSTOM-SQL-REPORT.
           DISPLAY " "
           DISPLAY "=== CUSTOM SQL REPORT ==="
           DISPLAY " "
           DISPLAY "Enter SQL query: " WITH NO ADVANCING
           ACCEPT WS-RPT-INPUT-PATH FROM CONSOLE

      * VULNERABILITY V07: Direct SQL injection
      *   User can enter ANY SQL statement for execution
           MOVE WS-RPT-INPUT-PATH TO WS-SQL-STMT

      * VULNERABILITY V23: Display the SQL
           DISPLAY "Executing: " WS-SQL-STMT

      >>IF DB2-ENABLED IS DEFINED
           EXEC SQL
               EXECUTE IMMEDIATE :WS-SQL-STMT
           END-EXEC
      >>END-IF
      * VULNERABILITY V06: SQLCODE not checked

           DISPLAY "Query executed.".

      ******************************************************************
      * 8000 - FORMAT REPORT OUTPUT
      * VULNERABILITY V24: Command injection in formatting
      ******************************************************************
       8000-FORMAT-REPORT.
           DISPLAY " "
           DISPLAY "Format report for printing? (Y/N): "
               WITH NO ADVANCING
           ACCEPT WS-RPT-INPUT-FORMAT FROM CONSOLE

           IF WS-RPT-INPUT-FORMAT = "Y"
      * VULNERABILITY V24: Command injection
      *   Report path used in system command without sanitization
               MOVE SPACES TO WS-CMD-BUFFER
               STRING "lp -d ALTOROPRT "
                      WS-RPT-OUTPUT-PATH
                      DELIMITED SIZE INTO WS-CMD-BUFFER

      * VULNERABILITY V24: User-controlled path in SYSTEM call
               DISPLAY "Sending to printer..."
               CALL "SYSTEM" USING WS-CMD-BUFFER
      * VULNERABILITY V20: Return code ignored

      * VULNERABILITY V24: Second command injection
      *   Convert to PDF using user-influenced path
               MOVE SPACES TO WS-CMD-BUFFER
               STRING "enscript -p "
                      WS-RPT-OUTPUT-PATH ".pdf "
                      WS-RPT-OUTPUT-PATH
                      DELIMITED SIZE INTO WS-CMD-BUFFER
               CALL "SYSTEM" USING WS-CMD-BUFFER
      * VULNERABILITY V20: Return code ignored

               DISPLAY "Report formatted and sent to printer."
           END-IF.

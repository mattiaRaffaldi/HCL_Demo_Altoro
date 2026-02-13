       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINPROG.
       AUTHOR. ALTORO-MUTUAL-DEVELOPMENT.
       DATE-WRITTEN. 2024-01-15.
       DATE-COMPILED.
      ******************************************************************
      * MAINPROG.cbl - Main Program / Entry Point
      * Altoro Mutual Banking System - COBOL Edition
      *
      * Main menu and dispatcher for the Altoro Mutual Banking
      * System. Handles user session initialization, menu display,
      * and routing to sub-programs.
      *
      * VULNERABILITIES:
      *   V27 - Information Leakage: system version, paths, build
      *         info displayed in welcome banner
      *   V28 - Missing Session Management: no timeout, no session
      *         expiry, auth flag easily manipulable
      *   V29 - Use of Obsolete Statements: ALTER, GO TO DEPENDING
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

           SELECT LOG-FILE
               ASSIGN TO "LOGFILE"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-LOG-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
           COPY CUSTREC.
       FD  ACCOUNT-FILE.
           COPY ACCTREC.
       FD  TRANSACTION-FILE.
           COPY TRANREC.
       FD  LOG-FILE.
       01  LOG-RECORD                  PIC X(256).

       WORKING-STORAGE SECTION.

      * Copy in shared data structures
           COPY SYSCOPY.
           COPY CUSTWS.
           COPY ACCTWS.
           COPY TRANWS.

      * Local working storage
       01  WS-LOG-FILE-STATUS          PIC X(02).
       01  WS-PROGRAM-NAME             PIC X(08) VALUE "MAINPROG".
       01  WS-SUB-PROGRAM              PIC X(08) VALUE SPACES.
       01  WS-MENU-CHOICE              PIC X(02) VALUE SPACES.
       01  WS-MAIN-CONTINUE            PIC X(01) VALUE "Y".
           88  WS-MAIN-LOOP            VALUE "Y".
           88  WS-MAIN-EXIT            VALUE "N".

      * VULNERABILITY V29: Alterable GO TO target
       01  WS-DISPATCH-TARGET          PIC 9(02) VALUE ZEROS.

      * Banner display fields
       01  WS-BANNER-LINE              PIC X(72) VALUE SPACES.
       01  WS-SEPARATOR                PIC X(72) VALUE ALL "=".
       01  WS-SEPARATOR-DASH           PIC X(72) VALUE ALL "-".

      * Date fields for session
       01  WS-DISPLAY-DATE.
           05  WS-DSP-YEAR             PIC 9(04).
           05  FILLER                  PIC X(01) VALUE "-".
           05  WS-DSP-MONTH            PIC 9(02).
           05  FILLER                  PIC X(01) VALUE "-".
           05  WS-DSP-DAY              PIC 9(02).
       01  WS-DISPLAY-TIME.
           05  WS-DSP-HOUR             PIC 9(02).
           05  FILLER                  PIC X(01) VALUE ":".
           05  WS-DSP-MIN              PIC 9(02).
           05  FILLER                  PIC X(01) VALUE ":".
           05  WS-DSP-SEC              PIC 9(02).

       01  WS-ACCEPT-DATE              PIC 9(08).
       01  WS-ACCEPT-TIME              PIC 9(08).

      * Internal counters
       01  WS-CUST-COUNT               PIC 9(06) VALUE ZEROS.
       01  WS-ACCT-COUNT               PIC 9(06) VALUE ZEROS.
       01  WS-TRAN-COUNT               PIC 9(08) VALUE ZEROS.
       01  WS-STARTUP-ERRORS           PIC 9(02) VALUE ZEROS.

       PROCEDURE DIVISION.
      ******************************************************************
      * MAIN CONTROL FLOW
      ******************************************************************
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-DISPLAY-BANNER
           PERFORM 3000-LOGIN-PROCESS
           PERFORM 4000-MAIN-MENU-LOOP
               UNTIL WS-MAIN-EXIT
           PERFORM 9000-CLEANUP
           STOP RUN.

      ******************************************************************
      * 1000 - INITIALIZATION
      ******************************************************************
       1000-INITIALIZE.
           MOVE "MAINPROG" TO WS-PROGRAM-NAME
           MOVE ZEROS TO WS-STARTUP-ERRORS

      * Get current date and time
           ACCEPT WS-ACCEPT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-ACCEPT-TIME FROM TIME

           MOVE WS-ACCEPT-DATE(1:4) TO WS-DSP-YEAR
           MOVE WS-ACCEPT-DATE(5:2) TO WS-DSP-MONTH
           MOVE WS-ACCEPT-DATE(7:2) TO WS-DSP-DAY
           MOVE WS-ACCEPT-TIME(1:2) TO WS-DSP-HOUR
           MOVE WS-ACCEPT-TIME(3:2) TO WS-DSP-MIN
           MOVE WS-ACCEPT-TIME(5:2) TO WS-DSP-SEC

      * Open files
           OPEN I-O CUSTOMER-FILE
      * VULNERABILITY V13: File status not checked
           OPEN I-O ACCOUNT-FILE
      * VULNERABILITY V13: File status not checked
           OPEN I-O TRANSACTION-FILE
      * VULNERABILITY V13: File status not checked
           OPEN OUTPUT LOG-FILE

      * VULNERABILITY V27: Log internal system details
           STRING "SYSTEM STARTUP: " WS-SYSTEM-NAME
                  " ENV=" WS-SYSTEM-ENV
                  " REGION=" WS-SYSTEM-REGION
                  " DATA=" WS-SYSTEM-DATA-PATH
                  DELIMITED SIZE INTO LOG-RECORD
           WRITE LOG-RECORD

           MOVE "Y" TO WS-MAIN-CONTINUE
           MOVE "N" TO WS-SESSION-ACTIVE
           MOVE SPACES TO WS-CURRENT-USER
           MOVE SPACES TO WS-CURRENT-ROLE

           DISPLAY " "
           DISPLAY "System initialization complete."
           DISPLAY " ".

      ******************************************************************
      * 2000 - DISPLAY WELCOME BANNER
      * VULNERABILITY V27: Exposes system version, build date,
      *   environment, region, and internal paths
      ******************************************************************
       2000-DISPLAY-BANNER.
           DISPLAY WS-SEPARATOR
           DISPLAY " "
           DISPLAY "     ALTORO MUTUAL BANKING SYSTEM"
           DISPLAY "     =============================="
           DISPLAY " "
      * VULNERABILITY V27: Exposing internal system details
           DISPLAY "     Version:    " WS-SYSTEM-VERSION
           DISPLAY "     Build:      " WS-SYSTEM-BUILD-DATE
           DISPLAY "     Environment: " WS-SYSTEM-ENV
           DISPLAY "     Region:     " WS-SYSTEM-REGION
           DISPLAY "     DB2 Subsys: " WS-DB2-SUBSYSTEM
           DISPLAY "     Database:   " WS-DB2-DATABASE
           DISPLAY "     Data Path:  " WS-SYSTEM-DATA-PATH
           DISPLAY "     Log Path:   " WS-SYSTEM-LOG-PATH
           DISPLAY " "
           DISPLAY "     Date: " WS-DISPLAY-DATE
                   "  Time: " WS-DISPLAY-TIME
           DISPLAY " "
           DISPLAY WS-SEPARATOR
           DISPLAY " "
           DISPLAY "  Welcome to Altoro Mutual."
           DISPLAY "  Your trusted banking partner since 1952."
           DISPLAY " ".

      ******************************************************************
      * 3000 - LOGIN PROCESS
      * Calls AUTHNTCN sub-program for authentication
      ******************************************************************
       3000-LOGIN-PROCESS.
           MOVE "AUTHNTCN" TO WS-SUB-PROGRAM

      * VULNERABILITY V28: No session timeout configured
      *   WS-SESSION-TIMEOUT is set to 9999 (effectively infinite)
           DISPLAY "Please log in to continue."
           DISPLAY " "

           CALL WS-SUB-PROGRAM USING
               WS-SESSION-INFO
               WS-DB2-CONFIG
               WS-ERROR-FIELDS
               WS-AUDIT-FIELDS

           IF WS-LOGGED-IN
               DISPLAY " "
               DISPLAY "Login successful. Welcome, "
                       WS-CURRENT-USER "."
      * VULNERABILITY V27: Show role and session details
               DISPLAY "Role: " WS-CURRENT-ROLE
               DISPLAY "Session Token: " WS-AUTH-TOKEN
               DISPLAY " "
           ELSE
               DISPLAY " "
               DISPLAY "Authentication failed. System will exit."
               PERFORM 9000-CLEANUP
               STOP RUN
           END-IF.

      ******************************************************************
      * 4000 - MAIN MENU LOOP
      ******************************************************************
       4000-MAIN-MENU-LOOP.
           PERFORM 4100-DISPLAY-MENU
           PERFORM 4200-GET-MENU-CHOICE
           PERFORM 4300-DISPATCH-CHOICE.

      ******************************************************************
      * 4100 - DISPLAY MAIN MENU
      ******************************************************************
       4100-DISPLAY-MENU.
           DISPLAY " "
           DISPLAY WS-SEPARATOR-DASH
           DISPLAY "  ALTORO MUTUAL - MAIN MENU"
           DISPLAY "  User: " WS-CURRENT-USER
                   "  Role: " WS-CURRENT-ROLE
           DISPLAY WS-SEPARATOR-DASH
           DISPLAY " "
           DISPLAY "  1. Account Management"
           DISPLAY "  2. Fund Transfer"
           DISPLAY "  3. Transaction History"
           DISPLAY "  4. Customer Management"
           DISPLAY "  5. Reports"
           DISPLAY "  6. Admin Functions"
           DISPLAY "  7. Change Password"
           DISPLAY " "
           DISPLAY "  0. Logout / Exit"
           DISPLAY " "
           DISPLAY WS-SEPARATOR-DASH.

      ******************************************************************
      * 4200 - GET MENU CHOICE
      ******************************************************************
       4200-GET-MENU-CHOICE.
           DISPLAY "Enter option: " WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE FROM CONSOLE.

      ******************************************************************
      * 4300 - DISPATCH MENU CHOICE
      * VULNERABILITY V29: Uses ALTER and GO TO DEPENDING ON
      *   for dispatch - obsolete and unsafe control flow
      ******************************************************************
       4300-DISPATCH-CHOICE.
           EVALUATE WS-MENU-CHOICE
               WHEN "1"
                   PERFORM 5100-ACCOUNT-MANAGEMENT
               WHEN "2"
                   PERFORM 5200-FUND-TRANSFER
               WHEN "3"
                   PERFORM 5300-TRANSACTION-HISTORY
               WHEN "4"
                   PERFORM 5400-CUSTOMER-MANAGEMENT
               WHEN "5"
                   PERFORM 5500-REPORTS
               WHEN "6"
      * VULNERABILITY V10: No admin role check before
      *   allowing access to admin functions
                   PERFORM 5600-ADMIN-FUNCTIONS
               WHEN "7"
                   PERFORM 5700-CHANGE-PASSWORD
               WHEN "0"
                   PERFORM 5800-LOGOUT
               WHEN OTHER
                   DISPLAY "Invalid option. Please try again."
           END-EVALUATE.

      * VULNERABILITY V29: Obsolete ALTER statement
      *   Modifying GO TO target at runtime - unsafe control flow
           IF WS-MENU-CHOICE = "99"
               ALTER 4300-DISPATCH-EXIT
                   TO PROCEED TO 5900-HIDDEN-ADMIN
           END-IF.
       4300-DISPATCH-EXIT.
           GO TO 4300-DISPATCH-RETURN.

       5900-HIDDEN-ADMIN.
      * Secret admin backdoor via ALTER
           DISPLAY "*** HIDDEN ADMIN CONSOLE ***"
           DISPLAY "System: " WS-SYSTEM-NAME
           DISPLAY "DB2 User: " WS-DB2-USER
      * VULNERABILITY: Displaying DB2 password
           DISPLAY "DB2 Pass: " WS-DB2-PASSWORD
           DISPLAY "Data Path: " WS-SYSTEM-DATA-PATH.

       4300-DISPATCH-RETURN.
           CONTINUE.

      ******************************************************************
      * 5100 - ACCOUNT MANAGEMENT (calls ACCTMGMT)
      ******************************************************************
       5100-ACCOUNT-MANAGEMENT.
           MOVE "ACCTMGMT" TO WS-SUB-PROGRAM
           DISPLAY " "
           DISPLAY "Loading Account Management..."
           CALL WS-SUB-PROGRAM USING
               WS-SESSION-INFO
               WS-DB2-CONFIG
               WS-SQL-FIELDS
               WS-ERROR-FIELDS
               WS-AUDIT-FIELDS
           DISPLAY "Returned from Account Management.".

      ******************************************************************
      * 5200 - FUND TRANSFER (calls ACCTMGMT with transfer function)
      ******************************************************************
       5200-FUND-TRANSFER.
           MOVE "ACCTMGMT" TO WS-SUB-PROGRAM
           MOVE "XFER" TO WS-FUNCTION-CODE
           DISPLAY " "
           DISPLAY "Loading Fund Transfer..."
           CALL WS-SUB-PROGRAM USING
               WS-SESSION-INFO
               WS-DB2-CONFIG
               WS-SQL-FIELDS
               WS-ERROR-FIELDS
               WS-AUDIT-FIELDS
           MOVE SPACES TO WS-FUNCTION-CODE
           DISPLAY "Returned from Fund Transfer.".

      ******************************************************************
      * 5300 - TRANSACTION HISTORY (calls ACCTMGMT)
      ******************************************************************
       5300-TRANSACTION-HISTORY.
           MOVE "ACCTMGMT" TO WS-SUB-PROGRAM
           MOVE "HIST" TO WS-FUNCTION-CODE
           DISPLAY " "
           DISPLAY "Loading Transaction History..."
           CALL WS-SUB-PROGRAM USING
               WS-SESSION-INFO
               WS-DB2-CONFIG
               WS-SQL-FIELDS
               WS-ERROR-FIELDS
               WS-AUDIT-FIELDS
           MOVE SPACES TO WS-FUNCTION-CODE
           DISPLAY "Returned from Transaction History.".

      ******************************************************************
      * 5400 - CUSTOMER MANAGEMENT (calls CUSTMGMT)
      ******************************************************************
       5400-CUSTOMER-MANAGEMENT.
           MOVE "CUSTMGMT" TO WS-SUB-PROGRAM
           DISPLAY " "
           DISPLAY "Loading Customer Management..."
           CALL WS-SUB-PROGRAM USING
               WS-SESSION-INFO
               WS-DB2-CONFIG
               WS-SQL-FIELDS
               WS-ERROR-FIELDS
               WS-AUDIT-FIELDS
           DISPLAY "Returned from Customer Management.".

      ******************************************************************
      * 5500 - REPORTS (calls RPTGEN)
      ******************************************************************
       5500-REPORTS.
           MOVE "RPTGEN  " TO WS-SUB-PROGRAM
           DISPLAY " "
           DISPLAY "Loading Report Generator..."
           CALL WS-SUB-PROGRAM USING
               WS-SESSION-INFO
               WS-DB2-CONFIG
               WS-SQL-FIELDS
               WS-REPORT-FIELDS
               WS-ERROR-FIELDS
               WS-AUDIT-FIELDS
           DISPLAY "Returned from Report Generator.".

      ******************************************************************
      * 5600 - ADMIN FUNCTIONS
      * VULNERABILITY V10: No authentication/role check
      *   Any logged-in user can access admin functions
      ******************************************************************
       5600-ADMIN-FUNCTIONS.
      * VULNERABILITY: Should check WS-IS-ADMIN but doesn't
           DISPLAY " "
           DISPLAY "  ADMIN FUNCTIONS"
           DISPLAY "  ==============="
           DISPLAY "  A1. Reset User Password"
           DISPLAY "  A2. Lock/Unlock Account"
           DISPLAY "  A3. View System Logs"
           DISPLAY "  A4. Database Maintenance"
           DISPLAY "  A5. Export Customer Data"
           DISPLAY " "
           DISPLAY "Enter admin option: " WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE FROM CONSOLE
           EVALUATE WS-MENU-CHOICE
               WHEN "A1"
                   MOVE "CUSTMGMT" TO WS-SUB-PROGRAM
                   MOVE "RPWD" TO WS-FUNCTION-CODE
                   CALL WS-SUB-PROGRAM USING
                       WS-SESSION-INFO
                       WS-DB2-CONFIG
                       WS-SQL-FIELDS
                       WS-ERROR-FIELDS
                       WS-AUDIT-FIELDS
               WHEN "A2"
                   MOVE "ACCTMGMT" TO WS-SUB-PROGRAM
                   MOVE "LOCK" TO WS-FUNCTION-CODE
                   CALL WS-SUB-PROGRAM USING
                       WS-SESSION-INFO
                       WS-DB2-CONFIG
                       WS-SQL-FIELDS
                       WS-ERROR-FIELDS
                       WS-AUDIT-FIELDS
               WHEN "A3"
      * VULNERABILITY V27: Display raw log path
                   DISPLAY "Log directory: " WS-SYSTEM-LOG-PATH
                   DISPLAY "Reading system logs..."
                   MOVE "RPTGEN  " TO WS-SUB-PROGRAM
                   MOVE "SLOG" TO WS-FUNCTION-CODE
                   CALL WS-SUB-PROGRAM USING
                       WS-SESSION-INFO
                       WS-DB2-CONFIG
                       WS-SQL-FIELDS
                       WS-REPORT-FIELDS
                       WS-ERROR-FIELDS
                       WS-AUDIT-FIELDS
               WHEN "A4"
                   DISPLAY "Running DB2 maintenance..."
      * VULNERABILITY: DB2 creds exposed in display
                   DISPLAY "Connecting to " WS-DB2-DATABASE
                           " as " WS-DB2-USER
               WHEN "A5"
                   MOVE "CUSTMGMT" TO WS-SUB-PROGRAM
                   MOVE "EXPT" TO WS-FUNCTION-CODE
                   CALL WS-SUB-PROGRAM USING
                       WS-SESSION-INFO
                       WS-DB2-CONFIG
                       WS-SQL-FIELDS
                       WS-ERROR-FIELDS
                       WS-AUDIT-FIELDS
               WHEN OTHER
                   DISPLAY "Invalid admin option."
           END-EVALUATE
           MOVE SPACES TO WS-FUNCTION-CODE.

      ******************************************************************
      * 5700 - CHANGE PASSWORD (calls AUTHNTCN)
      ******************************************************************
       5700-CHANGE-PASSWORD.
           MOVE "AUTHNTCN" TO WS-SUB-PROGRAM
           MOVE "CPWD" TO WS-FUNCTION-CODE
           DISPLAY " "
           DISPLAY "Loading Change Password..."
           CALL WS-SUB-PROGRAM USING
               WS-SESSION-INFO
               WS-DB2-CONFIG
               WS-ERROR-FIELDS
               WS-AUDIT-FIELDS
           MOVE SPACES TO WS-FUNCTION-CODE
           DISPLAY "Password change process complete.".

      ******************************************************************
      * 5800 - LOGOUT
      * VULNERABILITY V28: Session not properly invalidated
      *   Only sets flag, doesn't clear sensitive data
      ******************************************************************
       5800-LOGOUT.
           DISPLAY " "
           DISPLAY "Logging out..."

      * VULNERABILITY V28: Incomplete session cleanup
      *   Only setting the flag, not clearing user data,
      *   auth token, or sensitive session information
           MOVE "N" TO WS-MAIN-CONTINUE
           MOVE "N" TO WS-SESSION-ACTIVE

      * Should also clear:
      *   WS-CURRENT-USER, WS-CURRENT-ROLE, WS-AUTH-TOKEN
      *   But intentionally doesn't

      * VULNERABILITY V23: Log sensitive session data
           STRING "LOGOUT: User=" WS-CURRENT-USER
                  " Token=" WS-AUTH-TOKEN
                  " Role=" WS-CURRENT-ROLE
                  DELIMITED SIZE INTO LOG-RECORD
           WRITE LOG-RECORD

           DISPLAY "Goodbye, " WS-CURRENT-USER "."
           DISPLAY "Thank you for using Altoro Mutual.".

      ******************************************************************
      * 9000 - CLEANUP AND EXIT
      ******************************************************************
       9000-CLEANUP.
      * Close all files - ignore status
           CLOSE CUSTOMER-FILE
           CLOSE ACCOUNT-FILE
           CLOSE TRANSACTION-FILE

      * VULNERABILITY V23: Log full session info on exit
           STRING "SHUTDOWN: User=" WS-CURRENT-USER
                  " Session=" WS-AUTH-TOKEN
                  " Env=" WS-SYSTEM-ENV
                  " Region=" WS-SYSTEM-REGION
                  DELIMITED SIZE INTO LOG-RECORD
           WRITE LOG-RECORD
           CLOSE LOG-FILE

           DISPLAY " "
           DISPLAY "System shutdown complete."
           DISPLAY WS-SEPARATOR.

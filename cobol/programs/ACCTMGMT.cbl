       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTMGMT.
       AUTHOR. ALTORO-MUTUAL-DEVELOPMENT.
       DATE-WRITTEN. 2024-01-15.
       DATE-COMPILED.
      ******************************************************************
      * ACCTMGMT.cbl - Account Management Module
      * Altoro Mutual Banking System - COBOL Edition
      *
      * Handles account viewing, creation, modification,
      * fund transfers, and transaction history.
      *
      * VULNERABILITIES:
      *   V07 - SQL Injection: account ID in query concatenation
      *   V08 - Buffer Overflow: input into short fields
      *   V09 - Numeric Overflow: COMPUTE without ON SIZE ERROR
      *   V10 - Missing Authentication: admin ops without role check
      *   V11 - IDOR: view any account without ownership check
      *   V12 - Missing Input Validation: negative amounts accepted
      *   V13 - Improper Error Handling: FILE STATUS ignored
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

           SELECT AUDIT-LOG-FILE
               ASSIGN TO "AUDITLOG"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-AUDIT-LOG-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
           COPY CUSTREC.

       FD  ACCOUNT-FILE.
           COPY ACCTREC.

       FD  TRANSACTION-FILE.
           COPY TRANREC.

       FD  AUDIT-LOG-FILE.
       01  AUDIT-LOG-RECORD            PIC X(256).

       WORKING-STORAGE SECTION.

      * Copy in shared data structures
           COPY SYSCOPY.
           COPY CUSTWS.
           COPY ACCTWS.
           COPY TRANWS.

      * Local working storage
       01  WS-PROGRAM-NAME             PIC X(08) VALUE "ACCTMGMT".
       01  WS-AUDIT-LOG-STATUS         PIC X(02).
       01  WS-ACCT-CONTINUE            PIC X(01) VALUE "Y".
           88  WS-ACCT-LOOP            VALUE "Y".
           88  WS-ACCT-EXIT-FLAG       VALUE "N".

      * Menu and input fields
       01  WS-ACCT-MENU-CHOICE         PIC X(02) VALUE SPACES.
       01  WS-ACCT-INPUT-ID            PIC X(20).
       01  WS-ACCT-INPUT-NAME          PIC X(80).
       01  WS-ACCT-INPUT-TYPE          PIC X(02).
       01  WS-ACCT-INPUT-AMOUNT        PIC X(20).
       01  WS-ACCT-INPUT-BALANCE       PIC X(20).
       01  WS-ACCT-INPUT-LIMIT         PIC X(20).
       01  WS-ACCT-INPUT-OWNER         PIC X(80).
       01  WS-ACCT-INPUT-MEMO          PIC X(80).

      * Numeric conversion fields
       01  WS-NUMERIC-AMOUNT           PIC S9(13)V99.
       01  WS-NUMERIC-BALANCE          PIC S9(13)V99.
       01  WS-NUMERIC-LIMIT            PIC S9(13)V99.
       01  WS-NUMERIC-ACCT-ID          PIC 9(16).
       01  WS-NUMERIC-TEMP             PIC S9(15)V99.
       01  WS-NUMERIC-INTEREST         PIC S9(13)V99.

      * Transfer working fields
       01  WS-XFER-FROM-ID             PIC X(20).
       01  WS-XFER-TO-ID              PIC X(20).
       01  WS-XFER-AMOUNT-STR         PIC X(20).
       01  WS-LOCAL-XFER-AMT            PIC S9(13)V99.
       01  WS-XFER-FROM-BALANCE       PIC S9(13)V99.
       01  WS-XFER-TO-BALANCE         PIC S9(13)V99.
       01  WS-LOCAL-XFER-MEMO           PIC X(50).

      * Transaction history fields
       01  WS-HIST-ACCT-ID            PIC X(20).
       01  WS-HIST-START-DATE         PIC X(10).
       01  WS-HIST-END-DATE           PIC X(10).
       01  WS-HIST-COUNT              PIC 9(06) VALUE ZEROS.
       01  WS-HIST-TOTAL-CREDIT       PIC S9(15)V99 VALUE ZEROS.
       01  WS-HIST-TOTAL-DEBIT        PIC S9(15)V99 VALUE ZEROS.

      * SQL working fields
       01  WS-SQL-ACCT-ID             PIC X(20).
       01  WS-SQL-ACCT-BALANCE        PIC S9(13)V99.
       01  WS-SQL-ACCT-NAME           PIC X(30).
       01  WS-SQL-ACCT-TYPE           PIC X(02).
       01  WS-SQL-ACCT-OWNER          PIC X(20).

      * Display fields
       01  WS-DSP-BALANCE             PIC Z(12)9.99-.
       01  WS-DSP-AMOUNT              PIC Z(12)9.99-.
       01  WS-DSP-ACCT-ID             PIC 9(16).
       01  WS-DSP-LINE                PIC X(80).

      * Separator
       01  WS-SEPARATOR               PIC X(72) VALUE ALL "-".

      * Date/time fields
       01  WS-ACCEPT-DATE             PIC 9(08).
       01  WS-ACCEPT-TIME             PIC 9(08).
       01  WS-FORMATTED-DT            PIC X(26).

      * Next transaction ID
       01  WS-NEXT-TRAN-ID            PIC 9(10) VALUE ZEROS.

      * Account counters for display
       01  WS-DSP-ACCT-COUNT          PIC 9(04) VALUE ZEROS.
       01  WS-DSP-TRAN-COUNT          PIC 9(06) VALUE ZEROS.

      * Interest calculation fields
       01  WS-INTEREST-DAYS           PIC 9(05).
       01  WS-INTEREST-RATE           PIC 9(02)V9(04).
       01  WS-INTEREST-AMOUNT         PIC S9(13)V99.
       01  WS-DAILY-RATE              PIC 9V9(08).

      * Files open flags
       01  WS-FILES-OPEN              PIC X(01) VALUE "N".
       01  WS-AUDIT-OPEN              PIC X(01) VALUE "N".

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
           LS-ERROR-FIELDS
           LS-AUDIT-FIELDS.

      ******************************************************************
      * MAIN CONTROL
      ******************************************************************
       0000-MAIN-CONTROL.
           MOVE "ACCTMGMT" TO WS-PROGRAM-NAME
           PERFORM 0100-OPEN-FILES

      * Check function code from caller
           EVALUATE LS-AUDIT-ACTION
               WHEN "XFER"
                   PERFORM 3000-FUND-TRANSFER
               WHEN "HIST"
                   PERFORM 4000-TRANSACTION-HISTORY
               WHEN "LOCK"
                   PERFORM 6000-LOCK-UNLOCK-ACCOUNT
               WHEN OTHER
                   PERFORM 1000-ACCOUNT-MENU
           END-EVALUATE

           PERFORM 0900-CLOSE-FILES
           GOBACK.

      ******************************************************************
      * 0100 - OPEN FILES
      ******************************************************************
       0100-OPEN-FILES.
           OPEN I-O ACCOUNT-FILE
      * VULNERABILITY V13: File status not checked
           OPEN I-O TRANSACTION-FILE
      * VULNERABILITY V13: File status not checked
           OPEN I-O CUSTOMER-FILE
      * VULNERABILITY V13: File status not checked
           OPEN EXTEND AUDIT-LOG-FILE
      * VULNERABILITY V13: File status not checked
           MOVE "Y" TO WS-FILES-OPEN
           MOVE "Y" TO WS-AUDIT-OPEN.

      ******************************************************************
      * 0900 - CLOSE FILES
      ******************************************************************
       0900-CLOSE-FILES.
           IF WS-FILES-OPEN = "Y"
               CLOSE ACCOUNT-FILE
               CLOSE TRANSACTION-FILE
               CLOSE CUSTOMER-FILE
           END-IF
           IF WS-AUDIT-OPEN = "Y"
               CLOSE AUDIT-LOG-FILE
           END-IF.

      ******************************************************************
      * 1000 - ACCOUNT MANAGEMENT MENU
      ******************************************************************
       1000-ACCOUNT-MENU.
           MOVE "Y" TO WS-ACCT-CONTINUE

           PERFORM UNTIL WS-ACCT-EXIT-FLAG
               DISPLAY " "
               DISPLAY WS-SEPARATOR
               DISPLAY "  ACCOUNT MANAGEMENT"
               DISPLAY "  User: " LS-CURRENT-USER
               DISPLAY WS-SEPARATOR
               DISPLAY " "
               DISPLAY "  1. View My Accounts"
               DISPLAY "  2. View Account Details"
               DISPLAY "  3. Create New Account"
               DISPLAY "  4. Fund Transfer"
               DISPLAY "  5. Transaction History"
               DISPLAY "  6. Close Account"
               DISPLAY "  7. Modify Account Limits"
               DISPLAY "  8. Calculate Interest"
               DISPLAY " "
               DISPLAY "  0. Return to Main Menu"
               DISPLAY " "
               DISPLAY "Enter option: " WITH NO ADVANCING
               ACCEPT WS-ACCT-MENU-CHOICE FROM CONSOLE

               EVALUATE WS-ACCT-MENU-CHOICE
                   WHEN "1"
                       PERFORM 2000-VIEW-MY-ACCOUNTS
                   WHEN "2"
                       PERFORM 2100-VIEW-ACCOUNT-DETAILS
                   WHEN "3"
                       PERFORM 2200-CREATE-ACCOUNT
                   WHEN "4"
                       PERFORM 3000-FUND-TRANSFER
                   WHEN "5"
                       PERFORM 4000-TRANSACTION-HISTORY
                   WHEN "6"
      * VULNERABILITY V10: No admin check for close
                       PERFORM 5000-CLOSE-ACCOUNT
                   WHEN "7"
      * VULNERABILITY V10: No admin check for limit change
                       PERFORM 5100-MODIFY-LIMITS
                   WHEN "8"
                       PERFORM 5200-CALCULATE-INTEREST
                   WHEN "0"
                       MOVE "N" TO WS-ACCT-CONTINUE
                   WHEN OTHER
                       DISPLAY "Invalid option."
               END-EVALUATE
           END-PERFORM.

      ******************************************************************
      * 2000 - VIEW MY ACCOUNTS
      ******************************************************************
       2000-VIEW-MY-ACCOUNTS.
           DISPLAY " "
           DISPLAY "  YOUR ACCOUNTS"
           DISPLAY "  " WS-SEPARATOR
           DISPLAY "  Account ID       | Name"
                   "                  | Type | Balance"
           DISPLAY "  " WS-SEPARATOR
           MOVE ZEROS TO WS-DSP-ACCT-COUNT

      * Read all accounts sequentially looking for user's
           MOVE LOW-VALUES TO ACCT-ID
           START ACCOUNT-FILE KEY >= ACCT-ID
      * VULNERABILITY V13: START status not checked

           PERFORM UNTIL WS-ACCT-FILE-STATUS NOT = "00"
               READ ACCOUNT-FILE NEXT
      * VULNERABILITY V13: READ status not checked
               IF WS-ACCT-FILE-STATUS = "00"
                   IF ACCT-OWNER-ID = LS-CURRENT-USER
                       ADD 1 TO WS-DSP-ACCT-COUNT
                       MOVE ACCT-BALANCE TO WS-DSP-BALANCE
                       DISPLAY "  " ACCT-ID " | "
                               ACCT-NAME " | "
                               ACCT-TYPE " | $"
                               WS-DSP-BALANCE
                   END-IF
               END-IF
           END-PERFORM

           DISPLAY "  " WS-SEPARATOR
           DISPLAY "  Total accounts: " WS-DSP-ACCT-COUNT.

      ******************************************************************
      * 2100 - VIEW ACCOUNT DETAILS
      * VULNERABILITY V11: IDOR - No ownership verification
      *   Any user can view any account by entering the ID
      ******************************************************************
       2100-VIEW-ACCOUNT-DETAILS.
           DISPLAY " "
           DISPLAY "Enter Account ID: " WITH NO ADVANCING
           ACCEPT WS-ACCT-INPUT-ID FROM CONSOLE

      * VULNERABILITY V08: Buffer overflow
      *   WS-ACCT-INPUT-ID is PIC X(20) but ACCT-ID is PIC 9(16)
      *   No length or numeric validation before MOVE
           MOVE WS-ACCT-INPUT-ID TO ACCT-ID

      * VULNERABILITY V11: No check if account belongs to user
      *   Should verify ACCT-OWNER-ID = LS-CURRENT-USER
           READ ACCOUNT-FILE
      * VULNERABILITY V13: File status not checked

           IF WS-ACCT-FILE-STATUS = "00"
               MOVE ACCT-BALANCE TO WS-DSP-BALANCE
               DISPLAY " "
               DISPLAY "  ACCOUNT DETAILS"
               DISPLAY "  " WS-SEPARATOR
               DISPLAY "  Account ID:    " ACCT-ID
               DISPLAY "  Owner:         " ACCT-OWNER-ID
               DISPLAY "  Account Name:  " ACCT-NAME
               DISPLAY "  Type:          " ACCT-TYPE
               DISPLAY "  Balance:       $" WS-DSP-BALANCE
               MOVE ACCT-AVAILABLE-BAL TO WS-DSP-BALANCE
               DISPLAY "  Available:     $" WS-DSP-BALANCE
               MOVE ACCT-CREDIT-LIMIT TO WS-DSP-BALANCE
               DISPLAY "  Credit Limit:  $" WS-DSP-BALANCE
               DISPLAY "  Interest Rate: " ACCT-INTEREST-RATE "%"
               DISPLAY "  Opened:        " ACCT-OPEN-DATE
               DISPLAY "  Last Activity: " ACCT-LAST-ACTIVITY
               DISPLAY "  Status:        " ACCT-STATUS
      * VULNERABILITY V05: Display sensitive account details
               DISPLAY "  PIN:           " ACCT-PIN
               DISPLAY "  Daily Limit:   $" ACCT-DAILY-LIMIT
               DISPLAY "  Monthly Limit: $" ACCT-MONTHLY-LIMIT
               DISPLAY "  " WS-SEPARATOR

      * Also query via DB2 for cross-reference
      * VULNERABILITY V07: SQL Injection
               PERFORM 2150-QUERY-ACCOUNT-DB2
           ELSE
               DISPLAY "Account not found."
           END-IF.

      ******************************************************************
      * 2150 - QUERY ACCOUNT VIA DB2
      * VULNERABILITY V07: SQL Injection via account ID
      ******************************************************************
       2150-QUERY-ACCOUNT-DB2.
      * VULNERABILITY V07: SQL Injection
      *   Account ID concatenated directly into SQL
      *   Attacker can manipulate account ID to inject SQL
           MOVE SPACES TO WS-SQL-STMT
           STRING
               "SELECT ACCOUNT_ID, ACCOUNT_NAME, BALANCE,"
               " USERID FROM ACCOUNTS"
               " WHERE ACCOUNT_ID = '"
               WS-ACCT-INPUT-ID "'"
               DELIMITED SIZE INTO WS-SQL-STMT
           END-STRING

      * VULNERABILITY V05: Display SQL for debugging
           DISPLAY "DEBUG SQL: " WS-SQL-STMT

      >>IF DB2-ENABLED IS DEFINED
           EXEC SQL
               SELECT ACCOUNT_ID, ACCOUNT_NAME, BALANCE,
                      USERID
               INTO :WS-SQL-ACCT-ID,
                    :WS-SQL-ACCT-NAME,
                    :WS-SQL-ACCT-BALANCE,
                    :WS-SQL-ACCT-OWNER
               FROM ACCOUNTS
               WHERE ACCOUNT_ID = :WS-ACCT-INPUT-ID
           END-EXEC
      >>END-IF
           CONTINUE.
      * VULNERABILITY V06: SQLCODE not checked

      ******************************************************************
      * 2200 - CREATE NEW ACCOUNT
      * VULNERABILITY V10: No admin role check
      * VULNERABILITY V08: Buffer overflow on account name
      ******************************************************************
       2200-CREATE-ACCOUNT.
      * VULNERABILITY V10: No authentication/role check
      *   Any user can create accounts for any owner
           DISPLAY " "
           DISPLAY "=== CREATE NEW ACCOUNT ==="
           DISPLAY " "

           DISPLAY "Account Owner ID: " WITH NO ADVANCING
           ACCEPT WS-ACCT-INPUT-OWNER FROM CONSOLE

      * VULNERABILITY V08: Buffer overflow
      *   WS-ACCT-INPUT-OWNER is PIC X(80) but
      *   ACCT-OWNER-ID is PIC X(20) - truncation without warning
           MOVE WS-ACCT-INPUT-OWNER TO ACCT-OWNER-ID

           DISPLAY "Account Name: " WITH NO ADVANCING
           ACCEPT WS-ACCT-INPUT-NAME FROM CONSOLE

      * VULNERABILITY V08: Buffer overflow
      *   WS-ACCT-INPUT-NAME is PIC X(80) but
      *   ACCT-NAME is PIC X(30) - 50 bytes silently truncated
           MOVE WS-ACCT-INPUT-NAME TO ACCT-NAME

           DISPLAY "Account Type (CH/SA/IR/CC/CO): "
               WITH NO ADVANCING
           ACCEPT WS-ACCT-INPUT-TYPE FROM CONSOLE
      * VULNERABILITY V12: No validation of account type
           MOVE WS-ACCT-INPUT-TYPE TO ACCT-TYPE

           DISPLAY "Initial Balance: " WITH NO ADVANCING
           ACCEPT WS-ACCT-INPUT-BALANCE FROM CONSOLE

      * VULNERABILITY V12: No numeric validation
      *   No check for valid number, no check for negative
           COMPUTE ACCT-BALANCE =
               FUNCTION NUMVAL(WS-ACCT-INPUT-BALANCE)
      * VULNERABILITY V09: No ON SIZE ERROR
           MOVE ACCT-BALANCE TO ACCT-AVAILABLE-BAL

           DISPLAY "Credit Limit (0 if N/A): " WITH NO ADVANCING
           ACCEPT WS-ACCT-INPUT-LIMIT FROM CONSOLE
           COMPUTE ACCT-CREDIT-LIMIT =
               FUNCTION NUMVAL(WS-ACCT-INPUT-LIMIT)
      * VULNERABILITY V09: No ON SIZE ERROR

           DISPLAY "Daily Transaction Limit: " WITH NO ADVANCING
           ACCEPT WS-ACCT-INPUT-LIMIT FROM CONSOLE
           COMPUTE ACCT-DAILY-LIMIT =
               FUNCTION NUMVAL(WS-ACCT-INPUT-LIMIT)

           DISPLAY "Monthly Transaction Limit: " WITH NO ADVANCING
           ACCEPT WS-ACCT-INPUT-LIMIT FROM CONSOLE
           COMPUTE ACCT-MONTHLY-LIMIT =
               FUNCTION NUMVAL(WS-ACCT-INPUT-LIMIT)

      * Generate account ID (simple sequential)
           MOVE 800100 TO ACCT-ID
           DISPLAY "Enter Account PIN: " WITH NO ADVANCING
           ACCEPT ACCT-PIN FROM CONSOLE
      * VULNERABILITY V04: PIN stored in plaintext

      * Set dates
           ACCEPT WS-ACCEPT-DATE FROM DATE YYYYMMDD
           MOVE WS-ACCEPT-DATE TO ACCT-OPEN-DATE
           MOVE WS-ACCEPT-DATE TO ACCT-LAST-ACTIVITY
           SET ACCT-ACTIVE TO TRUE
           SET ACCT-OVERDRAFT-NO TO TRUE
           MOVE 2.5000 TO ACCT-INTEREST-RATE

           WRITE ACCOUNT-RECORD
      * VULNERABILITY V13: WRITE status not checked

      * VULNERABILITY V07: SQL Injection in INSERT
           MOVE SPACES TO WS-SQL-STMT
           STRING
               "INSERT INTO ACCOUNTS"
               " (ACCOUNT_ID, USERID, ACCOUNT_NAME, BALANCE)"
               " VALUES ('"
               ACCT-ID "', '"
               WS-ACCT-INPUT-OWNER "', '"
               WS-ACCT-INPUT-NAME "', "
               WS-ACCT-INPUT-BALANCE ")"
               DELIMITED SIZE INTO WS-SQL-STMT
           END-STRING

      >>IF DB2-ENABLED IS DEFINED
           EXEC SQL
               EXECUTE IMMEDIATE :WS-SQL-STMT
           END-EXEC
      >>END-IF
      * VULNERABILITY V06: SQLCODE not checked

           DISPLAY " "
           DISPLAY "Account created successfully."
           DISPLAY "Account ID: " ACCT-ID

      * VULNERABILITY V05: Log with all details
           STRING "ACCT_CREATE: ID=" ACCT-ID
                  " Owner=" ACCT-OWNER-ID
                  " Balance=" WS-ACCT-INPUT-BALANCE
                  " PIN=" ACCT-PIN
                  " By=" LS-CURRENT-USER
                  DELIMITED SIZE INTO AUDIT-LOG-RECORD
           WRITE AUDIT-LOG-RECORD.

      ******************************************************************
      * 3000 - FUND TRANSFER
      * VULNERABILITY V09: Numeric overflow in balance calc
      * VULNERABILITY V11: IDOR on source account
      * VULNERABILITY V12: Negative amounts accepted
      ******************************************************************
       3000-FUND-TRANSFER.
           DISPLAY " "
           DISPLAY "=== FUND TRANSFER ==="
           DISPLAY " "

           DISPLAY "From Account ID: " WITH NO ADVANCING
           ACCEPT WS-XFER-FROM-ID FROM CONSOLE

           DISPLAY "To Account ID: " WITH NO ADVANCING
           ACCEPT WS-XFER-TO-ID FROM CONSOLE

           DISPLAY "Transfer Amount: " WITH NO ADVANCING
           ACCEPT WS-XFER-AMOUNT-STR FROM CONSOLE

      * VULNERABILITY V12: No validation on transfer amount
      *   Negative amounts accepted - could reverse a transfer
      *   Zero amounts accepted - pointless transaction
      *   No maximum limit check
           COMPUTE WS-LOCAL-XFER-AMT =
               FUNCTION NUMVAL(WS-XFER-AMOUNT-STR)
      * VULNERABILITY V09: No ON SIZE ERROR

           DISPLAY "Memo (optional): " WITH NO ADVANCING
           ACCEPT WS-LOCAL-XFER-MEMO FROM CONSOLE

      * Read source account
      * VULNERABILITY V08: Input overflow on account ID
           MOVE WS-XFER-FROM-ID TO ACCT-ID
           READ ACCOUNT-FILE
      * VULNERABILITY V13: File status not checked

      * VULNERABILITY V11: No ownership check on source account
      *   Should verify ACCT-OWNER-ID = LS-CURRENT-USER
           IF WS-ACCT-FILE-STATUS NOT = "00"
               DISPLAY "Source account not found."
               GO TO 3000-TRANSFER-EXIT
           END-IF

           MOVE ACCT-BALANCE TO WS-XFER-FROM-BALANCE

      * VULNERABILITY V12: No balance sufficiency check
      *   Transfer proceeds even if balance < amount
      *   This mirrors AltoroJ which also lacks balance checks

      * VULNERABILITY V09: Numeric overflow - no ON SIZE ERROR
      *   If balance is very large, subtraction could overflow
           COMPUTE ACCT-BALANCE =
               ACCT-BALANCE - WS-LOCAL-XFER-AMT
      * No ON SIZE ERROR clause

      * Update available balance too
           COMPUTE ACCT-AVAILABLE-BAL =
               ACCT-AVAILABLE-BAL - WS-LOCAL-XFER-AMT

      * Update last activity
           ACCEPT WS-ACCEPT-DATE FROM DATE YYYYMMDD
           MOVE WS-ACCEPT-DATE TO ACCT-LAST-ACTIVITY

           REWRITE ACCOUNT-RECORD
      * VULNERABILITY V13: REWRITE status not checked

      * Record debit transaction
           PERFORM 3100-RECORD-DEBIT-TRANSACTION

      * Now credit the destination account
           MOVE WS-XFER-TO-ID TO ACCT-ID
           READ ACCOUNT-FILE
      * VULNERABILITY V13: File status not checked

           IF WS-ACCT-FILE-STATUS NOT = "00"
      * VULNERABILITY V13: Source already debited but
      *   destination not found - no rollback!
               DISPLAY "ERROR: Destination account not found!"
               DISPLAY "WARNING: Source account already debited!"
               DISPLAY "Contact administrator."
               GO TO 3000-TRANSFER-EXIT
           END-IF

           MOVE ACCT-BALANCE TO WS-XFER-TO-BALANCE

      * VULNERABILITY V09: Numeric overflow on credit side
           COMPUTE ACCT-BALANCE =
               ACCT-BALANCE + WS-LOCAL-XFER-AMT
           COMPUTE ACCT-AVAILABLE-BAL =
               ACCT-AVAILABLE-BAL + WS-LOCAL-XFER-AMT
           MOVE WS-ACCEPT-DATE TO ACCT-LAST-ACTIVITY

           REWRITE ACCOUNT-RECORD
      * VULNERABILITY V13: REWRITE status not checked

      * Record credit transaction
           PERFORM 3200-RECORD-CREDIT-TRANSACTION

      * Display transfer summary
           MOVE WS-LOCAL-XFER-AMT TO WS-DSP-AMOUNT
           DISPLAY " "
           DISPLAY "Transfer completed successfully."
           DISPLAY "Amount: $" WS-DSP-AMOUNT
           DISPLAY "From: " WS-XFER-FROM-ID
           DISPLAY "To:   " WS-XFER-TO-ID

      * VULNERABILITY V07: SQL Injection in transfer log
           MOVE SPACES TO WS-SQL-STMT
           STRING
               "INSERT INTO TRANSACTIONS"
               " (ACCOUNTID, DATE, TYPE, AMOUNT)"
               " VALUES ('"
               WS-XFER-FROM-ID "', CURRENT DATE, 'XF', "
               WS-XFER-AMOUNT-STR ")"
               DELIMITED SIZE INTO WS-SQL-STMT
           END-STRING

      >>IF DB2-ENABLED IS DEFINED
           EXEC SQL
               EXECUTE IMMEDIATE :WS-SQL-STMT
           END-EXEC
      >>END-IF
      * VULNERABILITY V06: SQLCODE not checked

      * Audit log
           STRING "TRANSFER: From=" WS-XFER-FROM-ID
                  " To=" WS-XFER-TO-ID
                  " Amount=" WS-XFER-AMOUNT-STR
                  " By=" LS-CURRENT-USER
                  " Token=" LS-AUTH-TOKEN
                  DELIMITED SIZE INTO AUDIT-LOG-RECORD
           WRITE AUDIT-LOG-RECORD.

       3000-TRANSFER-EXIT.
           CONTINUE.

      ******************************************************************
      * 3100 - RECORD DEBIT TRANSACTION
      ******************************************************************
       3100-RECORD-DEBIT-TRANSACTION.
           ADD 1 TO WS-NEXT-TRAN-ID
           MOVE WS-NEXT-TRAN-ID TO TRAN-ID
           MOVE WS-XFER-FROM-ID TO TRAN-ACCOUNT-ID
           ACCEPT WS-ACCEPT-DATE FROM DATE YYYYMMDD
           MOVE WS-ACCEPT-DATE TO TRAN-DATE
           ACCEPT WS-ACCEPT-TIME FROM TIME
           MOVE WS-ACCEPT-TIME(1:6) TO TRAN-TIME
           SET TRAN-TYPE-TRANSFER TO TRUE
           COMPUTE TRAN-AMOUNT = WS-LOCAL-XFER-AMT * -1
      * VULNERABILITY V09: No ON SIZE ERROR
           MOVE ACCT-BALANCE TO TRAN-BALANCE-AFTER
           STRING "Transfer to " WS-XFER-TO-ID
                  DELIMITED SIZE INTO TRAN-DESCRIPTION
           MOVE LS-CURRENT-USER TO TRAN-USER-ID
           MOVE "TERM001" TO TRAN-TERMINAL-ID
           SET TRAN-COMPLETED TO TRUE
           MOVE WS-XFER-TO-ID TO TRAN-TO-ACCOUNT

           WRITE TRANSACTION-RECORD.
      * VULNERABILITY V13: WRITE status not checked

      ******************************************************************
      * 3200 - RECORD CREDIT TRANSACTION
      ******************************************************************
       3200-RECORD-CREDIT-TRANSACTION.
           ADD 1 TO WS-NEXT-TRAN-ID
           MOVE WS-NEXT-TRAN-ID TO TRAN-ID
           MOVE WS-XFER-TO-ID TO TRAN-ACCOUNT-ID
           ACCEPT WS-ACCEPT-DATE FROM DATE YYYYMMDD
           MOVE WS-ACCEPT-DATE TO TRAN-DATE
           ACCEPT WS-ACCEPT-TIME FROM TIME
           MOVE WS-ACCEPT-TIME(1:6) TO TRAN-TIME
           SET TRAN-TYPE-TRANSFER TO TRUE
           MOVE WS-LOCAL-XFER-AMT TO TRAN-AMOUNT
           MOVE ACCT-BALANCE TO TRAN-BALANCE-AFTER
           STRING "Transfer from " WS-XFER-FROM-ID
                  DELIMITED SIZE INTO TRAN-DESCRIPTION
           MOVE LS-CURRENT-USER TO TRAN-USER-ID
           MOVE "TERM001" TO TRAN-TERMINAL-ID
           SET TRAN-COMPLETED TO TRUE
           MOVE WS-XFER-FROM-ID TO TRAN-TO-ACCOUNT

           WRITE TRANSACTION-RECORD.
      * VULNERABILITY V13: WRITE status not checked

      ******************************************************************
      * 4000 - TRANSACTION HISTORY
      * VULNERABILITY V11: IDOR - can view any account's history
      * VULNERABILITY V07: SQL Injection in date range query
      ******************************************************************
       4000-TRANSACTION-HISTORY.
           DISPLAY " "
           DISPLAY "=== TRANSACTION HISTORY ==="
           DISPLAY " "

           DISPLAY "Account ID: " WITH NO ADVANCING
           ACCEPT WS-HIST-ACCT-ID FROM CONSOLE

      * VULNERABILITY V11: No ownership check
      *   Any user can view transaction history for any account

           DISPLAY "Start Date (YYYY-MM-DD or blank): "
               WITH NO ADVANCING
           ACCEPT WS-HIST-START-DATE FROM CONSOLE

           DISPLAY "End Date (YYYY-MM-DD or blank): "
               WITH NO ADVANCING
           ACCEPT WS-HIST-END-DATE FROM CONSOLE

      * Query via DB2 with date range
      * VULNERABILITY V07: SQL Injection in date parameters
           MOVE SPACES TO WS-SQL-STMT
           STRING
               "SELECT TRANSACTION_ID, DATE, TYPE, AMOUNT"
               " FROM TRANSACTIONS"
               " WHERE ACCOUNTID = '" WS-HIST-ACCT-ID "'"
               " AND DATE >= '" WS-HIST-START-DATE "'"
               " AND DATE <= '" WS-HIST-END-DATE "'"
               " ORDER BY DATE DESC"
               DELIMITED SIZE INTO WS-SQL-STMT
           END-STRING

      * VULNERABILITY V05: Display SQL query
           DISPLAY "DEBUG SQL: " WS-SQL-STMT

      >>IF DB2-ENABLED IS DEFINED
           EXEC SQL
               EXECUTE IMMEDIATE :WS-SQL-STMT
           END-EXEC
      >>END-IF
      * VULNERABILITY V06: SQLCODE not checked

      * Also read from file
           DISPLAY " "
           DISPLAY "  TRANSACTION HISTORY"
           DISPLAY "  " WS-SEPARATOR
           DISPLAY "  ID         | Date       | Type | Amount"
                   "          | Description"
           DISPLAY "  " WS-SEPARATOR
           MOVE ZEROS TO WS-HIST-COUNT
           MOVE ZEROS TO WS-HIST-TOTAL-CREDIT
           MOVE ZEROS TO WS-HIST-TOTAL-DEBIT

           MOVE LOW-VALUES TO TRAN-ID
           START TRANSACTION-FILE KEY >= TRAN-ID
      * VULNERABILITY V13: START status not checked

           PERFORM UNTIL WS-TRAN-FILE-STATUS NOT = "00"
               READ TRANSACTION-FILE NEXT
               IF WS-TRAN-FILE-STATUS = "00"
                   IF TRAN-ACCOUNT-ID = WS-HIST-ACCT-ID
                       ADD 1 TO WS-HIST-COUNT
                       MOVE TRAN-AMOUNT TO WS-DSP-AMOUNT
                       DISPLAY "  "
                               TRAN-ID " | "
                               TRAN-DATE " | "
                               TRAN-TYPE "   | $"
                               WS-DSP-AMOUNT " | "
                               TRAN-DESCRIPTION

      * VULNERABILITY V09: Accumulate without overflow check
                       IF TRAN-AMOUNT >= 0
                           COMPUTE WS-HIST-TOTAL-CREDIT =
                               WS-HIST-TOTAL-CREDIT + TRAN-AMOUNT
                       ELSE
                           COMPUTE WS-HIST-TOTAL-DEBIT =
                               WS-HIST-TOTAL-DEBIT + TRAN-AMOUNT
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           DISPLAY "  " WS-SEPARATOR
           MOVE WS-HIST-TOTAL-CREDIT TO WS-DSP-AMOUNT
           DISPLAY "  Total Credits: $" WS-DSP-AMOUNT
           MOVE WS-HIST-TOTAL-DEBIT TO WS-DSP-AMOUNT
           DISPLAY "  Total Debits:  $" WS-DSP-AMOUNT
           COMPUTE WS-NUMERIC-TEMP =
               WS-HIST-TOTAL-CREDIT + WS-HIST-TOTAL-DEBIT
      * VULNERABILITY V09: No ON SIZE ERROR
           MOVE WS-NUMERIC-TEMP TO WS-DSP-AMOUNT
           DISPLAY "  Net:           $" WS-DSP-AMOUNT
           DISPLAY "  Transactions:  " WS-HIST-COUNT.

      ******************************************************************
      * 5000 - CLOSE ACCOUNT
      * VULNERABILITY V10: No admin role check
      * VULNERABILITY V11: No ownership check
      ******************************************************************
       5000-CLOSE-ACCOUNT.
      * VULNERABILITY V10: No check if user is admin
      *   Any user can close any account
           DISPLAY " "
           DISPLAY "=== CLOSE ACCOUNT ==="
           DISPLAY " "
           DISPLAY "Account ID to close: " WITH NO ADVANCING
           ACCEPT WS-ACCT-INPUT-ID FROM CONSOLE

      * VULNERABILITY V11: No ownership verification
           MOVE WS-ACCT-INPUT-ID TO ACCT-ID
           READ ACCOUNT-FILE
      * VULNERABILITY V13: File status not checked

           IF WS-ACCT-FILE-STATUS = "00"
               DISPLAY "Account: " ACCT-NAME
               DISPLAY "Owner:   " ACCT-OWNER-ID
               MOVE ACCT-BALANCE TO WS-DSP-BALANCE
               DISPLAY "Balance: $" WS-DSP-BALANCE
               DISPLAY " "
      * VULNERABILITY V12: No check if balance is zero
      *   Account can be closed with remaining balance
               DISPLAY "Confirm close (Y/N): "
                   WITH NO ADVANCING
               ACCEPT WS-CONFIRM-FLAG FROM CONSOLE
               IF WS-CONFIRM-FLAG = "Y"
                   SET ACCT-CLOSED TO TRUE
                   REWRITE ACCOUNT-RECORD
      * VULNERABILITY V13: REWRITE status not checked
                   DISPLAY "Account " ACCT-ID " closed."

      * VULNERABILITY V07: SQL Injection in close
                   MOVE SPACES TO WS-SQL-STMT
                   STRING
                       "UPDATE ACCOUNTS SET STATUS = 'C'"
                       " WHERE ACCOUNT_ID = '"
                       WS-ACCT-INPUT-ID "'"
                       DELIMITED SIZE INTO WS-SQL-STMT
                   END-STRING
      >>IF DB2-ENABLED IS DEFINED
                   EXEC SQL
                       EXECUTE IMMEDIATE :WS-SQL-STMT
                   END-EXEC
      >>END-IF
      * VULNERABILITY V06: SQLCODE not checked
               END-IF
           ELSE
               DISPLAY "Account not found."
           END-IF.

      ******************************************************************
      * 5100 - MODIFY ACCOUNT LIMITS
      * VULNERABILITY V10: No admin role check
      ******************************************************************
       5100-MODIFY-LIMITS.
      * VULNERABILITY V10: No admin check
      *   Any user can modify limits on any account
           DISPLAY " "
           DISPLAY "=== MODIFY ACCOUNT LIMITS ==="
           DISPLAY " "
           DISPLAY "Account ID: " WITH NO ADVANCING
           ACCEPT WS-ACCT-INPUT-ID FROM CONSOLE

           MOVE WS-ACCT-INPUT-ID TO ACCT-ID
           READ ACCOUNT-FILE
      * VULNERABILITY V13: File status not checked

           IF WS-ACCT-FILE-STATUS = "00"
               DISPLAY "Current Daily Limit:   $"
                       ACCT-DAILY-LIMIT
               DISPLAY "Current Monthly Limit: $"
                       ACCT-MONTHLY-LIMIT
               DISPLAY " "

               DISPLAY "New Daily Limit: " WITH NO ADVANCING
               ACCEPT WS-ACCT-INPUT-LIMIT FROM CONSOLE
      * VULNERABILITY V12: No validation on limit values
               COMPUTE ACCT-DAILY-LIMIT =
                   FUNCTION NUMVAL(WS-ACCT-INPUT-LIMIT)
      * VULNERABILITY V09: No ON SIZE ERROR

               DISPLAY "New Monthly Limit: " WITH NO ADVANCING
               ACCEPT WS-ACCT-INPUT-LIMIT FROM CONSOLE
               COMPUTE ACCT-MONTHLY-LIMIT =
                   FUNCTION NUMVAL(WS-ACCT-INPUT-LIMIT)
      * VULNERABILITY V09: No ON SIZE ERROR

               REWRITE ACCOUNT-RECORD
      * VULNERABILITY V13: REWRITE status not checked
               DISPLAY "Limits updated successfully."

      * VULNERABILITY: Audit log with all details
               STRING "LIMIT_CHANGE: Acct=" ACCT-ID
                      " DailyLimit=" ACCT-DAILY-LIMIT
                      " MonthlyLimit=" ACCT-MONTHLY-LIMIT
                      " ChangedBy=" LS-CURRENT-USER
                      DELIMITED SIZE INTO AUDIT-LOG-RECORD
               WRITE AUDIT-LOG-RECORD
           ELSE
               DISPLAY "Account not found."
           END-IF.

      ******************************************************************
      * 5200 - CALCULATE INTEREST
      * VULNERABILITY V09: Numeric overflow in interest calc
      ******************************************************************
       5200-CALCULATE-INTEREST.
           DISPLAY " "
           DISPLAY "=== INTEREST CALCULATION ==="
           DISPLAY " "
           DISPLAY "Account ID: " WITH NO ADVANCING
           ACCEPT WS-ACCT-INPUT-ID FROM CONSOLE

           MOVE WS-ACCT-INPUT-ID TO ACCT-ID
           READ ACCOUNT-FILE
      * VULNERABILITY V13: File status not checked

           IF WS-ACCT-FILE-STATUS = "00"
               DISPLAY "Number of days: " WITH NO ADVANCING
               ACCEPT WS-ACCT-INPUT-AMOUNT FROM CONSOLE
               COMPUTE WS-INTEREST-DAYS =
                   FUNCTION NUMVAL(WS-ACCT-INPUT-AMOUNT)

               MOVE ACCT-INTEREST-RATE TO WS-INTEREST-RATE

      * VULNERABILITY V09: Multiple COMPUTE without ON SIZE ERROR
      *   Interest calculation can overflow with large balances
      *   and many days
               COMPUTE WS-DAILY-RATE =
                   WS-INTEREST-RATE / 365

               COMPUTE WS-INTEREST-AMOUNT =
                   ACCT-BALANCE * WS-DAILY-RATE
                   * WS-INTEREST-DAYS / 100
      * No ON SIZE ERROR - silent overflow possible

               COMPUTE ACCT-BALANCE =
                   ACCT-BALANCE + WS-INTEREST-AMOUNT
      * No ON SIZE ERROR

               REWRITE ACCOUNT-RECORD
      * VULNERABILITY V13: REWRITE status not checked

               MOVE WS-INTEREST-AMOUNT TO WS-DSP-AMOUNT
               DISPLAY "Interest calculated: $" WS-DSP-AMOUNT
               MOVE ACCT-BALANCE TO WS-DSP-BALANCE
               DISPLAY "New balance: $" WS-DSP-BALANCE
           ELSE
               DISPLAY "Account not found."
           END-IF.

      ******************************************************************
      * 6000 - LOCK/UNLOCK ACCOUNT
      * VULNERABILITY V10: No admin role check
      ******************************************************************
       6000-LOCK-UNLOCK-ACCOUNT.
      * VULNERABILITY V10: No admin check
           DISPLAY " "
           DISPLAY "=== LOCK/UNLOCK ACCOUNT ==="
           DISPLAY " "
           DISPLAY "Account ID: " WITH NO ADVANCING
           ACCEPT WS-ACCT-INPUT-ID FROM CONSOLE

           MOVE WS-ACCT-INPUT-ID TO ACCT-ID
           READ ACCOUNT-FILE
      * VULNERABILITY V13: File status not checked

           IF WS-ACCT-FILE-STATUS = "00"
               DISPLAY "Current Status: " ACCT-STATUS
               DISPLAY "1. Lock (Freeze)"
               DISPLAY "2. Unlock (Activate)"
               DISPLAY "Choice: " WITH NO ADVANCING
               ACCEPT WS-ACCT-MENU-CHOICE FROM CONSOLE
               EVALUATE WS-ACCT-MENU-CHOICE
                   WHEN "1"
                       SET ACCT-FROZEN TO TRUE
                       DISPLAY "Account frozen."
                   WHEN "2"
                       SET ACCT-ACTIVE TO TRUE
                       DISPLAY "Account activated."
                   WHEN OTHER
                       DISPLAY "Invalid choice."
                       GO TO 6000-LOCK-EXIT
               END-EVALUATE
               REWRITE ACCOUNT-RECORD
      * VULNERABILITY V13: REWRITE status not checked

      * Audit
               STRING "ACCT_STATUS: ID=" ACCT-ID
                      " NewStatus=" ACCT-STATUS
                      " By=" LS-CURRENT-USER
                      DELIMITED SIZE INTO AUDIT-LOG-RECORD
               WRITE AUDIT-LOG-RECORD
           ELSE
               DISPLAY "Account not found."
           END-IF.

       6000-LOCK-EXIT.
           CONTINUE.

      ******************************************************************
      * 7000 - DEPOSIT FUNDS
      * VULNERABILITY V12: No amount validation
      * VULNERABILITY V09: Numeric overflow
      ******************************************************************
       7000-DEPOSIT-FUNDS.
           DISPLAY " "
           DISPLAY "=== DEPOSIT FUNDS ==="
           DISPLAY " "
           DISPLAY "Account ID: " WITH NO ADVANCING
           ACCEPT WS-ACCT-INPUT-ID FROM CONSOLE

           MOVE WS-ACCT-INPUT-ID TO ACCT-ID
           READ ACCOUNT-FILE
      * VULNERABILITY V13: File status not checked

           IF WS-ACCT-FILE-STATUS = "00"
               MOVE ACCT-BALANCE TO WS-DSP-BALANCE
               DISPLAY "Current Balance: $" WS-DSP-BALANCE

               DISPLAY "Deposit Amount: " WITH NO ADVANCING
               ACCEPT WS-ACCT-INPUT-AMOUNT FROM CONSOLE

      * VULNERABILITY V12: No validation
      *   Negative deposits accepted (effectively withdrawals)
      *   No maximum limit check
               COMPUTE WS-NUMERIC-AMOUNT =
                   FUNCTION NUMVAL(WS-ACCT-INPUT-AMOUNT)
      * VULNERABILITY V09: No ON SIZE ERROR

      * VULNERABILITY V09: Balance update without overflow check
               COMPUTE ACCT-BALANCE =
                   ACCT-BALANCE + WS-NUMERIC-AMOUNT
               COMPUTE ACCT-AVAILABLE-BAL =
                   ACCT-AVAILABLE-BAL + WS-NUMERIC-AMOUNT

               ACCEPT WS-ACCEPT-DATE FROM DATE YYYYMMDD
               MOVE WS-ACCEPT-DATE TO ACCT-LAST-ACTIVITY
               REWRITE ACCOUNT-RECORD
      * VULNERABILITY V13: REWRITE status not checked

      * Record deposit transaction
               ADD 1 TO WS-NEXT-TRAN-ID
               MOVE WS-NEXT-TRAN-ID TO TRAN-ID
               MOVE ACCT-ID TO TRAN-ACCOUNT-ID
               MOVE WS-ACCEPT-DATE TO TRAN-DATE
               ACCEPT WS-ACCEPT-TIME FROM TIME
               MOVE WS-ACCEPT-TIME(1:6) TO TRAN-TIME
               SET TRAN-TYPE-DEPOSIT TO TRUE
               MOVE WS-NUMERIC-AMOUNT TO TRAN-AMOUNT
               MOVE ACCT-BALANCE TO TRAN-BALANCE-AFTER
               MOVE "Cash Deposit" TO TRAN-DESCRIPTION
               MOVE LS-CURRENT-USER TO TRAN-USER-ID
               MOVE "TERM001" TO TRAN-TERMINAL-ID
               SET TRAN-COMPLETED TO TRUE
               MOVE ZEROS TO TRAN-TO-ACCOUNT
               WRITE TRANSACTION-RECORD
      * VULNERABILITY V13: WRITE status not checked

               MOVE ACCT-BALANCE TO WS-DSP-BALANCE
               MOVE WS-NUMERIC-AMOUNT TO WS-DSP-AMOUNT
               DISPLAY "Deposited: $" WS-DSP-AMOUNT
               DISPLAY "New Balance: $" WS-DSP-BALANCE

      * VULNERABILITY V07: SQL Injection in deposit INSERT
               MOVE SPACES TO WS-SQL-STMT
               STRING
                   "INSERT INTO TRANSACTIONS"
                   " (ACCOUNTID, DATE, TYPE, AMOUNT)"
                   " VALUES ('"
                   WS-ACCT-INPUT-ID
                   "', CURRENT DATE, 'DP', "
                   WS-ACCT-INPUT-AMOUNT ")"
                   DELIMITED SIZE INTO WS-SQL-STMT
               END-STRING
      >>IF DB2-ENABLED IS DEFINED
               EXEC SQL
                   EXECUTE IMMEDIATE :WS-SQL-STMT
               END-EXEC
      >>END-IF
      * VULNERABILITY V06: SQLCODE not checked

      * Audit log
               STRING "DEPOSIT: Acct=" ACCT-ID
                      " Amount=" WS-ACCT-INPUT-AMOUNT
                      " NewBal=" ACCT-BALANCE
                      " By=" LS-CURRENT-USER
                      DELIMITED SIZE INTO AUDIT-LOG-RECORD
               WRITE AUDIT-LOG-RECORD
           ELSE
               DISPLAY "Account not found."
           END-IF.

      ******************************************************************
      * 7100 - WITHDRAW FUNDS
      * VULNERABILITY V12: No balance check, negatives accepted
      * VULNERABILITY V09: Numeric overflow on subtraction
      ******************************************************************
       7100-WITHDRAW-FUNDS.
           DISPLAY " "
           DISPLAY "=== WITHDRAW FUNDS ==="
           DISPLAY " "
           DISPLAY "Account ID: " WITH NO ADVANCING
           ACCEPT WS-ACCT-INPUT-ID FROM CONSOLE

           MOVE WS-ACCT-INPUT-ID TO ACCT-ID
           READ ACCOUNT-FILE
      * VULNERABILITY V13: File status not checked

           IF WS-ACCT-FILE-STATUS = "00"
               MOVE ACCT-BALANCE TO WS-DSP-BALANCE
               DISPLAY "Current Balance: $" WS-DSP-BALANCE
               MOVE ACCT-AVAILABLE-BAL TO WS-DSP-BALANCE
               DISPLAY "Available:       $" WS-DSP-BALANCE

               DISPLAY "Withdrawal Amount: " WITH NO ADVANCING
               ACCEPT WS-ACCT-INPUT-AMOUNT FROM CONSOLE

      * VULNERABILITY V12: No validation
      *   Amount not checked against available balance
      *   Negative withdrawals accepted (effectively deposits)
      *   No daily/monthly limit enforcement
               COMPUTE WS-NUMERIC-AMOUNT =
                   FUNCTION NUMVAL(WS-ACCT-INPUT-AMOUNT)
      * VULNERABILITY V09: No ON SIZE ERROR

      * VULNERABILITY V12: No overdraft protection
      *   Withdrawal proceeds even if amount > balance
               COMPUTE ACCT-BALANCE =
                   ACCT-BALANCE - WS-NUMERIC-AMOUNT
               COMPUTE ACCT-AVAILABLE-BAL =
                   ACCT-AVAILABLE-BAL - WS-NUMERIC-AMOUNT
      * VULNERABILITY V09: No ON SIZE ERROR on either COMPUTE

               ACCEPT WS-ACCEPT-DATE FROM DATE YYYYMMDD
               MOVE WS-ACCEPT-DATE TO ACCT-LAST-ACTIVITY
               REWRITE ACCOUNT-RECORD
      * VULNERABILITY V13: REWRITE status not checked

      * Record withdrawal transaction
               ADD 1 TO WS-NEXT-TRAN-ID
               MOVE WS-NEXT-TRAN-ID TO TRAN-ID
               MOVE ACCT-ID TO TRAN-ACCOUNT-ID
               MOVE WS-ACCEPT-DATE TO TRAN-DATE
               ACCEPT WS-ACCEPT-TIME FROM TIME
               MOVE WS-ACCEPT-TIME(1:6) TO TRAN-TIME
               SET TRAN-TYPE-WITHDRAW TO TRUE
               COMPUTE TRAN-AMOUNT = WS-NUMERIC-AMOUNT * -1
               MOVE ACCT-BALANCE TO TRAN-BALANCE-AFTER
               MOVE "Cash Withdrawal" TO TRAN-DESCRIPTION
               MOVE LS-CURRENT-USER TO TRAN-USER-ID
               MOVE "TERM001" TO TRAN-TERMINAL-ID
               SET TRAN-COMPLETED TO TRUE
               MOVE ZEROS TO TRAN-TO-ACCOUNT
               WRITE TRANSACTION-RECORD
      * VULNERABILITY V13: WRITE status not checked

               MOVE WS-NUMERIC-AMOUNT TO WS-DSP-AMOUNT
               MOVE ACCT-BALANCE TO WS-DSP-BALANCE
               DISPLAY "Withdrawn: $" WS-DSP-AMOUNT
               DISPLAY "New Balance: $" WS-DSP-BALANCE

      * Audit log
               STRING "WITHDRAW: Acct=" ACCT-ID
                      " Amount=" WS-ACCT-INPUT-AMOUNT
                      " NewBal=" ACCT-BALANCE
                      " By=" LS-CURRENT-USER
                      DELIMITED SIZE INTO AUDIT-LOG-RECORD
               WRITE AUDIT-LOG-RECORD
           ELSE
               DISPLAY "Account not found."
           END-IF.

      ******************************************************************
      * 7200 - PAY BILL / PAYMENT
      * VULNERABILITY V07: SQL Injection in payee lookup
      * VULNERABILITY V12: No amount validation
      ******************************************************************
       7200-PAY-BILL.
           DISPLAY " "
           DISPLAY "=== BILL PAYMENT ==="
           DISPLAY " "
           DISPLAY "From Account ID: " WITH NO ADVANCING
           ACCEPT WS-ACCT-INPUT-ID FROM CONSOLE
           DISPLAY "Payee Name: " WITH NO ADVANCING
           ACCEPT WS-ACCT-INPUT-NAME FROM CONSOLE
           DISPLAY "Payment Amount: " WITH NO ADVANCING
           ACCEPT WS-ACCT-INPUT-AMOUNT FROM CONSOLE
           DISPLAY "Payment Reference: " WITH NO ADVANCING
           ACCEPT WS-ACCT-INPUT-MEMO FROM CONSOLE

      * VULNERABILITY V12: No validation on any input
           MOVE WS-ACCT-INPUT-ID TO ACCT-ID
           READ ACCOUNT-FILE
      * VULNERABILITY V13: File status not checked

           IF WS-ACCT-FILE-STATUS = "00"
      * VULNERABILITY V12: No balance check before payment
               COMPUTE WS-NUMERIC-AMOUNT =
                   FUNCTION NUMVAL(WS-ACCT-INPUT-AMOUNT)

               COMPUTE ACCT-BALANCE =
                   ACCT-BALANCE - WS-NUMERIC-AMOUNT
      * VULNERABILITY V09: No ON SIZE ERROR
               COMPUTE ACCT-AVAILABLE-BAL =
                   ACCT-AVAILABLE-BAL - WS-NUMERIC-AMOUNT

               REWRITE ACCOUNT-RECORD
      * VULNERABILITY V13: REWRITE status not checked

      * Record payment transaction
               ADD 1 TO WS-NEXT-TRAN-ID
               MOVE WS-NEXT-TRAN-ID TO TRAN-ID
               MOVE ACCT-ID TO TRAN-ACCOUNT-ID
               ACCEPT WS-ACCEPT-DATE FROM DATE YYYYMMDD
               MOVE WS-ACCEPT-DATE TO TRAN-DATE
               SET TRAN-TYPE-PAYMENT TO TRUE
               COMPUTE TRAN-AMOUNT = WS-NUMERIC-AMOUNT * -1
               MOVE ACCT-BALANCE TO TRAN-BALANCE-AFTER
      * VULNERABILITY V08: Buffer overflow on payee name
               STRING "Payment to " WS-ACCT-INPUT-NAME
                      DELIMITED SIZE INTO TRAN-DESCRIPTION
               MOVE LS-CURRENT-USER TO TRAN-USER-ID
               SET TRAN-COMPLETED TO TRUE
               WRITE TRANSACTION-RECORD
      * VULNERABILITY V13: WRITE status not checked

      * VULNERABILITY V07: SQL Injection in payee lookup
               MOVE SPACES TO WS-SQL-STMT
               STRING
                   "SELECT PAYEE_ID FROM PAYEES"
                   " WHERE PAYEE_NAME = '"
                   WS-ACCT-INPUT-NAME "'"
                   DELIMITED SIZE INTO WS-SQL-STMT
               END-STRING
      >>IF DB2-ENABLED IS DEFINED
               EXEC SQL
                   EXECUTE IMMEDIATE :WS-SQL-STMT
               END-EXEC
      >>END-IF
      * VULNERABILITY V06: SQLCODE not checked

               MOVE WS-NUMERIC-AMOUNT TO WS-DSP-AMOUNT
               DISPLAY "Payment of $" WS-DSP-AMOUNT
                       " to " WS-ACCT-INPUT-NAME " processed."
           ELSE
               DISPLAY "Account not found."
           END-IF.

      ******************************************************************
      * 7300 - BATCH INTEREST CALCULATION
      * VULNERABILITY V09: Compound interest overflow
      * VULNERABILITY V10: No admin role check
      ******************************************************************
       7300-BATCH-INTEREST.
      * VULNERABILITY V10: No admin check for batch operation
           DISPLAY " "
           DISPLAY "=== BATCH INTEREST CALCULATION ==="
           DISPLAY "Processing all savings accounts..."

           MOVE LOW-VALUES TO ACCT-ID
           START ACCOUNT-FILE KEY >= ACCT-ID
           MOVE ZEROS TO WS-DSP-ACCT-COUNT

           PERFORM UNTIL WS-ACCT-FILE-STATUS NOT = "00"
               READ ACCOUNT-FILE NEXT
               IF WS-ACCT-FILE-STATUS = "00"
                   IF ACCT-TYPE = "SA" OR ACCT-TYPE = "CH"
                       ADD 1 TO WS-DSP-ACCT-COUNT

                       MOVE ACCT-INTEREST-RATE
                           TO WS-INTEREST-RATE

      * VULNERABILITY V09: Compound interest without overflow
      *   For large balances this can overflow
                       COMPUTE WS-DAILY-RATE =
                           WS-INTEREST-RATE / 365 / 100

                       COMPUTE WS-INTEREST-AMOUNT =
                           ACCT-BALANCE * WS-DAILY-RATE
      * No ON SIZE ERROR

      * VULNERABILITY V09: Balance update overflow
                       COMPUTE ACCT-BALANCE =
                           ACCT-BALANCE + WS-INTEREST-AMOUNT
      * No ON SIZE ERROR

                       COMPUTE ACCT-AVAILABLE-BAL =
                           ACCT-AVAILABLE-BAL
                           + WS-INTEREST-AMOUNT

                       REWRITE ACCOUNT-RECORD
      * VULNERABILITY V13: REWRITE status not checked

                       MOVE WS-INTEREST-AMOUNT TO WS-DSP-AMOUNT
                       DISPLAY "  Acct " ACCT-ID
                               " Interest: $" WS-DSP-AMOUNT
                   END-IF
               END-IF
           END-PERFORM

           DISPLAY " "
           DISPLAY "Batch interest applied to "
                   WS-DSP-ACCT-COUNT " accounts."

      * VULNERABILITY V07: SQL Injection in batch update
           MOVE SPACES TO WS-SQL-STMT
           STRING
               "UPDATE ACCOUNTS SET BALANCE = BALANCE *"
               " (1 + INTEREST_RATE / 365 / 100)"
               " WHERE ACCOUNT_TYPE IN ('SA','CH')"
               DELIMITED SIZE INTO WS-SQL-STMT
           END-STRING
      >>IF DB2-ENABLED IS DEFINED
           EXEC SQL
               EXECUTE IMMEDIATE :WS-SQL-STMT
           END-EXEC
      >>END-IF
           CONTINUE.
      * VULNERABILITY V06: SQLCODE not checked

      ******************************************************************
      * 7400 - ACCOUNT STATEMENT
      * VULNERABILITY V11: IDOR - view any account statement
      * VULNERABILITY V05: PII in statement output
      ******************************************************************
       7400-ACCOUNT-STATEMENT.
           DISPLAY " "
           DISPLAY "=== ACCOUNT STATEMENT ==="
           DISPLAY " "
           DISPLAY "Account ID: " WITH NO ADVANCING
           ACCEPT WS-ACCT-INPUT-ID FROM CONSOLE

      * VULNERABILITY V11: No ownership check
           MOVE WS-ACCT-INPUT-ID TO ACCT-ID
           READ ACCOUNT-FILE
      * VULNERABILITY V13: File status not checked

           IF WS-ACCT-FILE-STATUS = "00"
      * Print statement header
               DISPLAY " "
               DISPLAY "========================================"
               DISPLAY "  ALTORO MUTUAL BANK"
               DISPLAY "  ACCOUNT STATEMENT"
               DISPLAY "========================================"
               DISPLAY "  Account:  " ACCT-ID
               DISPLAY "  Name:     " ACCT-NAME
               DISPLAY "  Owner:    " ACCT-OWNER-ID
               DISPLAY "  Type:     " ACCT-TYPE

      * Get owner details for statement
               MOVE ACCT-OWNER-ID TO CUST-USER-ID
               READ CUSTOMER-FILE
               IF WS-CUST-FILE-STATUS = "00"
      * VULNERABILITY V05: PII in statement
                   DISPLAY "  Customer: " CUST-FIRST-NAME " "
                           CUST-LAST-NAME
                   DISPLAY "  SSN:      " CUST-SSN
                   DISPLAY "  Address:  " CUST-STREET
                   DISPLAY "            " CUST-CITY ", "
                           CUST-STATE " " CUST-ZIP
               END-IF

               DISPLAY "========================================"
               DISPLAY "  Date       Type   Amount"
                       "          Balance"
               DISPLAY "  --------   ----   ---------------"
                       "  ---------------"

      * Read transactions for this account
               MOVE LOW-VALUES TO TRAN-ID
               START TRANSACTION-FILE KEY >= TRAN-ID
               MOVE ZEROS TO WS-DSP-TRAN-COUNT

               PERFORM UNTIL WS-TRAN-FILE-STATUS NOT = "00"
                   READ TRANSACTION-FILE NEXT
                   IF WS-TRAN-FILE-STATUS = "00"
                       IF TRAN-ACCOUNT-ID = ACCT-ID
                           ADD 1 TO WS-DSP-TRAN-COUNT
                           MOVE TRAN-AMOUNT
                               TO WS-DSP-AMOUNT
                           MOVE TRAN-BALANCE-AFTER
                               TO WS-DSP-BALANCE
                           DISPLAY "  " TRAN-DATE " "
                                   TRAN-TYPE "     $"
                                   WS-DSP-AMOUNT "  $"
                                   WS-DSP-BALANCE
                       END-IF
                   END-IF
               END-PERFORM

               DISPLAY "  --------   ----   ---------------"
                       "  ---------------"
               MOVE ACCT-BALANCE TO WS-DSP-BALANCE
               DISPLAY "  Current Balance:  $" WS-DSP-BALANCE
               DISPLAY "========================================"
               DISPLAY "  Transactions: " WS-DSP-TRAN-COUNT
           ELSE
               DISPLAY "Account not found."
           END-IF.

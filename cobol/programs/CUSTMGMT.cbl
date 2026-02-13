       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTMGMT.
       AUTHOR. ALTORO-MUTUAL-DEVELOPMENT.
       DATE-WRITTEN. 2024-01-15.
       DATE-COMPILED.
      ******************************************************************
      * CUSTMGMT.cbl - Customer Management Module
      * Altoro Mutual Banking System - COBOL Edition
      *
      * Handles customer CRUD operations, customer search,
      * data export, and administrative user management.
      *
      * VULNERABILITIES:
      *   V14 - Command Injection: CALL "SYSTEM" with user input
      *   V15 - Buffer Overflow: SSN and name field overflows
      *   V16 - Path Traversal: unvalidated file paths in OPEN
      *   V17 - Race Condition: TOCTOU in file operations
      *   V18 - Missing Authentication: admin ops without role check
      *   V19 - SQL Injection: customer search with LIKE
      *   V20 - Improper Error Handling: SYSTEM return code ignored
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

           SELECT EXPORT-FILE
               ASSIGN TO WS-EXPORT-FILE-PATH
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-EXPORT-STATUS.

           SELECT IMPORT-FILE
               ASSIGN TO WS-IMPORT-FILE-PATH
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-IMPORT-STATUS.

           SELECT BACKUP-FILE
               ASSIGN TO WS-BACKUP-FILE-PATH
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-BACKUP-STATUS.

           SELECT AUDIT-FILE
               ASSIGN TO "CUSTAUDT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CUST-AUDIT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
           COPY CUSTREC.

       FD  EXPORT-FILE.
       01  EXPORT-RECORD               PIC X(512).

       FD  IMPORT-FILE.
       01  IMPORT-RECORD               PIC X(512).

       FD  BACKUP-FILE.
       01  BACKUP-RECORD               PIC X(512).

       FD  AUDIT-FILE.
       01  AUDIT-RECORD                PIC X(256).

       WORKING-STORAGE SECTION.

      * Copy in shared data structures
           COPY SYSCOPY.
           COPY CUSTWS.

      * Local working fields
       01  WS-PROGRAM-NAME             PIC X(08) VALUE "CUSTMGMT".
       01  WS-CUST-CONTINUE            PIC X(01) VALUE "Y".
           88  WS-CUST-LOOP            VALUE "Y".
           88  WS-CUST-EXIT-FLG        VALUE "N".

      * Menu fields
       01  WS-CUST-MENU-CHOICE         PIC X(02) VALUE SPACES.

      * Input fields - intentionally oversized for buffer overflow
       01  WS-INPUT-FIELDS.
           05  WS-INPUT-USER-ID        PIC X(80).
           05  WS-INPUT-FIRST-NAME     PIC X(80).
           05  WS-INPUT-LAST-NAME      PIC X(80).
           05  WS-INPUT-PASSWORD       PIC X(80).
           05  WS-INPUT-ROLE           PIC X(80).
           05  WS-INPUT-SSN            PIC X(80).
           05  WS-INPUT-DOB            PIC X(80).
           05  WS-INPUT-EMAIL          PIC X(80).
           05  WS-INPUT-PHONE          PIC X(80).
           05  WS-INPUT-STREET         PIC X(80).
           05  WS-INPUT-CITY           PIC X(80).
           05  WS-INPUT-STATE          PIC X(80).
           05  WS-INPUT-ZIP            PIC X(80).
           05  WS-INPUT-SEC-QUESTION   PIC X(256).
           05  WS-INPUT-SEC-ANSWER     PIC X(256).
           05  WS-INPUT-SEARCH-TERM    PIC X(256).
           05  WS-INPUT-CONFIRM        PIC X(01).

      * File path fields
       01  WS-EXPORT-FILE-PATH         PIC X(256) VALUE SPACES.
       01  WS-IMPORT-FILE-PATH         PIC X(256) VALUE SPACES.
       01  WS-BACKUP-FILE-PATH         PIC X(256) VALUE SPACES.

      * File status fields
       01  WS-EXPORT-STATUS            PIC X(02).
       01  WS-IMPORT-STATUS            PIC X(02).
       01  WS-BACKUP-STATUS            PIC X(02).
       01  WS-CUST-AUDIT-STATUS        PIC X(02).

      * System command fields
       01  WS-CMD-BUFFER               PIC X(512).
       01  WS-CMD-RETURN-CODE          PIC S9(04) VALUE ZEROS.
       01  WS-CMD-FILENAME             PIC X(256).
       01  WS-CMD-PARAM                PIC X(256).

      * Counter and display fields
       01  WS-CUST-COUNT               PIC 9(06) VALUE ZEROS.
       01  WS-DSP-COUNT                PIC Z(05)9.
       01  WS-IMPORT-COUNT             PIC 9(06) VALUE ZEROS.
       01  WS-EXPORT-COUNT             PIC 9(06) VALUE ZEROS.

      * Separator
       01  WS-SEPARATOR                PIC X(72) VALUE ALL "-".

      * Date/time
       01  WS-ACCEPT-DATE              PIC 9(08).
       01  WS-ACCEPT-TIME              PIC 9(08).
       01  WS-TIMESTAMP                PIC X(26).

      * File existence check
       01  WS-FILE-EXISTS-FLAG         PIC X(01) VALUE "N".
           88  WS-FILE-EXISTS          VALUE "Y".
           88  WS-FILE-NOT-EXISTS      VALUE "N".
       01  WS-FILE-CHECK-STATUS        PIC X(02).

      * Audit open flag
       01  WS-AUDIT-OPEN-FLAG          PIC X(01) VALUE "N".
       01  WS-FILES-OPEN-FLAG          PIC X(01) VALUE "N".

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
           MOVE "CUSTMGMT" TO WS-PROGRAM-NAME
           PERFORM 0100-OPEN-FILES

      * Check function code from caller
           EVALUATE LS-AUDIT-ACTION
               WHEN "RPWD"
                   PERFORM 5000-RESET-PASSWORD
               WHEN "EXPT"
                   PERFORM 6000-EXPORT-CUSTOMERS
               WHEN OTHER
                   PERFORM 1000-CUSTOMER-MENU
           END-EVALUATE

           PERFORM 0900-CLOSE-FILES
           GOBACK.

      ******************************************************************
      * 0100 - OPEN FILES
      ******************************************************************
       0100-OPEN-FILES.
           OPEN I-O CUSTOMER-FILE
      * VULNERABILITY V20: File status not checked
           OPEN EXTEND AUDIT-FILE
      * VULNERABILITY V20: File status not checked
           MOVE "Y" TO WS-FILES-OPEN-FLAG
           MOVE "Y" TO WS-AUDIT-OPEN-FLAG.

      ******************************************************************
      * 0900 - CLOSE FILES
      ******************************************************************
       0900-CLOSE-FILES.
           IF WS-FILES-OPEN-FLAG = "Y"
               CLOSE CUSTOMER-FILE
           END-IF
           IF WS-AUDIT-OPEN-FLAG = "Y"
               CLOSE AUDIT-FILE
           END-IF.

      ******************************************************************
      * 1000 - CUSTOMER MANAGEMENT MENU
      ******************************************************************
       1000-CUSTOMER-MENU.
           MOVE "Y" TO WS-CUST-CONTINUE

           PERFORM UNTIL WS-CUST-EXIT-FLG
               DISPLAY " "
               DISPLAY WS-SEPARATOR
               DISPLAY "  CUSTOMER MANAGEMENT"
               DISPLAY "  User: " LS-CURRENT-USER
               DISPLAY WS-SEPARATOR
               DISPLAY " "
               DISPLAY "  1. View Customer"
               DISPLAY "  2. Add New Customer"
               DISPLAY "  3. Update Customer"
               DISPLAY "  4. Delete Customer"
               DISPLAY "  5. Search Customers"
               DISPLAY "  6. Reset Password"
               DISPLAY "  7. Export Customer Data"
               DISPLAY "  8. Import Customer Data"
               DISPLAY "  9. Backup Customer File"
               DISPLAY " "
               DISPLAY "  0. Return to Main Menu"
               DISPLAY " "
               DISPLAY "Enter option: " WITH NO ADVANCING
               ACCEPT WS-CUST-MENU-CHOICE FROM CONSOLE

               EVALUATE WS-CUST-MENU-CHOICE
                   WHEN "1"
                       PERFORM 2000-VIEW-CUSTOMER
                   WHEN "2"
                       PERFORM 3000-ADD-CUSTOMER
                   WHEN "3"
                       PERFORM 3500-UPDATE-CUSTOMER
                   WHEN "4"
      * VULNERABILITY V18: No admin check
                       PERFORM 4000-DELETE-CUSTOMER
                   WHEN "5"
                       PERFORM 4500-SEARCH-CUSTOMERS
                   WHEN "6"
      * VULNERABILITY V18: No admin check
                       PERFORM 5000-RESET-PASSWORD
                   WHEN "7"
                       PERFORM 6000-EXPORT-CUSTOMERS
                   WHEN "8"
                       PERFORM 7000-IMPORT-CUSTOMERS
                   WHEN "9"
                       PERFORM 8000-BACKUP-CUSTOMER-FILE
                   WHEN "0"
                       MOVE "N" TO WS-CUST-CONTINUE
                   WHEN OTHER
                       DISPLAY "Invalid option."
               END-EVALUATE
           END-PERFORM.

      ******************************************************************
      * 2000 - VIEW CUSTOMER
      ******************************************************************
       2000-VIEW-CUSTOMER.
           DISPLAY " "
           DISPLAY "Enter Customer ID: " WITH NO ADVANCING
           ACCEPT WS-INPUT-USER-ID FROM CONSOLE

      * VULNERABILITY V15: Buffer overflow
      *   WS-INPUT-USER-ID is PIC X(80) but
      *   CUST-USER-ID is PIC X(20)
           MOVE WS-INPUT-USER-ID TO CUST-USER-ID

           READ CUSTOMER-FILE
      * VULNERABILITY V20: File status not checked

           IF WS-CUST-FILE-STATUS = "00"
               DISPLAY " "
               DISPLAY "  CUSTOMER DETAILS"
               DISPLAY "  " WS-SEPARATOR
               DISPLAY "  User ID:    " CUST-USER-ID
               DISPLAY "  Name:       " CUST-FIRST-NAME " "
                       CUST-LAST-NAME
               DISPLAY "  Role:       " CUST-ROLE
      * VULNERABILITY V05: Display PII
               DISPLAY "  SSN:        " CUST-SSN
               DISPLAY "  DOB:        " CUST-DOB
               DISPLAY "  Email:      " CUST-EMAIL
               DISPLAY "  Phone:      " CUST-PHONE
               DISPLAY "  Address:    " CUST-STREET
               DISPLAY "              " CUST-CITY ", "
                       CUST-STATE " " CUST-ZIP
      * VULNERABILITY V05: Display password in plaintext
               DISPLAY "  Password:   " CUST-PASSWORD
               DISPLAY "  Created:    " CUST-CREATED-DATE
               DISPLAY "  Last Login: " CUST-LAST-LOGIN
               DISPLAY "  Attempts:   " CUST-LOGIN-ATTEMPTS
               DISPLAY "  Locked:     " CUST-LOCKED-FLAG
      * VULNERABILITY V05: Display security Q&A
               DISPLAY "  Sec Q:      " CUST-SECURITY-QUESTION
               DISPLAY "  Sec A:      " CUST-SECURITY-ANSWER
               DISPLAY "  " WS-SEPARATOR

      * VULNERABILITY V05: Log full customer view with PII
               STRING "CUST_VIEW: ID=" CUST-USER-ID
                      " SSN=" CUST-SSN
                      " Password=" CUST-PASSWORD
                      " ViewedBy=" LS-CURRENT-USER
                      DELIMITED SIZE INTO AUDIT-RECORD
               WRITE AUDIT-RECORD
           ELSE
               DISPLAY "Customer not found."
           END-IF.

      ******************************************************************
      * 3000 - ADD NEW CUSTOMER
      * VULNERABILITY V15: Buffer overflow on multiple fields
      * VULNERABILITY V18: No admin check
      ******************************************************************
       3000-ADD-CUSTOMER.
      * VULNERABILITY V18: No admin role check
      *   Any user can create new customers
           DISPLAY " "
           DISPLAY "=== ADD NEW CUSTOMER ==="
           DISPLAY " "

           DISPLAY "User ID: " WITH NO ADVANCING
           ACCEPT WS-INPUT-USER-ID FROM CONSOLE
      * VULNERABILITY V15: 80 chars into 20-char field
           MOVE WS-INPUT-USER-ID TO CUST-USER-ID

           DISPLAY "Password: " WITH NO ADVANCING
           ACCEPT WS-INPUT-PASSWORD FROM CONSOLE
      * VULNERABILITY V04: Password stored in plaintext
      * VULNERABILITY V15: 80 chars into 20-char field
           MOVE WS-INPUT-PASSWORD TO CUST-PASSWORD

           DISPLAY "First Name: " WITH NO ADVANCING
           ACCEPT WS-INPUT-FIRST-NAME FROM CONSOLE
      * VULNERABILITY V15: 80 chars into 30-char field
           MOVE WS-INPUT-FIRST-NAME TO CUST-FIRST-NAME

           DISPLAY "Last Name: " WITH NO ADVANCING
           ACCEPT WS-INPUT-LAST-NAME FROM CONSOLE
      * VULNERABILITY V15: 80 chars into 30-char field
           MOVE WS-INPUT-LAST-NAME TO CUST-LAST-NAME

           DISPLAY "Role (USER/ADMIN): " WITH NO ADVANCING
           ACCEPT WS-INPUT-ROLE FROM CONSOLE
      * VULNERABILITY V12: No validation on role value
      * VULNERABILITY V15: 80 chars into 5-char field
           MOVE WS-INPUT-ROLE TO CUST-ROLE

           DISPLAY "SSN (XXX-XX-XXXX): " WITH NO ADVANCING
           ACCEPT WS-INPUT-SSN FROM CONSOLE
      * VULNERABILITY V15: 80 chars into 11-char field
      *   No format validation on SSN
           MOVE WS-INPUT-SSN TO CUST-SSN

           DISPLAY "Date of Birth (YYYY-MM-DD): " WITH NO ADVANCING
           ACCEPT WS-INPUT-DOB FROM CONSOLE
           MOVE WS-INPUT-DOB TO CUST-DOB

           DISPLAY "Email: " WITH NO ADVANCING
           ACCEPT WS-INPUT-EMAIL FROM CONSOLE
           MOVE WS-INPUT-EMAIL TO CUST-EMAIL

           DISPLAY "Phone: " WITH NO ADVANCING
           ACCEPT WS-INPUT-PHONE FROM CONSOLE
           MOVE WS-INPUT-PHONE TO CUST-PHONE

           DISPLAY "Street: " WITH NO ADVANCING
           ACCEPT WS-INPUT-STREET FROM CONSOLE
           MOVE WS-INPUT-STREET TO CUST-STREET

           DISPLAY "City: " WITH NO ADVANCING
           ACCEPT WS-INPUT-CITY FROM CONSOLE
           MOVE WS-INPUT-CITY TO CUST-CITY

           DISPLAY "State: " WITH NO ADVANCING
           ACCEPT WS-INPUT-STATE FROM CONSOLE
      * VULNERABILITY V15: 80 chars into 2-char field
           MOVE WS-INPUT-STATE TO CUST-STATE

           DISPLAY "ZIP: " WITH NO ADVANCING
           ACCEPT WS-INPUT-ZIP FROM CONSOLE
           MOVE WS-INPUT-ZIP TO CUST-ZIP

           DISPLAY "Security Question: " WITH NO ADVANCING
           ACCEPT WS-INPUT-SEC-QUESTION FROM CONSOLE
      * VULNERABILITY V15: 256 chars into 80-char field
           MOVE WS-INPUT-SEC-QUESTION TO CUST-SECURITY-QUESTION

           DISPLAY "Security Answer: " WITH NO ADVANCING
           ACCEPT WS-INPUT-SEC-ANSWER FROM CONSOLE
      * VULNERABILITY V15: 256 chars into 40-char field
           MOVE WS-INPUT-SEC-ANSWER TO CUST-SECURITY-ANSWER

      * Set defaults
           ACCEPT WS-ACCEPT-DATE FROM DATE YYYYMMDD
           MOVE WS-ACCEPT-DATE TO CUST-CREATED-DATE
           MOVE SPACES TO CUST-LAST-LOGIN
           MOVE ZEROS TO CUST-LOGIN-ATTEMPTS
           SET CUST-IS-UNLOCKED TO TRUE

           WRITE CUSTOMER-RECORD
      * VULNERABILITY V20: WRITE status not checked

      * Also insert via DB2
      * VULNERABILITY V19: SQL Injection with user input
           MOVE SPACES TO WS-SQL-STMT
           STRING
               "INSERT INTO PEOPLE"
               " (USER_ID, PASSWORD, FIRST_NAME, LAST_NAME,"
               " ROLE) VALUES ('"
               WS-INPUT-USER-ID "', '"
               WS-INPUT-PASSWORD "', '"
               WS-INPUT-FIRST-NAME "', '"
               WS-INPUT-LAST-NAME "', '"
               WS-INPUT-ROLE "')"
               DELIMITED SIZE INTO WS-SQL-STMT
           END-STRING

      >>IF DB2-ENABLED IS DEFINED
           EXEC SQL
               EXECUTE IMMEDIATE :WS-SQL-STMT
           END-EXEC
      >>END-IF
           CONTINUE.
      * VULNERABILITY V06: SQLCODE not checked

           DISPLAY " "
           DISPLAY "Customer created successfully."
           DISPLAY "User ID: " CUST-USER-ID

      * VULNERABILITY V05: Log with password and SSN
           STRING "CUST_ADD: ID=" CUST-USER-ID
                  " Password=" CUST-PASSWORD
                  " SSN=" CUST-SSN
                  " Role=" CUST-ROLE
                  " CreatedBy=" LS-CURRENT-USER
                  DELIMITED SIZE INTO AUDIT-RECORD
           WRITE AUDIT-RECORD.

      ******************************************************************
      * 3500 - UPDATE CUSTOMER
      * VULNERABILITY V15: Buffer overflow on updates
      ******************************************************************
       3500-UPDATE-CUSTOMER.
           DISPLAY " "
           DISPLAY "=== UPDATE CUSTOMER ==="
           DISPLAY " "
           DISPLAY "Customer ID: " WITH NO ADVANCING
           ACCEPT WS-INPUT-USER-ID FROM CONSOLE

      * VULNERABILITY V15: Buffer overflow
           MOVE WS-INPUT-USER-ID TO CUST-USER-ID
           READ CUSTOMER-FILE
      * VULNERABILITY V20: File status not checked

           IF WS-CUST-FILE-STATUS = "00"
               DISPLAY "Current Name: " CUST-FIRST-NAME " "
                       CUST-LAST-NAME
               DISPLAY "New First Name (blank to keep): "
                   WITH NO ADVANCING
               ACCEPT WS-INPUT-FIRST-NAME FROM CONSOLE
               IF WS-INPUT-FIRST-NAME NOT = SPACES
      * VULNERABILITY V15: Buffer overflow
                   MOVE WS-INPUT-FIRST-NAME TO CUST-FIRST-NAME
               END-IF

               DISPLAY "New Last Name (blank to keep): "
                   WITH NO ADVANCING
               ACCEPT WS-INPUT-LAST-NAME FROM CONSOLE
               IF WS-INPUT-LAST-NAME NOT = SPACES
                   MOVE WS-INPUT-LAST-NAME TO CUST-LAST-NAME
               END-IF

               DISPLAY "New Email (blank to keep): "
                   WITH NO ADVANCING
               ACCEPT WS-INPUT-EMAIL FROM CONSOLE
               IF WS-INPUT-EMAIL NOT = SPACES
                   MOVE WS-INPUT-EMAIL TO CUST-EMAIL
               END-IF

               DISPLAY "New Phone (blank to keep): "
                   WITH NO ADVANCING
               ACCEPT WS-INPUT-PHONE FROM CONSOLE
               IF WS-INPUT-PHONE NOT = SPACES
                   MOVE WS-INPUT-PHONE TO CUST-PHONE
               END-IF

               REWRITE CUSTOMER-RECORD
      * VULNERABILITY V20: REWRITE status not checked

      * VULNERABILITY V19: SQL Injection in update
               MOVE SPACES TO WS-SQL-STMT
               STRING
                   "UPDATE PEOPLE SET FIRST_NAME = '"
                   CUST-FIRST-NAME "', LAST_NAME = '"
                   CUST-LAST-NAME
                   "' WHERE USER_ID = '"
                   WS-INPUT-USER-ID "'"
                   DELIMITED SIZE INTO WS-SQL-STMT
               END-STRING

      >>IF DB2-ENABLED IS DEFINED
               EXEC SQL
                   EXECUTE IMMEDIATE :WS-SQL-STMT
               END-EXEC
      >>END-IF
      * VULNERABILITY V06: SQLCODE not checked

               DISPLAY "Customer updated successfully."
           ELSE
               DISPLAY "Customer not found."
           END-IF.

      ******************************************************************
      * 4000 - DELETE CUSTOMER
      * VULNERABILITY V18: No admin role check
      ******************************************************************
       4000-DELETE-CUSTOMER.
      * VULNERABILITY V18: No admin check
      *   Any authenticated user can delete any customer
           DISPLAY " "
           DISPLAY "=== DELETE CUSTOMER ==="
           DISPLAY " "
           DISPLAY "Customer ID to delete: " WITH NO ADVANCING
           ACCEPT WS-INPUT-USER-ID FROM CONSOLE

           MOVE WS-INPUT-USER-ID TO CUST-USER-ID
           READ CUSTOMER-FILE
      * VULNERABILITY V20: File status not checked

           IF WS-CUST-FILE-STATUS = "00"
               DISPLAY "Customer: " CUST-FIRST-NAME " "
                       CUST-LAST-NAME
      * VULNERABILITY V05: Show SSN during delete confirmation
               DISPLAY "SSN: " CUST-SSN
               DISPLAY "Confirm delete (Y/N): "
                   WITH NO ADVANCING
               ACCEPT WS-INPUT-CONFIRM FROM CONSOLE
               IF WS-INPUT-CONFIRM = "Y"
                   DELETE CUSTOMER-FILE RECORD
      * VULNERABILITY V20: DELETE status not checked

      * VULNERABILITY V19: SQL Injection in delete
                   MOVE SPACES TO WS-SQL-STMT
                   STRING
                       "DELETE FROM PEOPLE WHERE USER_ID = '"
                       WS-INPUT-USER-ID "'"
                       DELIMITED SIZE INTO WS-SQL-STMT
                   END-STRING

      >>IF DB2-ENABLED IS DEFINED
                   EXEC SQL
                       EXECUTE IMMEDIATE :WS-SQL-STMT
                   END-EXEC
      >>END-IF
      * VULNERABILITY V06: SQLCODE not checked

                   DISPLAY "Customer deleted."

      * VULNERABILITY V05: Log deleted customer with PII
                   STRING "CUST_DELETE: ID=" WS-INPUT-USER-ID
                          " SSN=" CUST-SSN
                          " Password=" CUST-PASSWORD
                          " DeletedBy=" LS-CURRENT-USER
                          DELIMITED SIZE INTO AUDIT-RECORD
                   WRITE AUDIT-RECORD
               ELSE
                   DISPLAY "Delete cancelled."
               END-IF
           ELSE
               DISPLAY "Customer not found."
           END-IF.

      ******************************************************************
      * 4500 - SEARCH CUSTOMERS
      * VULNERABILITY V19: SQL Injection with LIKE pattern
      ******************************************************************
       4500-SEARCH-CUSTOMERS.
           DISPLAY " "
           DISPLAY "=== SEARCH CUSTOMERS ==="
           DISPLAY " "
           DISPLAY "Search term (name or ID): " WITH NO ADVANCING
           ACCEPT WS-INPUT-SEARCH-TERM FROM CONSOLE

      * VULNERABILITY V19: SQL Injection
      *   Search term concatenated into LIKE clause
      *   Attacker can enter: %' OR '1'='1
           MOVE SPACES TO WS-SQL-STMT
           STRING
               "SELECT USER_ID, FIRST_NAME, LAST_NAME, ROLE"
               " FROM PEOPLE"
               " WHERE USER_ID LIKE '%"
               WS-INPUT-SEARCH-TERM "%'"
               " OR FIRST_NAME LIKE '%"
               WS-INPUT-SEARCH-TERM "%'"
               " OR LAST_NAME LIKE '%"
               WS-INPUT-SEARCH-TERM "%'"
               DELIMITED SIZE INTO WS-SQL-STMT
           END-STRING

      * VULNERABILITY V05: Display SQL
           DISPLAY "DEBUG SQL: " WS-SQL-STMT

      >>IF DB2-ENABLED IS DEFINED
           EXEC SQL
               EXECUTE IMMEDIATE :WS-SQL-STMT
           END-EXEC
      >>END-IF
           CONTINUE.
      * VULNERABILITY V06: SQLCODE not checked

      * Also search via file
           DISPLAY " "
           DISPLAY "  SEARCH RESULTS"
           DISPLAY "  " WS-SEPARATOR
           DISPLAY "  User ID            | Name"
                   "                       | Role  | SSN"
           DISPLAY "  " WS-SEPARATOR
           MOVE ZEROS TO WS-CUST-COUNT

           MOVE LOW-VALUES TO CUST-USER-ID
           START CUSTOMER-FILE KEY >= CUST-USER-ID
      * VULNERABILITY V20: START status not checked

           PERFORM UNTIL WS-CUST-FILE-STATUS NOT = "00"
               READ CUSTOMER-FILE NEXT
               IF WS-CUST-FILE-STATUS = "00"
                   IF CUST-USER-ID(1:FUNCTION LENGTH(
                       FUNCTION TRIM(WS-INPUT-SEARCH-TERM)))
                       = FUNCTION TRIM(WS-INPUT-SEARCH-TERM)
                   OR CUST-FIRST-NAME(1:FUNCTION LENGTH(
                       FUNCTION TRIM(WS-INPUT-SEARCH-TERM)))
                       = FUNCTION TRIM(WS-INPUT-SEARCH-TERM)
                   OR CUST-LAST-NAME(1:FUNCTION LENGTH(
                       FUNCTION TRIM(WS-INPUT-SEARCH-TERM)))
                       = FUNCTION TRIM(WS-INPUT-SEARCH-TERM)
                       ADD 1 TO WS-CUST-COUNT
      * VULNERABILITY V05: Display SSN in search results
                       DISPLAY "  " CUST-USER-ID " | "
                               CUST-FIRST-NAME " "
                               CUST-LAST-NAME " | "
                               CUST-ROLE " | "
                               CUST-SSN
                   END-IF
               END-IF
           END-PERFORM

           DISPLAY "  " WS-SEPARATOR
           DISPLAY "  Results found: " WS-CUST-COUNT.

      ******************************************************************
      * 5000 - RESET PASSWORD
      * VULNERABILITY V18: No admin check
      * VULNERABILITY V04: New password in plaintext
      ******************************************************************
       5000-RESET-PASSWORD.
      * VULNERABILITY V18: No admin role verification
           DISPLAY " "
           DISPLAY "=== RESET USER PASSWORD ==="
           DISPLAY " "
           DISPLAY "User ID: " WITH NO ADVANCING
           ACCEPT WS-INPUT-USER-ID FROM CONSOLE

           MOVE WS-INPUT-USER-ID TO CUST-USER-ID
           READ CUSTOMER-FILE
      * VULNERABILITY V20: File status not checked

           IF WS-CUST-FILE-STATUS = "00"
               DISPLAY "Customer: " CUST-FIRST-NAME " "
                       CUST-LAST-NAME

               DISPLAY "New Password: " WITH NO ADVANCING
               ACCEPT WS-INPUT-PASSWORD FROM CONSOLE

      * VULNERABILITY V04: Store password in plaintext
               MOVE WS-INPUT-PASSWORD TO CUST-PASSWORD
               MOVE ZEROS TO CUST-LOGIN-ATTEMPTS
               SET CUST-IS-UNLOCKED TO TRUE

               REWRITE CUSTOMER-RECORD
      * VULNERABILITY V20: REWRITE status not checked

      * VULNERABILITY V19: SQL Injection in password update
               MOVE SPACES TO WS-SQL-STMT
               STRING
                   "UPDATE PEOPLE SET PASSWORD = '"
                   WS-INPUT-PASSWORD
                   "' WHERE USER_ID = '"
                   WS-INPUT-USER-ID "'"
                   DELIMITED SIZE INTO WS-SQL-STMT
               END-STRING

      >>IF DB2-ENABLED IS DEFINED
               EXEC SQL
                   EXECUTE IMMEDIATE :WS-SQL-STMT
               END-EXEC
      >>END-IF
      * VULNERABILITY V06: SQLCODE not checked

               DISPLAY "Password reset successfully."
      * VULNERABILITY V05: Display new password
               DISPLAY "New password: " WS-INPUT-PASSWORD

      * VULNERABILITY V05: Log password reset with new password
               STRING "PWD_RESET: ID=" WS-INPUT-USER-ID
                      " NewPwd=" WS-INPUT-PASSWORD
                      " ResetBy=" LS-CURRENT-USER
                      DELIMITED SIZE INTO AUDIT-RECORD
               WRITE AUDIT-RECORD
           ELSE
               DISPLAY "Customer not found."
           END-IF.

      ******************************************************************
      * 6000 - EXPORT CUSTOMER DATA
      * VULNERABILITY V14: Command Injection via filename
      * VULNERABILITY V16: Path Traversal in export path
      ******************************************************************
       6000-EXPORT-CUSTOMERS.
           DISPLAY " "
           DISPLAY "=== EXPORT CUSTOMER DATA ==="
           DISPLAY " "

           DISPLAY "Export filename: " WITH NO ADVANCING
           ACCEPT WS-CMD-FILENAME FROM CONSOLE

      * VULNERABILITY V16: Path traversal
      *   User can enter: ../../etc/passwd or /tmp/../../sensitive
      *   No validation or sanitization of the path
           STRING "/opt/altoro/export/" WS-CMD-FILENAME
                  DELIMITED SIZE INTO WS-EXPORT-FILE-PATH

      * VULNERABILITY V16: Path traversal - direct use
           DISPLAY "Export path: " WS-EXPORT-FILE-PATH
           OPEN OUTPUT EXPORT-FILE
      * VULNERABILITY V20: File status not checked

           IF WS-EXPORT-STATUS = "00"
      * Read all customers and write to export file
               MOVE LOW-VALUES TO CUST-USER-ID
               START CUSTOMER-FILE KEY >= CUST-USER-ID
               MOVE ZEROS TO WS-EXPORT-COUNT

               PERFORM UNTIL WS-CUST-FILE-STATUS NOT = "00"
                   READ CUSTOMER-FILE NEXT
                   IF WS-CUST-FILE-STATUS = "00"
                       ADD 1 TO WS-EXPORT-COUNT
      * VULNERABILITY V21: Export includes passwords and SSNs
                       STRING
                           CUST-USER-ID ","
                           CUST-PASSWORD ","
                           CUST-FIRST-NAME ","
                           CUST-LAST-NAME ","
                           CUST-ROLE ","
                           CUST-SSN ","
                           CUST-DOB ","
                           CUST-EMAIL ","
                           CUST-PHONE
                           DELIMITED SIZE INTO EXPORT-RECORD
                       WRITE EXPORT-RECORD
      * VULNERABILITY V20: WRITE status not checked
                   END-IF
               END-PERFORM

               CLOSE EXPORT-FILE
               DISPLAY "Exported " WS-EXPORT-COUNT " customers."

      * VULNERABILITY V14: Command Injection
      *   After export, set permissions via system command
      *   using the user-supplied filename
               MOVE SPACES TO WS-CMD-BUFFER
               STRING "chmod 777 /opt/altoro/export/"
                      WS-CMD-FILENAME
                      DELIMITED SIZE INTO WS-CMD-BUFFER

      * VULNERABILITY V14: User input in CALL "SYSTEM"
               CALL "SYSTEM" USING WS-CMD-BUFFER
      * VULNERABILITY V20: Return code ignored

      * VULNERABILITY V14: Second command injection point
      *   Notify via email using system command
               DISPLAY "Notification email (blank to skip): "
                   WITH NO ADVANCING
               ACCEPT WS-CMD-PARAM FROM CONSOLE

               IF WS-CMD-PARAM NOT = SPACES
                   MOVE SPACES TO WS-CMD-BUFFER
                   STRING
                       "echo 'Export complete: "
                       WS-CMD-FILENAME
                       "' | mail -s 'Altoro Export' "
                       WS-CMD-PARAM
                       DELIMITED SIZE INTO WS-CMD-BUFFER

      * VULNERABILITY V14: Command injection via email address
                   CALL "SYSTEM" USING WS-CMD-BUFFER
      * VULNERABILITY V20: Return code ignored
               END-IF
           ELSE
               DISPLAY "Error opening export file."
           END-IF.

      ******************************************************************
      * 7000 - IMPORT CUSTOMER DATA
      * VULNERABILITY V16: Path Traversal on import file
      * VULNERABILITY V17: Race Condition (TOCTOU)
      ******************************************************************
       7000-IMPORT-CUSTOMERS.
           DISPLAY " "
           DISPLAY "=== IMPORT CUSTOMER DATA ==="
           DISPLAY " "

           DISPLAY "Import file path: " WITH NO ADVANCING
           ACCEPT WS-CMD-FILENAME FROM CONSOLE

      * VULNERABILITY V16: Path traversal
      *   No validation on file path - attacker can read any file
           MOVE WS-CMD-FILENAME TO WS-IMPORT-FILE-PATH

      * VULNERABILITY V17: Race Condition (TOCTOU)
      *   Check if file exists, then open it later
      *   File could be swapped between check and open
           MOVE "N" TO WS-FILE-EXISTS-FLAG

      * Step 1: Check if file exists (Time Of Check)
           MOVE SPACES TO WS-CMD-BUFFER
           STRING "test -f " WS-CMD-FILENAME
                  DELIMITED SIZE INTO WS-CMD-BUFFER
           CALL "SYSTEM" USING WS-CMD-BUFFER
               RETURNING WS-CMD-RETURN-CODE
      * VULNERABILITY V20: Return code not properly evaluated

           IF WS-CMD-RETURN-CODE = ZEROS
               MOVE "Y" TO WS-FILE-EXISTS-FLAG
               DISPLAY "File found. Proceeding with import..."
           ELSE
               DISPLAY "File not found: " WS-CMD-FILENAME
               GO TO 7000-IMPORT-EXIT
           END-IF

      * VULNERABILITY V17: Time gap between check and use
      *   File could be replaced with a symlink or different file

      * Step 2: Open the file (Time Of Use)
           OPEN INPUT IMPORT-FILE
      * VULNERABILITY V20: File status not checked

           IF WS-IMPORT-STATUS = "00"
               MOVE ZEROS TO WS-IMPORT-COUNT

               PERFORM UNTIL WS-IMPORT-STATUS NOT = "00"
                   READ IMPORT-FILE INTO IMPORT-RECORD
                   IF WS-IMPORT-STATUS = "00"
                       ADD 1 TO WS-IMPORT-COUNT
      * VULNERABILITY V15: Buffer overflow
      *   Import record parsed into customer fields without
      *   length validation
                       MOVE IMPORT-RECORD(1:20)
                           TO CUST-USER-ID
                       MOVE IMPORT-RECORD(22:20)
                           TO CUST-PASSWORD
                       MOVE IMPORT-RECORD(43:30)
                           TO CUST-FIRST-NAME
                       MOVE IMPORT-RECORD(74:30)
                           TO CUST-LAST-NAME
                       MOVE IMPORT-RECORD(105:5)
                           TO CUST-ROLE
                       MOVE IMPORT-RECORD(111:11)
                           TO CUST-SSN

                       WRITE CUSTOMER-RECORD
      * VULNERABILITY V20: WRITE status not checked
                   END-IF
               END-PERFORM

               CLOSE IMPORT-FILE
               DISPLAY "Imported " WS-IMPORT-COUNT " customers."
           ELSE
               DISPLAY "Error opening import file."
           END-IF.

       7000-IMPORT-EXIT.
           CONTINUE.

      ******************************************************************
      * 8000 - BACKUP CUSTOMER FILE
      * VULNERABILITY V14: Command injection in backup path
      * VULNERABILITY V16: Path traversal in backup destination
      * VULNERABILITY V17: Race condition in backup process
      ******************************************************************
       8000-BACKUP-CUSTOMER-FILE.
           DISPLAY " "
           DISPLAY "=== BACKUP CUSTOMER FILE ==="
           DISPLAY " "

           DISPLAY "Backup destination path: " WITH NO ADVANCING
           ACCEPT WS-CMD-FILENAME FROM CONSOLE

      * VULNERABILITY V16: Path traversal
      *   No validation on backup destination path
           MOVE WS-CMD-FILENAME TO WS-BACKUP-FILE-PATH

      * VULNERABILITY V17: Race Condition
      *   Check directory exists, then write to it
           MOVE SPACES TO WS-CMD-BUFFER
           STRING "test -d " WS-CMD-FILENAME
                  DELIMITED SIZE INTO WS-CMD-BUFFER
           CALL "SYSTEM" USING WS-CMD-BUFFER
               RETURNING WS-CMD-RETURN-CODE

      * VULNERABILITY V14: Command Injection
      *   Backup using cp command with user-supplied path
           ACCEPT WS-ACCEPT-DATE FROM DATE YYYYMMDD
           MOVE SPACES TO WS-CMD-BUFFER
           STRING "cp /opt/altoro/data/CUSTFILE.dat "
                  WS-CMD-FILENAME
                  "/CUSTFILE_" WS-ACCEPT-DATE ".bak"
                  DELIMITED SIZE INTO WS-CMD-BUFFER

      * VULNERABILITY V14: User input in system command
           DISPLAY "Executing: " WS-CMD-BUFFER
           CALL "SYSTEM" USING WS-CMD-BUFFER
      * VULNERABILITY V20: Return code ignored

      * VULNERABILITY V14: Another command injection
      *   Compress the backup
           MOVE SPACES TO WS-CMD-BUFFER
           STRING "gzip " WS-CMD-FILENAME
                  "/CUSTFILE_" WS-ACCEPT-DATE ".bak"
                  DELIMITED SIZE INTO WS-CMD-BUFFER
           CALL "SYSTEM" USING WS-CMD-BUFFER
      * VULNERABILITY V20: Return code ignored

      * Log backup
           STRING "BACKUP: Dest=" WS-CMD-FILENAME
                  " Date=" WS-ACCEPT-DATE
                  " By=" LS-CURRENT-USER
                  DELIMITED SIZE INTO AUDIT-RECORD
           WRITE AUDIT-RECORD

           DISPLAY "Backup completed."
           DISPLAY "Location: " WS-CMD-FILENAME.

      ******************************************************************
      * 8500 - BULK PASSWORD RESET
      * VULNERABILITY V18: No admin check
      * VULNERABILITY V04: All passwords set to same plaintext value
      * VULNERABILITY V14: Command injection in notification
      ******************************************************************
       8500-BULK-PASSWORD-RESET.
      * VULNERABILITY V18: No admin role check
           DISPLAY " "
           DISPLAY "=== BULK PASSWORD RESET ==="
           DISPLAY "WARNING: This will reset ALL user passwords."
           DISPLAY "Continue? (Y/N): " WITH NO ADVANCING
           ACCEPT WS-INPUT-CONFIRM FROM CONSOLE

           IF WS-INPUT-CONFIRM = "Y"
               DISPLAY "New default password: " WITH NO ADVANCING
               ACCEPT WS-INPUT-PASSWORD FROM CONSOLE

               MOVE LOW-VALUES TO CUST-USER-ID
               START CUSTOMER-FILE KEY >= CUST-USER-ID
      * VULNERABILITY V20: START status not checked
               MOVE ZEROS TO WS-CUST-COUNT

               PERFORM UNTIL WS-CUST-FILE-STATUS NOT = "00"
                   READ CUSTOMER-FILE NEXT
                   IF WS-CUST-FILE-STATUS = "00"
                       ADD 1 TO WS-CUST-COUNT
      * VULNERABILITY V04: Set all passwords to same value
                       MOVE WS-INPUT-PASSWORD
                           TO CUST-PASSWORD
                       MOVE ZEROS TO CUST-LOGIN-ATTEMPTS
                       SET CUST-IS-UNLOCKED TO TRUE
                       REWRITE CUSTOMER-RECORD
      * VULNERABILITY V20: REWRITE status not checked

      * VULNERABILITY V05: Log each reset with password
                       STRING "BULK_RESET: ID=" CUST-USER-ID
                              " NewPwd=" WS-INPUT-PASSWORD
                              DELIMITED SIZE INTO AUDIT-RECORD
                       WRITE AUDIT-RECORD
                   END-IF
               END-PERFORM

               DISPLAY WS-CUST-COUNT " passwords reset."

      * VULNERABILITY V19: SQL Injection in bulk update
               MOVE SPACES TO WS-SQL-STMT
               STRING
                   "UPDATE PEOPLE SET PASSWORD = '"
                   WS-INPUT-PASSWORD "'"
                   DELIMITED SIZE INTO WS-SQL-STMT
               END-STRING
      >>IF DB2-ENABLED IS DEFINED
               EXEC SQL
                   EXECUTE IMMEDIATE :WS-SQL-STMT
               END-EXEC
      >>END-IF
      * VULNERABILITY V06: SQLCODE not checked

      * VULNERABILITY V14: Command injection in notification
               MOVE SPACES TO WS-CMD-BUFFER
               STRING "echo 'Bulk password reset: "
                      WS-CUST-COUNT
                      " accounts' | wall"
                      DELIMITED SIZE INTO WS-CMD-BUFFER
               CALL "SYSTEM" USING WS-CMD-BUFFER
      * VULNERABILITY V20: Return code ignored
           ELSE
               DISPLAY "Bulk reset cancelled."
           END-IF.

      ******************************************************************
      * 8600 - CUSTOMER DATA PURGE
      * VULNERABILITY V18: No admin check
      * VULNERABILITY V14: Command injection in purge
      ******************************************************************
       8600-PURGE-CUSTOMER-DATA.
      * VULNERABILITY V18: No admin role check
           DISPLAY " "
           DISPLAY "=== PURGE INACTIVE CUSTOMERS ==="
           DISPLAY "Days inactive threshold: " WITH NO ADVANCING
           ACCEPT WS-INPUT-USER-ID FROM CONSOLE

      * VULNERABILITY V14: Command injection
      *   Uses user input to construct purge command
           MOVE SPACES TO WS-CMD-BUFFER
           STRING "find /opt/altoro/data -name '*.dat'"
                  " -mtime +" WS-INPUT-USER-ID
                  " -exec rm {} ;"
                  DELIMITED SIZE INTO WS-CMD-BUFFER

           DISPLAY "Executing: " WS-CMD-BUFFER
           CALL "SYSTEM" USING WS-CMD-BUFFER
      * VULNERABILITY V20: Return code ignored

      * Also purge via SQL
      * VULNERABILITY V19: SQL Injection in purge
           MOVE SPACES TO WS-SQL-STMT
           STRING
               "DELETE FROM PEOPLE WHERE LAST_LOGIN < "
               "CURRENT DATE - " WS-INPUT-USER-ID " DAYS"
               DELIMITED SIZE INTO WS-SQL-STMT
           END-STRING

           DISPLAY "SQL: " WS-SQL-STMT
      >>IF DB2-ENABLED IS DEFINED
           EXEC SQL
               EXECUTE IMMEDIATE :WS-SQL-STMT
           END-EXEC
      >>END-IF
           CONTINUE.
      * VULNERABILITY V06: SQLCODE not checked

           DISPLAY "Purge completed."

      * Log purge action
           STRING "PURGE: Threshold=" WS-INPUT-USER-ID
                  " days By=" LS-CURRENT-USER
                  DELIMITED SIZE INTO AUDIT-RECORD
           WRITE AUDIT-RECORD.

      ******************************************************************
      * 8700 - GENERATE CUSTOMER EXTRACT FOR THIRD PARTY
      * VULNERABILITY V21: PII sent to external system
      * VULNERABILITY V16: Path traversal in output
      * VULNERABILITY V14: Command injection in transfer
      ******************************************************************
       8700-THIRD-PARTY-EXTRACT.
           DISPLAY " "
           DISPLAY "=== THIRD PARTY DATA EXTRACT ==="
           DISPLAY " "
           DISPLAY "Destination server: " WITH NO ADVANCING
           ACCEPT WS-CMD-PARAM FROM CONSOLE
           DISPLAY "Output file path: " WITH NO ADVANCING
           ACCEPT WS-CMD-FILENAME FROM CONSOLE

      * VULNERABILITY V16: Path traversal
           MOVE WS-CMD-FILENAME TO WS-EXPORT-FILE-PATH
           OPEN OUTPUT EXPORT-FILE
      * VULNERABILITY V20: File status not checked

           IF WS-EXPORT-STATUS = "00"
               MOVE LOW-VALUES TO CUST-USER-ID
               START CUSTOMER-FILE KEY >= CUST-USER-ID
               MOVE ZEROS TO WS-EXPORT-COUNT

               PERFORM UNTIL WS-CUST-FILE-STATUS NOT = "00"
                   READ CUSTOMER-FILE NEXT
                   IF WS-CUST-FILE-STATUS = "00"
                       ADD 1 TO WS-EXPORT-COUNT
      * VULNERABILITY V21: Full PII in extract
      *   SSN, DOB, password all included
                       STRING
                           CUST-USER-ID "|"
                           CUST-FIRST-NAME " "
                           CUST-LAST-NAME "|"
                           CUST-SSN "|"
                           CUST-DOB "|"
                           CUST-EMAIL "|"
                           CUST-PHONE "|"
                           CUST-PASSWORD
                           DELIMITED SIZE INTO EXPORT-RECORD
                       WRITE EXPORT-RECORD
      * VULNERABILITY V20: WRITE status not checked
                   END-IF
               END-PERFORM

               CLOSE EXPORT-FILE

      * VULNERABILITY V14: Command injection
      *   scp with user-supplied server name
               MOVE SPACES TO WS-CMD-BUFFER
               STRING "scp " WS-CMD-FILENAME
                      " " WS-CMD-PARAM
                      ":/incoming/customer_extract.dat"
                      DELIMITED SIZE INTO WS-CMD-BUFFER

               DISPLAY "Transferring: " WS-CMD-BUFFER
               CALL "SYSTEM" USING WS-CMD-BUFFER
      * VULNERABILITY V20: Return code ignored

               DISPLAY WS-EXPORT-COUNT
                       " customer records extracted and"
                       " transferred."

      * VULNERABILITY V05: Log extract details
               STRING "3RD_PARTY_EXTRACT: Count="
                      WS-EXPORT-COUNT
                      " Server=" WS-CMD-PARAM
                      " File=" WS-CMD-FILENAME
                      " By=" LS-CURRENT-USER
                      DELIMITED SIZE INTO AUDIT-RECORD
               WRITE AUDIT-RECORD
           ELSE
               DISPLAY "Error creating extract file."
           END-IF.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. AUTHNTCN.
       AUTHOR. ALTORO-MUTUAL-DEVELOPMENT.
       DATE-WRITTEN. 2024-01-15.
       DATE-COMPILED.
      ******************************************************************
      * AUTHNTCN.cbl - Authentication and Security Module
      * Altoro Mutual Banking System - COBOL Edition
      *
      * Handles user authentication, password management,
      * credential validation, and session establishment.
      *
      * VULNERABILITIES:
      *   V01 - SQL Injection: STRING concatenation in EXEC SQL
      *   V02 - Hardcoded Credentials: admin password in VALUE
      *   V03 - Weak Cryptography: XOR-based password "encryption"
      *   V04 - Plaintext Password Storage: passwords in clear text
      *   V05 - Information Leakage: credentials in error messages
      *   V06 - Improper Error Handling: SQLCODE not checked
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

           SELECT SECURITY-LOG-FILE
               ASSIGN TO "SECLOG"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-SECLOG-STATUS.

           SELECT PASSWORD-HISTORY-FILE
               ASSIGN TO "PWDHIST"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-PWDHIST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
           COPY CUSTREC.

       FD  SECURITY-LOG-FILE.
       01  SECLOG-RECORD               PIC X(256).

       FD  PASSWORD-HISTORY-FILE.
       01  PWDHIST-RECORD.
           05  PWDHIST-USER-ID         PIC X(20).
           05  PWDHIST-OLD-PASSWORD    PIC X(20).
           05  PWDHIST-NEW-PASSWORD    PIC X(20).
           05  PWDHIST-CHANGE-DATE     PIC X(26).
           05  PWDHIST-CHANGED-BY      PIC X(20).
           05  FILLER                  PIC X(150).

       WORKING-STORAGE SECTION.

      * Copy in shared data structures
           COPY SYSCOPY.
           COPY CUSTWS.

      * VULNERABILITY V02: Hardcoded admin credentials
      *   These should be stored in a secure vault or config
      *   but are hardcoded directly in source code
       01  WS-HARDCODED-CREDS.
           05  WS-ADMIN-PASSWORD       PIC X(10)
               VALUE "Altoro1234".
           05  WS-MASTER-KEY           PIC X(08)
               VALUE "MSTR2024".
           05  WS-BACKDOOR-USER        PIC X(10)
               VALUE "superadmin".
           05  WS-BACKDOOR-PASS        PIC X(13)
               VALUE "Alt0r0!Mast3r".
           05  WS-SERVICE-ACCOUNT      PIC X(10)
               VALUE "svc_altoro".
           05  WS-SERVICE-PASSWORD     PIC X(10)
               VALUE "SvcP@ss123".
           05  WS-DB2-CONNECT-STR      PIC X(60)
               VALUE "DSN=ALTORODB;UID=DB2ADMIN;PWD=db2pass;".

      * Authentication working fields
       01  WS-AUTH-FIELDS.
           05  WS-INPUT-USER           PIC X(80).
           05  WS-INPUT-PASSWORD       PIC X(80).
           05  WS-INPUT-NEW-PASSWORD   PIC X(80).
           05  WS-INPUT-CONFIRM-PWD    PIC X(80).
           05  WS-STORED-PASSWORD      PIC X(20).
           05  WS-ENCRYPTED-PASSWORD   PIC X(20).
           05  WS-AUTH-RESULT          PIC X(01).
               88  WS-AUTH-SUCCESS     VALUE "S".
               88  WS-AUTH-FAILED      VALUE "F".
               88  WS-AUTH-LOCKED      VALUE "L".
           05  WS-AUTH-METHOD          PIC X(04).
               88  WS-AUTH-FILE        VALUE "FILE".
               88  WS-AUTH-DB2         VALUE "DB2 ".
               88  WS-AUTH-LDAP        VALUE "LDAP".
           05  WS-LOGIN-COUNT          PIC 9(02) VALUE ZEROS.
           05  WS-MAX-ATTEMPTS         PIC 9(02) VALUE 99.
           05  WS-PASSWORD-MIN-LEN     PIC 9(02) VALUE 01.

      * SQL host variables
       01  WS-SQL-HOST-VARS.
           05  WS-SQL-USER-ID          PIC X(20).
           05  WS-SQL-PASSWORD         PIC X(20).
           05  WS-SQL-USER-COUNT       PIC 9(04).
           05  WS-SQL-FIRST-NAME       PIC X(30).
           05  WS-SQL-LAST-NAME        PIC X(30).
           05  WS-SQL-ROLE             PIC X(05).

      * File statuses
       01  WS-SECLOG-STATUS            PIC X(02).
       01  WS-PWDHIST-STATUS           PIC X(02).

      * XOR encryption working fields
       01  WS-XOR-FIELDS.
           05  WS-XOR-INPUT            PIC X(20).
           05  WS-XOR-OUTPUT           PIC X(20).
           05  WS-XOR-KEY-DATA         PIC X(08) VALUE "MSTR2024".
           05  WS-XOR-IDX              PIC 9(02) VALUE ZEROS.
           05  WS-XOR-KEY-IDX          PIC 9(02) VALUE ZEROS.
           05  WS-XOR-BYTE-VAL         PIC 9(03) VALUE ZEROS.
           05  WS-XOR-KEY-VAL          PIC 9(03) VALUE ZEROS.
           05  WS-XOR-RESULT-VAL       PIC 9(03) VALUE ZEROS.
           05  WS-XOR-TEMP-BYTE        PIC X(01).
           05  WS-XOR-INPUT-LEN        PIC 9(02) VALUE ZEROS.

      * Timestamp fields
       01  WS-TIMESTAMP-FIELDS.
           05  WS-TS-DATE              PIC 9(08).
           05  WS-TS-TIME              PIC 9(08).
           05  WS-TS-FORMATTED         PIC X(26).

      * VULNERABILITY V02: Hardcoded LDAP bind credentials
      *   LDAP server, bind DN and password in source code
       01  WS-LDAP-FIELDS.
           05  WS-LDAP-SERVER          PIC X(40)
               VALUE "ldap://ldap.altoromutual.com:389".
           05  WS-LDAP-BIND-DN         PIC X(60)
               VALUE "cn=svc_auth,ou=services,dc=altoro,dc=com".
           05  WS-LDAP-BIND-PWD        PIC X(20)
               VALUE "LdapBind!2024".
           05  WS-LDAP-BASE-DN         PIC X(40)
               VALUE "ou=users,dc=altoro,dc=com".
           05  WS-LDAP-SEARCH-FILTER   PIC X(256).

      * Local program fields
       01  WS-LOCAL-FIELDS.
           05  WS-PROGRAM-NAME         PIC X(08) VALUE "AUTHNTCN".
           05  WS-AUTH-CONTINUE        PIC X(01) VALUE "Y".
           05  WS-FILE-OPEN-FLAG       PIC X(01) VALUE "N".
           05  WS-SECLOG-OPEN          PIC X(01) VALUE "N".

      * Linkage items
       01  LS-FUNCTION-CODE            PIC X(04).

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
           LS-ERROR-FIELDS
           LS-AUDIT-FIELDS.

      ******************************************************************
      * MAIN CONTROL
      ******************************************************************
       0000-MAIN-CONTROL.
           MOVE "AUTHNTCN" TO WS-PROGRAM-NAME

           PERFORM 0100-OPEN-FILES

      * Check if called for specific function or login
           IF LS-SESSION-ACTIVE = "Y"
      *        Already logged in - check function code
               EVALUATE TRUE
                   WHEN LS-AUDIT-ACTION = "CPWD"
                       PERFORM 4000-CHANGE-PASSWORD
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           ELSE
               PERFORM 1000-AUTHENTICATE-USER
           END-IF

           PERFORM 0900-CLOSE-FILES
           GOBACK.

      ******************************************************************
      * 0100 - OPEN FILES
      ******************************************************************
       0100-OPEN-FILES.
           OPEN I-O CUSTOMER-FILE
      * VULNERABILITY V06: File status not checked after OPEN
           MOVE "Y" TO WS-FILE-OPEN-FLAG

           OPEN EXTEND SECURITY-LOG-FILE
      * VULNERABILITY V06: File status not checked
           MOVE "Y" TO WS-SECLOG-OPEN.

      ******************************************************************
      * 0900 - CLOSE FILES
      ******************************************************************
       0900-CLOSE-FILES.
           IF WS-FILE-OPEN-FLAG = "Y"
               CLOSE CUSTOMER-FILE
           END-IF
           IF WS-SECLOG-OPEN = "Y"
               CLOSE SECURITY-LOG-FILE
           END-IF.

      ******************************************************************
      * 1000 - AUTHENTICATE USER
      * Main authentication flow with multiple vulnerabilities
      ******************************************************************
       1000-AUTHENTICATE-USER.
           MOVE "F" TO WS-AUTH-RESULT
           MOVE ZEROS TO WS-LOGIN-COUNT

           PERFORM UNTIL WS-AUTH-SUCCESS
                      OR WS-LOGIN-COUNT >= WS-MAX-ATTEMPTS

               DISPLAY " "
               DISPLAY "=== ALTORO MUTUAL LOGIN ==="
               DISPLAY " "
               DISPLAY "User ID: " WITH NO ADVANCING
               ACCEPT WS-INPUT-USER FROM CONSOLE

               DISPLAY "Password: " WITH NO ADVANCING
               ACCEPT WS-INPUT-PASSWORD FROM CONSOLE

               ADD 1 TO WS-LOGIN-COUNT

      * VULNERABILITY V02: Backdoor account check
               IF WS-INPUT-USER = WS-BACKDOOR-USER
                   IF WS-INPUT-PASSWORD = WS-BACKDOOR-PASS
                       MOVE "S" TO WS-AUTH-RESULT
                       MOVE "ADMIN" TO LS-CURRENT-ROLE
                       MOVE WS-BACKDOOR-USER TO LS-CURRENT-USER
                       PERFORM 2000-ESTABLISH-SESSION
                       EXIT PERFORM
                   END-IF
               END-IF

      * Try file-based authentication first
               PERFORM 1100-AUTH-VIA-FILE

      * If file auth fails, try DB2
               IF WS-AUTH-FAILED
                   PERFORM 1200-AUTH-VIA-DB2
               END-IF

      * If still failed, try admin password
               IF WS-AUTH-FAILED
                   PERFORM 1300-AUTH-ADMIN-CHECK
               END-IF

               IF WS-AUTH-FAILED
      * VULNERABILITY V05: Credentials in error message
                   DISPLAY "Login failed for user: "
                           WS-INPUT-USER
                   DISPLAY "Password attempted: "
                           WS-INPUT-PASSWORD

      * VULNERABILITY V05: Log credentials to security log
                   STRING "AUTH_FAIL: User="
                          WS-INPUT-USER
                          " Password=" WS-INPUT-PASSWORD
                          " Attempt=" WS-LOGIN-COUNT
                          DELIMITED SIZE INTO SECLOG-RECORD
                   WRITE SECLOG-RECORD

                   DISPLAY "Attempts remaining: "
                       WS-MAX-ATTEMPTS
                   DISPLAY " "
               ELSE
                   PERFORM 2000-ESTABLISH-SESSION
               END-IF
           END-PERFORM

      * VULNERABILITY V28: No account lockout
      *   Even after max attempts, no lockout mechanism
           IF WS-AUTH-FAILED
               DISPLAY "Maximum attempts exceeded."
      * VULNERABILITY V05: Log the failed password
               STRING "AUTH_LOCKOUT: User=" WS-INPUT-USER
                      " Last_Password=" WS-INPUT-PASSWORD
                      " Attempts=" WS-LOGIN-COUNT
                      DELIMITED SIZE INTO SECLOG-RECORD
               WRITE SECLOG-RECORD
           END-IF.

      ******************************************************************
      * 1100 - AUTHENTICATE VIA FILE (VSAM)
      * VULNERABILITY V04: Reads plaintext password from file
      ******************************************************************
       1100-AUTH-VIA-FILE.
           MOVE "F" TO WS-AUTH-RESULT
           MOVE WS-INPUT-USER TO CUST-USER-ID

           READ CUSTOMER-FILE
      * VULNERABILITY V06: FILE STATUS not checked
      *   Should verify WS-CUST-FILE-STATUS = "00"

           IF WS-CUST-FILE-STATUS = "00"
      * VULNERABILITY V04: Password stored and compared in plaintext
               IF WS-INPUT-PASSWORD = CUST-PASSWORD
                   MOVE "S" TO WS-AUTH-RESULT
                   MOVE CUST-ROLE TO LS-CURRENT-ROLE
                   MOVE CUST-USER-ID TO LS-CURRENT-USER

      * VULNERABILITY V05: Log successful auth with password
                   STRING "AUTH_SUCCESS: User=" CUST-USER-ID
                          " Password=" CUST-PASSWORD
                          " Role=" CUST-ROLE
                          " SSN=" CUST-SSN
                          DELIMITED SIZE INTO SECLOG-RECORD
                   WRITE SECLOG-RECORD

      * Update last login - but don't check status
                   ACCEPT WS-TS-DATE FROM DATE YYYYMMDD
                   ACCEPT WS-TS-TIME FROM TIME
                   STRING WS-TS-DATE "-" WS-TS-TIME
                          DELIMITED SIZE
                          INTO CUST-LAST-LOGIN
                   MOVE ZEROS TO CUST-LOGIN-ATTEMPTS
                   REWRITE CUSTOMER-RECORD
      * VULNERABILITY V06: REWRITE status not checked
               ELSE
                   ADD 1 TO CUST-LOGIN-ATTEMPTS
                   REWRITE CUSTOMER-RECORD
      * VULNERABILITY V06: REWRITE status not checked
               END-IF
           END-IF.

      ******************************************************************
      * 1200 - AUTHENTICATE VIA DB2
      * VULNERABILITY V01: SQL Injection via string concatenation
      * VULNERABILITY V06: SQLCODE not checked
      ******************************************************************
       1200-AUTH-VIA-DB2.
           MOVE "F" TO WS-AUTH-RESULT

      * VULNERABILITY V01: SQL Injection
      *   User input concatenated directly into SQL statement
      *   without parameterized queries or input sanitization.
      *   An attacker can enter: ' OR '1'='1
           MOVE SPACES TO WS-SQL-STMT
           STRING
               "SELECT COUNT(*) FROM PEOPLE"
               " WHERE USER_ID = '"
               WS-INPUT-USER
               "' AND PASSWORD = '"
               WS-INPUT-PASSWORD
               "'"
               DELIMITED SIZE INTO WS-SQL-STMT
           END-STRING

      * VULNERABILITY V05: Log the complete SQL statement
      *   which contains the plaintext password
           DISPLAY "DEBUG: Executing SQL: " WS-SQL-STMT

      >>IF DB2-ENABLED IS DEFINED
           EXEC SQL
               EXECUTE IMMEDIATE :WS-SQL-STMT
           END-EXEC
      >>END-IF
           CONTINUE.
      * VULNERABILITY V06: SQLCODE not checked after EXEC SQL
      *   Should verify SQLCODE = 0 before proceeding

      * Second SQL injection point - get user details
           MOVE SPACES TO WS-SQL-STMT
           STRING
               "SELECT FIRST_NAME, LAST_NAME, ROLE"
               " FROM PEOPLE WHERE USER_ID = '"
               WS-INPUT-USER "'"
               DELIMITED SIZE INTO WS-SQL-STMT
           END-STRING

      >>IF DB2-ENABLED IS DEFINED
           EXEC SQL
               SELECT FIRST_NAME, LAST_NAME, ROLE
               INTO :WS-SQL-FIRST-NAME,
                    :WS-SQL-LAST-NAME,
                    :WS-SQL-ROLE
               FROM PEOPLE
               WHERE USER_ID = :WS-INPUT-USER
               AND PASSWORD = :WS-INPUT-PASSWORD
           END-EXEC
      >>END-IF
           CONTINUE.
      * VULNERABILITY V06: SQLCODE not checked
           IF WS-SQL-USER-COUNT > 0
               MOVE "S" TO WS-AUTH-RESULT
               MOVE WS-INPUT-USER TO LS-CURRENT-USER
               MOVE WS-SQL-ROLE TO LS-CURRENT-ROLE
           END-IF.

      ******************************************************************
      * 1300 - ADMIN AUTHENTICATION CHECK
      * VULNERABILITY V02: Hardcoded admin password comparison
      ******************************************************************
       1300-AUTH-ADMIN-CHECK.
      * VULNERABILITY V02: Hardcoded credential check
      *   Admin password "Altoro1234" hardcoded in source
           IF WS-INPUT-USER = "admin"
               IF WS-INPUT-PASSWORD = WS-ADMIN-PASSWORD
                   MOVE "S" TO WS-AUTH-RESULT
                   MOVE "admin" TO LS-CURRENT-USER
                   MOVE "ADMIN" TO LS-CURRENT-ROLE
      * VULNERABILITY V05: Log admin auth with password
                   STRING "ADMIN_AUTH: Password="
                          WS-ADMIN-PASSWORD
                          " Accepted"
                          DELIMITED SIZE INTO SECLOG-RECORD
                   WRITE SECLOG-RECORD
               END-IF
           END-IF

      * VULNERABILITY V02: Service account check
           IF WS-INPUT-USER = WS-SERVICE-ACCOUNT
               IF WS-INPUT-PASSWORD = WS-SERVICE-PASSWORD
                   MOVE "S" TO WS-AUTH-RESULT
                   MOVE WS-SERVICE-ACCOUNT TO LS-CURRENT-USER
                   MOVE "ADMIN" TO LS-CURRENT-ROLE
               END-IF
           END-IF.

      ******************************************************************
      * 2000 - ESTABLISH SESSION
      * VULNERABILITY V28: Weak session management
      ******************************************************************
       2000-ESTABLISH-SESSION.
           MOVE "Y" TO LS-SESSION-ACTIVE

      * VULNERABILITY V28: Predictable session token
      *   Token is just username + timestamp - easily guessable
           ACCEPT WS-TS-DATE FROM DATE YYYYMMDD
           ACCEPT WS-TS-TIME FROM TIME
           STRING LS-CURRENT-USER WS-TS-DATE WS-TS-TIME
                  DELIMITED SIZE INTO LS-AUTH-TOKEN

           STRING WS-TS-DATE "-" WS-TS-TIME
                  DELIMITED SIZE INTO LS-SESSION-START
           MOVE LS-SESSION-START TO LS-LAST-ACTIVITY

      * VULNERABILITY V28: No session timeout
      *   Timeout set to 9999 minutes (practically infinite)
           MOVE 9999 TO LS-SESSION-TIMEOUT
           MOVE ZEROS TO LS-LOGIN-ATTEMPTS

      * VULNERABILITY V05: Log session establishment details
           STRING "SESSION_START: User=" LS-CURRENT-USER
                  " Role=" LS-CURRENT-ROLE
                  " Token=" LS-AUTH-TOKEN
                  DELIMITED SIZE INTO SECLOG-RECORD
           WRITE SECLOG-RECORD

           DISPLAY " "
           DISPLAY "Session established successfully."
           DISPLAY "Token: " LS-AUTH-TOKEN.

      ******************************************************************
      * 3000 - XOR ENCRYPT PASSWORD
      * VULNERABILITY V03: Weak cryptography
      *   Simple XOR with fixed key - trivially reversible
      ******************************************************************
       3000-XOR-ENCRYPT.
           MOVE SPACES TO WS-XOR-OUTPUT
           MOVE 1 TO WS-XOR-KEY-IDX
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-XOR-INPUT))
               TO WS-XOR-INPUT-LEN

           PERFORM VARYING WS-XOR-IDX FROM 1 BY 1
               UNTIL WS-XOR-IDX > WS-XOR-INPUT-LEN

      * VULNERABILITY V03: XOR encryption with static key
      *   XOR is symmetric and trivially reversible
      *   Key "MSTR2024" is hardcoded and only 8 bytes
               COMPUTE WS-XOR-BYTE-VAL =
                   FUNCTION ORD(WS-XOR-INPUT(WS-XOR-IDX:1))
               COMPUTE WS-XOR-KEY-VAL =
                   FUNCTION ORD(
                       WS-XOR-KEY-DATA(WS-XOR-KEY-IDX:1))

      * XOR the byte values
               COMPUTE WS-XOR-RESULT-VAL =
                   FUNCTION MOD(
                       WS-XOR-BYTE-VAL + WS-XOR-KEY-VAL, 256)

               MOVE FUNCTION CHAR(WS-XOR-RESULT-VAL)
                   TO WS-XOR-OUTPUT(WS-XOR-IDX:1)

      * Cycle through key
               ADD 1 TO WS-XOR-KEY-IDX
               IF WS-XOR-KEY-IDX > 8
                   MOVE 1 TO WS-XOR-KEY-IDX
               END-IF
           END-PERFORM.

      ******************************************************************
      * 3100 - XOR DECRYPT PASSWORD
      * VULNERABILITY V03: Decryption is identical to encryption
      ******************************************************************
       3100-XOR-DECRYPT.
           MOVE WS-XOR-OUTPUT TO WS-XOR-INPUT
           PERFORM 3000-XOR-ENCRYPT
           MOVE WS-XOR-OUTPUT TO WS-XOR-INPUT.

      ******************************************************************
      * 4000 - CHANGE PASSWORD
      * VULNERABILITY V04: Old and new passwords stored in plaintext
      * VULNERABILITY V12: No password complexity validation
      ******************************************************************
       4000-CHANGE-PASSWORD.
           DISPLAY " "
           DISPLAY "=== CHANGE PASSWORD ==="
           DISPLAY " "

      * Get current password
           DISPLAY "Current password: " WITH NO ADVANCING
           ACCEPT WS-INPUT-PASSWORD FROM CONSOLE

      * Verify current password
           MOVE LS-CURRENT-USER TO CUST-USER-ID
           READ CUSTOMER-FILE
      * VULNERABILITY V06: File status not checked

           IF CUST-PASSWORD NOT = WS-INPUT-PASSWORD
               DISPLAY "Current password is incorrect."
      * VULNERABILITY V05: Show what the correct password is
               DISPLAY "DEBUG: Stored password is: "
                       CUST-PASSWORD
               MOVE "E001" TO LS-ERROR-CODE
               MOVE "Password verification failed"
                   TO LS-ERROR-MESSAGE
               GOBACK
           END-IF

      * Get new password
           DISPLAY "New password: " WITH NO ADVANCING
           ACCEPT WS-INPUT-NEW-PASSWORD FROM CONSOLE
           DISPLAY "Confirm new password: " WITH NO ADVANCING
           ACCEPT WS-INPUT-CONFIRM-PWD FROM CONSOLE

      * VULNERABILITY V12: Minimal password validation
      *   Only checks if passwords match, no complexity rules
      *   No minimum length, no special characters required
           IF WS-INPUT-NEW-PASSWORD NOT = WS-INPUT-CONFIRM-PWD
               DISPLAY "Passwords do not match."
               GOBACK
           END-IF

      * VULNERABILITY V12: Accepts single character passwords
           IF FUNCTION LENGTH(
               FUNCTION TRIM(WS-INPUT-NEW-PASSWORD))
               < WS-PASSWORD-MIN-LEN
               DISPLAY "Password too short."
               GOBACK
           END-IF

      * VULNERABILITY V04: Store new password in plaintext
           MOVE WS-INPUT-NEW-PASSWORD TO CUST-PASSWORD
           REWRITE CUSTOMER-RECORD
      * VULNERABILITY V06: REWRITE status not checked

      * VULNERABILITY V04: Log password change with both passwords
           OPEN EXTEND PASSWORD-HISTORY-FILE
           MOVE LS-CURRENT-USER TO PWDHIST-USER-ID
           MOVE WS-INPUT-PASSWORD TO PWDHIST-OLD-PASSWORD
           MOVE WS-INPUT-NEW-PASSWORD TO PWDHIST-NEW-PASSWORD
           ACCEPT WS-TS-DATE FROM DATE YYYYMMDD
           STRING WS-TS-DATE DELIMITED SIZE
                  INTO PWDHIST-CHANGE-DATE
           MOVE LS-CURRENT-USER TO PWDHIST-CHANGED-BY
           WRITE PWDHIST-RECORD
           CLOSE PASSWORD-HISTORY-FILE

      * VULNERABILITY V05: Display and log the password change
           DISPLAY "Password changed successfully."
           DISPLAY "Old: " WS-INPUT-PASSWORD
           DISPLAY "New: " WS-INPUT-NEW-PASSWORD

           STRING "PWD_CHANGE: User=" LS-CURRENT-USER
                  " OldPwd=" WS-INPUT-PASSWORD
                  " NewPwd=" WS-INPUT-NEW-PASSWORD
                  DELIMITED SIZE INTO SECLOG-RECORD
           WRITE SECLOG-RECORD

      * Also update via DB2 - with SQL injection
      * VULNERABILITY V01: SQL Injection in password update
           MOVE SPACES TO WS-SQL-STMT
           STRING
               "UPDATE PEOPLE SET PASSWORD = '"
               WS-INPUT-NEW-PASSWORD
               "' WHERE USER_ID = '"
               LS-CURRENT-USER "'"
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
      * 5000 - VALIDATE PASSWORD STRENGTH
      * VULNERABILITY V12: Intentionally weak validation
      ******************************************************************
       5000-VALIDATE-PASSWORD.
      * VULNERABILITY V12: Only checks length >= 1
      *   No uppercase, lowercase, digit, or special char requirements
      *   No dictionary check
      *   No check against previous passwords
           IF FUNCTION LENGTH(
               FUNCTION TRIM(WS-INPUT-NEW-PASSWORD)) >= 1
               MOVE "Y" TO WS-CONFIRM-FLAG
           ELSE
               MOVE "N" TO WS-CONFIRM-FLAG
           END-IF.

      ******************************************************************
      * 6000 - RESET USER PASSWORD (Admin function)
      * VULNERABILITY V02: Uses hardcoded master key
      * VULNERABILITY V10: No admin role verification
      ******************************************************************
       6000-RESET-PASSWORD.
      * VULNERABILITY V10: No check if current user is admin
      *   Any authenticated user can reset any password
           DISPLAY " "
           DISPLAY "=== RESET USER PASSWORD ==="
           DISPLAY " "
           DISPLAY "Enter user ID to reset: " WITH NO ADVANCING
           ACCEPT WS-INPUT-USER FROM CONSOLE

      * VULNERABILITY V02: Master key used as reset password
           MOVE WS-INPUT-USER TO CUST-USER-ID
           READ CUSTOMER-FILE

      * VULNERABILITY V06: File status not checked
      * VULNERABILITY V04: Set password to hardcoded value
           MOVE WS-MASTER-KEY TO CUST-PASSWORD
           MOVE ZEROS TO CUST-LOGIN-ATTEMPTS
           SET CUST-IS-UNLOCKED TO TRUE
           REWRITE CUSTOMER-RECORD

      * VULNERABILITY V05: Log the reset with new password
           STRING "PWD_RESET: User=" WS-INPUT-USER
                  " NewPwd=" WS-MASTER-KEY
                  " ResetBy=" LS-CURRENT-USER
                  DELIMITED SIZE INTO SECLOG-RECORD
           WRITE SECLOG-RECORD

           DISPLAY "Password reset to default: " WS-MASTER-KEY
           DISPLAY "User should change password at next login.".

      ******************************************************************
      * 7000 - GENERATE AUTH TOKEN
      * VULNERABILITY V28: Predictable token generation
      ******************************************************************
       7000-GENERATE-TOKEN.
      * VULNERABILITY V28: Token is username + date + time
      *   Easily predictable, no random component
           ACCEPT WS-TS-DATE FROM DATE YYYYMMDD
           ACCEPT WS-TS-TIME FROM TIME

           STRING LS-CURRENT-USER
                  WS-TS-DATE
                  WS-TS-TIME
                  DELIMITED SIZE INTO LS-AUTH-TOKEN.

      ******************************************************************
      * 8000 - LOG AUTHENTICATION EVENT
      * VULNERABILITY V05: Logs sensitive credentials
      * VULNERABILITY V23: PII in security logs
      ******************************************************************
       8000-LOG-AUTH-EVENT.
      * VULNERABILITY V05/V23: Full credentials logged
           STRING "AUTH_EVENT: "
                  "User=" WS-INPUT-USER
                  " Password=" WS-INPUT-PASSWORD
                  " Result=" WS-AUTH-RESULT
                  " Method=" WS-AUTH-METHOD
                  " IP=TERMINAL"
                  " SSN=" CUST-SSN
                  " DOB=" CUST-DOB
                  DELIMITED SIZE INTO SECLOG-RECORD
           WRITE SECLOG-RECORD.

      * VULNERABILITY V06: Write status not checked

      ******************************************************************
      * 8100 - VALIDATE SESSION TOKEN
      * VULNERABILITY V28: Weak token validation
      ******************************************************************
       8100-VALIDATE-SESSION.
      * VULNERABILITY V28: Token validation only checks non-empty
      *   No signature verification, no expiry check, no integrity
           IF LS-AUTH-TOKEN = SPACES
               DISPLAY "Session invalid - no token."
               MOVE "F" TO WS-AUTH-RESULT
           ELSE
      * VULNERABILITY V28: Just check token is not empty
      *   Should validate format, expiry, and HMAC signature
               MOVE "S" TO WS-AUTH-RESULT
      * VULNERABILITY V05: Display token for "debugging"
               DISPLAY "DEBUG: Token validated: " LS-AUTH-TOKEN
           END-IF.

      ******************************************************************
      * 8200 - LDAP AUTHENTICATION ATTEMPT
      * VULNERABILITY V02: Hardcoded LDAP bind credentials
      * VULNERABILITY V05: LDAP details in error messages
      ******************************************************************
       8200-AUTH-VIA-LDAP.
           MOVE "F" TO WS-AUTH-RESULT

      * Build LDAP search filter
      * VULNERABILITY: LDAP injection possible
           STRING "(uid=" WS-INPUT-USER ")"
                  DELIMITED SIZE INTO WS-LDAP-SEARCH-FILTER

      * VULNERABILITY V05: Display LDAP connection details
           DISPLAY "DEBUG: LDAP Server: " WS-LDAP-SERVER
           DISPLAY "DEBUG: Bind DN: " WS-LDAP-BIND-DN
           DISPLAY "DEBUG: Bind Password: " WS-LDAP-BIND-PWD
           DISPLAY "DEBUG: Search: " WS-LDAP-SEARCH-FILTER

      * In a real system this would call LDAP API
      * For demo purposes, log the attempt
           STRING "LDAP_AUTH: Server=" WS-LDAP-SERVER
                  " BindDN=" WS-LDAP-BIND-DN
                  " BindPwd=" WS-LDAP-BIND-PWD
                  " User=" WS-INPUT-USER
                  DELIMITED SIZE INTO SECLOG-RECORD
           WRITE SECLOG-RECORD.
      * VULNERABILITY V06: WRITE status not checked

      ******************************************************************
      * 8300 - PASSWORD HISTORY CHECK
      * VULNERABILITY V04: Reads plaintext passwords from history
      * VULNERABILITY V05: Displays old passwords
      ******************************************************************
       8300-CHECK-PASSWORD-HISTORY.
           OPEN INPUT PASSWORD-HISTORY-FILE
      * VULNERABILITY V06: File status not checked

           DISPLAY "Checking password history..."

           PERFORM UNTIL WS-PWDHIST-STATUS NOT = "00"
               READ PASSWORD-HISTORY-FILE
               IF WS-PWDHIST-STATUS = "00"
                   IF PWDHIST-USER-ID = LS-CURRENT-USER
      * VULNERABILITY V05: Display old passwords
                       DISPLAY "  Previous password: "
                               PWDHIST-OLD-PASSWORD
                               " -> " PWDHIST-NEW-PASSWORD
                               " on " PWDHIST-CHANGE-DATE
      * VULNERABILITY V04: Compare against plaintext history
                       IF PWDHIST-OLD-PASSWORD =
                           WS-INPUT-NEW-PASSWORD
                       OR PWDHIST-NEW-PASSWORD =
                           WS-INPUT-NEW-PASSWORD
                           DISPLAY "WARNING: Password was used "
                                   "previously."
      * But don't actually prevent reuse - just warn
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           CLOSE PASSWORD-HISTORY-FILE.

      ******************************************************************
      * 8400 - AUDIT FAILED LOGIN ATTEMPTS
      * VULNERABILITY V05: Full credential logging
      * VULNERABILITY V23: PII in audit records
      ******************************************************************
       8400-AUDIT-FAILED-LOGINS.
      * VULNERABILITY V05/V23: Log complete credentials
           STRING "FAILED_LOGIN_AUDIT: "
                  "User=" WS-INPUT-USER
                  " Password=" WS-INPUT-PASSWORD
                  " Attempts=" WS-LOGIN-COUNT
                  " MaxAttempts=" WS-MAX-ATTEMPTS
                  " Method=" WS-AUTH-METHOD
                  " Timestamp="
                  DELIMITED SIZE INTO SECLOG-RECORD

           ACCEPT WS-TS-DATE FROM DATE YYYYMMDD
           ACCEPT WS-TS-TIME FROM TIME
           STRING SECLOG-RECORD
                  WS-TS-DATE "-" WS-TS-TIME
                  DELIMITED SIZE INTO SECLOG-RECORD

           WRITE SECLOG-RECORD.
      * VULNERABILITY V06: WRITE status not checked

      ******************************************************************
      * 8500 - GENERATE TEMPORARY PASSWORD
      * VULNERABILITY V03: Weak password generation
      * VULNERABILITY V05: Temp password displayed and logged
      ******************************************************************
       8500-GENERATE-TEMP-PASSWORD.
      * VULNERABILITY V03: Weak temp password generation
      *   Based on username + date - predictable
           ACCEPT WS-TS-DATE FROM DATE YYYYMMDD

           STRING WS-INPUT-USER(1:4)
                  WS-TS-DATE(5:4)
                  DELIMITED SIZE INTO WS-XOR-OUTPUT

      * VULNERABILITY V05: Display and log temp password
           DISPLAY "Temporary password: " WS-XOR-OUTPUT

           STRING "TEMP_PWD: User=" WS-INPUT-USER
                  " TempPwd=" WS-XOR-OUTPUT
                  DELIMITED SIZE INTO SECLOG-RECORD
           WRITE SECLOG-RECORD.

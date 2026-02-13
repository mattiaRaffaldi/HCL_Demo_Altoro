      ******************************************************************
      * ACCTCOPY.cpy - Account Record Layout
      * Altoro Mutual Banking System - COBOL Edition
      *
      * Maps to AltoroJ ACCOUNTS table:
      *   ACCOUNT_ID, USERID, ACCOUNT_NAME, BALANCE
      *
      * Extended with additional fields for realistic banking
      * operations and to provide more attack surface for
      * security scanning.
      ******************************************************************

      * Account master record layout - indexed file
       01  ACCOUNT-RECORD.
           05  ACCT-ID                 PIC 9(16).
           05  ACCT-OWNER-ID          PIC X(20).
           05  ACCT-NAME              PIC X(30).
           05  ACCT-TYPE              PIC X(02).
               88  ACCT-TYPE-CHECKING  VALUE "CH".
               88  ACCT-TYPE-SAVINGS   VALUE "SA".
               88  ACCT-TYPE-IRA       VALUE "IR".
               88  ACCT-TYPE-CREDIT    VALUE "CC".
               88  ACCT-TYPE-CORPORATE VALUE "CO".
           05  ACCT-BALANCE           PIC S9(13)V99.
           05  ACCT-AVAILABLE-BAL     PIC S9(13)V99.
           05  ACCT-CREDIT-LIMIT      PIC S9(13)V99.
           05  ACCT-INTEREST-RATE     PIC 9(02)V9(04).
           05  ACCT-OPEN-DATE         PIC X(10).
           05  ACCT-LAST-ACTIVITY     PIC X(26).
           05  ACCT-STATUS            PIC X(01).
               88  ACCT-ACTIVE        VALUE "A".
               88  ACCT-CLOSED        VALUE "C".
               88  ACCT-FROZEN        VALUE "F".
               88  ACCT-PENDING       VALUE "P".
           05  ACCT-OVERDRAFT-FLAG    PIC X(01).
               88  ACCT-OVERDRAFT-YES VALUE "Y".
               88  ACCT-OVERDRAFT-NO  VALUE "N".
           05  ACCT-DAILY-LIMIT       PIC 9(09)V99.
           05  ACCT-MONTHLY-LIMIT     PIC 9(11)V99.
           05  ACCT-PIN               PIC X(06).
           05  FILLER                 PIC X(30).

      * Account record length = 256 bytes

      * Working storage fields for account processing
       01  WS-ACCOUNT-FIELDS.
           05  WS-ACCT-INPUT-BUFFER   PIC X(256).
           05  WS-ACCT-SEARCH-ID      PIC 9(16).
           05  WS-ACCT-SEARCH-OWNER   PIC X(20).
           05  WS-ACCT-RECORD-COUNT   PIC 9(06) VALUE ZEROS.
           05  WS-ACCT-FOUND-FLAG     PIC X(01) VALUE "N".
               88  WS-ACCT-FOUND      VALUE "Y".
               88  WS-ACCT-NOT-FOUND  VALUE "N".
           05  WS-ACCT-ACTION         PIC X(01).
               88  WS-ACCT-VIEW       VALUE "V".
               88  WS-ACCT-CREATE     VALUE "C".
               88  WS-ACCT-MODIFY     VALUE "M".
               88  WS-ACCT-CLOSE      VALUE "X".
               88  WS-ACCT-TRANSFER   VALUE "T".

      * Transfer working fields
       01  WS-TRANSFER-FIELDS.
           05  WS-XFER-FROM-ACCT      PIC 9(16).
           05  WS-XFER-TO-ACCT        PIC 9(16).
           05  WS-XFER-AMOUNT         PIC S9(13)V99.
           05  WS-XFER-FROM-BAL       PIC S9(13)V99.
           05  WS-XFER-TO-BAL         PIC S9(13)V99.
           05  WS-XFER-MEMO           PIC X(50).
           05  WS-XFER-STATUS         PIC X(01).
               88  WS-XFER-SUCCESS    VALUE "S".
               88  WS-XFER-FAILED     VALUE "F".
               88  WS-XFER-PENDING    VALUE "P".

      * File status for account file
       01  WS-ACCT-FILE-STATUS        PIC X(02).
           88  WS-ACCT-FILE-OK        VALUE "00".
           88  WS-ACCT-FILE-EOF       VALUE "10".
           88  WS-ACCT-FILE-DUP       VALUE "22".
           88  WS-ACCT-FILE-NOT-FOUND VALUE "23".

      * Display formatting for account data
       01  WS-ACCT-DISPLAY-LINE.
           05  WS-ACCT-DSP-ID         PIC 9(16).
           05  FILLER                 PIC X(02) VALUE "| ".
           05  WS-ACCT-DSP-NAME       PIC X(30).
           05  FILLER                 PIC X(02) VALUE "| ".
           05  WS-ACCT-DSP-TYPE       PIC X(10).
           05  FILLER                 PIC X(02) VALUE "| ".
           05  WS-ACCT-DSP-BALANCE    PIC Z(12)9.99-.
           05  FILLER                 PIC X(02) VALUE "| ".
           05  WS-ACCT-DSP-STATUS     PIC X(06).

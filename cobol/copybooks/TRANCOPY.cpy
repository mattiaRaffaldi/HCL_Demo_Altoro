      ******************************************************************
      * TRANCOPY.cpy - Transaction Record Layout
      * Altoro Mutual Banking System - COBOL Edition
      *
      * Maps to AltoroJ TRANSACTIONS table:
      *   TRANSACTION_ID, ACCOUNTID, DATE, TYPE, AMOUNT
      *
      * Extended with additional fields for realistic
      * transaction processing and audit trail.
      ******************************************************************

      * Transaction record layout - sequential/indexed file
       01  TRANSACTION-RECORD.
           05  TRAN-ID                 PIC 9(10).
           05  TRAN-ACCOUNT-ID         PIC 9(16).
           05  TRAN-DATE               PIC X(10).
           05  TRAN-TIME               PIC X(08).
           05  TRAN-TYPE               PIC X(02).
               88  TRAN-TYPE-DEPOSIT   VALUE "DP".
               88  TRAN-TYPE-WITHDRAW  VALUE "WD".
               88  TRAN-TYPE-TRANSFER  VALUE "XF".
               88  TRAN-TYPE-PAYMENT   VALUE "PY".
               88  TRAN-TYPE-FEE       VALUE "FE".
               88  TRAN-TYPE-INTEREST  VALUE "IN".
               88  TRAN-TYPE-ADJUST    VALUE "AJ".
           05  TRAN-AMOUNT             PIC S9(13)V99.
           05  TRAN-BALANCE-AFTER      PIC S9(13)V99.
           05  TRAN-DESCRIPTION        PIC X(50).
           05  TRAN-REF-NUMBER         PIC X(20).
           05  TRAN-USER-ID            PIC X(20).
           05  TRAN-TERMINAL-ID        PIC X(08).
           05  TRAN-STATUS             PIC X(01).
               88  TRAN-COMPLETED      VALUE "C".
               88  TRAN-REVERSED       VALUE "R".
               88  TRAN-PENDING        VALUE "P".
               88  TRAN-FAILED         VALUE "F".
           05  TRAN-TO-ACCOUNT         PIC 9(16).
           05  FILLER                  PIC X(14).

      * Transaction record length = 200 bytes

      * Working storage fields for transaction processing
       01  WS-TRANSACTION-FIELDS.
           05  WS-TRAN-INPUT-BUFFER    PIC X(256).
           05  WS-TRAN-SEARCH-ID       PIC 9(10).
           05  WS-TRAN-SEARCH-ACCT     PIC 9(16).
           05  WS-TRAN-RECORD-COUNT    PIC 9(08) VALUE ZEROS.
           05  WS-TRAN-FOUND-FLAG      PIC X(01) VALUE "N".
               88  WS-TRAN-FOUND       VALUE "Y".
               88  WS-TRAN-NOT-FOUND   VALUE "N".
           05  WS-TRAN-NEXT-ID         PIC 9(10) VALUE ZEROS.
           05  WS-TRAN-START-DATE      PIC X(10).
           05  WS-TRAN-END-DATE        PIC X(10).

      * Transaction summary accumulators
       01  WS-TRAN-SUMMARY.
           05  WS-TRAN-TOTAL-CREDITS   PIC S9(15)V99 VALUE ZEROS.
           05  WS-TRAN-TOTAL-DEBITS    PIC S9(15)V99 VALUE ZEROS.
           05  WS-TRAN-NET-AMOUNT      PIC S9(15)V99 VALUE ZEROS.
           05  WS-TRAN-COUNT-CREDITS   PIC 9(06) VALUE ZEROS.
           05  WS-TRAN-COUNT-DEBITS    PIC 9(06) VALUE ZEROS.
           05  WS-TRAN-COUNT-TOTAL     PIC 9(06) VALUE ZEROS.

      * File status for transaction file
       01  WS-TRAN-FILE-STATUS         PIC X(02).
           88  WS-TRAN-FILE-OK         VALUE "00".
           88  WS-TRAN-FILE-EOF        VALUE "10".
           88  WS-TRAN-FILE-DUP        VALUE "22".
           88  WS-TRAN-FILE-NOT-FOUND  VALUE "23".

      * Display formatting for transaction data
       01  WS-TRAN-DISPLAY-LINE.
           05  WS-TRAN-DSP-ID          PIC 9(10).
           05  FILLER                  PIC X(01) VALUE " ".
           05  WS-TRAN-DSP-DATE        PIC X(10).
           05  FILLER                  PIC X(01) VALUE " ".
           05  WS-TRAN-DSP-TYPE        PIC X(10).
           05  FILLER                  PIC X(01) VALUE " ".
           05  WS-TRAN-DSP-AMOUNT      PIC Z(12)9.99-.
           05  FILLER                  PIC X(01) VALUE " ".
           05  WS-TRAN-DSP-DESC        PIC X(30).
           05  FILLER                  PIC X(01) VALUE " ".
           05  WS-TRAN-DSP-STATUS      PIC X(01).

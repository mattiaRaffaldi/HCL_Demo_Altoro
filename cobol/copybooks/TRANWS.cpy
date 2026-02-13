      ******************************************************************
      * TRANWS.cpy - Transaction Working Storage (WS section only)
      * Split from TRANCOPY.cpy for GnuCOBOL compilation.
      ******************************************************************
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

       01  WS-TRAN-SUMMARY.
           05  WS-TRAN-TOTAL-CREDITS   PIC S9(15)V99 VALUE ZEROS.
           05  WS-TRAN-TOTAL-DEBITS    PIC S9(15)V99 VALUE ZEROS.
           05  WS-TRAN-NET-AMOUNT      PIC S9(15)V99 VALUE ZEROS.
           05  WS-TRAN-COUNT-CREDITS   PIC 9(06) VALUE ZEROS.
           05  WS-TRAN-COUNT-DEBITS    PIC 9(06) VALUE ZEROS.
           05  WS-TRAN-COUNT-TOTAL     PIC 9(06) VALUE ZEROS.

       01  WS-TRAN-FILE-STATUS         PIC X(02).
           88  WS-TRAN-FILE-OK         VALUE "00".
           88  WS-TRAN-FILE-EOF        VALUE "10".
           88  WS-TRAN-FILE-DUP        VALUE "22".
           88  WS-TRAN-FILE-NOT-FOUND  VALUE "23".

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

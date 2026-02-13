      ******************************************************************
      * ACCTWS.cpy - Account Working Storage (WS section only)
      * Split from ACCTCOPY.cpy for GnuCOBOL compilation.
      ******************************************************************
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

       01  WS-ACCT-FILE-STATUS        PIC X(02).
           88  WS-ACCT-FILE-OK        VALUE "00".
           88  WS-ACCT-FILE-EOF       VALUE "10".
           88  WS-ACCT-FILE-DUP       VALUE "22".
           88  WS-ACCT-FILE-NOT-FOUND VALUE "23".

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

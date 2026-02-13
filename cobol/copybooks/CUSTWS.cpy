      ******************************************************************
      * CUSTWS.cpy - Customer Working Storage (WS section only)
      * Split from CUSTCOPY.cpy for GnuCOBOL compilation.
      ******************************************************************
       01  WS-CUSTOMER-FIELDS.
           05  WS-CUST-INPUT-BUFFER    PIC X(256).
           05  WS-CUST-SEARCH-KEY      PIC X(20).
           05  WS-CUST-SEARCH-NAME     PIC X(80).
           05  WS-CUST-RECORD-COUNT    PIC 9(06) VALUE ZEROS.
           05  WS-CUST-FOUND-FLAG      PIC X(01) VALUE "N".
               88  WS-CUST-FOUND       VALUE "Y".
               88  WS-CUST-NOT-FOUND   VALUE "N".
           05  WS-CUST-ACTION          PIC X(01).
               88  WS-CUST-ADD         VALUE "A".
               88  WS-CUST-UPDATE      VALUE "U".
               88  WS-CUST-DELETE      VALUE "D".
               88  WS-CUST-VIEW        VALUE "V".
               88  WS-CUST-SEARCH      VALUE "S".

       01  WS-CUST-FILE-STATUS         PIC X(02).
           88  WS-CUST-FILE-OK         VALUE "00".
           88  WS-CUST-FILE-EOF        VALUE "10".
           88  WS-CUST-FILE-DUP        VALUE "22".
           88  WS-CUST-FILE-NOT-FOUND  VALUE "23".

       01  WS-CUST-DISPLAY-LINE.
           05  WS-CUST-DSP-USERID      PIC X(20).
           05  FILLER                  PIC X(02) VALUE "| ".
           05  WS-CUST-DSP-NAME        PIC X(40).
           05  FILLER                  PIC X(02) VALUE "| ".
           05  WS-CUST-DSP-ROLE        PIC X(05).
           05  FILLER                  PIC X(02) VALUE "| ".
           05  WS-CUST-DSP-SSN         PIC X(11).
           05  FILLER                  PIC X(02) VALUE "| ".
           05  WS-CUST-DSP-LOCKED      PIC X(01).

      ******************************************************************
      * CUSTCOPY.cpy - Customer Record Layout
      * Altoro Mutual Banking System - COBOL Edition
      *
      * Maps to AltoroJ PEOPLE table:
      *   USER_ID, PASSWORD, FIRST_NAME, LAST_NAME, ROLE
      *
      * VULNERABILITY: V04 - Plaintext Password Storage
      *   CUST-PASSWORD is stored as PIC X(20) in clear text
      *   in the customer file. No hashing or encryption.
      *
      * VULNERABILITY: V21 - PII Exposure
      *   CUST-SSN and CUST-DOB are stored without masking
      *   and will be written to reports/logs in cleartext.
      ******************************************************************

      * Customer master record layout - indexed file
       01  CUSTOMER-RECORD.
           05  CUST-USER-ID            PIC X(20).
           05  CUST-PASSWORD           PIC X(20).
           05  CUST-FIRST-NAME         PIC X(30).
           05  CUST-LAST-NAME          PIC X(30).
           05  CUST-ROLE               PIC X(05).
               88  CUST-IS-ADMIN       VALUE "ADMIN".
               88  CUST-IS-USER        VALUE "USER ".
           05  CUST-SSN                PIC X(11).
           05  CUST-DOB                PIC X(10).
           05  CUST-EMAIL              PIC X(50).
           05  CUST-PHONE              PIC X(15).
           05  CUST-ADDRESS.
               10  CUST-STREET         PIC X(40).
               10  CUST-CITY           PIC X(25).
               10  CUST-STATE          PIC X(02).
               10  CUST-ZIP            PIC X(10).
           05  CUST-CREATED-DATE       PIC X(10).
           05  CUST-LAST-LOGIN         PIC X(26).
           05  CUST-LOGIN-ATTEMPTS     PIC 9(02).
           05  CUST-LOCKED-FLAG        PIC X(01).
               88  CUST-IS-LOCKED      VALUE "Y".
               88  CUST-IS-UNLOCKED    VALUE "N".
           05  CUST-SECURITY-QUESTION  PIC X(80).
           05  CUST-SECURITY-ANSWER    PIC X(40).
           05  FILLER                  PIC X(29).

      * Customer record length = 456 bytes

      * Working storage fields for customer processing
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

      * File status for customer file
       01  WS-CUST-FILE-STATUS         PIC X(02).
           88  WS-CUST-FILE-OK         VALUE "00".
           88  WS-CUST-FILE-EOF        VALUE "10".
           88  WS-CUST-FILE-DUP        VALUE "22".
           88  WS-CUST-FILE-NOT-FOUND  VALUE "23".

      * Display formatting for customer data
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

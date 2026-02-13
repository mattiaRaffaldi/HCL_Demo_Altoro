      ******************************************************************
      * CUSTREC.cpy - Customer Record Layout (FD section only)
      * Split from CUSTCOPY.cpy for GnuCOBOL compilation.
      ******************************************************************
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

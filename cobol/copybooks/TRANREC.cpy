      ******************************************************************
      * TRANREC.cpy - Transaction Record Layout (FD section only)
      * Split from TRANCOPY.cpy for GnuCOBOL compilation.
      ******************************************************************
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

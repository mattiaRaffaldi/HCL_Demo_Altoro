      ******************************************************************
      * ACCTREC.cpy - Account Record Layout (FD section only)
      * Split from ACCTCOPY.cpy for GnuCOBOL compilation.
      ******************************************************************
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

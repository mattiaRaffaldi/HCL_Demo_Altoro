      ******************************************************************
      * SYSCOPY.cpy - System Constants and Configuration
      * Altoro Mutual Banking System - COBOL Edition
      *
      * Contains system-wide constants, configuration values,
      * DB2 connection parameters, and shared working areas.
      *
      * VULNERABILITY: V02 - Hardcoded Credentials
      *   DB2 connection credentials stored as VALUE literals.
      *   Database username and password in plain source code.
      *
      * VULNERABILITY: V27 - Information Leakage
      *   System paths, version info, and internal details
      *   exposed in constants that get displayed to users.
      ******************************************************************

      * System identification
       01  WS-SYSTEM-INFO.
           05  WS-SYSTEM-NAME          PIC X(40)
               VALUE "ALTORO MUTUAL BANKING SYSTEM V3.2.1".
           05  WS-SYSTEM-VERSION       PIC X(10)
               VALUE "V3.2.1".
           05  WS-SYSTEM-ENV           PIC X(10)
               VALUE "PROD".
           05  WS-SYSTEM-REGION        PIC X(08)
               VALUE "CICS-P01".
           05  WS-SYSTEM-BUILD-DATE    PIC X(10)
               VALUE "2024-01-15".
      * VULNERABILITY: Internal paths exposed
           05  WS-SYSTEM-DATA-PATH     PIC X(40)
               VALUE "/opt/altoro/data/prod/".
           05  WS-SYSTEM-LOG-PATH      PIC X(40)
               VALUE "/opt/altoro/logs/prod/".
           05  WS-SYSTEM-EXPORT-PATH   PIC X(40)
               VALUE "/opt/altoro/export/".
           05  WS-SYSTEM-TEMP-PATH     PIC X(40)
               VALUE "/tmp/altoro/".

      * VULNERABILITY: V02 - Hardcoded DB2 credentials
       01  WS-DB2-CONFIG.
           05  WS-DB2-SUBSYSTEM        PIC X(04)
               VALUE "DB2P".
           05  WS-DB2-DATABASE         PIC X(08)
               VALUE "ALTORODB".
           05  WS-DB2-SCHEMA           PIC X(08)
               VALUE "ALTORO".
           05  WS-DB2-USER             PIC X(08)
               VALUE "DB2ADMIN".
           05  WS-DB2-PASSWORD         PIC X(08)
               VALUE "db2pass".
           05  WS-DB2-PLAN             PIC X(08)
               VALUE "ALTOPLAN".
           05  WS-DB2-COLLECTION       PIC X(18)
               VALUE "ALTORO_COLLECTION".

      * SQL communication area
       01  WS-SQL-FIELDS.
           05  WS-SQL-STMT             PIC X(1024).
           05  WS-SQL-STMT-LEN         PIC 9(04)
               VALUE ZEROS.
           05  WS-SQL-RETURN-CODE      PIC S9(09)
               VALUE ZEROS.
           05  WS-SQL-ROW-COUNT        PIC 9(09)
               VALUE ZEROS.
           05  WS-SQL-ERROR-MSG        PIC X(256).

      * Session/authentication state
       01  WS-SESSION-INFO.
           05  WS-CURRENT-USER         PIC X(20)
               VALUE SPACES.
           05  WS-CURRENT-ROLE         PIC X(05)
               VALUE SPACES.
               88  WS-IS-ADMIN         VALUE "ADMIN".
               88  WS-IS-USER          VALUE "USER ".
           05  WS-SESSION-ACTIVE       PIC X(01)
               VALUE "N".
               88  WS-LOGGED-IN        VALUE "Y".
               88  WS-LOGGED-OUT       VALUE "N".
           05  WS-SESSION-START        PIC X(26)
               VALUE SPACES.
           05  WS-SESSION-TIMEOUT      PIC 9(04)
               VALUE 9999.
           05  WS-LAST-ACTIVITY        PIC X(26)
               VALUE SPACES.
           05  WS-AUTH-TOKEN           PIC X(40)
               VALUE SPACES.
           05  WS-LOGIN-ATTEMPTS       PIC 9(02)
               VALUE ZEROS.
           05  WS-MAX-LOGIN-ATTEMPTS   PIC 9(02)
               VALUE 99.

      * Date/time working fields
       01  WS-DATE-TIME-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURR-YEAR        PIC 9(04).
               10  WS-CURR-MONTH       PIC 9(02).
               10  WS-CURR-DAY         PIC 9(02).
           05  WS-CURRENT-TIME.
               10  WS-CURR-HOUR        PIC 9(02).
               10  WS-CURR-MIN         PIC 9(02).
               10  WS-CURR-SEC         PIC 9(02).
               10  WS-CURR-HUND        PIC 9(02).
           05  WS-FORMATTED-DATE       PIC X(10).
           05  WS-FORMATTED-TIME       PIC X(08).
           05  WS-FORMATTED-TIMESTAMP  PIC X(26).

      * Error handling
       01  WS-ERROR-FIELDS.
           05  WS-ERROR-CODE           PIC X(04)
               VALUE SPACES.
           05  WS-ERROR-MESSAGE        PIC X(80)
               VALUE SPACES.
           05  WS-ERROR-PROGRAM        PIC X(08)
               VALUE SPACES.
           05  WS-ERROR-PARAGRAPH      PIC X(30)
               VALUE SPACES.
           05  WS-ERROR-SEVERITY       PIC 9(01)
               VALUE ZEROS.
               88  WS-ERR-INFO         VALUE 1.
               88  WS-ERR-WARNING      VALUE 2.
               88  WS-ERR-ERROR        VALUE 3.
               88  WS-ERR-CRITICAL     VALUE 4.
           05  WS-ERROR-DETAIL         PIC X(256)
               VALUE SPACES.

      * General working fields
       01  WS-GENERAL-FIELDS.
           05  WS-RETURN-CODE          PIC S9(04)
               VALUE ZEROS.
           05  WS-FUNCTION-CODE        PIC X(04)
               VALUE SPACES.
           05  WS-CONFIRM-FLAG         PIC X(01)
               VALUE "N".
               88  WS-CONFIRMED        VALUE "Y".
               88  WS-NOT-CONFIRMED    VALUE "N".
           05  WS-CONTINUE-FLAG        PIC X(01)
               VALUE "Y".
               88  WS-CONTINUE         VALUE "Y".
               88  WS-STOP             VALUE "N".
           05  WS-MENU-OPTION          PIC X(02)
               VALUE SPACES.
           05  WS-DISPLAY-BUFFER       PIC X(132)
               VALUE SPACES.
           05  WS-INPUT-BUFFER         PIC X(256)
               VALUE SPACES.
           05  WS-NUMERIC-INPUT        PIC X(20)
               VALUE SPACES.
           05  WS-NUMERIC-VALID        PIC X(01)
               VALUE "N".
           05  WS-TEMP-STRING          PIC X(256)
               VALUE SPACES.
           05  WS-TEMP-NUMBER          PIC S9(15)V99
               VALUE ZEROS.
           05  WS-IDX                  PIC 9(04)
               VALUE ZEROS.
           05  WS-IDX2                 PIC 9(04)
               VALUE ZEROS.
           05  WS-STRING-LEN           PIC 9(04)
               VALUE ZEROS.
           05  WS-POINTER              PIC 9(04)
               VALUE 1.

      * System command working fields
       01  WS-SYSTEM-CMD-FIELDS.
           05  WS-SYSTEM-CMD           PIC X(512)
               VALUE SPACES.
           05  WS-SYSTEM-CMD-RC        PIC S9(04)
               VALUE ZEROS.
           05  WS-FILE-PATH            PIC X(256)
               VALUE SPACES.
           05  WS-TEMP-FILENAME        PIC X(256)
               VALUE SPACES.
           05  WS-EXPORT-FILENAME      PIC X(256)
               VALUE SPACES.

      * Report working fields
       01  WS-REPORT-FIELDS.
           05  WS-REPORT-DATE          PIC X(10)
               VALUE SPACES.
           05  WS-REPORT-TITLE         PIC X(60)
               VALUE SPACES.
           05  WS-REPORT-PAGE          PIC 9(04)
               VALUE ZEROS.
           05  WS-REPORT-LINE          PIC 9(02)
               VALUE ZEROS.
           05  WS-REPORT-MAX-LINES     PIC 9(02)
               VALUE 60.
           05  WS-REPORT-FILENAME      PIC X(256)
               VALUE SPACES.

      * Encryption working fields (weak XOR)
       01  WS-CRYPTO-FIELDS.
           05  WS-XOR-KEY              PIC X(08)
               VALUE "MSTR2024".
           05  WS-PLAIN-TEXT           PIC X(256)
               VALUE SPACES.
           05  WS-CIPHER-TEXT          PIC X(256)
               VALUE SPACES.
           05  WS-PLAIN-BYTE          PIC X(01).
           05  WS-CIPHER-BYTE         PIC X(01).
           05  WS-KEY-IDX              PIC 9(02)
               VALUE 1.
           05  WS-CRYPTO-LEN           PIC 9(04)
               VALUE ZEROS.
           05  WS-BYTE-VALUE           PIC 9(03)
               VALUE ZEROS.
           05  WS-KEY-VALUE            PIC 9(03)
               VALUE ZEROS.
           05  WS-XOR-RESULT           PIC 9(03)
               VALUE ZEROS.

      * Audit trail fields
       01  WS-AUDIT-FIELDS.
           05  WS-AUDIT-TIMESTAMP      PIC X(26)
               VALUE SPACES.
           05  WS-AUDIT-USER           PIC X(20)
               VALUE SPACES.
           05  WS-AUDIT-ACTION         PIC X(20)
               VALUE SPACES.
           05  WS-AUDIT-DETAIL         PIC X(256)
               VALUE SPACES.
           05  WS-AUDIT-SEVERITY       PIC X(04)
               VALUE "INFO".
           05  WS-AUDIT-PROGRAM        PIC X(08)
               VALUE SPACES.

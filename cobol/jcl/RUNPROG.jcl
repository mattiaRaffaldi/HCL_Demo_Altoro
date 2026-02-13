//ALTORUN  JOB (ALTORO),'RUN ALTORO BANK',CLASS=A,
//         MSGCLASS=X,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*
//*****************************************************************
//* RUNPROG.JCL - Execute AltoroJ COBOL Banking System
//* Altoro Mutual Banking System
//*
//* Runs the main banking application with all required
//* data files allocated.
//*
//* Data files:
//*   CUSTFILE - Customer master file (VSAM KSDS)
//*   ACCTFILE - Account master file (VSAM KSDS)
//*   TRANFILE - Transaction file (VSAM KSDS)
//*   LOGFILE  - System log (sequential)
//*   SECLOG   - Security log (sequential)
//*   AUDITLOG - Audit trail (sequential)
//*   PWDHIST  - Password history (sequential)
//*****************************************************************
//*
//*------- EXECUTE MAIN PROGRAM ------------------------------------
//*
//RUNSTEP  EXEC PGM=MAINPROG,REGION=0M
//STEPLIB  DD DSN=ALTORO.COBOL.LOAD,DISP=SHR
//         DD DSN=DSNC10.SDSNLOAD,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//*
//*------- DATA FILES ----------------------------------------------
//*
//CUSTFILE DD DSN=ALTORO.DATA.CUSTFILE,DISP=SHR
//ACCTFILE DD DSN=ALTORO.DATA.ACCTFILE,DISP=SHR
//TRANFILE DD DSN=ALTORO.DATA.TRANFILE,DISP=SHR
//*
//*------- LOG FILES -----------------------------------------------
//*
//LOGFILE  DD DSN=ALTORO.LOG.SYSLOG,
//         DISP=(MOD,CATLG,CATLG),
//         SPACE=(TRK,(50,20),RLSE),
//         DCB=(RECFM=FB,LRECL=256,BLKSIZE=0)
//SECLOG   DD DSN=ALTORO.LOG.SECLOG,
//         DISP=(MOD,CATLG,CATLG),
//         SPACE=(TRK,(50,20),RLSE),
//         DCB=(RECFM=FB,LRECL=256,BLKSIZE=0)
//AUDITLOG DD DSN=ALTORO.LOG.AUDITLOG,
//         DISP=(MOD,CATLG,CATLG),
//         SPACE=(TRK,(50,20),RLSE),
//         DCB=(RECFM=FB,LRECL=256,BLKSIZE=0)
//PWDHIST  DD DSN=ALTORO.LOG.PWDHIST,
//         DISP=(MOD,CATLG,CATLG),
//         SPACE=(TRK,(10,5),RLSE),
//         DCB=(RECFM=FB,LRECL=256,BLKSIZE=0)
//CUSTAUDT DD DSN=ALTORO.LOG.CUSTAUDT,
//         DISP=(MOD,CATLG,CATLG),
//         SPACE=(TRK,(20,10),RLSE),
//         DCB=(RECFM=FB,LRECL=256,BLKSIZE=0)
//*
//*------- SYSTEM I/O ----------------------------------------------
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
/*
//

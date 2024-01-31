      ******************************************************************
      * Author: Luis Angel Baez Nieto
      * Date: 26/01/2024
      * Purpose: Learning project 5
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 5EMPREP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPSAL ASSIGN TO DISK.
           SELECT EMPORD ASSIGN TO DISK.
           SELECT EMPREP ASSIGN TO PRINTER.
           SELECT SOREMP ASSIGN TO DISK.
       DATA DIVISION.
       FILE SECTION.
       FD  EMPSAL.
       01  EMS-REG.
           02 EMS-NOMI PIC 9(06).
           02 EMS-NOMB PIC X(20).
           02 EMS-DEPT PIC X(03).
           02 EMS-PERC PIC 9(05)V99.
           02 EMS-DEDU PIC 9(05).
           02 EMS-SALA PIC S9(05)V99.
           02 FILLER       PIC XX.
       FD  EMPORD.
       01  EMO-REG.
           02 EMO-NOMI PIC 9(06).
           02 EMO-NOMB PIC X(20).
           02 EMO-DEPT PIC X(03).
           02 EMO-PERC PIC 9(05)V99.
           02 EMO-DEDU PIC 9(05).
           02 EMO-SALA PIC S9(05)V99.
           02 FILLER       PIC XX.
       SD  SOREMP.
       01  SEM-REG.
           02 SEM-NOMI PIC 9(06).
           02 SEM-NOMB PIC X(20).
           02 SEM-DEPT PIC X(03).
           02 SEM-PERC PIC 9(05)V99.
           02 SEM-DEDU PIC 9(05).
           02 SEM-SALA PIC S9(05)V99.
           02 FILLER       PIC XX.
       FD  EMPREP.
       01  EMR-REG PIC X(132).
       WORKING-STORAGE SECTION.

       77  ANT-DEPT    PIC X(3).
       77  LIN         PIC 99.
       77  MAXLIN      PIC 99 VALUE 9.
       77  EMP-CONT    PIC 9(04).
       77  EMO-EOF     PIC 9.
       77  PAG         PIC 99.
       77  PERC-ST     PIC 9(09)V99.
       77  DEDU-ST     PIC 9(09).
       77  SALA-ST     PIC S9(09)V99.
       77  ORG-DEPTS   PIC 99.
       77  ORG-EMPS    PIC 9(06).
       77  ORG-PERC    PIC 9(12)V99.
       77  ORG-DEDU    PIC 9(12).
       77  ORG-SALA    PIC S9(12)V99.
       01  MESES.
           03 FILLER   PIC x(36) VALUE
                       "ENEFEBMARABRMAYJUNJULAGOSEPOCTNOVDIC".
       01  MESES-R REDEFINES MESES.
           02 MESES-ROW OCCURS 12 TIMES.
               03 MES  PIC X(03).
       01  REPORT-LAYOUT.
           02 EMS-TIT-0.
               03 FILLER       PIC X(22) VALUE ">>--------------------".
               03 FILLER       PIC X(22) VALUE "----------------------".
               03 FILLER       PIC X(22) VALUE "----------------------".
               03 FILLER       PIC X(22) VALUE "----------------------".
               03 FILLER       PIC X(22) VALUE "----------------------".
               03 FILLER       PIC X(11) VALUE "---------<<".
           02 EMS-TIT-1.
               03 FILLER       PIC X(05) VALUE "PAG: ".
               03 EMS-TI-PAG   PIC ZZ.
               03 FILLER       PIC X(43) VALUE SPACES.
               03 FILLER       PIC X(21) VALUE "\\ STEFANINI GROUP //".
               03 FILLER       PIC X(39) VALUE SPACES.
               03 EMS-T1-DD    PIC 99.
               03 FILLER       PIC X VALUE "/".
               03 EMS-T1-MM    PIC X(03).
               03 FILLER       PIC X(03) VALUE "/20".
               03 EMS-T1-AA    PIC 99.
           02 EMS-TIT-2.
               03 FILLER       PIC X(47) VALUE SPACES.
               03 FILLER       PIC X(27) VALUE
                                   "REPORTE MENSUAL DE SALARIOS".
           02 EMS-TAB-TIT.
               03 FILLER       PIC X(17) VALUE SPACES.
               03 FILLER       PIC XX VALUE "| ".
               03 FILLER       PIC X(06) VALUE "NOMINA".
               03 FILLER       PIC X(03) VALUE " | ".
               03 FILLER       PIC X(07) VALUE SPACES.
               03 FILLER       PIC X(06) VALUE "NOMBRE".
               03 FILLER       PIC X(07) VALUE SPACES.
               03 FILLER       PIC X(03) VALUE " | ".
               03 FILLER       PIC X(12) VALUE "DEPARTAMENTO".
               03 FILLER       PIC X(03) VALUE " | ".
               03 FILLER       PIC X(10) VALUE "PERCEPCION".
               03 FILLER       PIC X(03) VALUE " | ".
               03 FILLER       PIC X(10) VALUE "DEDUCCION ".
               03 FILLER       PIC X(03) VALUE " | ".
               03 FILLER       PIC X(11) VALUE "  SALARIO  ".
               03 FILLER       PIC XX VALUE " |".
           02 EMS-TAB-SEP.
               03 FILLER       PIC X(17) VALUE SPACES.
               03 FILLER       PIC X(22) VALUE "======================".
               03 FILLER       PIC X(22) VALUE "======================".
               03 FILLER       PIC X(22) VALUE "======================".
               03 FILLER       PIC X(22) VALUE "======================".
           02 EMS-TAB-INF.
               03 FILLER       PIC X(17) VALUE SPACES.
               03 FILLER       PIC XX VALUE "| ".
               03 EMS-TAB-NOMI PIC Z(05)9.
               03 FILLER       PIC X(03) VALUE " | ".
               03 EMS-TAB-NOMB PIC X(20).
               03 FILLER       PIC X(03) VALUE " | ".
               03 FILLER       PIC X(04) VALUE SPACES.
               03 EMS-TAB-DEPT PIC X(03).
               03 FILLER       PIC X(05) VALUE SPACES.
               03 FILLER       PIC X(03) VALUE " | ".
               03 EMS-TAB-PERC PIC $$$,$$9.99.
               03 FILLER       PIC X(06) VALUE " |    ".
               03 EMS-TAB-DEDU PIC $$$,$$$.
               03 FILLER       PIC X(03) VALUE " | ".
               03 EMS-TAB-SIGN PIC X.
               03 EMS-TAB-SALA PIC $$$,$$9.99.
               03 FILLER       PIC XX VALUE " |".
           02 EMS-CORTE.
               03 FILLER       PIC X(17) VALUE SPACES.
               03 FILLER       PIC X(11) VALUE "EMPLEADOS: ".
               03 EMS-C-EMP    PIC Z(04).
               03 FILLER       PIC X(9) VALUE " SUBTOTAL".
               03 FILLER       PIC X(3) VALUE " P:".
               03 EMS-C-PERC   PIC $$$$,$$$,$$$,$$9.99.
               03 FILLER       PIC X(3) VALUE " D:".
               03 EMS-C-DEDU   PIC $$$$,$$$,$$$,$$9.
               03 FILLER       PIC X(3) VALUE " S:".
               03 EMS-C-SIGN   PIC X.
               03 EMS-C-SALA   PIC $$$$,$$$,$$$,$$9.99.
           02 EMS-TOTAL.
               03 FILLER       PIC X(8) VALUE SPACES.
               03 FILLER       PIC X(11) VALUE "EMPLEADOS: ".
               03 EMS-T-EMP    PIC Z(04).
               03 FILLER       PIC X(16) VALUE " DEPARTAMENTOS: ".
               03 EMS-T-DEPT   PIC Z9.
               03 FILLER       PIC X(9) VALUE " TOTAL".
               03 FILLER       PIC X(3) VALUE " P:".
               03 EMS-T-PERC   PIC $$$$,$$$,$$$,$$9.99.
               03 FILLER       PIC X(3) VALUE " D:".
               03 EMS-T-DEDU   PIC $$$$,$$$,$$$,$$9.
               03 FILLER       PIC X(3) VALUE " S:".
               03 EMS-T-SIGN   PIC X.
               03 EMS-T-SALA   PIC $$$$,$$$,$$$,$$9.99.
       01  FECHA.
           02 FEC-AA   PIC 99.
           02 FEC-MM   PIC 99.
           02 FEC-DD   PIC 99.

       PROCEDURE DIVISION.
      *---------------------------- Main ----------------------------
       MAIN-PROCEDURE.
           OPEN OUTPUT EMPREP.
           SORT SOREMP ON ASCENDING KEY SEM-DEPT SEM-NOMI
                                    USING EMPSAL
                                    GIVING EMPORD.
           OPEN INPUT EMPORD.
           ACCEPT FECHA FROM DATE.
           MOVE FEC-AA         TO EMS-T1-AA.
           MOVE MES(FEC-MM)    TO EMS-T1-MM.
           MOVE FEC-DD         TO EMS-T1-DD.
           PERFORM LEE-EMPORD.
           MOVE EMO-DEPT TO ANT-DEPT.
           COMPUTE LIN = MAXLIN + 1.
           PERFORM GEN-LINEAS-REP UNTIL EMO-EOF = 1.
           PERFORM CORTE-DEPT.
           MOVE ORG-EMPS TO EMS-T-EMP.
           MOVE ORG-DEPTS TO EMS-T-DEPT.
           MOVE ORG-PERC TO EMS-T-PERC.
           MOVE ORG-DEDU TO EMS-T-DEDU.
           IF ORG-SALA < 0
               MOVE "-" TO EMS-T-SIGN
           ELSE
               MOVE " " TO EMS-T-SIGN.
           MOVE ORG-SALA TO EMS-T-SALA.
           WRITE EMR-REG FROM EMS-TOTAL BEFORE 1 LINE.
           CLOSE EMPREP, EMPORD.
           STOP RUN.

      *--------------------- LEE ARCHIVO ORDENADO ---------------------
       LEE-EMPORD.
           READ EMPORD AT END MOVE 1 TO EMO-EOF.

      *----------------------- GENERA LINEAS REP -----------------------
       GEN-LINEAS-REP.
           IF ANT-DEPT NOT = EMO-DEPT
               PERFORM CORTE-DEPT.
           IF LIN >= MAXLIN
               PERFORM ESC-TITULOS.
           MOVE EMO-NOMI TO EMS-TAB-NOMI.
           MOVE EMO-NOMB TO EMS-TAB-NOMB.
           MOVE EMO-DEPT TO EMS-TAB-DEPT.
           MOVE EMO-PERC TO EMS-TAB-PERC.
           MOVE EMO-DEDU TO EMS-TAB-DEDU.
           IF EMO-SALA < 0
               MOVE "-" TO EMS-TAB-SIGN
           ELSE
               MOVE " " TO EMS-TAB-SIGN.
           MOVE EMO-SALA TO EMS-TAB-SALA.
           WRITE EMR-REG FROM EMS-TAB-INF BEFORE 1 LINE.
           ADD 1 TO LIN.
           ADD 1 TO EMP-CONT.
           ADD EMO-PERC TO PERC-ST.
           ADD EMO-DEDU TO DEDU-ST.
           ADD EMO-SALA TO SALA-ST.
           PERFORM LEE-EMPORD.

      *----------------------- ESCRIBIR TITULOS -----------------------
       ESC-TITULOS.
           ADD 1 TO PAG.
           MOVE PAG TO EMS-TI-PAG.
           WRITE EMR-REG FROM EMS-TIT-0 BEFORE PAGE.
           WRITE EMR-REG FROM EMS-TIT-1 BEFORE 1 LINE.
           WRITE EMR-REG FROM EMS-TIT-2 BEFORE 3 LINES.
           WRITE EMR-REG FROM EMS-TAB-TIT BEFORE 1 LINE.
           WRITE EMR-REG FROM EMS-TAB-SEP BEFORE 1 LINE.
           MOVE 7 TO LIN.

      *---------------------------- CORTE ----------------------------
       CORTE-DEPT.
           MOVE EMP-CONT TO EMS-C-EMP.
           MOVE PERC-ST TO EMS-C-PERC.
           MOVE DEDU-ST TO EMS-C-DEDU.
           IF SALA-ST < 0
               MOVE "-" TO EMS-C-SIGN
           ELSE
               MOVE " " TO EMS-C-SIGN.
           MOVE SALA-ST TO EMS-C-SALA.
           WRITE EMR-REG FROM SPACES BEFORE 1 LINE.
           WRITE EMR-REG FROM EMS-CORTE BEFORE 2 LINES.
           ADD 1 TO ORG-DEPTS.
           ADD EMP-CONT TO ORG-EMPS.
           ADD PERC-ST TO ORG-PERC.
           ADD DEDU-ST TO ORG-DEDU.
           ADD SALA-ST TO ORG-SALA.
           MOVE 0 TO EMP-CONT.
           MOVE 0 TO PERC-ST.
           MOVE 0 TO DEDU-ST.
           MOVE 0 TO SALA-ST.
           COMPUTE LIN = MAXLIN + 1.
           MOVE EMO-DEPT TO ANT-DEPT.

       END PROGRAM 5EMPREP.

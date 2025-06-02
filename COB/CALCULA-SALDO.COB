      ******************************************************************
      * FILE NAME   : CALCSALDO                                        *
      * DATE        : 2025-05-13                                       *
      * AUTHOR      : EDU360 COLLAB TEAM                               *
      *               THIAGO SOUZA                                     *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : CALCULAR SALDO FINAL                             *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCSALDO.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCONTA ASSIGN TO "C:\teste\cadconta.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS IDCONTA
               FILE STATUS IS WK-FS-CADCONTA.

           SELECT CADTRANS ASSIGN TO "C:\teste\cadtrans.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FD-IDTRANS
               ALTERNATE RECORD KEY IS FD-IDCONTA
               FILE STATUS IS WK-FS-CADTRANS.

           SELECT CADCONTA-N ASSIGN TO "C:\teste\cadconta-n.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS IDCONTAN
               FILE STATUS IS WK-FS-CADCONTAN.
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  CADCONTA.
       COPY CADCONTA.

       FD  CADTRANS.
           01  REG-TRANS.
               03 FD-DATA            PIC 9(08).
               03 FD-TIPO            PIC 9(01).
               03 FD-VALOR           PIC 9(05)V9(02).
               03 FD-DESCR           PIC X(05).
               03 FD-IDCONTA         PIC 9(04).
               03 FD-IDTRANS         PIC 9(08).

       FD  CADCONTA-N.
           01 REG-CADNOVO.
               03 IDCONTAN           PIC X(04).
               03 NOMEN              PIC X(30).
               03 SALDOININ          PIC 9(10)V9(02).
               03 FILLER             PIC X(54).

       WORKING-STORAGE SECTION.
       77  WK-FS-CADCONTA            PIC X(02)    VALUE ZEROS.
       77  WK-FS-CADTRANS            PIC X(02)    VALUE ZEROS.
       77  WK-FS-CADCONTAN           PIC X(02)    VALUE ZEROS.
       77  WK-CONTADOR               PIC 9(04)    VALUE ZEROS.
      *
       77  WK-ABEND-CODE             PIC XX       VALUE SPACES.
       77  WK-ABEND-MESSAGE          PIC X(30)    VALUE SPACES.
      ******************************************************************
       PROCEDURE DIVISION.
       0000-PRINCIPAL              SECTION.
       0010-PRINCIPAL.
           PERFORM 0110-OPEN-DATA.
           PERFORM 0210-PROCESS-DATA.
           PERFORM 0400-CLOSE-DATA.
           PERFORM 0600-END-PROGRAM.
      ******************************************************************
       0100-OPEN-DATA              SECTION.
       0110-OPEN-DATA.
           OPEN INPUT CADCONTA.
           IF WK-FS-CADCONTA NOT EQUAL "00"
               MOVE WK-FS-CADCONTA             TO WK-ABEND-CODE
               MOVE "FILE CADCONTA I/ERROR"    TO WK-ABEND-MESSAGE
               PERFORM 0500-ABEND THRU 0600-END-PROGRAM
           END-IF.

           OPEN INPUT CADTRANS.
           IF WK-FS-CADTRANS NOT EQUAL "00"
               MOVE WK-FS-CADTRANS             TO WK-ABEND-CODE
               MOVE "FILE CADTRANS I/ERROR"    TO WK-ABEND-MESSAGE
               PERFORM 0500-ABEND THRU 0600-END-PROGRAM
           END-IF.

           OPEN OUTPUT CADCONTA-N.
           IF WK-FS-CADCONTAN NOT EQUAL "00"
               MOVE WK-FS-CADCONTAN             TO WK-ABEND-CODE
               MOVE "FILE CADCONTAN O/ERROR"    TO WK-ABEND-MESSAGE
               PERFORM 0500-ABEND THRU 0600-END-PROGRAM
           END-IF.
       0110-OPEN-DATA-EXIT.        EXIT.
      ******************************************************************
       0200-PROCESS-DATA           SECTION.
       0210-PROCESS-DATA.
           PERFORM UNTIL WK-FS-CADCONTA EQUAL "10"
               READ CADCONTA
                   AT END
                       MOVE "10" TO WK-FS-CADCONTA
                   NOT AT END
                       MOVE "00" TO WK-FS-CADTRANS
                       PERFORM 0220-PROCESS-RECORD
               END-READ
           END-PERFORM.
       0210-PROCESS-DATA-EXIT.     EXIT.

       0220-PROCESS-RECORD         SECTION.
       0221-PROCESS-RECORD.
           MOVE ZEROS      TO WK-CONTADOR.
           MOVE "00"       TO WK-FS-CADTRANS.

           MOVE IDCONTA    TO FD-IDCONTA.
           START CADTRANS KEY IS EQUAL TO FD-IDCONTA
               INVALID KEY
                   MOVE "10" TO WK-FS-CADTRANS
           END-START.

           PERFORM UNTIL WK-FS-CADTRANS EQUAL "10"
                      OR FD-IDCONTA NOT EQUAL IDCONTA

               READ CADTRANS NEXT RECORD
                   AT END
                       MOVE "10" TO WK-FS-CADTRANS
                   NOT AT END
                       IF FD-IDCONTA EQUAL IDCONTA
                           ADD 1 TO WK-CONTADOR
                           DISPLAY "ADD"
                       ELSE
                           MOVE "10" TO WK-FS-CADTRANS
                       END-IF
               END-READ
           END-PERFORM.

           DISPLAY "Conta: " IDCONTA " - Transacoes: " WK-CONTADOR.
       0220-PROCESS-RECORD-EXIT.   EXIT.
      ******************************************************************
       0400-CLOSE-DATA             SECTION.
       0410-CLOSE-DATA.
           CLOSE CADCONTA.
           CLOSE CADTRANS.
           CLOSE CADCONTA-N.
       0410-CLOSE-DATA-EXIT.       EXIT.
      ******************************************************************
       0500-ABEND                  SECTION.
       0510-ABEND.
           DISPLAY "**********************************************".
           DISPLAY "* ************** ABEND ROUTINE ************* *".
           DISPLAY "* ABEND CODE: " WK-ABEND-CODE
                                       "                             *".
           DISPLAY "* ABEND MSG : " WK-ABEND-MESSAGE " *".
       0600-END-PROGRAM            SECTION.
       0610-END-PROGRAM.
           DISPLAY "**********************************************".
           DISPLAY "* ******* PROGRAM CADCONTA STATISTICS ****** *".
      *     DISPLAY "* LOAD COMPLETE: " WK-REG-COUNT
      *                                      "                        *".
      *     DISPLAY "* ERRORS       : " WK-ERR-COUNT
      *                                      "                        *".
           DISPLAY "**********************************************".
           GOBACK.

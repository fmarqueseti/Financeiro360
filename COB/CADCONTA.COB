      ******************************************************************
      * FILE NAME   : CADCONT                                          *
      * DATE        : 2025-05-07                                       *
      * AUTHOR      : EDU360 COLLAB TEAM                               *
      *               FABIO MARQUES (FMARQUES@FMARQUES.ETI.BR)         *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : LOAD ACCOUNTS FROM SEQUENTIAL TO VSAM FILE       *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADCONT.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-IN   ASSIGN TO SEQIN
                           ORGANIZATION IS SEQUENTIAL
                           ACCESS MODE IS SEQUENTIAL
                           FILE STATUS IS WS-STATUS-IN.
      *
           SELECT VSAM-OUT ASSIGN TO VSAMOUT
                           ORGANIZATION IS INDEXED
                           ACCESS MODE IS DYNAMIC
                           RECORD KEY IS IDCONTA
                           FILE STATUS IS WS-STATUS-OUT.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  SEQ-IN
             RECORDING MODE IS F.
       01  SEQ-RECORD.
           03 SR-IDCONTA         PIC X(04).
           03 SR-NOME            PIC X(30).
           03 SR-SALDOINI        PIC 9(10)V9(02).
           03 FILLER             PIC X(54).
      *
       FD  VSAM-OUT.
       COPY CADCONTA.
      *
       WORKING-STORAGE SECTION.
       77  WS-STATUS-IN          PIC XX.
       77  WS-STATUS-OUT         PIC XX.
       77  WS-REG-COUNT          PIC 9(4)  VALUE ZEROES.
       77  WS-ERR-COUNT          PIC 9(4)  VALUE ZEROES.
      *
       77  WS-ABEND-CODE         PIC XX    VALUE SPACES.
       77  WS-ABEND-MESSAGE      PIC X(30) VALUE SPACES.
      *
       PROCEDURE DIVISION.
       100-OPEN-DATA SECTION.
           OPEN INPUT SEQ-IN.
           IF WS-STATUS-IN NOT EQUAL "00"
               MOVE WS-STATUS-IN          TO WS-ABEND-CODE
               MOVE "FILE SEQIN IO/ERROR" TO WS-ABEND-MESSAGE
               PERFORM 500-ABEND THRU 600-END-PROGRAM
           END-IF.

           OPEN I-O VSAM-OUT.
           IF WS-STATUS-OUT NOT EQUAL "00"
               MOVE WS-STATUS-OUT           TO WS-ABEND-CODE
               MOVE "FILE VSAMOUT IO/ERROR" TO WS-ABEND-MESSAGE
               PERFORM 500-ABEND THRU 600-END-PROGRAM
           END-IF.
      *
       200-VALIDATE-DATA SECTION.
           EXIT.
      *
       300-PROCESS-DATA SECTION.
           PERFORM UNTIL WS-STATUS-IN = "10"
               READ SEQ-IN
                   AT END
                       MOVE "10" TO WS-STATUS-IN
                   NOT AT END
                      PERFORM 310-PROCESS-RECORD
               END-READ
           END-PERFORM.

           PERFORM 400-CLOSE-DATA.
           PERFORM 600-END-PROGRAM.
      *
        310-PROCESS-RECORD SECTION.
           IF SR-IDCONTA EQUAL SPACES OR SR-NOME EQUAL SPACES
               ADD 1 TO WS-ERR-COUNT
           ELSE
               MOVE SR-IDCONTA  TO IDCONTA
               MOVE SR-NOME     TO NOME
               MOVE SR-SALDOINI TO SALDOINI

               WRITE REG-CTA
                   INVALID KEY
                       ADD 1 TO WS-ERR-COUNT
                   NOT INVALID KEY
                       ADD 1 TO WS-REG-COUNT
           END-IF.
      *
       400-CLOSE-DATA SECTION.
           CLOSE SEQ-IN.
           CLOSE VSAM-OUT.
      *
       500-ABEND SECTION.
           DISPLAY "**********************************************".
           DISPLAY "* ************** ABEND ROUTINE ************* *".
           DISPLAY "* ABEND CODE: " WS-ABEND-CODE
                                       "                             *".
           DISPLAY "* ABEND MSG : " WS-ABEND-MESSAGE " *".
       600-END-PROGRAM SECTION.
           DISPLAY "**********************************************".
           DISPLAY "* ******* PROGRAM CADCONTA STATISTICS ****** *".
           DISPLAY "* LOAD COMPLETE: " WS-REG-COUNT
                                            "                        *".
           DISPLAY "* ERRORS       : " WS-ERR-COUNT
                                            "                        *".
           DISPLAY "**********************************************".
           GOBACK.

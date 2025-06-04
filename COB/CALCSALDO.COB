      ******************************************************************
      * FILE NAME   : CALCSALDO                                        *
      * DATE        : 2025-05-13                                       *
      * AUTHOR      : EDU360 COLLAB TEAM                               *
      *               THIAGO SOUZA                                     *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : CALCULAR SALDO FINAL                             *
      ******************************************************************
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID. CALCSALDO.
      ******************************************************************
       ENVIRONMENT                     DIVISION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
           SELECT CADCONTA ASSIGN TO "C:\teste\cadconta.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS SEQUENTIAL
               RECORD KEY   IS IDCONTA
               FILE STATUS  IS WK-FS-CADCONTA.
      *     
           SELECT CADTRANS ASSIGN TO "C:\teste\cadtrans.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS FD-IDTRANS
               ALTERNATE RECORD KEY IS FD-IDCONTA
               WITH DUPLICATES
               FILE STATUS  IS WK-FS-CADTRANS.
      *         
           SELECT CADCONTAN ASSIGN TO "C:\teste\cadcontan.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS SEQUENTIAL
               RECORD KEY   IS IDCONTA-N
               FILE STATUS IS WK-FS-CADCONTAN.
      ******************************************************************
       DATA                            DIVISION.
       FILE                            SECTION.
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
               
       FD  CADCONTAN.
           01  REG-CONTAN.
               03 IDCONTA-N          PIC X(04).
               03 NOME-N             PIC X(30).
               03 SALDOINI-N         PIC 9(10)V9(02).
               03 FILLER             PIC X(54).

       WORKING-STORAGE                 SECTION.
      *---> Variaveis de status 
       77  WK-FS-CADCONTA            PIC X(02)    VALUE ZEROS.
       77  WK-FS-CADTRANS            PIC X(02)    VALUE ZEROS.
       77  WK-FS-CADCONTAN           PIC X(02)    VALUE ZEROS.
       
       77  SALDOFINAL                PIC 9(06)V99 VALUE ZEROS.
       77  WK-ULTIMO-IDTRANS         PIC 9(08)    VALUE ZEROS.

       01 WK-TRANSACAO-ENCONTRADA    PIC X        VALUE "N".
           88 TEM-TRANSACAO                       VALUE "S".
           88 NAO-TEM-TRANSACAO                   VALUE "N".



      ******************************************************************
       PROCEDURE                       DIVISION.
       0000-PRINCIPAL                  SECTION.
       0010-PRINCIPAL.
           PERFORM 0110-OPEN-DATA.
           PERFORM 0310-PROCESS-DATA UNTIL WK-FS-CADCONTA EQUAL "10".
           PERFORM 0400-CLOSE-DATA.
           PERFORM 0500-END-PROGRAM.
      ******************************************************************
       0100-OPEN-DATA                  SECTION.
       0110-OPEN-DATA.
           OPEN INPUT   CADCONTA.
           PERFORM 0210-VALIDATE-CADCONTA-OPEN.

           OPEN INPUT CADTRANS.
           PERFORM 0220-VALIDATE-CADTRANS-OPEN.
           
           OPEN OUTPUT CADCONTAN.
           PERFORM 0230-VALIDATE-CADCONTAN-OPEN.
      ******************************************************************
       0200-VALIDATE-DATA              SECTION.
       0210-VALIDATE-CADCONTA-OPEN.
           EVALUATE WK-FS-CADCONTA
               WHEN "00"
                   CONTINUE
               WHEN "10"
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERRO: " WK-FS-CADCONTA
                           " NA ABERTURA DO ARQUIVO CADCONTA"
                   STOP RUN
           END-EVALUATE.
      *
       0220-VALIDATE-CADTRANS-OPEN.
           EVALUATE WK-FS-CADTRANS
               WHEN "00"
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERRO: " WK-FS-CADTRANS
                           " NA ABERTURA DO ARQUIVO CADTRANS"
                   STOP RUN
           END-EVALUATE.
      *         
       0230-VALIDATE-CADCONTAN-OPEN.
           EVALUATE WK-FS-CADCONTAN
               WHEN "00"
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERRO: " WK-FS-CADCONTAN
                           " NA ABERTURA DO ARQUIVO CADCONTAN"
                   STOP RUN
           END-EVALUATE.
      *
       0240-VALIDATE-CADCONTA-READ.
           EVALUATE WK-FS-CADCONTA
               WHEN "00"
                   CONTINUE
               WHEN "10"
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERRO: " WK-FS-CADCONTA
                           " NA LEITURA DO ARQUIVO CADCONTA"
                   STOP RUN
           END-EVALUATE.
      *
       0250-VALIDATE-CADTRANS-READ.
           EVALUATE WK-FS-CADTRANS
               WHEN "00"
                   CONTINUE
               WHEN "10"
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERRO: " WK-FS-CADTRANS
                           " NA LEITURA DO ARQUIVO CADTRANS"
                   STOP RUN
           END-EVALUATE.
      *
       0260-VALIDATE-CADCONTAN-WRITE.
           EVALUATE WK-FS-CADCONTAN
               WHEN "00"
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERRO: " WK-FS-CADCONTAN
                           " NA ESCRITA DO ARQUIVO CADCONTAN"
                   PERFORM 0410-CLOSE-DATA
                   PERFORM 0510-END-PROGRAM
           END-EVALUATE.
      ******************************************************************
       0300-PROCESS-DATA               SECTION.
       0310-PROCESS-DATA.
               READ CADCONTA
                   AT END
                       MOVE "10" TO WK-FS-CADCONTA
                   NOT AT END
                       PERFORM 0240-VALIDATE-CADCONTA-READ
                       PERFORM 0320-VERIFICA-TRANSACAO

                       IF TEM-TRANSACAO
                           MOVE IDCONTA    TO IDCONTA-N
                           MOVE NOME       TO NOME-N
                           MOVE SALDOFINAL TO SALDOINI-N
                           WRITE REG-CONTAN
                           PERFORM 0260-VALIDATE-CADCONTAN-WRITE
                           DISPLAY "ESCRITO SUCESSO!"
                           PERFORM 0340-MOSTRA-REGISTRO-NOVO
                       END-IF
               END-READ.
      *
       0320-VERIFICA-TRANSACAO.
           SET     NAO-TEM-TRANSACAO      TO TRUE.
           MOVE    IDCONTA                TO FD-IDCONTA.
           MOVE    SALDOINI               TO SALDOFINAL.
           MOVE    ZEROS                  TO WK-ULTIMO-IDTRANS.
           MOVE    "00"                   TO WK-FS-CADTRANS.

           START CADTRANS KEY IS NOT LESS THAN FD-IDCONTA
               INVALID KEY
                   MOVE "10" TO WK-FS-CADTRANS
                   DISPLAY "NENHUMA TRANSACAO - CONTA: " IDCONTA
                   EXIT PARAGRAPH
           END-START.

           PERFORM UNTIL WK-FS-CADTRANS EQUAL "10"
               READ CADTRANS NEXT
                   AT END
                       MOVE "10" TO WK-FS-CADTRANS
                       EXIT PARAGRAPH
                   NOT AT END
                       IF FD-IDCONTA NOT EQUAL IDCONTA
                           EXIT PERFORM
                       END-IF

                       PERFORM 0250-VALIDATE-CADTRANS-READ

                       IF FD-IDTRANS EQUAL WK-ULTIMO-IDTRANS
                           EXIT PERFORM
                       ELSE
                           MOVE FD-IDTRANS     TO WK-ULTIMO-IDTRANS
                           PERFORM 0330-CALCULA-SALDO
                       END-IF
                       SET TEM-TRANSACAO   TO TRUE
               END-READ
           END-PERFORM.
      *            
       0330-CALCULA-SALDO.   
           EVALUATE FD-TIPO
               WHEN 1
                   SUBTRACT FD-VALOR FROM SALDOFINAL
               WHEN 2
                   ADD      FD-VALOR TO SALDOFINAL
           END-EVALUATE.           
      *
       0340-MOSTRA-REGISTRO-NOVO.
           DISPLAY "+====================================+".
           DISPLAY "IDCONTA................. " IDCONTA
           DISPLAY "NOME.................... " NOME         
           DISPLAY "TOTAL CONTA " IDCONTA ": " SALDOFINAL.
           DISPLAY "+====================================+".            
      ******************************************************************
       0400-CLOSE-DATA                 SECTION.
       0410-CLOSE-DATA.
           CLOSE CADCONTA.
           CLOSE CADTRANS.
           CLOSE CADCONTAN.
           IF WK-FS-CADCONTAN NOT EQUAL "00" THEN
               DISPLAY "ERRO " WK-FS-CADCONTAN " AO FECHAR CADCONTAN" 
           ELSE
               DISPLAY "ARQUIVO FECHADO COM SUCESSO".
      ******************************************************************
       0500-END-PROGRAM                SECTION.
       0510-END-PROGRAM.
           STOP RUN.
       END PROGRAM CALCSALDO.

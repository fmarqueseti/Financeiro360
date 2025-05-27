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

       WORKING-STORAGE SECTION.
       77  WK-FS-CADCONTA            PIC X(02)    VALUE ZEROS.
       77  WK-FS-CADTRANS            PIC X(02)    VALUE ZEROS.
       77  SALDOFINAL                PIC 9(06)V99 VALUE ZEROS.

       01 WK-TRANSACAO-ENCONTRADA    PIC X        VALUE "N".
           88 TEM-TRANSACAO                       VALUE "S".
           88 NAO-TEM-TRANSACAO                   VALUE "N".



      ******************************************************************
       PROCEDURE DIVISION.
       0000-PRINCIPAL SECTION.
       0010-PRINCIPAL.
           PERFORM 0110-OPEN-DATA.
           PERFORM 0310-PROCESS-DATA UNTIL WK-FS-CADCONTA EQUAL "10".
           PERFORM 0400-CLOSE-DATA.
           PERFORM 0500-END-PROGRAM.
      ******************************************************************
       0100-OPEN-DATA SECTION.
       0110-OPEN-DATA.
           OPEN I-O   CADCONTA.
           PERFORM 0210-VALIDATE-CADCONTA-OPEN.
           DISPLAY "ABRIU ARQUIVO CADCONTA".

           OPEN INPUT CADTRANS.
           PERFORM 0220-VALIDATE-CADTRANS-OPEN.
           DISPLAY "ABRIU ARQUIVO CADTRANS".
      ******************************************************************
       0200-VALIDATE-DATA SECTION.
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
       0230-VALIDATE-CADCONTA-READ.
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
       0240-VALIDATE-CADTRANS-READ.
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
       0250-VALIDATE-CADCONTA-REWRITE.
           EVALUATE WK-FS-CADCONTA
               WHEN "00"
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERRO: " WK-FS-CADCONTA
                           " NA REESCRITA DO ARQUIVO CADCONTA"
                   STOP RUN
           END-EVALUATE.
      ******************************************************************
       0300-PROCESS-DATA SECTION.
       0310-PROCESS-DATA. 
           READ CADCONTA
               AT END
                   MOVE "10" TO WK-FS-CADCONTA
           END-READ.
           PERFORM 0230-VALIDATE-CADCONTA-READ.
           
           MOVE IDCONTA TO FD-IDCONTA.
           DISPLAY "FD-IDCONTA.........." FD-IDCONTA.
           
           PERFORM 0320-VERIFICA-TRANSACAO.
           
           IF TEM-TRANSACAO
               MOVE SALDOFINAL TO SALDOINI
               REWRITE REG-CTA
               PERFORM 0250-VALIDATE-CADCONTA-REWRITE
               DISPLAY "REESCRITO COM SUCESSO"
           END-IF.
      *
       0320-VERIFICA-TRANSACAO.
           SET NAO-TEM-TRANSACAO           TO TRUE. 
           MOVE SALDOINI TO SALDOFINAL.
           
           MOVE IDCONTA TO FD-IDTRANS
          
           START CADTRANS KEY IS EQUAL FD-IDTRANS
               INVALID KEY
                   DISPLAY "SEM TRANSACOES NA CONTA...." FD-IDCONTA
                   EXIT PARAGRAPH
           END-START.
           PERFORM 0321-LE-TRANSACOES UNTIL WK-FS-CADTRANS EQUAL "10".               
      *                
       0321-LE-TRANSACOES. 
           READ CADTRANS NEXT
               AT END
                   MOVE "10" TO WK-FS-CADTRANS
                   EXIT PARAGRAPH
           END-READ.
           PERFORM 0240-VALIDATE-CADTRANS-READ

           IF FD-IDCONTA LESS THAN IDCONTA
               EXIT PARAGRAPH
           END-IF

           IF FD-IDCONTA GREATER THAN IDCONTA
               EXIT PARAGRAPH
           END-IF    
           
           IF FD-IDCONTA EQUAL IDCONTA
               SET TEM-TRANSACAO           TO TRUE
               EVALUATE FD-TIPO
                   WHEN 1
                       SUBTRACT FD-VALOR FROM SALDOFINAL
                   WHEN 2
                       ADD      FD-VALOR TO SALDOFINAL
               END-EVALUATE
           END-IF.   
      ******************************************************************
       0400-CLOSE-DATA SECTION.
       0410-CLOSE-DATA.
           CLOSE CADCONTA.
           CLOSE CADTRANS.
           IF WK-FS-CADCONTA NOT EQUAL "00" THEN
               DISPLAY "ERRO AO FECHAR O ARQUIVO:" WK-FS-CADCONTA
           ELSE
               DISPLAY "ARQUIVO FECHADO COM SUCESSO".
      ******************************************************************
       0500-END-PROGRAM SECTION.
       0510-END-PROGRAM.
           STOP RUN.
       END PROGRAM CALCSALDO.

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
               RECORD KEY IS FD-IDCONTA
               FILE STATUS IS WK-FS-CADTRANS.
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  CADCONTA.
       COPY CADCONTA.

       FD  CADTRANS.
           01  REG-TANS.
               03 FD-DATA            PIC 9(08).
               03 FD-TIPO            PIC 9(01).
               03 FD-VALOR           PIC 9(05)V9(02).
               03 FD-DESCR           PIC X(05).
               03 FD-IDCONTA         PIC 9(04).
               03 FILLER             PIC X(14).

       WORKING-STORAGE SECTION.
       77  WK-FS-CADCONTA             PIC X(02)    VALUE ZEROS.
       77  WK-FS-CADTRANS             PIC X(02)    VALUE ZEROS.
       77  SALDOFINAL                 PIC 9(06)V99 VALUE ZEROS.

       01 WK-TRANSACAO-ENCONTRADA     PIC X        VALUE "N".
           88 TEM-TRANSACAO                        VALUE "S".
           88 NAO-TEM-TRANSACAO                    VALUE "N".



      ******************************************************************
       PROCEDURE DIVISION.
       0000-PRINCIPAL SECTION.
       0010-PRINCIPAL.
           PERFORM 0110-OPEN-DATA.
           PERFORM 0310-PROCESS-DATA.
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
           EVALUATE WK-FS-CADTRANS
               WHEN "00"
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERRO: " WK-FS-CADTRANS
                           " NA REESCRITA DO ARQUIVO CADCONTA"
                   STOP RUN
           END-EVALUATE.
      ******************************************************************
       0300-PROCESS-DATA SECTION.
       0310-PROCESS-DATA.
           PERFORM UNTIL WK-FS-CADCONTA EQUAL "10"

              SET NAO-TEM-TRANSACAO        TO TRUE

              READ CADCONTA
               AT END
                   MOVE "10" TO WK-FS-CADCONTA
               NOT AT END
                   PERFORM 0230-VALIDATE-CADCONTA-READ
                   PERFORM 0340-VERIFICA-TRANSACAO

              MOVE SALDOINI TO SALDOFINAL

              IF TEM-TRANSACAO
                   EVALUATE FD-TIPO
                       WHEN 1
                           SUBTRACT FD-VALOR FROM SALDOFINAL
                       WHEN 2
                           ADD      FD-VALOR TO   SALDOFINAL
                   END-EVALUATE
              END-IF

              MOVE SALDOFINAL TO SALDOINI

              REWRITE REG-CTA
              PERFORM 0250-VALIDATE-CADCONTA-REWRITE

           END-PERFORM.

       0340-VERIFICA-TRANSACAO.
               MOVE IDCONTA TO FD-IDCONTA
               READ CADTRANS KEY IS FD-IDCONTA
                   INVALID KEY
                       DISPLAY "SEM TRANSACOES NA CONTA...." FD-IDCONTA
                       SET NAO-TEM-TRANSACAO   TO TRUE
                   NOT INVALID KEY
                       DISPLAY "TRANSACAO ENCONTRADA......." FD-IDCONTA
                       SET TEM-TRANSACAO       TO TRUE
               END-READ.


      *
       0330-MOSTRA-REGISTRO-DEPOIS.
           DISPLAY "======DEPOIS=============".
           DISPLAY "DATA....." FD-DATA.
           DISPLAY "TIPO....." FD-TIPO.
           DISPLAY "VALOR...." FD-VALOR.
           DISPLAY "DESCR...." FD-DESCR.
           DISPLAY "IDCONTA.." FD-IDCONTA.
           DISPLAY "=========================".
      *
       0331-MOSTRA-REGISTRO-ANTES.
           DISPLAY "======ANTES==============".
           DISPLAY "IDCONTA.." IDCONTA.
           DISPLAY "NOME....." NOME.
           DISPLAY "SALDOINI." SALDOINI.
           DISPLAY "=========================".
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
       END PROGRAM CALCSALDO.

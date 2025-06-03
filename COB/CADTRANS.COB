      ******************************************************************
      * FILE NAME   : CADTRANS                                          *
      * DATE        : 2025-05-08                                       *
      * AUTHOR      : EDU360 COLLAB TEAM                               *
      *               THIAGO SOUZA                                     *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : CARREGAR TRANSACOES DE UM ARQUIVO SEQUENCIAL     *
      *                PARA UM INDEXADO                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADTRANS.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CAD-TRANS ASSIGN TO
                       "C:\teste\cad-trans.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WK-FS-TRANS.

           SELECT CADTRANS ASSIGN TO
                       "C:\teste\cadtrans.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS FD-IDTRANS
           ALTERNATE KEY IS FD-IDCONTA
           WITH DUPLICATES
           FILE STATUS IS WK-FS-CADTRANS.
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD CAD-TRANS.
       01 REG-CAD-TRANS.
           03 CAD-DATA              PIC 9(08).
           03 CAD-TIPO              PIC 9.
           03 CAD-VALOR             PIC 9(5)V99.
           03 CAD-DESCR             PIC X(5).
           03 CAD-IDCONTA           PIC 9(04).

       FD CADTRANS.
       01 REG-CADTRANS.
           03 FD-DATA              PIC 9(08).
           03 FD-TIPO              PIC 9.
           03 FD-VALOR             PIC 9(5)V99.
           03 FD-DESCR             PIC X(5).
           03 FD-IDCONTA           PIC 9(04).
           03 FD-IDTRANS           PIC 9(08).

       WORKING-STORAGE SECTION.
       77  WK-FS-TRANS             PIC X(2)      VALUE SPACES.
       77  WK-FS-CADTRANS          PIC X(2)      VALUE SPACES.
       77  WK-CONTADOR             PIC 9(04)     VALUE 1.
       77  WK-QNT-REG-LIDO         PIC 9(04)     VALUE ZEROS.
     ******************************************************************
       PROCEDURE DIVISION.
       0000-PRINCIPAL SECTION.
       0010-PRINCIPAL.
           PERFORM 0110-OPEN-DATA.
           PERFORM 0210-VALIDATE-CAD-TRANS-OPEN.
           PERFORM 0220-VALIDATE-CADTRANS-OPEN.
           PERFORM 0310-PROCESS-DATA UNTIL WK-FS-TRANS  EQUAL "10".
           PERFORM 0410-CLOSE-DATA.
           PERFORM 0500-END-PROGRAM.
      ******************************************************************
       0100-OPEN-DATA SECTION.
       0110-OPEN-DATA.
           OPEN INPUT  CAD-TRANS.
           OPEN OUTPUT CADTRANS.
      ******************************************************************
       0200-VALIDATE-DATA SECTION.
       0210-VALIDATE-CAD-TRANS-OPEN.
           EVALUATE WK-FS-TRANS
               WHEN "00"
                   CONTINUE
               WHEN "10"
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERRO: " WK-FS-TRANS
                           " NA ABERTURA DO ARQUIVO CAD-TRANS"
                   STOP RUN
           END-EVALUATE.
       0220-VALIDATE-CADTRANS-OPEN.
           EVALUATE WK-FS-CADTRANS
               WHEN "00"
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERRO: " WK-FS-CADTRANS
                           " NA ABERTURA DO ARQUIVO CADTRANS"
                   STOP RUN
           END-EVALUATE.
       0230-VALIDATE-CAD-TRANS-READ.
           EVALUATE WK-FS-CADTRANS
               WHEN "00"
                   CONTINUE
               WHEN "10"
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERRO: " WK-FS-TRANS
                           " NA LEITURA DO ARQUIVO CAD-TRANS"
                   STOP RUN
           END-EVALUATE.
       0240-VALIDATE-CADTRANS-WRITE.
           EVALUATE WK-FS-CADTRANS
               WHEN "00"
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERRO: " WK-FS-CADTRANS
                           " NA ESCRITA DO ARQUIVO CADTRANS"
                   STOP RUN
           END-EVALUATE.
      ******************************************************************
       0300-PROCESS-DATA SECTION.
       0310-PROCESS-DATA.
           READ CAD-TRANS
               AT END
                   MOVE "10" TO WK-FS-TRANS
               NOT AT END
                   PERFORM 0230-VALIDATE-CAD-TRANS-READ
                   MOVE REG-CAD-TRANS TO REG-CADTRANS

      *----> DISPLAY DO REGISTRO ANTES DE GRAVAR
                   PERFORM 0330-MOSTRA-REGISTRO-ANTES

                   MOVE WK-CONTADOR TO FD-IDTRANS

                   WRITE REG-CADTRANS
                   PERFORM 0240-VALIDATE-CADTRANS-WRITE

      *----> DISPLAY DO REGISTRO DEPOIS DE GRAVAR
                   PERFORM 0331-MOSTRA-REGISTRO-DEPOIS

                   MOVE WK-CONTADOR TO FD-IDTRANS

                   ADD 1 TO WK-CONTADOR

                   DISPLAY 'GRAVADO COM SUCESSO'
           END-READ.

           MOVE WK-CONTADOR TO WK-QNT-REG-LIDO.
      *
       0330-MOSTRA-REGISTRO-ANTES.
           DISPLAY "======ANTES==============".
           DISPLAY "DATA....." CAD-DATA.
           DISPLAY "TIPO....." CAD-TIPO.
           DISPLAY "VALOR...." CAD-VALOR.
           DISPLAY "DESCR...." CAD-DESCR.
           DISPLAY "IDCONTA.." CAD-IDCONTA.
           DISPLAY "=========================".
      *
       0331-MOSTRA-REGISTRO-DEPOIS.
           DISPLAY "======DEPOIS=============".
           DISPLAY "DATA....." FD-DATA.
           DISPLAY "TIPO....." FD-TIPO.
           DISPLAY "VALOR...." FD-VALOR.
           DISPLAY "DESCR...." FD-DESCR.
           DISPLAY "IDCONTA.." FD-IDCONTA.
           DISPLAY "=========================".
      ******************************************************************
       0400-CLOSE-DATA SECTION.
       0410-CLOSE-DATA.
           CLOSE CAD-TRANS.
           CLOSE CADTRANS.
           IF WK-FS-CADTRANS NOT EQUAL "00" THEN
               DISPLAY "ERRO AO FECHAR O ARQUIVO:" WK-FS-CADTRANS
           ELSE
               DISPLAY "ARQUIVO FECHADO COM SUCESSO".
      ******************************************************************
       0500-END-PROGRAM SECTION.
       0510-END-PROGRAM.
           STOP RUN.
       END PROGRAM CADTRANS.

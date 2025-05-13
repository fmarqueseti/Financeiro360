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
           SELECT CADCONTA ASSIGN TO CADCONTA
                           ORGANIZATION IS INDEXED
                           ACCESS MODE IS DYNAMIC
                           RECORD KEY IS IDCONTA
                           FILE STATUS IS WK-FS-CADCONTA.

           SELECT CADTRANS ASSIGN TO CADTRANS
                           ORGANIZATION IS INDEXED
                           RECORD KEY IS FD-IDTRANS
                           FILE STATUS IS WK-FS-CADTRANS.
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  CADCONTA.
       COPY CADCONTA.

       FD  CADTRANS.
       COPY CADTRANS.

       WORKING-STORAGE SECTION.
       77  WK-FS-CADCONTA             PIC X(02)    VALUE ZEROS.
       77  WK-FS-CADTRANS             PIC X(02)    VALUE ZEROS.
       77  SALDOFINAL                 PIC 9(06)V99 VALUE ZEROS.

      ******************************************************************
       PROCEDURE DIVISION.
       0000-PRINCIPAL SECTION.
       0010-PRINCIPAL.
           PERFORM 0110-OPEN-DATA.
           PERFORM 0310-PROCESS-DATA.
           PERFORM 0400-CLOSE-DATA.
           PERFORM 0500-END-PROGRAM. 

       0100-OPEN-DATA SECTION.
       0110-OPEN-DATA.
           OPEN I-O   CADCONTA.
           PERFORM 0210-VALIDATE-CADCONTA-OPEN.

           OPEN INPUT CADTRANS.
           PERFORM 0220-VALIDATE-CADTRANS-OPEN.
      *
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
      *     
       0300-PROCESS-DATA SECTION.
       0310-PROCESS-DATA.
           PERFORM UNTIL WK-FS-CADCONTA EQUAL "10"

              READ CADCONTA
              PERFORM 0230-VALIDATE-CADCONTA-READ
      
              MOVE IDCONTA TO CHAVE-ATUAL
    
              MOVE SALDOINI TO SALDOFINAL

              START CADTRANS KEY IS EQUAL TO CHAVE-ATUAL   

              PERFORM UNTIL WK-FS-CADTRANS EQUAL "10"
                  
                  READ CADTRANS
                  PERFORM 0240-VALIDATE-CADTRANS-READ 

                  IF IDCONTA EQUAL CHAVE-ATUAL 
                     EVALUATE TIPO
                        WHEN 1
                           SUBTRACT VALOR FROM SALDOFINAL
                        WHEN 2
                           ADD      VALOR TO   SALDOFINAL     
                     END-EVALUATE
                  END-IF
                 
                  IF WK-FS-CADTRANS EQUAL "10"
                     EXIT PERFORM
                  END-IF
              END-PERFORM  
              
              MOVE SALDOFINAL TO SALDOINI  

              REWRITE REG-CTA
              PERFORM 0250-VALIDATE-CADCONTA-REWRITE

              IF WK-FS-CADCONTA EQUAL "10"
                EXIT PERFORM
              END-IF

           END-PERFORM.

       0400-CLOSE-DATA SECTION.
       0410-CLOSE-DATA.
           CLOSE CADCONTA.
           CLOSE CADTRANS.
           IF WK-FS-CADCONTA NOT EQUAL "00" THEN
               DISPLAY "ERRO AO FECHAR O ARQUIVO:" WK-FS-CADCONTA
           ELSE
               DISPLAY "ARQUIVO FECHADO COM SUCESSO".
       0500-END-PROGRAM SECTION.
       0510-END-PROGRAM.
       END PROGRAM CALCSALDO.     
                     
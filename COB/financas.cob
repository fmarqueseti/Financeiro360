       IDENTIFICATION DIVISION. 
       PROGRAM-ID. FINANCAS.
      ******************************************************************
       ENVIRONMENT DIVISION.
       
      ******************************************************************
       DATA DIVISION. 
       FILE SECTION.
      * 
       WORKING-STORAGE SECTION.
      *
      ******************************************************************
       PROCEDURE DIVISION. 
       0000-UNICA SECTION.
       0100-PRINCIPAL.
           PERFORM 0200-INICIAR.
           PERFORM 0300-PROCESSAR1.
           PERFORM 0310-CALCULA-SALDO-FINAL.
           PERFORM 0320-EXIBE-EXTRATO.
           PERFORM 0330-RELATORIO-MENSAL.
           PERFORM 0400-FINALIZAR.
           DISPLAY "Hello world"
           STOP RUN.
      ******************************************************************        
       0200-INICIAR.

      ******************************************************************
       0300-PROCESSAR1.
           
      ******************************************************************
       0310-CALCULA-SALDO-FINAL.
           
      ******************************************************************
       0320-EXIBE-EXTRATO.
           
      ******************************************************************
       0330-RELATORIO-MENSAL.
           
      ******************************************************************
       0400-FINALIZAR.
           
      ******************************************************************        
       END PROGRAM FINANCAS.

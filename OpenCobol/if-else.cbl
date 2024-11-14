       IDENTIFICATION DIVISION.
       PROGRAM-ID. NESTED-IF-DEMO-INCORRECT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CONDITION-FLAG1    PIC X VALUE 'Y'.
       01 CONDITION-FLAG2    PIC X VALUE 'N'.

       PROCEDURE DIVISION.
           IF CONDITION-FLAG1 = 'Y'
               DISPLAY "Condition 1 met"
           ELSE IF CONDITION-FLAG2 = 'Y'
               DISPLAY "Condition 2 met"
           END-IF
           STOP RUN.

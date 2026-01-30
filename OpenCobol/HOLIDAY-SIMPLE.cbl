      ******************************************************************
      * Author: AI Assistant                                         *
      * Date: 25/01/2025                                             *
      * Holiday Day Calculator - Simple version                      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOLIDAY-CALC.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-INPUT-YEAR          PIC 9(4) VALUE ZEROS.
       01 WS-HOLIDAY-CHOICE      PIC 9(1) VALUE ZEROS.
       01 WS-CONTINUE-FLAG       PIC X(1) VALUE 'Y'.
       01 WS-ERROR-FLAG          PIC X(1) VALUE 'N'.
       01 WS-FORMATTED-DATE      PIC X(8).
       01 WS-DATE-NUM            PIC 9(8).
       01 WS-DAY-RESULT          PIC 9(1).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM SHOW-MENU.
           PERFORM UNTIL WS-CONTINUE-FLAG = 'N'
               PERFORM GET-INPUT
               IF WS-ERROR-FLAG = 'N'
                   PERFORM CALC-HOLIDAY
               END-IF
               PERFORM ASK-AGAIN
           END-PERFORM.
           DISPLAY 'Goodbye!'.
           STOP RUN.
           
       SHOW-MENU.
           DISPLAY ' '.
           DISPLAY '================================================='
           DISPLAY '           HOLIDAY DAY CALCULATOR'
           DISPLAY '    What day does your holiday fall on?'
           DISPLAY '================================================='
           DISPLAY ' '.
           DISPLAY '1. New Years Day (January 1)'
           DISPLAY '2. Independence Day (July 4)'  
           DISPLAY '3. Halloween (October 31)'
           DISPLAY '4. Christmas (December 25)'
           DISPLAY ' '.

       GET-INPUT.
           MOVE 'N' TO WS-ERROR-FLAG.
           
           DISPLAY 'Enter holiday (1-4): ' WITH NO ADVANCING.
           ACCEPT WS-HOLIDAY-CHOICE.
           
           IF WS-HOLIDAY-CHOICE < 1 OR WS-HOLIDAY-CHOICE > 4
               DISPLAY 'Please enter 1, 2, 3, or 4'
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF.
           
           DISPLAY 'Enter year (1806-3000): ' WITH NO ADVANCING.
           ACCEPT WS-INPUT-YEAR.
           
           IF WS-INPUT-YEAR < 1806 OR WS-INPUT-YEAR > 3000
               DISPLAY 'Year must be 1806-3000'
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       CALC-HOLIDAY.
           EVALUATE WS-HOLIDAY-CHOICE
               WHEN 1
                   STRING WS-INPUT-YEAR '0101' INTO WS-FORMATTED-DATE
                   DISPLAY 'New Years Day ' WS-INPUT-YEAR
               WHEN 2
                   STRING WS-INPUT-YEAR '0704' INTO WS-FORMATTED-DATE  
                   DISPLAY 'Independence Day ' WS-INPUT-YEAR
               WHEN 3
                   STRING WS-INPUT-YEAR '1031' INTO WS-FORMATTED-DATE
                   DISPLAY 'Halloween ' WS-INPUT-YEAR
               WHEN 4
                   STRING WS-INPUT-YEAR '1225' INTO WS-FORMATTED-DATE
                   DISPLAY 'Christmas ' WS-INPUT-YEAR
           END-EVALUATE.
           
           COMPUTE WS-DATE-NUM = FUNCTION NUMVAL(WS-FORMATTED-DATE).
           COMPUTE WS-DAY-RESULT = FUNCTION MOD(
               FUNCTION INTEGER-OF-DATE(WS-DATE-NUM), 7) + 1.
               
           DISPLAY 'falls on:'
           EVALUATE WS-DAY-RESULT
               WHEN 1 DISPLAY 'MONDAY'
               WHEN 2 DISPLAY 'TUESDAY'
               WHEN 3 DISPLAY 'WEDNESDAY' 
               WHEN 4 DISPLAY 'THURSDAY'
               WHEN 5 DISPLAY 'FRIDAY'
               WHEN 6 DISPLAY 'SATURDAY'
               WHEN 7 DISPLAY 'SUNDAY'
           END-EVALUATE.
           DISPLAY ' '.

       ASK-AGAIN.
           DISPLAY 'Check another? (Y/N): ' WITH NO ADVANCING.
           ACCEPT WS-CONTINUE-FLAG.
           IF WS-CONTINUE-FLAG = 'y'
               MOVE 'Y' TO WS-CONTINUE-FLAG
           END-IF.

       END PROGRAM HOLIDAY-CALC.

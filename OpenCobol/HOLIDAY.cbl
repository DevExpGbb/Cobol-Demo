      ******************************************************************
      * Author: AI Assistant                                         *
      * Date: 25/01/2025                                             *
      * Holiday Day Calculator - Determine day of week for holidays  *
      * Supports years from 1806 to 3000                             *
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
       01 WS-CALC-DATE           PIC X(8).
       01 WS-DATE-INTEGER        PIC 9(8) COMP.
       01 WS-DAY-OF-WEEK         PIC 9(1) COMP.
       01 WS-HOLIDAY-MONTH       PIC 99.
       01 WS-HOLIDAY-DAY         PIC 99.
       01 WS-HOLIDAY-NAME        PIC X(25).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM DISPLAY-WELCOME
           
           PERFORM UNTIL WS-CONTINUE-FLAG = 'N'
               PERFORM GET-USER-INPUT
               IF WS-ERROR-FLAG = 'N'
                   PERFORM SET-HOLIDAY-DATA
                   PERFORM CALCULATE-DAY
                   PERFORM SHOW-RESULT
               END-IF
               PERFORM ASK-CONTINUE
           END-PERFORM
           
           DISPLAY 'Thank you for using Holiday Calculator!'
           STOP RUN.
           
       DISPLAY-WELCOME.
           DISPLAY ' '
           DISPLAY '================================================='
           DISPLAY '           HOLIDAY DAY CALCULATOR'
           DISPLAY '   What day does your holiday fall on?'
           DISPLAY '    (Historical dates from 1806-3000)'
           DISPLAY '================================================='
           DISPLAY ' '
           DISPLAY '1. New Years Day (1/1)'
           DISPLAY '2. Valentines Day (2/14)'  
           DISPLAY '3. Independence Day (7/4)'
           DISPLAY '4. Halloween (10/31)'
           DISPLAY '5. Christmas (12/25)'
           DISPLAY ' '.
           
       GET-USER-INPUT.
           MOVE 'N' TO WS-ERROR-FLAG
           
           DISPLAY 'Enter holiday number (1-5): ' WITH NO ADVANCING
           ACCEPT WS-HOLIDAY-CHOICE
           
           IF WS-HOLIDAY-CHOICE < 1 OR WS-HOLIDAY-CHOICE > 5
               DISPLAY 'Error: Please enter 1-5'
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           
           DISPLAY 'Enter year (1806-3000): ' WITH NO ADVANCING
           ACCEPT WS-INPUT-YEAR
           
           IF WS-INPUT-YEAR < 1806 OR WS-INPUT-YEAR > 3000
               DISPLAY 'Error: Year must be 1806-3000'
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.
           
       SET-HOLIDAY-DATA.
           EVALUATE WS-HOLIDAY-CHOICE
               WHEN 1
                   MOVE 'New Years Day          ' TO WS-HOLIDAY-NAME
                   MOVE 01 TO WS-HOLIDAY-MONTH
                   MOVE 01 TO WS-HOLIDAY-DAY
               WHEN 2
                   MOVE 'Valentines Day         ' TO WS-HOLIDAY-NAME
                   MOVE 02 TO WS-HOLIDAY-MONTH
                   MOVE 14 TO WS-HOLIDAY-DAY
               WHEN 3
                   MOVE 'Independence Day        ' TO WS-HOLIDAY-NAME
                   MOVE 07 TO WS-HOLIDAY-MONTH
                   MOVE 04 TO WS-HOLIDAY-DAY
               WHEN 4
                   MOVE 'Halloween               ' TO WS-HOLIDAY-NAME
                   MOVE 10 TO WS-HOLIDAY-MONTH
                   MOVE 31 TO WS-HOLIDAY-DAY
               WHEN 5
                   MOVE 'Christmas Day           ' TO WS-HOLIDAY-NAME
                   MOVE 12 TO WS-HOLIDAY-MONTH
                   MOVE 25 TO WS-HOLIDAY-DAY
           END-EVALUATE.
           
       CALCULATE-DAY.
      *    Create date in YYYYMMDD format
           MOVE WS-INPUT-YEAR TO WS-CALC-DATE(1:4)
           MOVE WS-HOLIDAY-MONTH TO WS-CALC-DATE(5:2)  
           MOVE WS-HOLIDAY-DAY TO WS-CALC-DATE(7:2)
           
           COMPUTE WS-DATE-INTEGER = FUNCTION INTEGER-OF-DATE(
               FUNCTION NUMVAL(WS-CALC-DATE))
           COMPUTE WS-DAY-OF-WEEK = FUNCTION MOD(WS-DATE-INTEGER, 7) + 1.
           
       SHOW-RESULT.
           DISPLAY ' '
           DISPLAY '================================================='
           DISPLAY WS-HOLIDAY-NAME ' (' WS-HOLIDAY-MONTH '/'
               WS-HOLIDAY-DAY '/' WS-INPUT-YEAR ') falls on:'
           
           EVALUATE WS-DAY-OF-WEEK
               WHEN 1
                   DISPLAY 'MONDAY'
               WHEN 2
                   DISPLAY 'TUESDAY'
               WHEN 3
                   DISPLAY 'WEDNESDAY'
               WHEN 4
                   DISPLAY 'THURSDAY'
               WHEN 5
                   DISPLAY 'FRIDAY'
               WHEN 6
                   DISPLAY 'SATURDAY - Weekend celebration!'
               WHEN 7
                   DISPLAY 'SUNDAY - Weekend celebration!'
           END-EVALUATE
           
           DISPLAY '================================================='
           DISPLAY ' '.
           
       ASK-CONTINUE.
           DISPLAY 'Check another holiday? (Y/N): ' WITH NO ADVANCING
           ACCEPT WS-CONTINUE-FLAG
           IF WS-CONTINUE-FLAG = 'y'
               MOVE 'Y' TO WS-CONTINUE-FLAG
           END-IF.

       END PROGRAM HOLIDAY-CALC.

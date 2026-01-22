# Calendar Subroutine for AS400 COBOL

## Overview
The Calendar Subroutine is a comprehensive COBOL function designed to determine the day of the week for any given date and identify if that date corresponds to a recognized holiday. This subroutine provides essential calendar functionality for AS400 COBOL applications.

## Programs Included

### CALENDAR.CBLLE
**Main Calendar Subroutine**

A callable subroutine that provides:
- Day of week calculation for any date (1600-3000)
- Holiday identification (Canada Day, New Year's Day, Christmas)
- Input validation and error handling
- Linkage section interface for external calling

### CALAPPS.CBLLE  
**Interactive Calendar Application**

A standalone interactive program that uses the CALENDAR subroutine to provide:
- Menu-driven interface for date checking
- Current date analysis
- Canada Day calculator for specific years
- Holiday listing functionality

### TESTCAL.CBLLE
**Test Program**

Comprehensive test suite that validates:
- Automated testing of known date/day combinations
- Holiday detection accuracy  
- Input validation and error handling
- Interactive testing capability

## Technical Features

### Date Calculation Algorithm
The subroutine uses COBOL intrinsic functions for accurate date calculations:
- `FUNCTION INTEGER-OF-DATE` - Converts YYYYMMDD format to integer days
- `FUNCTION MOD` - Calculates day of week using modulo 7 arithmetic

### Supported Date Range
- **Minimum Year**: 1600
- **Maximum Year**: 3000
- **Full Calendar**: All months and valid days within range

### Holiday Support
Currently recognizes these holidays:
- **Canada Day**: July 1st
- **New Year's Day**: January 1st  
- **Christmas Day**: December 25th

Additional holidays can be easily added to the holiday checking logic.

## Usage Examples

### Calling the Subroutine
```cobol
DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-INPUT-DATE.
   05 WS-YEAR           PIC 9(4) VALUE 2025.
   05 WS-MONTH          PIC 99   VALUE 07.
   05 WS-DAY            PIC 99   VALUE 01.

01 WS-OUTPUT-DATA.
   05 WS-DAY-NAME       PIC X(9).
   05 WS-HOLIDAY-NAME   PIC X(20).
   05 WS-IS-HOLIDAY     PIC X(1).
   05 WS-ERROR-FLAG     PIC X(1).
   05 WS-ERROR-MESSAGE  PIC X(50).

PROCEDURE DIVISION.
    CALL "CALENDAR" USING WS-INPUT-DATE, WS-OUTPUT-DATA.
    
    IF WS-ERROR-FLAG = 'N' THEN
        DISPLAY "Day: " WS-DAY-NAME
        IF WS-IS-HOLIDAY = 'Y' THEN
            DISPLAY "Holiday: " WS-HOLIDAY-NAME
        END-IF
    ELSE
        DISPLAY "Error: " WS-ERROR-MESSAGE
    END-IF.
```

### Sample Output
```
================================================
            CALENDAR APPLICATION
     Day of Week and Holiday Calculator
================================================

Date: 7/1/2025
Day of Week: Tuesday  
Holiday: Canada Day
*** This is a recognized holiday! ***
================================================
```

## Input Parameters

### LS-INPUT-DATE
- **LS-YEAR** (PIC 9(4)): Year to check (1600-3000)
- **LS-MONTH** (PIC 99): Month (1-12)  
- **LS-DAY** (PIC 99): Day of month (1-31)

## Output Parameters

### LS-OUTPUT-DATA
- **LS-DAY-NAME** (PIC X(9)): Full day name (e.g., "Monday   ", "Tuesday  ")
- **LS-HOLIDAY-NAME** (PIC X(20)): Holiday name if applicable
- **LS-IS-HOLIDAY** (PIC X(1)): 'Y' if holiday, 'N' if not
- **LS-ERROR-FLAG** (PIC X(1)): 'Y' if error occurred, 'N' if successful
- **LS-ERROR-MESSAGE** (PIC X(50)): Error description if error occurred

## Error Handling

The subroutine validates:
- **Year Range**: Must be between 1600 and 3000
- **Month Range**: Must be between 1 and 12
- **Day Range**: Must be between 1 and 31, with additional validation for:
  - February: Maximum 29 days
  - April, June, September, November: Maximum 30 days

## Compilation

### AS400 Environment
```
CRTCBLPGM PGM(MYLIB/CALENDAR) SRCFILE(MYLIB/QCBLLESRC)
CRTCBLPGM PGM(MYLIB/CALAPPS) SRCFILE(MYLIB/QCBLLESRC)  
CRTCBLPGM PGM(MYLIB/TESTCAL) SRCFILE(MYLIB/QCBLLESRC)
```

### OpenCOBOL Environment
```bash
cobc -x CALENDAR.CBLLE
cobc -x CALAPPS.CBLLE
cobc -x TESTCAL.CBLLE
```

## Testing

Run the test program to validate functionality:
```
CALL TESTCAL
```

The test program will:
1. Run automated tests on known dates
2. Validate holiday detection
3. Test error handling for invalid inputs
4. Provide interactive testing interface

Expected test results:
- Canada Day 2025: Tuesday (Holiday)
- New Year 2025: Wednesday (Holiday)  
- Christmas 2024: Wednesday (Holiday)
- Regular dates: Correct day calculation
- Invalid inputs: Proper error messages

## Integration

This subroutine can be integrated into existing COBOL applications by:

1. **Including the CALENDAR program** in your library
2. **Adding CALL statements** where date calculations are needed
3. **Handling the output parameters** appropriately in your application logic
4. **Extending holiday definitions** as needed for your specific requirements

## Future Enhancements

Potential improvements could include:
- **Additional Holidays**: Provincial holidays, international holidays
- **Leap Year Logic**: Enhanced February validation
- **Date Arithmetic**: Add/subtract days from dates
- **Week Number Calculation**: ISO week numbers
- **Business Day Logic**: Skip weekends and holidays
- **Localization**: Multiple language support for day names

## Dependencies
- No external files or copybooks required
- Uses standard COBOL intrinsic functions
- Self-contained subroutine with comprehensive error handling

---
**Created**: January 21, 2025  
**Author**: Copilot AI Assistant  
**Version**: 1.0  
**Compatible**: AS400 COBOL, OpenCOBOL
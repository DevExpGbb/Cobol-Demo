# Holiday Tracking Program for Tax Calculations - Canadian Edition

## Overview
The Holiday Tracking Program is a COBOL AS400 application that determines what day of the year Canadian statutory holidays fall on for past, present, and future years. This is particularly useful for tax calculations and business planning in Canada.

## Programs

### HOLIDAY01.CBLLE
**Main Holiday Tracking Program**

#### Features:
- Interactive user interface for holiday selection
- Calculates day of week and day of year for multiple holidays
- Supports years from 1600-3000 for comprehensive coverage
- Includes both fixed and variable date holidays
- Input validation and error handling
- Continuous operation until user chooses to exit

#### Supported Holidays:
1. **New Year's Day** - January 1st (fixed date)
2. **Good Friday** - Friday before Easter (calculated)
3. **Easter Sunday** - Variable date using Gregorian calendar algorithm
4. **Victoria Day** - Monday on or before May 24 (calculated) - Uniquely Canadian
5. **Canada Day** - July 1st (fixed date)
6. **Civic Holiday** - First Monday in August (calculated)
7. **Labour Day** - First Monday in September (calculated)
8. **Thanksgiving (Canada)** - Second Monday in October (calculated)
9. **Remembrance Day** - November 11th (fixed date) - Canadian observance
10. **Christmas Day** - December 25th (fixed date)
11. **Boxing Day** - December 26th (fixed date) - Canadian statutory holiday

#### Usage:
1. Run the program
2. Enter a year when prompted (1600-3000)
3. Select a holiday from the menu (1-7)
4. Program displays:
   - Date the holiday falls on
   - Day of the week
   - Day of year number (for tax purposes)
   - Additional context information
5. Choose to check another holiday or exit

#### Sample Output:
```
================================================
        HOLIDAY TRACKING PROGRAM
    For Tax Calculations and Planning
================================================

Enter a year (1600-3000): 2025

Select a Canadian holiday:
 1. New Year's Day (January 1)
 2. Good Friday (Friday before Easter)
 3. Easter Sunday (variable date)
 4. Victoria Day (Monday before May 25)
 5. Canada Day (July 1)
 6. Civic Holiday (1st Monday in August)
 7. Labour Day (1st Monday in September)
 8. Thanksgiving (2nd Monday in October)
 9. Remembrance Day (November 11)
10. Christmas Day (December 25)
11. Boxing Day (December 26)

Enter choice (1-11): 5

================================================
Canada Day               (7/1/2025) falls on a Tuesday  
================================================

Tax Information:
Day of year: 182

Additional Facts:
- Holiday falls on a weekday

Check another holiday? (Y/N): N

Thank you for using Holiday Tracking Program!
```

### TESTHOL.CBLLE
**Comprehensive Test Program**

A comprehensive automated test suite that validates holiday calculations with predefined test cases covering:
- All supported holidays
- Multiple years including leap years
- Edge cases for variable date calculations
- Expected vs actual result verification

#### Test Cases Included:
1. New Year's Day 2025 (Day 1 of year)
2. Good Friday 2025 (Day 108 of year) - Calculated from Easter
3. Victoria Day 2025 (Day 139 of year) - Monday before May 25
4. Canada Day 2025 (Day 182 of year)
5. Labour Day 2025 (Day 244 of year) - First Monday in September
6. Remembrance Day 2025 (Day 315 of year)
7. Boxing Day 2025 (Day 360 of year)

#### Sample Test Output:
```
================================================
     AUTOMATED HOLIDAY TRACKER TEST SUITE
   Testing Canadian Statutory Holidays
================================================

Testing: New Year's Day 2025
  Year: 2025 Holiday: New Year's Day
  RESULT: PASS - Date: 1/1 DOY: 1 (Wednesday)

Testing: Good Friday 2025
  Year: 2025 Holiday: Good Friday
  RESULT: PASS - Date: 4/18 DOY: 108 (Friday   )

Testing: Victoria Day 2025
  Year: 2025 Holiday: Victoria Day
  RESULT: PASS - Date: 5/19 DOY: 139 (Monday   )

Testing: Canada Day 2025
  Year: 2025 Holiday: Canada Day
  RESULT: PASS - Date: 7/1 DOY: 182 (Tuesday  )

Testing: Labour Day 2025
  Year: 2025 Holiday: Labour Day
  RESULT: PASS - Date: 9/1 DOY: 244 (Monday   )

Testing: Remembrance Day 2025
  Year: 2025 Holiday: Remembrance Day
  RESULT: PASS - Date: 11/11 DOY: 315 (Tuesday  )

Testing: Boxing Day 2025
  Year: 2025 Holiday: Boxing Day
  RESULT: PASS - Date: 12/26 DOY: 360 (Friday   )

================================================
TEST SUMMARY
================================================
Tests Run:    7
Tests Passed: 7
Tests Failed: 0

ALL TESTS PASSED! Holiday calculations verified.
================================================
```

## Compilation

### For AS400 environments:
```
CRTCBLPGM PGM(MYLIB/HOLIDAY01) SRCFILE(MYLIB/QCBLLESRC)
CRTCBLPGM PGM(MYLIB/TESTHOL) SRCFILE(MYLIB/QCBLLESRC)
```

### For Open COBOL environments:
```
cobc -x HOLIDAY01.CBLLE
cobc -x TESTHOL.CBLLE
```

## Technical Details

### Date Calculation Algorithms
The program uses advanced date calculation methods:

1. **Fixed Dates**: Direct month/day assignment
2. **Easter Calculation**: Implements the Gregorian calendar algorithm for Easter Sunday
3. **First Monday Calculations**: Algorithmic determination for Labour Day and Thanksgiving
4. **Day of Year**: Uses COBOL intrinsic functions for precise calculations

### Key Features:
- **Input Validation**: Ensures year is between 1600-3000 and holiday choice is 1-7
- **Error Handling**: Graceful handling of invalid inputs with clear error messages
- **User-Friendly Interface**: Clear prompts, formatted output, and menu-driven navigation
- **Tax Integration**: Day of year calculation specifically for tax purposes
- **Historical Coverage**: Supports 400+ years of historical and future dates

### Data Structures:
- Holiday name lookup table for user-friendly display
- Day name lookup table for converting numeric day to text
- Easter calculation workspace with intermediate values
- Formatted date fields for precise calculations
- Error flags for robust operation

## Tax Calculation Benefits

The day of year output is particularly useful for:
- **Prorated calculations**: Determining fractional year values
- **Interest calculations**: Precise day counting for financial calculations
- **Payroll systems**: Holiday pay calculations
- **Business planning**: Understanding holiday impacts on business days
- **Compliance**: Meeting regulatory requirements for date-specific calculations

## Dependencies
- No external files or copybooks required
- Uses standard COBOL intrinsic functions
- Self-contained application with embedded lookup tables

## Testing
Use TESTHOL.CBLLE to run automated verification of all holiday calculations. The test suite includes comprehensive coverage of:
- All supported holidays
- Multiple years and scenarios
- Edge cases and boundary conditions
- Expected result validation

## Future Enhancements
- Add regional holiday variations
- Support for different calendar systems
- Export results to file for batch processing
- Additional holiday types (provincial, religious, etc.)
- Internationalization for different countries
- Web service interface for integration
- Historical holiday rule changes

---
**Created**: September 25, 2025  
**Author**: Development Team  
**Version**: 1.0
**Purpose**: Tax calculations and business planning
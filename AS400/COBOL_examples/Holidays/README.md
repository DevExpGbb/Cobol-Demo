# Holiday Tracking System

This directory contains AS400 COBOL programs for tracking US Federal Holidays, useful for tax calculations and business planning.

## Programs

### HOLYTRK01.CBLLE - Holiday Tracking Program
A comprehensive holiday tracking program that calculates the dates of all major US Federal Holidays for any given year (1600-3000).

**Features:**
- Calculates fixed-date holidays (New Year's Day, Independence Day, Veterans Day, Christmas)
- Calculates floating holidays based on day-of-week rules:
  - Martin Luther King Jr. Day (3rd Monday in January)
  - Presidents' Day (3rd Monday in February)
  - Memorial Day (Last Monday in May)
  - Labor Day (1st Monday in September)
  - Columbus Day (2nd Monday in October)
  - Thanksgiving Day (4th Thursday in November)
- Displays day of week for each holiday
- Displays day of year (Julian day) for tax calculation purposes
- Interactive interface for checking multiple years

**Usage:**
```cobol
CALL HOLYTRK01
```

The program will prompt for a year and display all federal holidays for that year with their dates, day of week, and day of year.

### TESTHLDY.CBLLE - Holiday Test Program
An automated test program that validates the holiday calculations across multiple years (past, present, and future).

**Features:**
- Tests calculations for years: 2020, 2024, 2025, 2030, and 1776
- Validates both fixed-date and floating holidays
- Displays comprehensive test results
- Shows pass/fail summary

**Usage:**
```cobol
CALL TESTHLDY
```

The program will automatically run all tests and display results.

### CANDAY01.CBLLE - Canada Day Calculator
A specialized program for calculating what day of the week Canada Day (July 1st) falls on for any given year.

**Features:**
- Interactive year input
- Displays day of week for Canada Day
- Provides fun facts about weekend vs. weekday holidays
- Loop capability to check multiple years

**Usage:**
```cobol
CALL CANDAY01
```

### TESTCDAY.CBLLE - Canada Day Test Program
A test framework for the Canada Day calculator (implementation framework provided).

## Compilation

To compile these programs on AS/400:

1. Upload the source to QCBLLESRC source file:
```cl
FTP ${AS400}
// login
ascii
QUOTE SITE NAMEFMT 0
PUT HOLYTRK01.CBLLE ${LIB}/QCBLLESRC.HOLYTRK01 (REPLACE
PUT TESTHLDY.CBLLE ${LIB}/QCBLLESRC.TESTHLDY (REPLACE
PUT CANDAY01.CBLLE ${LIB}/QCBLLESRC.CANDAY01 (REPLACE
PUT TESTCDAY.CBLLE ${LIB}/QCBLLESRC.TESTCDAY (REPLACE
```

2. Compile using CRTBNDCBL:
```cl
CRTBNDCBL PGM(${LIB}/HOLYTRK01) SRCFILE(${LIB}/QCBLLESRC) SRCMBR(HOLYTRK01)
CRTBNDCBL PGM(${LIB}/TESTHLDY) SRCFILE(${LIB}/QCBLLESRC) SRCMBR(TESTHLDY)
CRTBNDCBL PGM(${LIB}/CANDAY01) SRCFILE(${LIB}/QCBLLESRC) SRCMBR(CANDAY01)
CRTBNDCBL PGM(${LIB}/TESTCDAY) SRCFILE(${LIB}/QCBLLESRC) SRCMBR(TESTCDAY)
```

## Holiday Calculation Methods

### Fixed-Date Holidays
These holidays always occur on the same date each year:
- New Year's Day: January 1
- Independence Day: July 4
- Veterans Day: November 11
- Christmas Day: December 25

### Nth Weekday Holidays
These holidays occur on a specific weekday occurrence within a month:
- Martin Luther King Jr. Day: 3rd Monday in January
- Presidents' Day: 3rd Monday in February
- Labor Day: 1st Monday in September
- Columbus Day: 2nd Monday in October
- Thanksgiving Day: 4th Thursday in November

### Last Weekday Holidays
These holidays occur on the last occurrence of a specific weekday:
- Memorial Day: Last Monday in May

## Tax Calculation Use Cases

The day-of-year (Julian day) output is particularly useful for:
- Calculating pro-rated tax amounts
- Determining fiscal year periods
- Planning estimated tax payment deadlines
- Managing payroll processing schedules
- Calculating business days for financial operations

## Technical Details

**Date Range:** 1600-3000 (limited by COBOL INTEGER-OF-DATE function)

**Date Calculations:**
- Uses COBOL intrinsic function `INTEGER-OF-DATE` for accurate date arithmetic
- Accounts for leap years using standard Gregorian calendar rules
- Day of week calculation using modulo 7 arithmetic
- Julian day calculation from January 1st of the year

**Error Handling:**
- Year range validation
- Input validation
- Graceful error messages

## Future Enhancements

Possible additions:
- Add state-specific holidays
- Add religious holidays
- Export to file for batch processing
- Add holiday observance rules (when holiday falls on weekend)
- Add fiscal year calculations
- International holiday support

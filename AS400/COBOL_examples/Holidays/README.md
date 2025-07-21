# Canada Day Calculator

## Overview
The Canada Day Calculator is a COBOL AS400 application that determines what day of the week Canada Day (July 1st) falls on for any given year, both historical and future.

## Programs

### CANDAY01.CBLLE
**Main Canada Day Calculator Program**

#### Features:
- Interactive user interface
- Calculates day of week for July 1st of any year (1600-3000)
- Displays fun facts about the day
- Input validation and error handling
- Continuous operation until user chooses to exit

#### Usage:
1. Run the program
2. Enter a year when prompted (1600-3000)
3. Program displays what day Canada Day falls on
4. Choose to check another year or exit

#### Sample Output:
```
================================================
        CANADA DAY CALCULATOR
    What day does Canada Day fall on?
================================================

Enter a year (1600-3000): 2025

================================================
Canada Day (July 1, 2025) falls on a Tuesday
================================================

Fun Facts:
- Canada Day is on a weekday - enjoy the long weekend!
```

### TESTCDAY.CBLLE
**Test Program**

A simple test program that demonstrates automated testing concepts for the Canada Day calculator.

## Technical Details

### Date Calculation Algorithm
The program uses COBOL intrinsic functions:
- `FUNCTION INTEGER-OF-DATE` - Converts YYYYMMDD to integer
- `FUNCTION MOD` - Calculates day of week using modulo 7

### Key Features:
- **Input Validation**: Ensures year is between 1600-3000
- **Error Handling**: Graceful handling of invalid inputs
- **User-Friendly Interface**: Clear prompts and formatted output
- **Fun Facts**: Additional context about weekend vs weekday celebrations

### Data Structures:
- Day name lookup table for converting numeric day to text
- Formatted date fields for calculations
- Error flags for robust operation

## Historical Context
- **Canada Day**: Celebrates July 1, 1867 - when the British North America Act came into effect
- **Year Range**: 1600-3000 provides comprehensive historical and future coverage
- **Practical Use**: Helps plan celebrations and understand historical context

## Compilation
For AS400 environments:
```
CRTCBLPGM PGM(MYLIB/CANDAY01) SRCFILE(MYLIB/QCBLLESRC)
```

For Open COBOL environments:
```
cobc -x CANDAY01.CBLLE
```

## Dependencies
- No external files or copybooks required
- Uses standard COBOL intrinsic functions
- Self-contained application

## Future Enhancements
- Add other Canadian holidays
- Include provincial holiday calculations
- Export results to file
- Batch processing mode
- Historical events lookup for specific Canada Days

## Testing
Use TESTCDAY.CBLLE as a starting point for automated testing scenarios.

---
**Created**: July 21, 2025  
**Author**: Development Team  
**Version**: 1.0

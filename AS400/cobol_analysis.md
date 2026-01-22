# COBOL Code Analysis Report

**Analysis Date:** 2025-01-21  
**Directories Analyzed:**
- /home/runner/work/Cobol-Demo/Cobol-Demo/AS400/QCBLLESRC
- /home/runner/work/Cobol-Demo/Cobol-Demo/AS400/COBOL_examples

---

## Table of Contents
1. [Executive Summary](#executive-summary)
2. [File Inventory](#file-inventory)
3. [Detailed Program Analysis](#detailed-program-analysis)
4. [Data Structures Analysis](#data-structures-analysis)
5. [Database Operations](#database-operations)
6. [Program Call Relationships](#program-call-relationships)
7. [Technical Observations](#technical-observations)

---

## Executive Summary

This repository contains **25 COBOL programs** across multiple categories:
- **Business Logic Programs**: Inventory management (STAOBR variants)
- **Interactive Programs**: STAPOR (screen-based data entry)
- **Utility Programs**: Logging (LOG0010CB), Data manipulation examples
- **Example/Test Programs**: Move operations, Picture clauses, Array handling
- **Date Calculations**: Canada Day calculator (CANDAY01)

### Key Technologies
- **Platform**: IBM AS/400 (IBM i)
- **COBOL Variant**: ILE COBOL (CBLLE)
- **Database**: DB2 for i (embedded file operations)
- **File Systems**: DDS-described files, indexed and sequential access

---

## File Inventory

### QCBLLESRC Directory (9 files)
| File | Purpose | Complexity |
|------|---------|------------|
| COND1.CBLLE | Conditional logic demonstration | Low |
| DUMP_FULL.CBLLE | Dump structure definitions (copybook) | Low |
| STAOBR.CBLLE | Material inventory update (sequential) | Medium |
| STAOBR2.CBLLE | Material inventory update (variant 2) | Medium |
| STAOBR_2.CBLLE | Inventory update with indexed processing | High |
| STAPOR.CBLLE | Interactive screen-based inventory management | High |
| TABULKY1.CBLLE | Single-dimension array example | Low |
| TABULKY2.CBLLE | Two-dimensional array example | Low |
| staobr0cp.cblle | Inventory update with logging | Medium |

### COBOL_examples Subdirectories (16 files)

#### CallingExample (4 files)
- CALLER.CBLLE - Program caller
- CALLER2.CBLLE - Alternative caller
- HERON.CBLLE - Triangle area calculation (Heron's formula)
- NESTEDCALL.CBLLE - Nested program demonstration

#### Holidays (2 files)
- CANDAY01.CBLLE - Canada Day calculator (interactive)
- TESTCDAY.CBLLE - Test harness for CANDAY01

#### Logging (1 file)
- LOG0010CB.cblle - Application logging utility

#### MoveClause (4 files)
- MOVE1.CBLLE - MOVE CORRESPONDING demonstration
- MOVE2.CBLLE - MOVE operations with data
- SUBSTR1.CBLLE - Substring operations with values
- SUBSTR2.CBLLE - Substring operations dynamic

#### PictureClause (4 files)
- PICTURE1.CBLLE - Numeric picture clauses
- PICTURE2.CBLLE - Binary numeric types
- PICTURE3.CBLLE - Edited numeric pictures with currency
- PICTURE4.CBLLE - Boolean/Indicator usage

---

## Detailed Program Analysis

### 1. COND1.CBLLE
**Purpose:** Demonstrates conditional name (88-level) usage

#### Data Division
```cobol
WORKING-STORAGE SECTION:
- CISLO (Level 77): PIC 9999
  - JEDNA-AZ-99 (88-level): VALUES 1 THRU 99
  - STO (88-level): VALUE 100
  - TISIC (88-level): VALUE 1000
```

#### Procedure Division Logic
- **Paragraph:** PAR
  - Tests condition names with different values
  - Demonstrates IF statements with 88-level conditions
  - Contains commented-out CALL to QlnDumpCobol

#### Key Features
- Simple conditional testing
- No file operations
- No external program calls (commented out)

---

### 2. DUMP_FULL.CBLLE
**Purpose:** Copybook defining error handling structures for system dump calls

#### Data Structures
```cobol
01 ERROR-CODE (Group)
   - BYTES-PROVIDED: PIC S9(6) BINARY (VALUE 100)
   - BYTES-AVAILABLE: PIC S9(6) BINARY (VALUE ZERO)
   - EXCEPTION-ID: PIC X(7)
   - RESERVED-X: PIC X
   - EXCEPTION-DATA: PIC X(64)

01 PROGRAM-NAME: PIC X(10)
01 LIBRARY-NAME: PIC X(10)
01 MODULE-NAME: PIC X(10)
01 PROGRAM-TYPE: PIC X(10) (VALUE "*PGM")
01 DUMP-TYPE: PIC X (VALUE "F")
```

#### Usage
- Included via COPY statement in multiple programs
- Used for QlnDumpCobol API calls (IBM i system service)

---

### 3. STAOBR.CBLLE
**Purpose:** Updates material quantities in STAVY (inventory) file based on OBRATY (transactions) file

#### Environment Division
```cobol
FILE-CONTROL:
- STAVY: DATABASE-STAVY
  - Access: DYNAMIC
  - Organization: INDEXED
  - Key: EXTERNALLY-DESCRIBED-KEY
  
- OBRATY: DATABASE-OBRATY
  - Access: SEQUENTIAL
  - Organization: SEQUENTIAL
```

#### Data Division
```cobol
FILE SECTION:
FD STAVY
   01 STAVYR
      - COPY DDS-ALL-FORMATS OF STAVY
      - Fields: ZAVOD, SKLAD, MATER, MNOZ (via DDS)

FD OBRATY
   01 OBRATY-RECORD
      - COPY DDS-ALL-FORMATS OF OBRATY
      - Fields: ZAVOD, SKLAD, MATER, MNOBR (via DDS)

WORKING-STORAGE:
01 INPUT-END: PIC X
   88 END-FILE: VALUE "E"
```

#### Procedure Division Logic
**Section:** HLAVNI-SEKCE
1. **File Operations:**
   - OPEN I-O STAVY (Update mode)
   - OPEN INPUT OBRATY (Read-only)

2. **Processing Loop:**
   ```cobol
   PERFORM UNTIL END-FILE
     - Move key fields from OBRATY to STAVY (ZAVOD, SKLAD, MATER)
     - READ STAVY with key
       - INVALID KEY: Continue (record not found)
       - NOT INVALID KEY: 
         - ADD MNOBR to MNOZ
         - REWRITE STAVYR
     - READ next OBRATY record
   END-PERFORM
   ```

3. **Cleanup:**
   - CLOSE OBRATY
   - CLOSE STAVY

#### File Linkage
- **Input:** OBRATY (transaction records)
- **Update:** STAVY (inventory master)
- **Logic:** Sequential read of transactions, random update of inventory

---

### 4. STAOBR2.CBLLE
**Purpose:** Variant of STAOBR with RANDOM access mode

#### Key Differences from STAOBR
```cobol
FILE-CONTROL:
- STAVY: ACCESS IS RANDOM (instead of DYNAMIC)
```

#### Data Division
```cobol
WORKING-STORAGE:
01 INPUT-END: PIC X
   (88-level condition name commented out)
```

#### Procedure Division Changes
- Uses MOVE CORR (CORRESPONDING) instead of individual field moves
- Line 54: `MOVE CORR OBRATYF0 TO STAVYF0`
- Simplified field mapping between structures

#### Processing Logic
- Similar to STAOBR but optimized for random access patterns
- Uses corresponding move for cleaner code

---

### 5. STAOBR_2.CBLLE
**Purpose:** Advanced inventory update with reversed processing order (process STAVY first, then match OBRATY)

#### Environment Division
```cobol
FILE-CONTROL:
- STAVY: ACCESS DYNAMIC, INDEXED, EXTERNALLY-DESCRIBED-KEY
- OBRATY: ACCESS DYNAMIC, INDEXED, WITH DUPLICATES
```

#### Data Division
```cobol
WORKING-STORAGE:
01 END-OD-STAVY: PIC X (VALUE SPACE)
   88 EOF-STAVY: VALUE "E"
   88 NOT-EOF-STAVY: VALUE " "
   
01 END-OF-OBRATY: PIC X (VALUE SPACE)
   88 EOF-OBRATY: VALUE "F"
   88 NOT-EOF-OBRAY: VALUE " " (typo: OBRAY)
   
01 KEY-STAVY (Group)
   05 ZAVOD: LIKE ZAVOD OF STAVYF0
   05 SKLAD: LIKE SKLAD OF STAVYF0
   05 MATER: LIKE MATER OF STAVYF0
   
01 KEY-OBRATY: LIKE KEY-STAVY (but redefined)
   05 ZAVOD: LIKE ZAVOD OF OBRATYF0
   05 SKLAD: LIKE SKLAD OF OBRATYF0
   05 MATER: LIKE MATER OF OBRATYF0
```

#### Procedure Division Logic
**Main Loop:**
```cobol
PERFORM UNTIL EOF-STAVY
  1. READ STAVY FIRST/NEXT
  2. Move key from STAVY to KEY-STAVY
  3. Move KEY-STAVY to OBRATY key fields
  4. START OBRATY KEY IS EQUAL TO KEY-STAVY
  5. PERFORM PROCESS-OBRATY (section)
  6. READ STAVY NEXT
END-PERFORM
```

**PROCESS-OBRATY Section:**
```cobol
READ OBRATY NEXT
PERFORM UNTIL EOF-OBRATY
  - Move OBRATY key to KEY-OBRATY
  - IF KEY-OBRATY = KEY-STAVY:
      ADD MNOBR to MNOZ
    ELSE:
      REWRITE STAVYR
      GO TO END-LOOP
  - READ OBRATY NEXT
END-PERFORM
END-LOOP (label)
```

#### Key Features
- Processes inventory records sequentially
- For each inventory record, processes all matching transactions
- Uses START verb for positioning in OBRATY file
- Handles duplicate keys in OBRATY
- More efficient when transactions are grouped by key

---

### 6. STAPOR.CBLLE
**Purpose:** Interactive workstation program for inventory maintenance (Add/Update/Delete)

#### Environment Division
```cobol
FILE-CONTROL:
- STAVYW: WORKSTATION-STAVYW
  - Access: SEQUENTIAL
  - Organization: TRANSACTION
  
- STAVY: DATABASE-STAVY
  - Access: DYNAMIC
  - Organization: INDEXED
  - Key: EXTERNALLY-DESCRIBED-KEY
```

#### Data Division
```cobol
FILE SECTION:
FD STAVYW (Display file)
   01 FORMATY
      - COPY DDS-ALL-FORMATS OF STAVYW
      - Screen formats: STAVYW1, STAVYW2

FD STAVY (Physical file)
   01 STAVY-RECORD
      - COPY DDS-STAVYF0 OF STAVY

WORKING-STORAGE:
01 IND-NENALEZENI: PIC X(1)
   88 NENALEZEN: VALUE "1"
   88 NALEZEN: VALUE "0"
   
01 INDIKATORY-OBRAZOVKY (Group)
   05 IN03: PIC 1 INDIC 03
      88 KONEC: VALUE B"1"
   05 IN12: PIC 1 INDIC 12
      88 NAVRAT: VALUE B"1"
   05 IN05: PIC 1 INDIC 05
      88 OBNOVA: VALUE B"1"
   05 IN23: PIC 1 INDIC 23
      88 ZRUSIT: VALUE B"1"
```

#### Procedure Division Flow

**UVODNI-AKCE:**
- OPEN I-O STAVYW
- OPEN I-O STAVY
- INITIALIZE indicators

**ZOBRAZIT-ZADANI:**
- Display first screen (STAVYW1) for key input
- READ screen input
- Check function keys (F3=Exit, F12=Cancel)
- Move screen fields to file key
- READ STAVY (search by key)
- Prepare second screen (STAVYW2) with data or blank

**ZNOVU-DRUHA:**
- Display/read second screen (STAVYW2) for data entry
- Handle function keys:
  - F5 (OBNOVA): Refresh display
  - F12 (NAVRAT): Return to first screen
  - F23 (ZRUSIT): Delete record
- Process based on record status:
  - New record (NENALEZEN): WRITE
  - Existing record: REWRITE or DELETE
- Return to ZOBRAZIT-ZADANI

**KONEC-PROGRAMU:**
- CLOSE files

#### Key Features
- Interactive screen handling with indicators
- CRUD operations (Create, Read, Update, Delete)
- Function key processing
- GO TO statements for flow control (older style)
- Two-screen interface pattern

---

### 7. TABULKY1.CBLLE
**Purpose:** Demonstrates single-dimension array (table) processing

#### Data Division
```cobol
WORKING-STORAGE:
77 IDX: PIC 999

01 STRUCT (Group)
   05 TEXT1: PIC X(5) VALUE "ABCDE"
   05 ARRAY REDEFINES TEXT1 OCCURS 5
      08 CHAR: PIC X
```

#### Procedure Division Logic
```cobol
PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5
   DISPLAY "ARRAY :" CHAR IN ARRAY(IDX)
END-PERFORM
```

#### Key Features
- REDEFINES to create array overlay
- OCCURS clause for array definition
- PERFORM VARYING for loop iteration
- Indexed array access

---

### 8. TABULKY2.CBLLE
**Purpose:** Demonstrates two-dimensional array processing

#### Data Division
```cobol
WORKING-STORAGE:
77 IDX1: PIC 999
77 IDX2: PIC 999

01 STRUCT (Group)
   10 TAB OCCURS 2
      20 TAB2 OCCURS 3
         30 NUM: PIC 999 VALUE ZERO
```

#### Procedure Division Logic
```cobol
PERFORM VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > 2
   PERFORM VARYING IDX2 FROM 1 BY 1 UNTIL IDX2 > 3
      COMPUTE NUM(IDX1 IDX2) = NUM(IDX1 IDX2) + IDX1 + IDX2
      DISPLAY "ARRAY NUM (" IDX1 ", " IDX2 "): " NUM(IDX1, IDX2)
   END-PERFORM
END-PERFORM
```

#### Key Features
- Nested OCCURS (2x3 array)
- Nested PERFORM loops
- Two-dimensional array indexing syntax variations
- COMPUTE statement for arithmetic

---

### 9. staobr0cp.cblle
**Purpose:** Production-quality inventory update program with error handling and logging

#### Program Identification
```cobol
PROGRAM-ID: STAOBR0CP
AUTHOR: Malanius & Co.
```

#### Environment Division
```cobol
FILE-CONTROL:
- STAVYP0: DATABASE-STAVYP0
  - Access: DYNAMIC
  - Organization: INDEXED
  - Key: EXTERNALLY-DESCRIBED-KEY
  - File Status: STAVYSTAV
  
- OBRATYP0: DATABASE-OBRATYP0
  - Access: SEQUENTIAL
  - Organization: SEQUENTIAL
  - File Status: OBRATYSTAV
```

#### Data Division
```cobol
WORKING-STORAGE:
01 LOGMSG: PIC X(40)
01 LINESUPD: PIC 9(4) VALUE ZERO
01 LINESADD: PIC 9(4) VALUE ZERO
01 STAVYSTAV: PIC XX (file status)
01 OBRATYSTAV: PIC XX (file status)
01 INPUT-END: PIC X
   88 END-FILE: VALUE "E"
```

#### Procedure Division Logic

**Initialization:**
```cobol
CALL "LOG0010CB" USING LOGMSG (log start)
OPEN I-O STAVYP0
OPEN INPUT OBRATYP0
IF file-status NOT = "00"
   Log error and GOBACK
```

**Main Processing:**
```cobol
READ OBRATYP0 (check EOF)
PERFORM UNTIL END-FILE
   1. Set key from OBRATY to STAVY
   2. READ STAVYP0
      INVALID KEY:
         - Log "New entry found"
         - MOVE MNOZ from OBRATY to STAVY
         - WRITE new record
         - Increment LINESADD counter
      NOT INVALID KEY:
         - Log "Updating record"
         - ADD MNOZ from OBRATY to STAVY
         - REWRITE record
         - Increment LINESUPD counter
   3. READ next OBRATY
END-PERFORM
```

**Cleanup:**
```cobol
CLOSE files
Log completion message
Log lines updated count
Log lines added count
GOBACK
```

#### External Calls
- **LOG0010CB**: Called 7 times for logging operations

#### Key Features
- File status checking
- Error logging
- Transaction counting
- Both INSERT and UPDATE operations
- Professional code structure with comments

---

### 10. CALLER.CBLLE & CALLER2.CBLLE
**Purpose:** Demonstrates external program calls

#### Data Division
```cobol
WORKING-STORAGE:
01 A: PACKED-DECIMAL PIC S9(10)V9(5) VALUE 19.0
01 B: PACKED-DECIMAL PIC S9(10)V9(5) VALUE 10.0
01 C: PACKED-DECIMAL PIC S9(10)V9(5) VALUE 10.0
01 AREA: PACKED-DECIMAL PIC S9(10)V9(5)
77 AREA-DISP: PIC Z(10).9(5)
```

#### Procedure Division
```cobol
MAIN-SECTION:
   CALL PROGRAM "HERON" USING A B C AREA
   MOVE AREA TO AREA-DISP
   DISPLAY "AREA = " PLOCHA-DISPLAYED (error: variable name mismatch)
   GOBACK
```

#### Issues
- Variable name error: PLOCHA-DISPLAYED not defined (should be AREA-DISP)
- Both CALLER and CALLER2 are identical

---

### 11. HERON.CBLLE
**Purpose:** Calculates triangle area using Heron's formula

#### Data Division
```cobol
WORKING-STORAGE:
01 S: PACKED-DECIMAL PIC S9(10)V9(5)
01 TEMP: PACKED-DECIMAL PIC S9(10)V9(5)

LINKAGE SECTION:
01 A: PACKED-DECIMAL PIC S9(10)V9(5)
01 B: PACKED-DECIMAL PIC S9(10)V9(5)
01 C: PACKED-DECIMAL PIC S9(10)V9(5)
01 AREA: PACKED-DECIMAL PIC S9(10)V9(5)
```

#### Procedure Division
```cobol
USING A B C AREA

MAIN-SECTION:
   IF A >= B + C OR B >= A + C OR C >= A + B
      MOVE -1.0 TO AREA (invalid triangle)
      GOBACK
   END-IF
   
   COMPUTE S = (A + B + C) / 2
   COMPUTE TEMP = S * (S - A) * (S - B) * (S - C)
   COMPUTE AREA = FUNCTION SQRT(TEMP)
   GOBACK
```

#### Key Features
- LINKAGE SECTION for parameters
- Triangle inequality validation
- Mathematical intrinsic function (SQRT)
- COMPUTE statements for complex calculations

---

### 12. NESTEDCALL.CBLLE
**Purpose:** Demonstrates nested program structure (program within program)

#### Structure
```
Outer Program: NESTEDCALL
   - Calls nested HERON
   
Nested Program: HERON
   - Performs calculation
```

#### Data Division (Outer)
```cobol
WORKING-STORAGE:
01 A: PACKED-DECIMAL PIC S9(10)V9(5) VALUE 3.0
01 B: PACKED-DECIMAL PIC S9(10)V9(5) VALUE 4.0
01 C: PACKED-DECIMAL PIC S9(10)V9(5) VALUE 5.0
01 AREA: PACKED-DECIMAL PIC S9(10)V9(5)
77 AREA-DISP: PIC Z(10).9(5)-.
```

#### Procedure Division (Outer)
```cobol
CALL "HERON" USING A B C AREA
MOVE PLOCHA TO PLOCHA-DISPLAYED (error: undefined variables)
DISPLAY "PLOCHA = " PLOCHA-DISPLAYED
```

#### Nested Program
- Same as standalone HERON.CBLLE
- Nested within same source member

#### Issues
- Variable name errors (PLOCHA undefined)
- Should use AREA and AREA-DISP

---

### 13. CANDAY01.CBLLE
**Purpose:** Interactive Canada Day calculator - determines day of week for July 1st

#### Program Identification
```cobol
PROGRAM-ID: CANDAY01
AUTHOR: Development Team
DATE-WRITTEN: July 21, 2025
```

#### Configuration
```cobol
SPECIAL-NAMES:
   DECIMAL-POINT IS COMMA
```

#### Data Division
```cobol
WORKING-STORAGE:
01 WS-INPUT-YEAR: PIC 9(4) VALUE ZEROS
01 WS-INPUT-YEAR-DISPLAY: PIC Z(4)

01 WS-CANADA-DATE (Group)
   05 WS-YEAR: PIC 9(4)
   05 WS-MONTH: PIC 9(2) VALUE 07
   05 WS-DAY: PIC 9(2) VALUE 01

01 WS-DAY-OF-WEEK: PIC 9(1)
01 WS-DAY-NAME: PIC X(9)
01 WS-DATE-NUMERIC: PIC 9(8)
01 WS-DATE-FORMATTED: PIC X(10)

01 WS-DAY-TABLE (array of day names)
   - Sunday through Saturday (7 entries)
   
01 WS-DAY-NAMES REDEFINES WS-DAY-TABLE
   05 WS-DAY-ENTRY: PIC X(9) OCCURS 7

01 WS-ERROR-FLAG: PIC X(1) VALUE 'N'
01 WS-CONTINUE-FLAG: PIC X(1) VALUE 'Y'
01 WS-DISPLAY-LINE: PIC X(80)
```

#### Procedure Division Structure

**MAIN-PROCEDURE:**
```cobol
Display banner
PERFORM UNTIL WS-CONTINUE-FLAG = 'N'
   PERFORM GET-YEAR-INPUT
   IF no error
      PERFORM CALCULATE-DAY-OF-WEEK
      PERFORM DISPLAY-RESULT
   PERFORM ASK-CONTINUE
END-PERFORM
```

**GET-YEAR-INPUT Section:**
- Accept year (1600-3000 validation)
- Set error flag if out of range

**CALCULATE-DAY-OF-WEEK Section:**
```cobol
Build date in YYYYMMDD format
COMPUTE WS-DAY-OF-WEEK = 
   FUNCTION MOD(
      FUNCTION INTEGER-OF-DATE(WS-DATE-NUMERIC), 7) + 1
Adjust for table index (1=Sunday)
Get day name from array
```

**DISPLAY-RESULT Section:**
- Format and display result using STRING
- Display "Fun Facts" based on day of week
- Weekend vs weekday messages
- Special messages for Monday/Friday

**ASK-CONTINUE Section:**
- Accept Y/N for continuing
- Normalize to uppercase

#### Key Features
- Date intrinsic functions (INTEGER-OF-DATE, MOD)
- Input validation
- Array-based lookup table
- STRING statement for formatting
- Interactive loop with user control
- Business logic (weekend detection)

---

### 14. TESTCDAY.CBLLE
**Purpose:** Automated test program for CANDAY01

#### Data Division
```cobol
WORKING-STORAGE:
01 WS-TEST-YEARS (initialized array)
   - 2024, 2025, 2026, 1867, 2030
   
01 WS-TEST-ARRAY REDEFINES WS-TEST-YEARS
   05 WS-TEST-YEAR: PIC 9(4) OCCURS 5

01 WS-INDEX: PIC 9(2) VALUE 1
```

#### Procedure Division
```cobol
MAIN-PROCEDURE:
   Display test banner
   PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 5
      PERFORM TEST-YEAR
   END-PERFORM
   Display completion message

TEST-YEAR Section:
   Display test year
   Display placeholder message
   (Actual call to CANDAY01 not implemented)
```

#### Key Features
- Test framework skeleton
- PERFORM VARYING loop
- Array iteration
- Placeholder for actual testing logic

---

### 15. LOG0010CB.cblle
**Purpose:** Centralized application logging utility

#### Program Identification
```cobol
PROGRAM-ID: LOG0010CB
AUTHOR: Malanius & Co.
```

#### Environment Division
```cobol
FILE-CONTROL:
- LOGP0: DATABASE-LOGP0
  - Access: SEQUENTIAL
  - Organization: SEQUENTIAL
  - File Status: logstat
```

#### Data Division
```cobol
FILE SECTION:
FD LOGP0
   01 LOG-RECORD
      - COPY DDS-ALL-FORMATS OF LOGP0
      - Fields: xdate, xtime, xjobname, xuser, xjobnum, xtext

LINKAGE SECTION:
01 msgtext: PIC X(40)

WORKING-STORAGE:
01 logstat: PIC XX

01 logline (Group)
   05 xdate: PIC X(8)
   05 xtime: PIC X(8)
   05 xjob: PIC X(10)
   05 xuser: PIC X(10)
   05 xjobnum: PIC X(6)
```

#### Procedure Division
```cobol
USING msgtext

1. CALL "getjoba1cl" USING xjob, xuser, xjobnum
   (Get job attributes from CL program)

2. ACCEPT xdate FROM DATE YYYYMMDD

3. ACCEPT xtime FROM TIME

4. OPEN EXTEND LOGP0

5. IF logstat = "00"
      Move log fields to record
      WRITE LOG-RECORD
   ELSE
      DISPLAY error message

6. CLOSE LOGP0

7. GOBACK
```

#### External Calls
- **getjoba1cl**: CL program to retrieve job information

#### Key Features
- EXTEND mode for appending to log
- System date/time capture (ACCEPT FROM)
- Job context tracking
- File status checking
- Called by multiple programs (staobr0cp)

---

### 16. MOVE1.CBLLE
**Purpose:** Demonstrates MOVE CORRESPONDING with initialized values

#### Data Division
```cobol
WORKING-STORAGE:
01 STRUCT_01 (Group)
   02 CHAR_01: PIC X(10) VALUE "ABCD"
   02 CHAR_02: PIC X(10) VALUE "ABCD"
   02 NUM_03: PIC S9(6) DISPLAY VALUE 123456
   02 NUM_04: PIC S9(3)V9(3) DISPLAY VALUE 777.888

01 TARGET_01 (Group - different field order)
   02 NUM_04: PIC S9(3)V9(3)
   02 CHAR_02: PIC X(10)
   02 NUM_03: PIC S9(6)
   02 CHAR_01: PIC X(10)

01 TARGET_02 (same structure as TARGET_01)
01 TARGET_03 (same field names as STRUCT_01)
```

#### Procedure Division Logic
```cobol
DISPLAY STRUCT_01

MOVE CORRESPONDING STRUCT_01 TO TARGET_02
DISPLAY TARGET_02 (shows fields matched by name)

MOVE CHAR_01 to CHAR_02 in TARGET_03
DISPLAY result (cross-field assignment)

MOVE NUM_03 to CHAR_01 in TARGET_03
DISPLAY result (numeric to alphanumeric)

MOVE NUM_03 to NUM_04 in TARGET_03
DISPLAY result (numeric size conversion)

MOVE CHAR_01 to NUM_04 in TARGET_03
DISPLAY result (alphanumeric to numeric - possible error)
```

#### Key Features
- MOVE CORRESPONDING (CORR) operation
- Field name matching across structures
- Cross-type conversions (numeric/alphanumeric)
- Demonstration of data truncation/padding

---

### 17. MOVE2.CBLLE
**Purpose:** Demonstrates MOVE operations with runtime data

#### Data Division
```cobol
WORKING-STORAGE:
(Same structure as MOVE1, but no VALUE clauses)
```

#### Procedure Division Logic
```cobol
MOVE "HELL" TO CHAR_01 in STRUCT_01
MOVE "HELLO" TO CHAR_02 in STRUCT_01
MOVE 6666 TO NUM_03 in STRUCT_01
MOVE 888.777 TO NUM_04 in STRUCT_01

DISPLAY STRUCT_01

(Same move operations as MOVE1)
```

#### Differences from MOVE1
- Data assigned at runtime vs compile-time
- No VALUE clauses in declarations
- Same move logic patterns

---

### 18. SUBSTR1.CBLLE
**Purpose:** Demonstrates substring (reference modification) operations with values

#### Data Division
```cobol
WORKING-STORAGE:
77 OPERAND-1: PIC X(10) VALUE "ABCDEFGHIJK"
77 OPERAND-2: PIC X(5) VALUE "12345"

77 POS-1: PIC 99 VALUE 5
77 POS-2: LIKE POS-1 VALUE 1

77 LEN-1: PIC 99 VALUE 2
77 LEN-2: LIKE LEN-1 VALUE 4
```

#### Procedure Division Logic
```cobol
DISPLAY OPERAND-1
DISPLAY OPERAND-2

MOVE OPERAND-1(POS-1 : LEN-1) TO OPERAND-2(POS-2 : LEN-2)
   Moves: OPERAND-1(5:2) = "EF"
   To: OPERAND-2(1:4) = "EF  " (padded)

DISPLAY OPERAND-2
```

#### Key Features
- Reference modification syntax: variable(start:length)
- Substring extraction and assignment
- Dynamic positioning and length
- LIKE clause for field definition

---

### 19. SUBSTR2.CBLLE
**Purpose:** Demonstrates substring operations without initial values

#### Data Division
```cobol
WORKING-STORAGE:
77 OPERAND-1: PIC X(10) (no value)
77 OPERAND-2: PIC X(5) (no value)

(Same position/length variables as SUBSTR1)
```

#### Procedure Division Logic
```cobol
DISPLAY OPERAND-1 (shows spaces)

MOVE "ABCDEF" TO OPERAND-1

DISPLAY OPERAND-2 (shows spaces)
MOVE "HELLO" TO OPERAND-2

MOVE OPERAND-1(POS-1 : LEN-1) TO OPERAND-2(POS-2 : LEN-2)
DISPLAY OPERAND-2
```

#### Differences from SUBSTR1
- Values assigned at runtime
- Shows initial state (spaces)

---

### 20. PICTURE1.CBLLE
**Purpose:** Demonstrates numeric PICTURE clause usage and formats

#### Configuration
```cobol
PROCESS OPTIONS NOMONOPRC
```

#### Data Division
```cobol
WORKING-STORAGE:
COPY DUMP_FULL (error handling structures)

77 NUMBER: PIC 99999V99
77 NUMBER_DISPLAY: PIC 99999V99 USAGE DISPLAY
77 NUMBER_COMP-3: PIC 99999V99 USAGE COMP-3
77 NUMBER_S: PIC S9(5)V9(2) DISPLAY
77 NUMBER_S_PACK: PIC S99999V99 PACKED-DECIMAL
77 NUMBER_P_UP: PIC 9999999PP PACKED-DECIMAL
77 NUMBER_P_DWN: PIC SPP9999999
```

#### Procedure Division Logic
```cobol
PAR:
   MOVE 12345.67 TO NUMBER
   MOVE 12345.67 TO NUMBER_DISP (typo: should be NUMBER_DISPLAY)
   MOVE 12345.67 TO NUMBER_COMP-3
   MOVE -12345.67 TO NUMBER_S
   MOVE -12345.6 TO NUMBER_S_PACK
   MOVE 12345.67 TO NUMBER_P_UP (scales up with PP)
   MOVE .001234567 TO NUMBER_P_DWN (scales down with PP)
   
   CALL "QlnDumpCobol" USING OMITTED, OMITTED, OMITTED,
        PROGRAM-TYPE, DUMP-TYPE, ERROR-CODE
```

#### Key Features
- V (implied decimal point)
- P (decimal scaling)
- S (signed)
- DISPLAY, COMP-3, PACKED-DECIMAL usage
- Calls dump utility to examine memory layout

#### Issues
- Variable name typo: NUMBER_DISP vs NUMBER_DISPLAY

---

### 21. PICTURE2.CBLLE
**Purpose:** Demonstrates binary numeric types

#### Configuration
```cobol
PROCESS OPTIONS NOMONOPRC NOSTDTRUNC
```

#### Data Division
```cobol
WORKING-STORAGE:
COPY DUMP_FULL

77 NUMBER_B2: PIC 9(4) BINARY (2-byte integer)
77 NUMBER_B4: PIC S9(9) BINARY (4-byte integer)
77 NUMBER_B8: PIC 9(18) COMP-4 (8-byte integer)
```

#### Procedure Division Logic
```cobol
PAR:
   MOVE 32767 TO NUMBER_B2 (max 2-byte signed)
   MOVE 2147483647 TO NUMBER_B4 (max 4-byte signed)
   MOVE 92233720368547758007 TO NUMBER_B8 (large value)
   
   CALL "QlnDumpCobol" (dump memory)
```

#### Key Features
- BINARY / COMP-4 usage
- Different integer sizes (2/4/8 bytes)
- Maximum value examples
- Memory layout inspection

---

### 22. PICTURE3.CBLLE
**Purpose:** Demonstrates edited numeric pictures with currency symbols

#### Configuration
```cobol
PROCESS OPTIONS NOMONOPRC NOSTDTRUNC

SPECIAL-NAMES:
   CURRENCY SIGN IS "Kč" WITH PICTURE SYMBOL "K"
```

#### Data Division
```cobol
WORKING-STORAGE:
COPY DUMP_FULL

77 NUMBER_ED1: PIC 9999.9 DISPLAY
77 NUMBER_ED2: PIC KBZZZ99.99-.
77 NUMBER_ED3: PIC ZZZZ.9-.
```

#### Procedure Division Logic
```cobol
PAR:
   MOVE -123.4 TO NUMBER_ED1
   MOVE -12.34 TO NUMBER_ED2 (currency + sign)
   MOVE -123.4 TO NUMBER_ED3 (zero suppression + sign)
   
   CALL "QlnDumpCobol" (dump formatted values)
```

#### Key Features
- K (currency symbol - Czech Koruna)
- B (blank insertion)
- Z (zero suppression)
- . (decimal point)
- - (trailing sign)
- Custom currency symbol definition

---

### 23. PICTURE4.CBLLE
**Purpose:** Demonstrates boolean/indicator usage

#### Configuration
```cobol
PROCESS OPTIONS NOMONOPRC NOSTDTRUNC
```

#### Data Division
```cobol
WORKING-STORAGE:
COPY DUMP_FULL

77 BOOL-1: PIC 1 INDICATOR 61 VALUE B"0" USAGE DISPLAY

01 BOOL-ARRAY (Group)
   08 BOOL-2 OCCURS 10: PIC 1 VALUE B"1"
```

#### Procedure Division Logic
```cobol
PAR:
   MOVE B"1" TO BOOL-1 (set indicator on)
   MOVE B"0" TO BOOL-2(2) (set array element off)
   
   CALL "QlnDumpCobol" (dump boolean states)
```

#### Key Features
- PIC 1 for boolean
- INDICATOR clause (links to display indicator)
- B"0" / B"1" boolean literals
- Array of booleans
- OCCURS with boolean fields

---

## Data Structures Analysis

### Common Data Patterns

#### 1. File Keys (Indexed Files)
```cobol
Typical key structure:
- ZAVOD (Plant/Factory): Numeric
- SKLAD (Warehouse): Numeric  
- MATER (Material): Numeric
```

#### 2. Condition Names (88-level)
```cobol
Pattern: Flag field with named conditions
01 flag-field: PIC X
   88 condition-true: VALUE "Y"
   88 condition-false: VALUE "N"
```

#### 3. DDS Integration
```cobol
All database files use:
COPY DDS-ALL-FORMATS OF filename
   or
COPY DDS-format-name OF filename
```

#### 4. Indicator Patterns
```cobol
Display file indicators:
05 INxx: PIC 1 INDIC xx
   88 function-name: VALUE B"1"
```

### Data Type Usage Summary

| Type | Usage | Programs |
|------|-------|----------|
| PIC X | Alphanumeric | All |
| PIC 9 | Unsigned numeric | All numeric programs |
| PIC S9 | Signed numeric | HERON, CANDAY01, MOVE examples |
| PACKED-DECIMAL | Efficient numeric | HERON, CALLER programs |
| BINARY / COMP-4 | Integer | PICTURE2, DUMP_FULL |
| PIC 1 | Boolean/Indicator | PICTURE4, STAPOR |
| OCCURS | Arrays/Tables | TABULKY1, TABULKY2, CANDAY01 |
| REDEFINES | Overlays | TABULKY1, CANDAY01 |

### Group Level Structures

#### Complex Groups (3+ levels):
1. **CANDAY01**: WS-DAY-TABLE (day names array)
2. **TABULKY2**: STRUCT (2D array)
3. **STAPOR**: INDIKATORY-OBRAZOVKY (indicators)
4. **DUMP_FULL**: ERROR-CODE (error structure)

---

## Database Operations

### File Access Patterns

#### 1. Sequential Processing
```
Programs: STAOBR, staobr0cp, LOG0010CB
Pattern:
- OPEN INPUT/EXTEND
- READ...AT END
- Process
- CLOSE
```

#### 2. Random/Dynamic Access
```
Programs: STAOBR, STAPOR, STAOBR_2
Pattern:
- OPEN I-O
- Set key fields
- READ with INVALID KEY
- REWRITE/WRITE/DELETE
- CLOSE
```

#### 3. Mixed Sequential/Random
```
Program: STAOBR_2
Pattern:
- Sequential read of master (STAVY)
- Random positioning of detail (OBRATY)
- START verb for positioning
- Process group
```

### Database Files Identified

| Logical File | Physical File | Access Mode | Operations | Programs |
|--------------|---------------|-------------|------------|----------|
| STAVY | Database | Dynamic/Random | R,U,D,I | STAOBR*, STAPOR |
| OBRATY | Database | Sequential/Dynamic | R | STAOBR* |
| STAVYW | Workstation | Sequential | R,W | STAPOR |
| LOGP0 | Database | Sequential | W | LOG0010CB |
| STAVYP0 | Database | Dynamic | R,U,I | staobr0cp |
| OBRATYP0 | Database | Sequential | R | staobr0cp |

**Legend:** R=Read, U=Update, D=Delete, I=Insert, W=Write

### File Operations Matrix

| Operation | Programs Using |
|-----------|----------------|
| READ | All file-processing programs |
| WRITE | STAPOR, staobr0cp, LOG0010CB |
| REWRITE | STAOBR*, STAPOR |
| DELETE | STAPOR |
| START | STAOBR_2 |
| OPEN I-O | STAOBR*, STAPOR, staobr0cp |
| OPEN INPUT | STAOBR*, staobr0cp |
| OPEN EXTEND | LOG0010CB |

---

## Program Call Relationships

### Call Graph

```
staobr0cp
   └─> LOG0010CB
         └─> getjoba1cl (CL program)

CALLER / CALLER2
   └─> HERON

NESTEDCALL
   └─> HERON (nested)

PICTURE1, PICTURE2, PICTURE3, PICTURE4
   └─> QlnDumpCobol (IBM i system API)

LOG0010CB
   └─> getjoba1cl (CL program)
```

### Program Types

#### 1. Main Programs (Entry Points)
- STAOBR, STAOBR2, STAOBR_2 (batch processing)
- staobr0cp (production batch)
- STAPOR (interactive)
- CANDAY01 (interactive)
- TESTCDAY (test driver)
- CALLER, CALLER2, NESTEDCALL (demo drivers)
- PICTURE1-4, MOVE1-2, SUBSTR1-2, TABULKY1-2 (examples)
- COND1 (example)

#### 2. Called Programs (Subroutines)
- HERON (calculation subroutine)
- LOG0010CB (utility subroutine)

#### 3. External Dependencies
- **getjoba1cl**: CL program for job attributes
- **QlnDumpCobol**: IBM i dump API

### Parameter Passing

#### USING Clause Patterns
```cobol
1. By Reference (default):
   CALL "HERON" USING A B C AREA

2. LINKAGE SECTION:
   PROCEDURE DIVISION USING A B C AREA
   
3. Single Parameter:
   CALL "LOG0010CB" USING LOGMSG
   PROCEDURE DIVISION USING msgtext
```

---

## Technical Observations

### Code Quality

#### Strengths
1. **Consistent Naming**: Hungarian notation in places (WS- prefix)
2. **Documentation**: Header comments in most programs
3. **Error Handling**: File status checking (staobr0cp, LOG0010CB)
4. **Modularization**: Separate logging utility
5. **DDS Integration**: Externally described files reduce maintenance

#### Weaknesses
1. **Variable Name Errors**: 
   - CALLER.CBLLE: PLOCHA-DISPLAYED undefined
   - PICTURE1: NUMBER_DISP vs NUMBER_DISPLAY
   - STAOBR_2: NOT-EOF-OBRAY (typo)
   
2. **GO TO Usage**: STAPOR uses GO TO (older style)

3. **Incomplete Programs**: TESTCDAY has placeholder logic

4. **Mixed Language**: Comments in Czech (STAOBR programs)

5. **No SQL**: All file I/O uses native COBOL (no embedded SQL)

### AS/400 (IBM i) Specific Features

1. **DDS Files**: All files externally described
2. **Workstation Files**: Transaction organization (STAPOR)
3. **Indicators**: Display file indicator integration
4. **CBLLE**: ILE COBOL variant
5. **System APIs**: QlnDumpCobol usage
6. **Mixed Language**: CL program calls (getjoba1cl)

### Processing Patterns

#### 1. Batch Update Pattern
```
Used in: STAOBR variants, staobr0cp
- Read transaction file sequentially
- Update master file randomly
- Count operations
- Error handling
```

#### 2. Interactive CRUD Pattern
```
Used in: STAPOR
- Display input screen
- Read record
- Display detail screen
- Process user action
- Loop
```

#### 3. Calculation Service Pattern
```
Used in: HERON
- Accept parameters via LINKAGE
- Validate inputs
- Calculate result
- Return via parameter
```

### Performance Considerations

1. **File Access**:
   - STAOBR: Dynamic access is flexible but slower than dedicated random
   - STAOBR2: Random access specified but might not match usage pattern
   - STAOBR_2: Reverse processing order might reduce I/O for grouped transactions

2. **Data Types**:
   - PACKED-DECIMAL used appropriately for calculations
   - BINARY for performance in PICTURE2
   - DISPLAY for human-readable in logs

### Modernization Opportunities

1. **Replace GO TO**: STAPOR could use EVALUATE or structured logic
2. **Add SQL**: Consider embedded SQL for complex queries
3. **Error Handling**: Standardize file status checking
4. **Logging**: Expand LOG0010CB with severity levels
5. **Testing**: Complete TESTCDAY implementation
6. **Variable Names**: Fix typos and use consistent conventions
7. **Internationalization**: Separate Czech comments/messages

### Security Observations

1. **No Password Fields**: No sensitive data handling observed
2. **File Security**: Relies on OS-level security
3. **Audit Trail**: LOG0010CB provides basic audit capability
4. **User Tracking**: Job user captured in logs

---

## Appendix: Quick Reference

### File by Purpose

**Production Business Logic:**
- STAOBR.CBLLE, STAOBR2.CBLLE, STAOBR_2.CBLLE, staobr0cp.cblle

**Interactive Programs:**
- STAPOR.CBLLE, CANDAY01.CBLLE

**Utilities:**
- LOG0010CB.cblle

**Libraries/Copybooks:**
- DUMP_FULL.CBLLE

**Examples/Training:**
- All PICTURE*.CBLLE, MOVE*.CBLLE, SUBSTR*.CBLLE, TABULKY*.CBLLE
- COND1.CBLLE, CALLER*.CBLLE, HERON.CBLLE, NESTEDCALL.CBLLE
- TESTCDAY.CBLLE

### Programs by Complexity

**High Complexity:**
- STAPOR.CBLLE (interactive, CRUD, indicators)
- STAOBR_2.CBLLE (complex file positioning)
- CANDAY01.CBLLE (interactive, calculations)

**Medium Complexity:**
- STAOBR.CBLLE, STAOBR2.CBLLE (file processing)
- staobr0cp.cblle (with logging)
- LOG0010CB.cblle (system integration)

**Low Complexity:**
- All example programs
- HERON.CBLLE (calculation only)
- TESTCDAY.CBLLE (skeleton)

---

## Variables Reference

### Level Structures Found

| Level | Purpose | Examples |
|-------|---------|----------|
| 01 | Group items, records | STRUCT_01, ERROR-CODE, WS-CANADA-DATE |
| 02-05 | Sub-fields | CHAR_01, NUM_03, WS-YEAR |
| 77 | Independent items | CISLO, IDX, NUMBER |
| 88 | Condition names | END-FILE, KONEC, EOF-STAVY |

### Picture Clause Catalog

| Picture | Type | Example Values | Programs |
|---------|------|----------------|----------|
| X(n) | Alphanumeric | "HELLO" | All |
| 9(n) | Numeric | 12345 | All numeric |
| S9(n) | Signed numeric | -123 | HERON, MOVE1 |
| 9(n)V9(m) | Decimal | 123.45 | PICTURE1, HERON |
| Z(n) | Zero suppressed | "  123" | CANDAY01, PICTURE3 |
| KBZZZ99.99- | Edited currency | "Kč 12.34-" | PICTURE3 |
| 1 | Boolean | B"1" | PICTURE4, STAPOR |

### Variables by Type

#### Counters/Indexes:
- IDX, IDX1, IDX2 (TABULKY programs)
- LINESUPD, LINESADD (staobr0cp)
- WS-INDEX (TESTCDAY)

#### Flags/Indicators:
- INPUT-END, END-FILE (STAOBR programs)
- WS-ERROR-FLAG, WS-CONTINUE-FLAG (CANDAY01)
- IND-NENALEZENI (STAPOR)

#### Keys:
- KEY-STAVY, KEY-OBRATY (STAOBR_2)
- ZAVOD, SKLAD, MATER (all STAOBR programs)

#### Messages:
- LOGMSG (staobr0cp, LOG0010CB)
- msgtext (LOG0010CB linkage)
- WS-DISPLAY-LINE (CANDAY01)

---

## Paragraphs and Sections Catalog

### Main Entry Points:
- **PAR** - Simple paragraph (COND1, PICTURE programs)
- **MAIN-SECTION** - Main section (CALLER, HERON)
- **MAIN-PROCEDURE** - Main procedure (CANDAY01, TESTCDAY)
- **HLAVNI-SEKCE** - Main section in Czech (STAOBR)

### Functional Sections:
- **PROCESS-OBRATY** - Process transactions (STAOBR_2)
- **CALCULATE-DAY-OF-WEEK** - Date calculation (CANDAY01)
- **GET-YEAR-INPUT** - Input validation (CANDAY01)
- **DISPLAY-RESULT** - Output formatting (CANDAY01)
- **ASK-CONTINUE** - User prompt (CANDAY01)
- **TEST-YEAR** - Test execution (TESTCDAY)

### Interactive Sections:
- **UVODNI-AKCE** - Initialization (STAPOR)
- **ZOBRAZIT-ZADANI** - Display input screen (STAPOR)
- **ZNOVU-DRUHA** - Second screen loop (STAPOR)
- **KONEC-PROGRAMU** - Cleanup (STAPOR)

---

## Summary Statistics

- **Total Programs Analyzed**: 25
- **Lines of Code**: ~2,500 (approximate)
- **Database Files**: 6 logical files
- **External Calls**: 3 (LOG0010CB, getjoba1cl, QlnDumpCobol)
- **Interactive Programs**: 2 (STAPOR, CANDAY01)
- **Batch Programs**: 4 (STAOBR variants)
- **Utility Programs**: 1 (LOG0010CB)
- **Example Programs**: 17
- **Languages**: COBOL (ILE), CL (called)
- **Platform**: IBM AS/400 (IBM i)

---

**End of Analysis Report**

*Generated by COBOL Analyzer Agent*

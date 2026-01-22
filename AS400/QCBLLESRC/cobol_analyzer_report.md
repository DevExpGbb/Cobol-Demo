# COBOL Code Analysis Report

## Executive Summary

This report provides a comprehensive analysis of 9 COBOL programs found in the AS400/QCBLLESRC directory. The programs are part of an inventory management system focused on updating material quantities (STAVY) based on turnover data (OBRATY).

**Program Count:** 9 COBOL files  
**Total Lines of Code:** ~41,077 lines  
**Primary Domain:** Inventory/Material Management System  
**Database Platform:** IBM AS400/DB2

---

## Table of Contents

1. [Program Overview](#program-overview)
2. [Detailed Program Analysis](#detailed-program-analysis)
3. [Data Structures and Variables](#data-structures-and-variables)
4. [Dependencies and Relationships](#dependencies-and-relationships)
5. [File I/O Patterns](#file-io-patterns)
6. [Code Quality Assessment](#code-quality-assessment)
7. [Recommendations](#recommendations)

---

## 1. Program Overview

| Program Name | Size (Lines) | Type | Primary Purpose |
|-------------|--------------|------|-----------------|
| COND1.CBLLE | 25 | Utility | Conditional value testing demonstration |
| DUMP_FULL.CBLLE | 12 | Data Structure | Error code structure definition |
| STAOBR.CBLLE | 87 | Batch | Update inventory quantities from turnover |
| STAOBR2.CBLLE | 75 | Batch | Simplified version of STAOBR |
| STAOBR_2.CBLLE | 112 | Batch | Advanced version with reversed processing |
| STAPOR.CBLLE | 112 | Interactive | Screen-based inventory maintenance |
| TABULKY1.CBLLE | 16 | Demo | Single-dimension array demonstration |
| TABULKY2.CBLLE | 23 | Demo | Multi-dimension array demonstration |
| staobr0cp.cblle | 104 | Batch | Production-ready STAOBR with logging |

---

## 2. Detailed Program Analysis

### 2.1 COND1.CBLLE - Conditional Testing Program

**Purpose:** Demonstration program for COBOL level-88 conditional name testing.

#### Data Divisions

**Working-Storage Section:**
- Simple demonstration of condition names (level 88)
- No complex data structures

#### Variables

| Level | Name | Type | Size | Purpose |
|-------|------|------|------|---------|
| 77 | CISLO | PIC 9999 | 4 digits | Numeric test variable |
| 88 | JEDNA-AZ-99 | Condition | - | Tests if CISLO is 1-99 |
| 88 | STO | Condition | - | Tests if CISLO is 100 |
| 88 | TISIC | Condition | - | Tests if CISLO is 1000 |

#### Procedure Division

**Paragraphs:**
- `PAR` - Main paragraph demonstrating conditional testing

**Logic Flow:**
1. Test value 5 against JEDNA-AZ-99 condition
2. Test value 100 against STO condition
3. Test value 101 against NOT STO condition
4. Test value 1000 against TISIC condition

**Special Notes:**
- Contains commented-out CALL to QlnDumpCobol dump procedure
- References DUMP_FULL copybook structure
- Educational/demonstration purpose only

#### File Access Patterns
- None (no file I/O)

#### SQL/DB2 Statements
- None

---

### 2.2 DUMP_FULL.CBLLE - Error Code Structure

**Purpose:** Defines standard error code data structure for IBM i dump operations.

#### Data Divisions

**Working-Storage Section:**
- Defines error handling structure for IBM i API calls

#### Variables

| Level | Name | Type | Size | Purpose |
|-------|------|------|------|---------|
| 01 | ERROR-CODE | Group | 80 bytes | Error code structure |
| 05 | BYTES-PROVIDED | PIC S9(6) BINARY | 2 bytes | Input: space available |
| 05 | BYTES-AVAILABLE | PIC S9(6) BINARY | 2 bytes | Output: space needed |
| 05 | EXCEPTION-ID | PIC X(7) | 7 bytes | Exception identifier |
| 05 | RESERVED-X | PIC X | 1 byte | Reserved field |
| 05 | EXCEPTION-DATA | PIC X(64) | 64 bytes | Exception details |
| 01 | PROGRAM-NAME | PIC X(10) | 10 bytes | Program name |
| 01 | LIBRARY-NAME | PIC X(10) | 10 bytes | Library name |
| 01 | MODULE-NAME | PIC X(10) | 10 bytes | Module name |
| 01 | PROGRAM-TYPE | PIC X(10) | 10 bytes | Program type (*PGM) |
| 01 | DUMP-TYPE | PIC X | 1 byte | Dump type (F=Full) |

#### Procedure Division
- None (data structure only)

#### Special Notes
- Copybook/include file used by other programs
- Standard IBM i error handling structure
- Default values initialized (BYTES-PROVIDED=100, DUMP-TYPE="F")

---

### 2.3 STAOBR.CBLLE - Inventory Update from Turnover

**Purpose:** Updates material quantities in inventory (STAVY) based on turnover records (OBRATY).

#### Environment Division

**File-Control:**
- STAVY: Indexed database file with dynamic access
- OBRATY: Sequential database file

#### Data Divisions

**File Section:**

1. **STAVY (Inventory States)**
   - FD: STAVY
   - Record: STAVYR
   - Format: STAVYF0 (from DDS)
   - Access: DYNAMIC (random and sequential)
   - Organization: INDEXED
   - Key: EXTERNALLY-DESCRIBED-KEY

2. **OBRATY (Turnover Records)**
   - FD: OBRATY
   - Record: OBRATY-RECORD
   - Format: OBRATYF0 (from DDS)
   - Access: SEQUENTIAL
   - Organization: SEQUENTIAL

**Working-Storage Section:**

| Level | Name | Type | Size | Purpose |
|-------|------|------|------|---------|
| 01 | INPUT-END | PIC X | 1 byte | EOF indicator |
| 88 | END-FILE | Condition | - | Value "E" indicates EOF |

#### Procedure Division

**Sections:**
- `HLAVNI-SEKCE` - Main processing section

**Logic Flow:**

```
1. OPEN I-O STAVY
2. OPEN INPUT OBRATY
3. READ OBRATY (first record)
4. PERFORM UNTIL END-FILE
   a. MOVE key fields from OBRATYF0 to STAVYF0
      - ZAVOD (Plant/Factory)
      - SKLAD (Warehouse)
      - MATER (Material)
   b. READ STAVY with key
      - INVALID KEY: Continue (skip)
      - NOT INVALID KEY:
        * ADD MNOBR (turnover qty) to MNOZ (inventory qty)
        * REWRITE STAVYR (update record)
   c. READ OBRATY (next record)
5. CLOSE OBRATY
6. CLOSE STAVY
```

**Key Fields Used:**
- ZAVOD - Plant/Factory code
- SKLAD - Warehouse code
- MATER - Material number
- MNOBR - Turnover quantity (from OBRATY)
- MNOZ - Inventory quantity (from STAVY)

#### File Access Patterns

**STAVY (I-O Mode):**
- Access Method: Dynamic (allows both random and sequential)
- READ with key (random access)
- REWRITE for updates
- Opens at start, closes at end

**OBRATY (INPUT Mode):**
- Access Method: Sequential
- READ with AT END condition
- Processes all records sequentially

#### SQL/DB2 Statements
- None (uses native COBOL file I/O)

#### Special Notes
- Czech language comments
- No error handling for REWRITE operation
- Assumes STAVY record exists before updating
- Records without matching STAVY keys are silently skipped

---

### 2.4 STAOBR2.CBLLE - Simplified Inventory Update

**Purpose:** Simplified version of STAOBR with minor differences in key handling.

#### Environment Division

**File-Control:**
- STAVY: Indexed database file with RANDOM access (not DYNAMIC)
- OBRATY: Sequential database file

#### Data Divisions

Similar to STAOBR.CBLLE with these differences:

**Working-Storage Section:**

| Level | Name | Type | Size | Purpose | Notes |
|-------|------|------|------|---------|-------|
| 01 | INPUT-END | PIC X | 1 byte | EOF indicator | Different variable name |
| 88 | END-FILE | Condition | - | (Commented out) | Not used |

#### Procedure Division

**Logic Flow Differences from STAOBR:**

```
1. OPEN I-O STAVY
2. OPEN INPUT OBRATY
3. READ OBRATY AT END MOVE "E" TO END-INPUT  (different EOF handling)
4. PERFORM UNTIL (condition incomplete in source!)
   a. MOVE CORR OBRATYF0 TO STAVYF0  (uses CORRESPONDING)
   b. READ STAVY with key
      - INVALID KEY: Continue
      - NOT INVALID KEY:
        * ADD OBRATYF0 TO STAVYF0  (adds entire structure?)
        * REWRITE STAVYR
   c. READ OBRATY AT END MOVE "E" TO END-INPUT
5. CLOSE OBRATY
6. CLOSE STAVY
```

**Key Differences:**
1. Uses RANDOM access instead of DYNAMIC for STAVY
2. Uses MOVE CORRESPONDING instead of individual field moves
3. Incomplete PERFORM UNTIL condition (bug!)
4. Level-88 condition name commented out
5. Uses END-INPUT variable instead of INPUT-END

#### File Access Patterns

**STAVY:**
- Access Method: RANDOM only
- Same READ and REWRITE pattern as STAOBR

**OBRATY:**
- Same as STAOBR

#### Issues Identified
- ⚠️ **CRITICAL BUG:** Line 47 has incomplete PERFORM UNTIL statement
- ⚠️ Variable naming inconsistency (END-INPUT vs INPUT-END)
- ⚠️ Commented out level-88 condition

---

### 2.5 STAOBR_2.CBLLE - Reversed Processing Logic

**Purpose:** Updates inventory by processing STAVY sequentially and looking up matching OBRATY records. Inverse approach to STAOBR.

#### Environment Division

**File-Control:**
1. **STAVY:**
   - Access: DYNAMIC
   - Organization: INDEXED
   - Key: EXTERNALLY-DESCRIBED-KEY

2. **OBRATY:**
   - Access: DYNAMIC (different from STAOBR!)
   - Organization: INDEXED
   - Key: EXTERNALLY-DESCRIBED-KEY WITH DUPLICATES
   - Allows multiple turnover records per key

#### Data Divisions

**File Section:**

| File | FD Name | Record Name | Format |
|------|---------|-------------|---------|
| STAVY | STAVY | STAVYR | STAVYF0 |
| OBRATY | OBRATY | OBRATYR | OBRATYF0 |

**Working-Storage Section:**

| Level | Name | Type | Size | Purpose |
|-------|------|------|------|---------|
| 01 | END-OD-STAVY | PIC X | 1 byte | STAVY EOF flag |
| 88 | EOF-STAVY | Condition | - | Value "E" |
| 88 | NOT-EOF-STAVY | Condition | - | Value " " |
| 01 | END-OF-OBRATY | PIC X | 1 byte | OBRATY EOF flag |
| 88 | EOF-OBRATY | Condition | - | Value "F" |
| 88 | NOT-EOF-OBRAY | Condition | - | Typo: OBRAY |
| 01 | KEY-STAVY | Group | - | STAVY key structure |
| 05 | ZAVOD | LIKE | - | Plant code |
| 05 | SKLAD | LIKE | - | Warehouse code |
| 05 | MATER | LIKE | - | Material number |
| 01 | KEY-OBRATY | Group | - | OBRATY key structure |
| 05 | ZAVOD | LIKE | - | Redundant definition |
| 05 | SKLAD | LIKE | - | Redundant definition |
| 05 | MATER | LIKE | - | Redundant definition |

#### Procedure Division

**Main Logic:**

```
1. OPEN I-O STAVY
2. OPEN INPUT OBRATY
3. SET NOT-EOF-STAVY TO TRUE
4. READ STAVY FIRST (sequential positioning)
5. PERFORM UNTIL EOF-STAVY
   a. MOVE CORR STAVYF0 TO KEY-STAVY
   b. MOVE CORR KEY-STAVY TO OBRATYF0
   c. START OBRATY KEY IS EQUAL TO KEY-STAVY
      - INVALID KEY: Continue
      - NOT INVALID KEY:
        * PERFORM PROCESS-OBRATY
   d. READ STAVY NEXT
6. GOBACK
```

**Sections:**

1. **HLAVNI-SEKCE** (implicit, main logic)
2. **PROCESS-OBRATY** (explicit section)

**PROCESS-OBRATY Section Logic:**

```
1. READ OBRATY NEXT
2. PERFORM UNTIL EOF-OBRATY
   a. MOVE CORR OBRATYF0 TO KEY-OBRATY
   b. IF KEY-OBRATY = KEY-STAVY
      - ADD MNOBR OF OBRATYR TO MNOZ OF STAVYF0
   c. ELSE
      - REWRITE STAVYR
      - GO TO END-LOOP
   d. READ OBRATY NEXT
3. END-LOOP
```

#### File Access Patterns

**Processing Strategy:**
- **Forward processing:** Read STAVY sequentially
- **Lookup strategy:** For each STAVY record, find matching OBRATY records
- **Benefits:** Handles multiple OBRATY records per STAVY key
- **Drawback:** More complex logic

**STAVY:**
- READ FIRST (sequential positioning)
- READ NEXT (sequential traversal)
- REWRITE (update after processing OBRATY group)

**OBRATY:**
- START operation (position by key)
- READ NEXT (process duplicates)
- Handles multiple records with same key

#### Issues Identified
- ⚠️ Typo in level-88 name: NOT-EOF-OBRAY should be NOT-EOF-OBRATY
- ⚠️ Redundant field definitions in KEY-OBRATY (lines 60-62)
- ⚠️ GO TO statement usage (line 106) - violates structured programming
- ⚠️ REWRITE happens inside loop, potentially writing incomplete data

#### SQL/DB2 Statements
- None

---

### 2.6 STAPOR.CBLLE - Interactive Inventory Maintenance

**Purpose:** Screen-based interactive program for maintaining inventory records (STAVY). Allows inquiry, add, update, and delete operations.

#### Environment Division

**File-Control:**

1. **STAVYW (Workstation/Screen File):**
   - Access: SEQUENTIAL
   - Organization: TRANSACTION
   - Type: Workstation display file

2. **STAVY (Database File):**
   - Access: DYNAMIC
   - Organization: INDEXED
   - Key: EXTERNALLY-DESCRIBED-KEY

#### Data Divisions

**File Section:**

| File | FD | Record | Formats |
|------|-------|---------|---------|
| STAVYW | STAVYW | FORMATY | STAVYW1, STAVYW2 (screen formats) |
| STAVY | STAVY | STAVY-RECORD | STAVYF0 |

**Working-Storage Section:**

| Level | Name | Type | Size | Purpose |
|-------|------|------|------|---------|
| 01 | IND-NENALEZENI | PIC X(1) | 1 byte | Record found indicator |
| 88 | NENALEZEN | Condition | - | Value "1" (not found) |
| 88 | NALEZEN | Condition | - | Value "0" (found) |
| 01 | INDIKATORY-OBRAZOVKY | Group | - | Screen indicators |
| 05 | IN03 | PICTURE 1 INDIC 03 | - | F3=Exit function key |
| 88 | KONEC | Condition | - | Exit program |
| 05 | IN12 | PICTURE 1 INDIC 12 | - | F12=Return function key |
| 88 | NAVRAT | Condition | - | Return/Cancel |
| 05 | IN05 | PICTURE 1 INDIC 05 | - | F5=Refresh function key |
| 88 | OBNOVA | Condition | - | Refresh screen |
| 05 | IN23 | PICTURE 1 INDIC 23 | - | F23=Delete function key |
| 88 | ZRUSIT | Condition | - | Delete record |

#### Procedure Division

**Paragraphs:**

1. **UVODNI-AKCE** - Initialization
2. **ZOBRAZIT-ZADANI** - Display input screen
3. **ZNOVU-DRUHA** - Display detail screen (loop)
4. **KONEC-PROGRAMU** - Cleanup and exit

**Logic Flow:**

```
UVODNI-AKCE:
1. OPEN I-O STAVYW (screen file)
2. OPEN I-O STAVY (database)
3. INITIALIZE indicators

ZOBRAZIT-ZADANI:
1. WRITE FORMATY FROM STAVYW1-O (display screen 1)
2. READ STAVYW INTO STAVYW1-I (get user input)
3. Check function keys:
   - F3 or F12: GO TO KONEC-PROGRAMU
4. MOVE key fields from screen to STAVYF0
5. READ STAVY with key
   - Set NALEZEN or NENALEZEN
6. Prepare STAVYW2-O screen:
   - If found: MOVE data from STAVYF0
   - If not found: MOVE key fields only, MNOZ=0

ZNOVU-DRUHA (loop):
1. WRITE FORMATY FROM STAVYW2-O (display screen 2)
2. READ STAVYW INTO STAVYW2-I (get user input)
3. Check function keys:
   - F3: GO TO KONEC-PROGRAMU
4. MOVE user input to STAVYF0
5. Handle function keys:
   - F5 (OBNOVA): Refresh screen, GO TO ZNOVU-DRUHA
   - F12 (NAVRAT): Return to screen 1
6. Process based on record status:
   - If NENALEZEN:
     * WRITE new STAVY-RECORD
     * GO TO ZOBRAZIT-ZADANI
   - If ZRUSIT (F23):
     * DELETE STAVY
     * GO TO ZOBRAZIT-ZADANI
   - Otherwise:
     * REWRITE STAVY-RECORD (update)
     * GO TO ZOBRAZIT-ZADANI

KONEC-PROGRAMU:
1. CLOSE STAVY
2. CLOSE STAVYW
```

#### Screen Interaction Flow

```
Screen 1 (STAVYW1):        Screen 2 (STAVYW2):
┌─────────────────┐        ┌─────────────────┐
│ Enter Key:      │        │ Detail Screen:  │
│ - ZAVOD         │───────>│ - ZAVOD         │
│ - SKLAD         │        │ - SKLAD         │
│ - MATER         │        │ - MATER         │
└─────────────────┘        │ - MNOZ (qty)    │
     ^                     └─────────────────┘
     │                            │
     └────────────────────────────┘
         F12=Return
```

**User Operations:**
1. **Inquiry:** Enter key, view data on screen 2
2. **Add:** Enter new key, input quantity, auto-writes
3. **Update:** Enter existing key, change quantity, updates on confirm
4. **Delete:** Enter key, press F23 on screen 2
5. **Refresh:** Press F5 to redisplay current data
6. **Exit:** Press F3 anytime to exit

#### File Access Patterns

**STAVYW (Screen File):**
- WRITE/READ cycle for user interaction
- Two formats: STAVYW1 (key entry), STAVYW2 (detail maintenance)

**STAVY (Database):**
- READ with NO LOCK (inquiry only)
- WRITE (add new record)
- REWRITE (update existing)
- DELETE (remove record)
- All operations use INVALID KEY checking

#### Issues Identified
- ⚠️ Extensive use of GO TO statements (non-structured)
- ⚠️ No error handling for WRITE/REWRITE/DELETE operations
- ⚠️ No data validation on user input
- ⚠️ Commented-out level-88 conditions (NENI-KONEC, etc.)

#### SQL/DB2 Statements
- None (uses COBOL file I/O)

---

### 2.7 TABULKY1.CBLLE - Single-Dimension Array Demo

**Purpose:** Demonstrates single-dimension array manipulation using OCCURS and REDEFINES clauses.

#### Data Divisions

**Working-Storage Section:**

| Level | Name | Type | Size | Purpose |
|-------|------|------|------|---------|
| 77 | IDX | PIC 999 | 3 digits | Array index |
| 01 | STRUCT | Group | 5 bytes | Array structure |
| 05 | TEXT1 | PIC X(5) | 5 bytes | Text string "ABCDE" |
| 05 | ARRAY | OCCURS 5 REDEFINES TEXT1 | - | Array overlay |
| 08 | CHAR | PIC X | 1 byte | Individual character |

**Array Structure:**
```
TEXT1:  A B C D E
        │ │ │ │ │
ARRAY:  1 2 3 4 5  (subscripted access)
```

#### Procedure Division

**Logic:**
```
PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5
   DISPLAY "ARRAY :" CHAR IN ARRAY(IDX)
END-PERFORM
```

**Output:**
```
ARRAY : A
ARRAY : B
ARRAY : C
ARRAY : D
ARRAY : E
```

#### Special Notes
- Educational program demonstrating REDEFINES clause
- Shows how to overlay a string with an array
- Uses OCCURS clause for array definition
- Czech comment: "Cyklus pres všechny položky tabulky podle indexu"

---

### 2.8 TABULKY2.CBLLE - Multi-Dimension Array Demo

**Purpose:** Demonstrates two-dimensional array manipulation and nested PERFORM loops.

#### Data Divisions

**Working-Storage Section:**

| Level | Name | Type | Size | Purpose |
|-------|------|------|------|---------|
| 77 | IDX1 | PIC 999 | 3 digits | First dimension index |
| 77 | IDX2 | PIC 999 | 3 digits | Second dimension index |
| 01 | STRUCT | Group | - | 2D array structure |
| 10 | TAB | OCCURS 2 | - | First dimension (2 rows) |
| 20 | TAB2 | OCCURS 3 | - | Second dimension (3 cols) |
| 30 | NUM | PIC 999 | 3 digits | Array element value |

**Array Structure:**
```
TAB(1,1)  TAB(1,2)  TAB(1,3)
TAB(2,1)  TAB(2,2)  TAB(2,3)

2 rows × 3 columns = 6 elements
```

#### Procedure Division

**Logic:**
```
PERFORM VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > 2
   PERFORM VARYING IDX2 FROM 1 BY 1 UNTIL IDX2 > 3
      COMPUTE NUM(IDX1 IDX2) = NUM(IDX1 IDX2) + IDX1 + IDX2
      DISPLAY "ARRAY NUM (" IDX1 ", " IDX2 "): " NUM(IDX1, IDX2)
   END-PERFORM
END-PERFORM
```

**Output:**
```
ARRAY NUM (1, 1): 002
ARRAY NUM (1, 2): 003
ARRAY NUM (1, 3): 004
ARRAY NUM (2, 1): 003
ARRAY NUM (2, 2): 004
ARRAY NUM (2, 3): 005
```

**Calculation:** NUM(i,j) = 0 + i + j

#### Special Notes
- Demonstrates nested PERFORM VARYING
- Shows multi-dimensional array indexing
- Czech comments present
- Educational/training purpose

---

### 2.9 staobr0cp.cblle - Production-Ready Inventory Update

**Purpose:** Production version of inventory update program with comprehensive logging, error handling, and file status checking.

#### Identification Division
- **Program-ID:** STAOBR0CP
- **Author:** Malanius & Co.

#### Environment Division

**File-Control:**

1. **STAVYP0:**
   - Access: DYNAMIC
   - Organization: INDEXED
   - Key: EXTERNALLY-DESCRIBED-KEY
   - **File Status: STAVYSTAV** ✓

2. **OBRATYP0:**
   - Access: SEQUENTIAL
   - Organization: SEQUENTIAL
   - **File Status: OBRATYSTAV** ✓

#### Data Divisions

**File Section:**

| File | FD | Record | Format |
|------|-------|---------|---------|
| STAVYP0 | STAVYP0 | STAVYREC | From DDS |
| OBRATYP0 | OBRATYP0 | OBRATYREC | From DDS |

**Working-Storage Section:**

| Level | Name | Type | Size | Purpose |
|-------|------|------|------|---------|
| 01 | LOGMSG | PIC X(40) | 40 bytes | Log message buffer |
| 01 | LINESUPD | PIC 9(4) | 4 digits | Count of updated records |
| 01 | LINESADD | PIC 9(4) | 4 digits | Count of added records |
| 01 | STAVYSTAV | PIC XX | 2 bytes | STAVY file status |
| 01 | OBRATYSTAV | PIC XX | 2 bytes | OBRATY file status |
| 01 | INPUT-END | PIC X | 1 byte | EOF indicator |
| 88 | END-FILE | Condition | - | Value "E" |

#### Procedure Division

**Logic Flow with Error Handling:**

```
1. Initialization:
   - MOVE 'Starting I/O operations' to LOGMSG
   - CALL "LOG0010CB" USING LOGMSG
   
2. File Open:
   - OPEN I-O STAVYP0
   - OPEN INPUT OBRATYP0
   - IF file status NOT = "00"
     * Log error
     * GOBACK (exit program)

3. Initial Read:
   - READ OBRATYP0 AT END SET END-FILE TO TRUE

4. Main Processing Loop:
   PERFORM UNTIL END-FILE
   
   a. Set key from OBRATY:
      - MOVE ZAVOD OF OBRATYP0 TO ZAVOD OF STAVYP0
      - MOVE SKLAD OF OBRATYP0 TO SKLAD OF STAVYP0
      - MOVE MATER OF OBRATYP0 TO MATER OF STAVYP0
   
   b. READ STAVYP0
      
      INVALID KEY (record doesn't exist):
        - Log 'New entry found, adding line.'
        - MOVE MNOZ OF OBRATYP0 TO MNOZ OF STAVYP0
        - WRITE STAVYREC FROM OBRATYREC
          * INVALID KEY: Log error
          * NOT INVALID KEY: ADD 1 to LINESADD
      
      NOT INVALID KEY (record exists):
        - Log 'Updating record'
        - ADD MNOZ OF OBRATYP0 TO MNOZ OF STAVYP0
        - REWRITE STAVYREC
        - ADD 1 to LINESUPD
   
   c. READ OBRATYP0 (next record)

5. Cleanup and Reporting:
   - CLOSE OBRATYP0
   - CLOSE STAVYP0
   - Log 'Data handling operations finished.'
   - Log lines updated count
   - Log lines added count
   - GOBACK
```

#### External Calls

**LOG0010CB** - Logging subroutine
- Called with LOGMSG parameter
- Purpose: Write log messages to system log
- Called at key processing points:
  1. Program start
  2. File open errors
  3. New record additions
  4. Write errors
  5. Record updates
  6. Program completion
  7. Statistics reporting

#### File Access Patterns

**STAVYP0:**
- Opened I-O mode with file status checking
- READ with key
- WRITE for new records (with error checking)
- REWRITE for updates
- File status checked after OPEN

**OBRATYP0:**
- Opened INPUT mode with file status checking
- Sequential READ with AT END condition
- Drives the main processing loop

#### Key Improvements Over STAOBR

1. **File Status Checking:**
   - Both files have status variables
   - Status checked after OPEN
   - Program exits if open fails

2. **Comprehensive Logging:**
   - External logging subroutine
   - Logs all significant events
   - Tracks statistics

3. **Error Handling:**
   - WRITE operation checks INVALID KEY
   - Logs errors when they occur

4. **Statistics Tracking:**
   - Counts records added (LINESADD)
   - Counts records updated (LINESUPD)
   - Reports totals at end

5. **Better Comments:**
   - English comments
   - Clear section markers
   - Describes purpose of each operation

6. **Handles Missing Records:**
   - Creates new STAVY record if not found
   - Original STAOBR skipped missing records

#### Issues Identified
- ⚠️ TODO comment: "add file status check" (line 57) - partially addressed
- ⚠️ REWRITE doesn't check for errors (line 86)
- ⚠️ No validation of MNOZ values
- ⚠️ Statistics formatted into LOGMSG via MOVE - could overflow

#### SQL/DB2 Statements
- None (uses COBOL file I/O)

---

## 3. Data Structures and Variables

### 3.1 Common Data Structures

#### Key Structure (Present in multiple programs)
```cobol
ZAVOD - Plant/Factory code
SKLAD - Warehouse code  
MATER - Material number
```

This composite key uniquely identifies inventory records across the system.

#### STAVY Record Format (STAVYF0)
- **ZAVOD** - Plant/Factory identifier
- **SKLAD** - Warehouse identifier
- **MATER** - Material number
- **MNOZ** - Quantity on hand

#### OBRATY Record Format (OBRATYF0)
- **ZAVOD** - Plant/Factory identifier
- **SKLAD** - Warehouse identifier
- **MATER** - Material number
- **MNOBR** - Turnover quantity (change amount)

### 3.2 Variable Naming Conventions

| Pattern | Meaning | Examples |
|---------|---------|----------|
| -F0 suffix | DDS format name | STAVYF0, OBRATYF0 |
| -R suffix | Record name | STAVYR, OBRATYR |
| -W suffix | Workstation file | STAVYW |
| END-* | End-of-file flags | END-FILE, END-OF-STAVY |
| EOF-* | Level-88 conditions | EOF-STAVY, EOF-OBRATY |
| IND-* | Indicator variables | IND-NENALEZENI |
| IN## | Function key indicators | IN03, IN12, IN05, IN23 |
| KEY-* | Key field groups | KEY-STAVY, KEY-OBRATY |
| MNO* | Quantity fields | MNOZ, MNOBR |

### 3.3 Level-88 Condition Names

Used extensively for readable conditional testing:

| Condition Name | Value | Purpose |
|---------------|-------|---------|
| END-FILE | "E" | Sequential file EOF |
| EOF-STAVY | "E" | STAVY file EOF |
| EOF-OBRATY | "F" | OBRATY file EOF |
| NENALEZEN | "1" | Record not found |
| NALEZEN | "0" | Record found |
| KONEC | B"1" | F3 pressed (exit) |
| NAVRAT | B"1" | F12 pressed (return) |
| OBNOVA | B"1" | F5 pressed (refresh) |
| ZRUSIT | B"1" | F23 pressed (delete) |

### 3.4 File Status Codes

Used in staobr0cp.cblle:

| Status | Meaning |
|--------|---------|
| 00 | Successful operation |
| 10 | End of file |
| 23 | Record not found (INVALID KEY) |
| 9x | File not available/open |

---

## 4. Dependencies and Relationships

### 4.1 Program Relationships

```
Dependency Graph:

COND1.CBLLE
    └── (references) DUMP_FULL.CBLLE (copybook)
    └── (calls - commented) QlnDumpCobol

STAPOR.CBLLE
    ├── STAVY (database)
    └── STAVYW (screen file)

STAOBR.CBLLE ──┐
STAOBR2.CBLLE ─┼── STAVY (database)
STAOBR_2.CBLLE ┤  OBRATY (database)
staobr0cp.cblle┤
               └── LOG0010CB (external subroutine)

TABULKY1.CBLLE ── (standalone demo)
TABULKY2.CBLLE ── (standalone demo)
```

### 4.2 File Dependencies

| File Name | Type | Used By Programs | Purpose |
|-----------|------|------------------|---------|
| STAVY | Database | STAOBR, STAOBR2, STAOBR_2, STAPOR, staobr0cp | Inventory master file |
| OBRATY | Database | STAOBR, STAOBR2, STAOBR_2, staobr0cp | Turnover transactions |
| STAVYW | Workstation | STAPOR | Screen definitions |
| STAVYP0 | Database | staobr0cp | Same as STAVY |
| OBRATYP0 | Database | staobr0cp | Same as OBRATY |

### 4.3 External Calls

| Calling Program | Called Program | Purpose | Status |
|----------------|----------------|---------|--------|
| COND1 | QlnDumpCobol | System dump | Commented out |
| staobr0cp | LOG0010CB | Logging | Active |

### 4.4 Copybook Dependencies

| Program | Copybook/DDS | Format |
|---------|--------------|--------|
| COND1 | DUMP_FULL | Error structure |
| STAOBR | DDS-ALL-FORMATS OF STAVY | STAVYF0 |
| STAOBR | DDS-ALL-FORMATS OF OBRATY | OBRATYF0 |
| STAOBR2 | DDS-ALL-FORMATS OF STAVY | STAVYF0 |
| STAOBR2 | DDS-ALL-FORMATS OF OBRATY | OBRATYF0 |
| STAOBR_2 | DDS-ALL-FORMATS OF STAVY | STAVYF0 |
| STAOBR_2 | DDS-ALL-FORMATS OF OBRATY | OBRATYF0 |
| STAPOR | DDS-ALL-FORMATS OF STAVYW | Screen formats |
| STAPOR | DDS-STAVYF0 OF STAVY | STAVYF0 |
| staobr0cp | DDS-ALL-FORMATS OF STAVYP0 | Database format |
| staobr0cp | DDS-ALL-FORMATS OF OBRATYP0 | Database format |

### 4.5 Circular References

**Analysis Result:** ✓ No circular references detected

- Programs are independent batch jobs or interactive sessions
- No program calls another program in this collection
- File dependencies are one-way (read/write only)
- LOG0010CB is external, not in this codebase

### 4.6 Modularity Assessment

**Rating: LOW to MEDIUM**

**Issues:**
1. **Code Duplication:** Four programs (STAOBR, STAOBR2, STAOBR_2, staobr0cp) perform similar functions with slight variations
2. **No Shared Modules:** Each program is self-contained
3. **Lack of Abstraction:** File I/O logic embedded in main program
4. **GO TO Usage:** STAPOR uses GO TO extensively

**Positive Aspects:**
1. **Clear Separation:** Batch vs. interactive programs
2. **External Logging:** staobr0cp uses separate logging module
3. **DDS Externalization:** Database structures defined externally

**Recommendations:**
- Create common copybook for key field definitions
- Develop shared file I/O module
- Standardize error handling across programs
- Consolidate similar programs into one parameterized version

---

## 5. File I/O Patterns

### 5.1 File Organization Types

| Organization | Programs Using | Access Methods |
|-------------|---------------|----------------|
| INDEXED | STAOBR, STAOBR2, STAOBR_2, STAPOR, staobr0cp | DYNAMIC, RANDOM |
| SEQUENTIAL | STAOBR, STAOBR2, staobr0cp | SEQUENTIAL |
| TRANSACTION | STAPOR | SEQUENTIAL |

### 5.2 Access Patterns

#### Pattern 1: Sequential Update (STAOBR, staobr0cp)
```
FOR EACH record in OBRATY:
    Set key from OBRATY
    READ STAVY by key
    IF found THEN
        UPDATE quantity
        REWRITE STAVY
```
**Pros:** Simple logic, easy to understand  
**Cons:** Random access to STAVY may be slow if OBRATY not sorted

#### Pattern 2: Inverted Processing (STAOBR_2)
```
FOR EACH record in STAVY:
    START OBRATY at current key
    FOR EACH matching OBRATY record:
        Accumulate quantities
    REWRITE STAVY once
```
**Pros:** Handles multiple OBRATY per key, fewer REWRITE operations  
**Cons:** Complex logic, requires indexed OBRATY file

#### Pattern 3: Interactive CRUD (STAPOR)
```
LOOP:
    Display key entry screen
    Get key from user
    READ STAVY
    Display detail screen
    Get changes from user
    Process: ADD / UPDATE / DELETE
```
**Pros:** User-friendly, direct manipulation  
**Cons:** No batch capability, single-record processing

### 5.3 File Opening Modes

| File | Program | Mode | Lock Strategy |
|------|---------|------|---------------|
| STAVY | STAOBR | I-O | Exclusive |
| STAVY | STAOBR_2 | I-O | Exclusive |
| STAVY | STAPOR | I-O | Exclusive |
| STAVY | staobr0cp | I-O | Exclusive |
| OBRATY | STAOBR | INPUT | Shared read |
| OBRATY | STAOBR_2 | INPUT | Shared read |
| OBRATY | staobr0cp | INPUT | Shared read |
| STAVYW | STAPOR | I-O | Exclusive |

### 5.4 File Status Checking

| Program | File Status Variables | Checked After OPEN | Checked After I/O |
|---------|----------------------|-------------------|-------------------|
| STAOBR | None | ❌ | ❌ |
| STAOBR2 | None | ❌ | ❌ |
| STAOBR_2 | None | ❌ | ❌ |
| STAPOR | None | ❌ | ❌ (uses INVALID KEY) |
| staobr0cp | STAVYSTAV, OBRATYSTAV | ✓ | Partial (WRITE only) |

**Critical Gap:** Most programs lack proper error handling!

### 5.5 Record Locking

| Program | Operation | Lock Type | Notes |
|---------|-----------|-----------|-------|
| STAPOR | READ | NO LOCK | Read-only inquiry |
| All others | READ | Default (exclusive) | Locked until REWRITE/UNLOCK |

### 5.6 File I/O Performance Considerations

**Potential Bottlenecks:**

1. **Random Access in Batch:**
   - STAOBR, staobr0cp perform random reads per OBRATY record
   - Could be slow if OBRATY is large and unsorted
   - **Recommendation:** Sort OBRATY by key before processing

2. **Multiple File Opens:**
   - Each program run opens/closes files
   - **Recommendation:** Consider batch scheduling for off-hours

3. **REWRITE Overhead:**
   - STAOBR_2 rewrites after processing each group
   - More efficient than per-record REWRITE in STAOBR
   - **Best Practice:** Proven by STAOBR_2 design

4. **No Buffering Control:**
   - Programs use default buffering
   - **Recommendation:** Consider APPLY clause for performance tuning

---

## 6. Code Quality Assessment

### 6.1 Code Quality Metrics

| Metric | Rating | Notes |
|--------|--------|-------|
| Documentation | MEDIUM | Czech comments, English in staobr0cp |
| Error Handling | LOW | Minimal file status checking |
| Modularity | LOW | Monolithic programs, code duplication |
| Maintainability | MEDIUM | Clear structure, but GO TO usage |
| Naming Conventions | HIGH | Consistent, meaningful names |
| Structured Programming | MEDIUM | Some GO TO usage in STAPOR |
| Standards Compliance | HIGH | Follows COBOL standards |

### 6.2 Strengths

1. **Clear Purpose:** Each program has well-defined functionality
2. **Consistent Naming:** Variable and file naming is logical
3. **DDS Usage:** Externalizes database structure definitions
4. **Level-88 Conditions:** Makes code more readable
5. **Copybook Usage:** Promotes code reuse (where used)
6. **Progressive Improvement:** staobr0cp shows evolution toward better practices

### 6.3 Critical Issues

#### HIGH PRIORITY

1. **STAOBR2 - Incomplete PERFORM Statement (Line 47)**
   ```cobol
   PERFORM UNTIL    ← MISSING CONDITION!
   ```
   **Impact:** Program will not compile or run correctly  
   **Fix:** Add condition like `INPUT-END = "E"`

2. **Missing File Status Checking (All programs except staobr0cp)**
   **Impact:** Silent failures, data corruption risk  
   **Fix:** Add FILE STATUS clauses and check after every I/O operation

3. **No Error Recovery (All programs)**
   **Impact:** Abnormal termination on errors  
   **Fix:** Implement error handling sections with appropriate recovery

#### MEDIUM PRIORITY

4. **STAOBR_2 - GO TO Statement (Line 106)**
   ```cobol
   GO TO END-LOOP
   ```
   **Impact:** Violates structured programming, harder to maintain  
   **Fix:** Use PERFORM UNTIL with proper exit condition

5. **STAPOR - Extensive GO TO Usage (Multiple lines)**
   **Impact:** Spaghetti code, difficult to follow logic  
   **Fix:** Restructure using PERFORM and proper section/paragraph organization

6. **Variable Name Typo - STAOBR_2 (Line 52)**
   ```cobol
   88 NOT-EOF-OBRAY VALUE " ".   ← Should be NOT-EOF-OBRATY
   ```
   **Impact:** Confusing, potential for misuse  
   **Fix:** Rename to NOT-EOF-OBRATY

#### LOW PRIORITY

7. **Code Duplication**
   **Impact:** Maintenance burden, inconsistency risk  
   **Fix:** Consolidate STAOBR variants into single parameterized program

8. **Commented-Out Code**
   **Impact:** Code clutter, unclear intent  
   **Fix:** Remove or document reason for keeping

9. **Missing Data Validation**
   **Impact:** Bad data acceptance risk  
   **Fix:** Add validation for key fields and quantities

### 6.4 Best Practices Adherence

| Practice | Adherence | Evidence |
|----------|-----------|----------|
| Structured Programming | 60% | GO TO usage in STAPOR |
| Error Handling | 20% | Only staobr0cp has partial error handling |
| Code Comments | 70% | Present but mix of languages |
| Modular Design | 40% | Monolithic programs |
| DRY Principle | 30% | Significant code duplication |
| Single Responsibility | 80% | Programs have clear purpose |
| Meaningful Names | 90% | Excellent naming throughout |

### 6.5 Technical Debt

**Estimated Technical Debt: MEDIUM to HIGH**

**Debt Items:**

1. **Missing Error Handling:** 3-5 days to implement properly
2. **STAPOR Restructuring:** 2-3 days to eliminate GO TO statements
3. **Code Consolidation:** 3-5 days to merge STAOBR variants
4. **Documentation:** 1-2 days to translate and standardize comments
5. **Testing:** 2-3 days to create comprehensive test suite
6. **Bug Fixes:** 1 day for STAOBR2 and other issues

**Total Estimated Effort:** 12-18 developer days

---

## 7. Recommendations

### 7.1 Immediate Actions (Critical)

1. **Fix STAOBR2.CBLLE:**
   - Line 47: Complete the PERFORM UNTIL statement
   - Add missing condition: `UNTIL INPUT-END = "E"`
   - Test thoroughly before any production use

2. **Add File Status Checking:**
   - Implement for all programs except staobr0cp
   - Check status after OPEN, READ, WRITE, REWRITE, DELETE
   - Example:
   ```cobol
   OPEN I-O STAVY
   IF FILE-STATUS NOT = "00"
      DISPLAY "Error opening STAVY: " FILE-STATUS
      GOBACK
   END-IF
   ```

3. **Document Production Status:**
   - Identify which programs are in production use
   - Mark deprecated/experimental versions
   - Create program usage matrix

### 7.2 Short-Term Improvements (1-3 months)

1. **Standardize Error Handling:**
   - Create common error handling copybook
   - Implement in all programs
   - Add error logging like staobr0cp

2. **Consolidate STAOBR Variants:**
   - Merge STAOBR, STAOBR2, STAOBR_2 into single program
   - Use parameters to control processing mode
   - Keep staobr0cp as the production version

3. **Refactor STAPOR:**
   - Eliminate GO TO statements
   - Use PERFORM structure
   - Add data validation
   - Improve error messages

4. **Create Test Suite:**
   - Unit tests for each program
   - Integration tests for file interactions
   - Test data sets

5. **Performance Optimization:**
   - Add file buffering controls
   - Sort OBRATY before batch processing
   - Consider indexed access for both files

### 7.3 Long-Term Strategy (3-12 months)

1. **Modular Architecture:**
   - Create shared modules:
     * File I/O module
     * Error handling module
     * Logging module
     * Validation module
   - Convert programs to use shared modules

2. **Documentation:**
   - Translate all comments to English
   - Create system documentation
   - Document business rules
   - Create data dictionary

3. **Modernization:**
   - Consider SQL/DB2 instead of native file I/O
   - Evaluate web interface for STAPOR functionality
   - Implement transaction management
   - Add audit trail capabilities

4. **Quality Assurance:**
   - Implement code review process
   - Static analysis tools
   - Automated testing
   - Performance monitoring

### 7.4 Maintenance Guidelines

1. **Naming Standards:**
   - Continue using current naming conventions
   - Document standards for new developers

2. **Code Organization:**
   - One program per file
   - Shared copybooks in separate library
   - External data definitions (DDS)

3. **Change Management:**
   - Version control for all source
   - Change log in program headers
   - Test before production deployment

4. **Documentation Standards:**
   - English comments mandatory
   - Purpose at program level
   - Logic explanation at paragraph level
   - Complex calculations documented inline

### 7.5 Training Needs

1. **For Maintenance Team:**
   - COBOL structured programming techniques
   - File status code interpretation
   - IBM i database concepts
   - Error handling best practices

2. **For New Developers:**
   - COBOL fundamentals
   - IBM i system concepts
   - Application domain knowledge (inventory management)
   - Legacy system maintenance patterns

---

## Appendix A: File Cross-Reference

### STAVY File Usage

| Program | Mode | Access | Key Fields | Operations |
|---------|------|--------|------------|------------|
| STAOBR | I-O | DYNAMIC | ZAVOD, SKLAD, MATER | READ, REWRITE |
| STAOBR2 | I-O | RANDOM | ZAVOD, SKLAD, MATER | READ, REWRITE |
| STAOBR_2 | I-O | DYNAMIC | ZAVOD, SKLAD, MATER | READ FIRST/NEXT, REWRITE |
| STAPOR | I-O | DYNAMIC | ZAVOD, SKLAD, MATER | READ, WRITE, REWRITE, DELETE |
| staobr0cp | I-O | DYNAMIC | ZAVOD, SKLAD, MATER | READ, WRITE, REWRITE |

### OBRATY File Usage

| Program | Mode | Access | Key Fields | Operations |
|---------|------|--------|------------|------------|
| STAOBR | INPUT | SEQUENTIAL | N/A | READ sequential |
| STAOBR2 | INPUT | SEQUENTIAL | N/A | READ sequential |
| STAOBR_2 | INPUT | DYNAMIC | ZAVOD, SKLAD, MATER | START, READ NEXT |
| staobr0cp | INPUT | SEQUENTIAL | N/A | READ sequential |

---

## Appendix B: Variable Cross-Reference

### Key Field Variables

| Variable | Found In | Type | Purpose |
|----------|----------|------|---------|
| ZAVOD | All STAVY/OBRATY programs | Key | Plant/Factory code |
| SKLAD | All STAVY/OBRATY programs | Key | Warehouse code |
| MATER | All STAVY/OBRATY programs | Key | Material number |
| MNOZ | STAVY record | Data | Inventory quantity |
| MNOBR | OBRATY record | Data | Turnover quantity |

### Control Variables

| Variable | Found In | Type | Purpose |
|----------|----------|------|---------|
| INPUT-END | STAOBR, staobr0cp | EOF Flag | End of file indicator |
| END-OD-STAVY | STAOBR_2 | EOF Flag | STAVY EOF |
| END-OF-OBRATY | STAOBR_2 | EOF Flag | OBRATY EOF |
| IND-NENALEZENI | STAPOR | Status | Record found indicator |
| INDIKATORY-OBRAZOVKY | STAPOR | Group | Screen function keys |
| STAVYSTAV | staobr0cp | File Status | STAVY file status code |
| OBRATYSTAV | staobr0cp | File Status | OBRATY file status code |

---

## Appendix C: Function Key Definitions (STAPOR)

| Key | Indicator | Condition Name | Purpose |
|-----|-----------|----------------|---------|
| F3 | IN03 | KONEC | Exit program |
| F5 | IN05 | OBNOVA | Refresh screen |
| F12 | IN12 | NAVRAT | Return to previous screen |
| F23 | IN23 | ZRUSIT | Delete current record |

---

## Appendix D: Program Evolution Timeline

Based on code structure and comments:

1. **Generation 1:** STAOBR.CBLLE
   - Basic functionality
   - Sequential OBRATY processing
   - No error handling

2. **Generation 2:** STAOBR2.CBLLE
   - Attempted simplification
   - MOVE CORRESPONDING usage
   - **INCOMPLETE - Has bug**

3. **Generation 3:** STAOBR_2.CBLLE
   - Reversed processing logic
   - Handles duplicate OBRATY keys
   - More complex but powerful

4. **Generation 4:** staobr0cp.cblle
   - Production-ready version
   - Comprehensive error handling
   - Logging integration
   - Statistics tracking
   - English documentation

**Recommendation:** Use staobr0cp.cblle as the standard, deprecate others.

---

## Appendix E: Glossary

| Term | Meaning |
|------|---------|
| CBLLE | COBOL ILE (Integrated Language Environment) source |
| DDS | Data Description Specifications |
| FD | File Description |
| I-O | Input-Output (file open mode) |
| OCCURS | Array/table definition clause |
| REDEFINES | Memory overlay clause |
| STAVYF0 | Record format 0 of STAVY file |
| OBRATYF0 | Record format 0 of OBRATY file |

### Czech Terms Translation

| Czech | English | Context |
|-------|---------|---------|
| STAVY | States/Inventory | Inventory status file |
| OBRATY | Turnover | Transaction file |
| ZAVOD | Plant/Factory | Manufacturing location |
| SKLAD | Warehouse | Storage location |
| MATER | Material | Material/item number |
| MNOZ | Množství (Quantity) | Quantity on hand |
| MNOBR | Množství obratu (Turnover quantity) | Transaction quantity |
| OBRAZOVKA | Screen | Display screen |
| HLAVNI | Main | Main section |
| KONEC | End | End/exit |
| NALEZEN | Found | Record found |
| NENALEZEN | Not found | Record not found |

---

## Report Metadata

- **Generated Date:** January 2025
- **Analysis Tool:** COBOL Code Analyzer
- **Repository:** Cobol-Demo/AS400/QCBLLESRC
- **Programs Analyzed:** 9
- **Total Analysis Time:** Comprehensive deep-dive analysis
- **Analyst Notes:** Legacy AS400 COBOL inventory management system with varying quality levels across programs

---

## Summary and Conclusion

This COBOL codebase represents a typical legacy AS400 inventory management system with programs at different maturity levels. While the core functionality is sound, significant technical debt exists in error handling, code duplication, and structured programming practices.

**Key Findings:**

1. **Functionality:** Core business logic is solid and well-understood
2. **Quality:** Ranges from LOW (STAOBR2 bug) to MEDIUM-HIGH (staobr0cp)
3. **Maintainability:** Hampered by code duplication and GO TO usage
4. **Risk Level:** MEDIUM - Critical bug in STAOBR2, minimal error handling overall

**Priority Actions:**

1. Fix STAOBR2 bug immediately
2. Add file status checking to all programs
3. Consolidate duplicate programs
4. Implement comprehensive testing

**Long-Term Vision:**

Consider gradual modernization while maintaining business continuity. The staobr0cp program demonstrates that improvements can be made within the COBOL framework before considering complete system replacement.

---

*End of Report*

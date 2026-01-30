# COBOL Program Analysis: STAPOR.CBLLE

## Executive Summary

**Program Name:** STAPOR.CBLLE  
**Program Type:** AS400 ILE COBOL Interactive Transaction Program  
**Purpose:** Inventory State (STAVY) Management - CRUD Operations  
**Language:** COBOL for AS/400  
**Date Analyzed:** 2025  

This program provides interactive maintenance capabilities for inventory states (STAVY) through a two-screen interface. It allows users to add, modify, delete, and display inventory records based on plant (ZAVOD), warehouse (SKLAD), and material (MATER) keys.

---

## 1. Program Structure and Organization

### 1.1 Program Architecture

The program follows a traditional AS/400 COBOL structure with:
- **Environment Division**: File definitions (lines 1-13)
- **Data Division**: File and Working-Storage sections (lines 15-45)
- **Procedure Division**: Main logic flow (lines 47-112)

### 1.2 Program Flow

```
UVODNI-AKCE (Initialization)
    ↓
ZOBRAZIT-ZADANI (Display Input Screen)
    ↓
    ├→ Read STAVY record
    │   ↓
    ├→ ZNOVU-DRUHA (Display/Edit Screen)
    │   ↓
    │   ├→ OBNOVA (Refresh) → Loop back to ZNOVU-DRUHA
    │   ├→ NAVRAT (Return) → ZOBRAZIT-ZADANI
    │   ├→ NENALEZEN (Not Found) → INSERT → ZOBRAZIT-ZADANI
    │   ├→ ZRUSIT (Delete) → DELETE → ZOBRAZIT-ZADANI
    │   └→ REWRITE → ZOBRAZIT-ZADANI
    │
    └→ KONEC-PROGRAMU (End Program)
```

### 1.3 Program Sections

| Section | Lines | Purpose |
|---------|-------|---------|
| UVODNI-AKCE | 49-52 | Initialize files and indicators |
| ZOBRAZIT-ZADANI | 54-77 | Display input screen and read record |
| ZNOVU-DRUHA | 79-107 | Display detail screen and process actions |
| KONEC-PROGRAMU | 109-111 | Clean up and close files |

---

## 2. Dependencies and External Resources

### 2.1 File Dependencies

#### Display File (STAVYW.DSPF)
- **Physical Name:** STAVYW
- **Type:** Workstation Display File
- **Access:** Sequential
- **Organization:** Transaction
- **Formats:**
  - `STAVYW1`: Input screen for key entry (ZAVOD, SKLAD, MATER)
  - `STAVYW2`: Detail screen for quantity (MNOZ) entry/display
- **Indicators:**
  - IN03 (F3): End program
  - IN12 (F12): Return
  - IN05 (F5): Refresh
  - IN23 (F23): Delete

#### Database File (STAVY.PF)
- **Physical Name:** STAVY
- **Type:** Physical Database File (Indexed)
- **Access:** Dynamic
- **Organization:** Indexed
- **Key Structure:** Composite key (ZAVOD + SKLAD + MATER)
- **Record Format:** STAVYF0

### 2.2 DDS Copybooks

```cobol
Line 21: COPY DDS-ALL-FORMATS OF STAVYW
Line 26: COPY DDS-STAVYF0 OF STAVY
```

These COPY statements import externally-described DDS (Data Description Specifications) definitions:
- **DDS-ALL-FORMATS OF STAVYW**: Screen formats from STAVYW.DSPF
- **DDS-STAVYF0 OF STAVY**: Database record layout from STAVY.PF

### 2.3 Reference File (REF.PF)

The STAVY.PF references REF.PF (line 2 in STAVY.PF) for field definitions:
- **MATER** (5 characters): Material number
- **MNOZ** (8 digits, packed decimal): Quantity in warehouse
- **SKLAD** (2 characters): Warehouse code
- **ZAVOD** (2 characters): Plant code

### 2.4 Data Structure Diagram

```
┌─────────────────────────────────────┐
│         REF.PF                      │
│    (Reference File)                 │
│  - Field definitions                │
└──────────────┬──────────────────────┘
               │ Referenced by
               ↓
┌─────────────────────────────────────┐
│        STAVY.PF                     │
│   (Physical File)                   │
│  Record: STAVYF0                    │
│  - ZAVOD (2)  }                     │
│  - SKLAD (2)  } Composite Key       │
│  - MATER (5)  }                     │
│  - MNOZ  (8P0)                      │
└──────────────┬──────────────────────┘
               │
               │ Read/Write
               ↓
┌─────────────────────────────────────┐
│      STAPOR.CBLLE                   │
│   (COBOL Program)                   │
└──────────────┬──────────────────────┘
               │
               │ Display I/O
               ↓
┌─────────────────────────────────────┐
│        STAVYW.DSPF                  │
│   (Display File)                    │
│  - STAVYW1 (Input screen)           │
│  - STAVYW2 (Detail screen)          │
└─────────────────────────────────────┘
```

---

## 3. Data Divisions and Variables

### 3.1 File Section

#### Display File (STAVYW)
```cobol
FD STAVYW
01 FORMATY
   COPY DDS-ALL-FORMATS OF STAVYW
```

Expected to contain:
- `STAVYW1-I`: Input from first screen
- `STAVYW1-O`: Output to first screen
- `STAVYW1-I-INDIC`: Indicators from first screen
- `STAVYW2-I`: Input from second screen
- `STAVYW2-O`: Output to second screen
- `STAVYW2-I-INDIC`: Indicators from second screen

#### Database File (STAVY)
```cobol
FD STAVY
01 STAVY-RECORD
   COPY DDS-STAVYF0 OF STAVY
```

Expected structure (based on REF.PF):
- `STAVYF0`
  - `ZAVOD` (2 characters): Plant code
  - `SKLAD` (2 characters): Warehouse code
  - `MATER` (5 characters): Material number
  - `MNOZ` (8 packed decimal, 0 decimals): Quantity

### 3.2 Working-Storage Section

#### Record Status Indicator
```cobol
01 IND-NENALEZENI        PICTURE IS X(1)
   88 NENALEZEN          VALUE IS "1"
   88 NALEZEN            VALUE IS "0"
```
- **Purpose**: Tracks whether a record was found during READ operation
- **Type**: Character (1 byte)
- **Level-88 Conditions**: 
  - NENALEZEN: Record not found
  - NALEZEN: Record found

#### Screen Indicators
```cobol
01 INDIKATORY-OBRAZOVKY
   05 IN03          PICTURE 1 INDIC 03
      88 KONEC      VALUE IS B"1"
   05 IN12          PICTURE 1 INDIC 12
      88 NAVRAT     VALUE IS B"1"
   05 IN05          PICTURE 1 INDIC 05
      88 OBNOVA     VALUE IS B"1"
   05 IN23          PICTURE 1 INDIC 23
      88 ZRUSIT     VALUE IS B"1"
```

| Indicator | Function Key | Purpose | Condition Name |
|-----------|--------------|---------|----------------|
| IN03 | F3 | End program | KONEC |
| IN12 | F12 | Return to previous screen | NAVRAT |
| IN05 | F5 | Refresh/reload data | OBNOVA |
| IN23 | F23 | Delete record | ZRUSIT |

---

## 4. Procedure Division Logic Flow

### 4.1 Paragraph Descriptions

#### UVODNI-AKCE (Lines 49-52)
**Purpose:** Program initialization

**Logic:**
1. Open STAVYW display file for I-O
2. Open STAVY database file for I-O
3. Initialize indicator structure

**File Operations:**
- OPEN I-O STAVYW
- OPEN I-O STAVY

#### ZOBRAZIT-ZADANI (Lines 54-77)
**Purpose:** Display input screen and lookup record

**Logic:**
1. Display STAVYW1 format (input screen)
2. Read user input (ZAVOD, SKLAD, MATER)
3. Check for F3 (KONEC) or F12 (NAVRAT) - exit if pressed
4. Move input fields to STAVYF0 (database record key)
5. READ STAVY with NO LOCK using the key
6. Set NENALEZEN or NALEZEN based on INVALID KEY condition
7. Prepare data for STAVYW2 display:
   - If found: Move all fields from STAVYF0 to STAVYW2-O
   - If not found: Move key fields and set MNOZ to 0

**File Operations:**
- WRITE FORMATY (Display)
- READ STAVYW (User input)
- READ STAVY (Database lookup)

**Data Flow:**
```
User Input (STAVYW1-I) 
    ↓
STAVYF0 (Key fields)
    ↓
READ STAVY
    ↓
STAVYF0 (Complete record if found)
    ↓
STAVYW2-O (Display preparation)
```

#### ZNOVU-DRUHA (Lines 79-107)
**Purpose:** Display detail screen and process CRUD operations

**Logic:**
1. Display STAVYW2 format (detail screen)
2. Read user input (MNOZ modification)
3. Check for F3 (KONEC) - exit if pressed
4. Move input fields to STAVYF0
5. Process based on function key:
   - **F5 (OBNOVA)**: Refresh screen with current data → loop back
   - **F12 (NAVRAT)**: Move data back to input screen → return to ZOBRAZIT-ZADANI
   - **F23 (ZRUSIT) + NALEZEN**: DELETE record → return to ZOBRAZIT-ZADANI
   - **No key + NENALEZEN**: INSERT new record (WRITE) → return to ZOBRAZIT-ZADANI
   - **No key + NALEZEN**: UPDATE record (REWRITE) → return to ZOBRAZIT-ZADANI

**File Operations:**
- WRITE FORMATY (Display)
- READ STAVYW (User input)
- WRITE STAVY-RECORD (Insert)
- DELETE STAVY (Delete)
- REWRITE STAVY-RECORD (Update)

**Decision Tree:**
```
ZNOVU-DRUHA
    ├─ IF KONEC → KONEC-PROGRAMU
    ├─ IF OBNOVA → ZNOVU-DRUHA (loop)
    ├─ IF NAVRAT → ZOBRAZIT-ZADANI
    ├─ IF NENALEZEN → WRITE (INSERT) → ZOBRAZIT-ZADANI
    ├─ IF ZRUSIT → DELETE → ZOBRAZIT-ZADANI
    └─ ELSE → REWRITE (UPDATE) → ZOBRAZIT-ZADANI
```

#### KONEC-PROGRAMU (Lines 109-111)
**Purpose:** Program termination and cleanup

**Logic:**
1. Close STAVY database file
2. Close STAVYW display file

**File Operations:**
- CLOSE STAVY
- CLOSE STAVYW

### 4.2 Control Flow Relationships

```
UVODNI-AKCE
    ↓
ZOBRAZIT-ZADANI ←──────────┐
    ↓                       │
    ├→ F3/F12 → KONEC-PROGRAMU
    ↓                       │
ZNOVU-DRUHA                │
    ├→ F3 → KONEC-PROGRAMU │
    ├→ F5 → ZNOVU-DRUHA (self-loop)
    ├→ F12 → ZOBRAZIT-ZADANI
    ├→ INSERT/UPDATE/DELETE ┘
    └→ (returns to ZOBRAZIT-ZADANI)
```

### 4.3 Database Operations Summary

| Operation | Condition | Statement | Line |
|-----------|-----------|-----------|------|
| **SELECT** | Always on key entry | READ STAVY ... NO LOCK | 65 |
| **INSERT** | Record not found | WRITE STAVY-RECORD | 94 |
| **UPDATE** | Record found, no special key | REWRITE STAVY-RECORD | 105 |
| **DELETE** | Record found + F23 pressed | DELETE STAVY | 101 |

---

## 5. Identified Issues and Code Quality Analysis

### 5.1 Critical Issues

#### 1. **Unstructured Control Flow - GO TO Statements**
**Severity:** HIGH  
**Lines:** 58, 59, 89, 96, 102, 107

**Issue:**
The program uses multiple `GO TO` statements for control flow, which is considered poor practice in modern programming:
```cobol
IF KONEC  GO TO KONEC-PROGRAMU.       (Line 58)
IF NAVRAT GO TO KONEC-PROGRAMU.       (Line 59)
GO TO ZNOVU-DRUHA.                    (Line 89)
GO TO ZOBRAZIT-ZADANI                 (Line 96)
GO TO ZOBRAZIT-ZADANI                 (Line 102)
GO TO ZOBRAZIT-ZADANI.                (Line 107)
```

**Impact:**
- Difficult to trace program flow
- Hard to maintain and debug
- Creates "spaghetti code"
- Increases cognitive complexity

**Recommendation:**
Replace with PERFORM statements or structured programming constructs:
```cobol
IF KONEC
   PERFORM KONEC-PROGRAMU
   STOP RUN
END-IF

IF NOT KONEC AND NOT NAVRAT
   PERFORM PROCESS-RECORD
END-IF
```

#### 2. **Missing Error Handling**
**Severity:** HIGH  
**Lines:** 50, 51, 94, 101, 105

**Issue:**
No FILE STATUS checking or error handling for:
- File OPEN operations (lines 50-51)
- WRITE operations (line 94)
- DELETE operations (line 101)
- REWRITE operations (line 105)

**Example:**
```cobol
OPEN I-O STAVYW.     ← No status check
OPEN I-O STAVY.      ← No status check
```

**Impact:**
- Runtime failures without graceful degradation
- Data corruption risk
- No user feedback on errors
- Difficult to diagnose issues

**Recommendation:**
Add FILE STATUS variables and error handling:
```cobol
SELECT STAVY ASSIGN TO DATABASE-STAVY
       FILE STATUS IS WS-STAVY-STATUS
...

OPEN I-O STAVY
IF WS-STAVY-STATUS NOT = "00"
   DISPLAY "Error opening STAVY: " WS-STAVY-STATUS
   PERFORM KONEC-PROGRAMU
   STOP RUN
END-IF
```

#### 3. **Incomplete Logic - Missing NAVRAT Processing**
**Severity:** MEDIUM  
**Line:** 91

**Issue:**
When NAVRAT (F12) is pressed from the detail screen, the code moves data but doesn't explicitly return to ZOBRAZIT-ZADANI:
```cobol
IF NAVRAT MOVE CORR STAVYF0 TO STAVYW1-O
```
The code then falls through to the next IF statement without a proper return path.

**Impact:**
- Unclear execution path
- Potential for unintended operations
- Logic bug that may cause unexpected behavior

**Recommendation:**
```cobol
IF NAVRAT 
   MOVE CORR STAVYF0 TO STAVYW1-O
   GO TO ZOBRAZIT-ZADANI
END-IF
```

#### 4. **Logic Conflict - ZRUSIT on Non-Existent Record**
**Severity:** MEDIUM  
**Lines:** 99-103

**Issue:**
The code allows DELETE operation even when NENALEZEN is true:
```cobol
IF ZRUSIT
   MOVE CORR STAVYF0 TO STAVYW1-O
   DELETE STAVY
   GO TO ZOBRAZIT-ZADANI
END-IF
```

**Impact:**
- Attempting to delete a non-existent record
- Runtime error or unpredictable behavior
- Data integrity concern

**Recommendation:**
```cobol
IF ZRUSIT
   IF NALEZEN
      MOVE CORR STAVYF0 TO STAVYW1-O
      DELETE STAVY
      DISPLAY "Record deleted successfully"
   ELSE
      DISPLAY "Cannot delete - record not found"
   END-IF
   GO TO ZOBRAZIT-ZADANI
END-IF
```

### 5.2 Modularity Issues

#### 1. **Monolithic Paragraph Structure**
**Issue:** ZNOVU-DRUHA paragraph (lines 79-107) handles multiple responsibilities:
- Display logic
- User input processing
- Business logic (INSERT/UPDATE/DELETE)
- Screen navigation

**Impact:**
- Difficult to test individual operations
- Hard to reuse code
- Maintenance complexity

**Recommendation:**
Break into smaller, focused paragraphs:
```cobol
ZNOVU-DRUHA.
    PERFORM DISPLAY-DETAIL-SCREEN
    PERFORM PROCESS-USER-INPUT
    PERFORM EXECUTE-OPERATION
    ...

DISPLAY-DETAIL-SCREEN.
    WRITE FORMATY FROM STAVYW2-O, FORMAT IS "STAVYW2"
    READ STAVYW RECORD INTO STAVYW2-I
    ...

EXECUTE-OPERATION.
    EVALUATE TRUE
       WHEN OBNOVA
           PERFORM REFRESH-SCREEN
       WHEN NENALEZEN
           PERFORM INSERT-RECORD
       WHEN ZRUSIT
           PERFORM DELETE-RECORD
       WHEN OTHER
           PERFORM UPDATE-RECORD
    END-EVALUATE.
```

#### 2. **Code Duplication**
**Lines:** 71-76, 88, 91, 95, 100, 106

Multiple similar MOVE CORR operations repeated throughout the code:
```cobol
MOVE CORR STAVYF0 TO STAVYW2-O        (Line 71)
MOVE CORR STAVYF0 TO STAVYW2-O        (Line 88)
MOVE CORR STAVYF0 TO STAVYW1-O        (Line 91)
MOVE CORR STAVYF0 TO STAVYW1-O        (Line 95)
MOVE CORR STAVYF0 TO STAVYW1-O        (Line 100)
MOVE CORR STAVYF0 TO STAVYW1-O        (Line 106)
```

**Recommendation:**
Create reusable paragraphs:
```cobol
REFRESH-INPUT-SCREEN.
    MOVE CORR STAVYF0 TO STAVYW1-O.

REFRESH-DETAIL-SCREEN.
    MOVE CORR STAVYF0 TO STAVYW2-O.
```

### 5.3 Data Quality Issues

#### 1. **Implicit Data Conversions**
**Issue:** Using MOVE CORRESPONDING without explicit field mapping may hide data type mismatches.

**Lines:** 62, 71, 86, 88, 91, 95, 100, 106

**Recommendation:**
Use explicit field-to-field moves for critical data or add comments documenting expected field correspondence.

#### 2. **No Data Validation**
**Issue:** No validation of user input before database operations:
- ZAVOD, SKLAD, MATER could be blank or invalid
- MNOZ could be negative or exceed limits

**Recommendation:**
Add validation routines:
```cobol
VALIDATE-INPUT.
    IF ZAVOD = SPACES OR ZAVOD = LOW-VALUES
       DISPLAY "Plant code required"
       SET INPUT-ERROR TO TRUE
    END-IF
    
    IF MNOZ < 0
       DISPLAY "Quantity cannot be negative"
       SET INPUT-ERROR TO TRUE
    END-IF.
```

### 5.4 Maintainability Issues

#### 1. **No Comments for Business Logic**
**Issue:** Limited comments explaining WHY operations are performed, only WHAT (Czech comments on file descriptions)

**Recommendation:**
Add inline comments explaining business rules:
```cobol
* Check if this is a new record addition
IF NENALEZEN
   * Business Rule: New inventory records start with entered quantity
   WRITE STAVY-RECORD FROM STAVYF0
   ...
```

#### 2. **Magic Values**
**Issue:** Hard-coded values without named constants:
- "1" and "0" for NENALEZEN/NALEZEN
- B"1" for indicators

**Recommendation:**
Already using level-88 conditions (good), but ensure consistency.

#### 3. **Mixed Language Comments**
**Issue:** Comments are in Czech (e.g., "KLIC PRO HLEDÁNÍ V SOUBORU STAVY"), which may limit maintainability in international teams.

**Recommendation:**
Consider bilingual comments or English for international teams.

---

## 6. Best Practices Compliance

### 6.1 Compliance Summary

| Best Practice | Status | Notes |
|---------------|--------|-------|
| Structured Programming | ❌ Failed | Uses GO TO statements |
| Error Handling | ❌ Failed | No FILE STATUS or error checking |
| Modular Design | ⚠️ Partial | Some modularity but paragraphs too large |
| Code Documentation | ⚠️ Partial | Some comments but mostly structural |
| Data Validation | ❌ Failed | No input validation |
| Magic Numbers | ✅ Passed | Uses level-88 conditions |
| File Organization | ✅ Passed | Proper use of FD and WORKING-STORAGE |
| Screen Handling | ✅ Passed | Proper use of DDS and indicators |
| Database Access | ✅ Passed | Proper use of READ/WRITE/REWRITE/DELETE |
| Naming Conventions | ✅ Passed | Clear, descriptive names (Czech context) |

### 6.2 COBOL Standards Compliance

#### Strong Points:
1. **Proper use of COPY for DDS**: Leverages AS/400 externally described files
2. **Level-88 conditions**: Good use of meaningful condition names
3. **MOVE CORRESPONDING**: Efficient data movement between similar structures
4. **Screen indicators**: Standard AS/400 workstation file handling
5. **Dynamic access**: Appropriate file access mode for keyed retrieval

#### Weaknesses:
1. **Legacy control flow**: Heavy reliance on GO TO
2. **No paragraph hierarchy**: Flat structure instead of nested PERFORMs
3. **Minimal error handling**: Assumes all operations succeed
4. **No transaction control**: No explicit commit/rollback logic

---

## 7. Security and Data Integrity

### 7.1 Security Issues

#### 1. **No Record Locking on Update**
**Line:** 65

```cobol
READ STAVY RECORD INTO STAVYF0 NO LOCK
```

**Issue:** Using NO LOCK on READ means other users could modify the record between READ and REWRITE/DELETE.

**Risk:** MEDIUM - Lost update problem in multi-user environment

**Recommendation:**
```cobol
* For updates, read with lock
READ STAVY RECORD INTO STAVYF0
   INVALID KEY SET NENALEZEN TO TRUE
   NOT INVALID KEY SET NALEZEN TO TRUE
END-READ

* Later, if just viewing (not updating), use NO LOCK
```

#### 2. **No Audit Trail**
**Issue:** No logging of who created, modified, or deleted records

**Recommendation:**
Add audit fields to database:
- Created by / Created date
- Modified by / Modified date
- Deleted flag (soft delete)

### 7.2 Data Integrity

#### 1. **No Referential Integrity Checks**
**Issue:** No validation that ZAVOD, SKLAD, or MATER exist in master files

**Recommendation:**
```cobol
VALIDATE-ZAVOD.
    READ ZAVOD-MASTER
        KEY IS ZAVOD
        INVALID KEY
            DISPLAY "Invalid plant code"
            SET VALIDATION-ERROR TO TRUE
    END-READ.
```

#### 2. **No Concurrent Access Protection**
**Issue:** Multiple users could attempt to add the same record simultaneously

**Recommendation:**
Use record locking and handle duplicate key errors:
```cobol
WRITE STAVY-RECORD FROM STAVYF0
    INVALID KEY
        DISPLAY "Record already exists or duplicate key"
        PERFORM ERROR-HANDLING
END-WRITE
```

---

## 8. Performance Considerations

### 8.1 Current Performance Characteristics

#### Strengths:
1. **Dynamic access mode** (line 11): Efficient for random access
2. **Indexed organization** (line 12): Fast key-based retrieval
3. **NO LOCK on initial READ** (line 65): Reduces lock contention for inquiry
4. **Single record operations**: No batch processing overhead

#### Potential Issues:
1. **Screen refresh on F5**: Unnecessary re-read of already loaded data (line 88-89)
2. **Multiple screen writes**: Could potentially be optimized
3. **No buffering**: Each operation is immediate (appropriate for transaction processing)

### 8.2 Scalability

**Current Design:** Single-user transaction model
**Scalability:** MEDIUM - Acceptable for typical AS/400 workload

**Considerations:**
- Program is stateless between screens (good)
- No connection pooling needed (AS/400 job model)
- File locks released between transactions (good)

---

## 9. Testing Recommendations

### 9.1 Unit Test Scenarios

| Test Case | Input | Expected Behavior |
|-----------|-------|-------------------|
| **TC-01: Insert New Record** | Valid ZAVOD/SKLAD/MATER not in DB | Record inserted, success message |
| **TC-02: Update Existing Record** | Valid key, modify MNOZ | Record updated with new MNOZ |
| **TC-03: Delete Existing Record** | Valid key, press F23 | Record deleted |
| **TC-04: Delete Non-Existent** | Invalid key, press F23 | Error message (currently fails) |
| **TC-05: Duplicate Insert** | Key already exists | Error message (currently no check) |
| **TC-06: F3 Exit** | Press F3 from any screen | Program terminates cleanly |
| **TC-07: F12 Return** | Press F12 from detail screen | Returns to input screen |
| **TC-08: F5 Refresh** | Modify screen data, press F5 | Screen refreshes with DB data |
| **TC-09: Invalid Characters** | Non-numeric MNOZ | Validation error (currently no check) |
| **TC-10: Negative Quantity** | MNOZ < 0 | Validation error (currently no check) |
| **TC-11: Blank Key Fields** | Empty ZAVOD/SKLAD/MATER | Validation error (currently no check) |
| **TC-12: File Open Failure** | STAVY file locked/unavailable | Error message (currently no check) |

### 9.2 Integration Test Scenarios

1. **Concurrent Access Test**
   - User A reads record
   - User B modifies same record
   - User A attempts update
   - Expected: Lost update (current design flaw)

2. **File Integrity Test**
   - Verify ZAVOD/SKLAD/MATER exist in master files
   - Expected: Referential integrity maintained (not checked)

3. **Transaction Rollback Test**
   - Simulate system failure during WRITE/REWRITE/DELETE
   - Expected: Database integrity maintained (AS/400 commitment control)

---

## 10. Recommendations for Improvement

### 10.1 High Priority (Critical)

#### 1. Add Error Handling Framework
```cobol
WORKING-STORAGE SECTION.
01  WS-FILE-STATUS.
    05  WS-STAVYW-STATUS     PIC XX.
    05  WS-STAVY-STATUS      PIC XX.
01  WS-ERROR-MSG            PIC X(80).

SELECT STAVYW ASSIGN TO WORKSTATION-STAVYW
       FILE STATUS IS WS-STAVYW-STATUS
       ...

SELECT STAVY ASSIGN TO DATABASE-STAVY
       FILE STATUS IS WS-STAVY-STATUS
       ...

CHECK-FILE-STATUS.
    IF WS-STAVY-STATUS NOT = "00"
       EVALUATE WS-STAVY-STATUS
           WHEN "23"
               MOVE "Record not found" TO WS-ERROR-MSG
           WHEN "22"
               MOVE "Duplicate key" TO WS-ERROR-MSG
           WHEN "30"
               MOVE "Permanent file error" TO WS-ERROR-MSG
           WHEN OTHER
               STRING "File error: " WS-STAVY-STATUS
                   DELIMITED BY SIZE INTO WS-ERROR-MSG
       END-EVALUATE
       PERFORM DISPLAY-ERROR-MESSAGE
    END-IF.
```

#### 2. Restructure Control Flow
```cobol
MAIN-PROCESS.
    PERFORM UVODNI-AKCE
    PERFORM UNTIL KONEC OR NAVRAT
        PERFORM ZOBRAZIT-ZADANI
        IF NOT KONEC AND NOT NAVRAT
            PERFORM PROCESS-DETAIL-SCREEN
        END-IF
    END-PERFORM
    PERFORM KONEC-PROGRAMU
    STOP RUN.

PROCESS-DETAIL-SCREEN.
    PERFORM UNTIL KONEC OR EXIT-DETAIL
        PERFORM ZNOVU-DRUHA
    END-PERFORM.

ZNOVU-DRUHA.
    PERFORM DISPLAY-DETAIL
    PERFORM GET-USER-ACTION
    PERFORM EXECUTE-USER-ACTION.

EXECUTE-USER-ACTION.
    EVALUATE TRUE
        WHEN KONEC
            SET EXIT-DETAIL TO TRUE
        WHEN OBNOVA
            PERFORM REFRESH-DETAIL
        WHEN NAVRAT
            SET EXIT-DETAIL TO TRUE
        WHEN ZRUSIT
            PERFORM DELETE-OPERATION
            SET EXIT-DETAIL TO TRUE
        WHEN NENALEZEN
            PERFORM INSERT-OPERATION
            SET EXIT-DETAIL TO TRUE
        WHEN OTHER
            PERFORM UPDATE-OPERATION
            SET EXIT-DETAIL TO TRUE
    END-EVALUATE.
```

#### 3. Add Input Validation
```cobol
VALIDATE-KEY-FIELDS.
    SET VALIDATION-OK TO TRUE
    
    IF ZAVOD = SPACES OR ZAVOD = LOW-VALUES
        DISPLAY "Plant code is required"
        SET VALIDATION-ERROR TO TRUE
    END-IF
    
    IF SKLAD = SPACES OR SKLAD = LOW-VALUES
        DISPLAY "Warehouse code is required"
        SET VALIDATION-ERROR TO TRUE
    END-IF
    
    IF MATER = SPACES OR MATER = LOW-VALUES
        DISPLAY "Material number is required"
        SET VALIDATION-ERROR TO TRUE
    END-IF.

VALIDATE-QUANTITY.
    IF MNOZ < 0
        DISPLAY "Quantity cannot be negative"
        SET VALIDATION-ERROR TO TRUE
    END-IF.
```

#### 4. Fix Logic Bugs
```cobol
* Fix NAVRAT processing (line 91)
IF NAVRAT 
   MOVE CORR STAVYF0 TO STAVYW1-O
   SET EXIT-DETAIL TO TRUE  * Instead of falling through
END-IF

* Fix ZRUSIT logic (lines 99-103)
IF ZRUSIT
   IF NALEZEN
      DELETE STAVY
      IF WS-STAVY-STATUS = "00"
         DISPLAY "Record deleted successfully"
         MOVE CORR STAVYF0 TO STAVYW1-O
      ELSE
         PERFORM CHECK-FILE-STATUS
      END-IF
   ELSE
      DISPLAY "Cannot delete - record does not exist"
   END-IF
   SET EXIT-DETAIL TO TRUE
END-IF
```

### 10.2 Medium Priority (Important)

#### 1. Add Record Locking for Updates
```cobol
READ-FOR-UPDATE.
    READ STAVY RECORD INTO STAVYF0
        INVALID KEY 
            SET NENALEZEN TO TRUE
        NOT INVALID KEY 
            SET NALEZEN TO TRUE
    END-READ
    
    IF WS-STAVY-STATUS = "9D"  * Record locked
        DISPLAY "Record in use by another user"
        PERFORM WAIT-AND-RETRY
    END-IF.
```

#### 2. Implement Audit Trail
```cobol
* Add to STAVYF0 structure:
01  STAVYF0-EXTENDED.
    05  STAVYF0-BASE.
        10  ZAVOD  ...
        10  SKLAD  ...
        10  MATER  ...
        10  MNOZ   ...
    05  CREATED-BY      PIC X(10).
    05  CREATED-DATE    PIC 9(8).
    05  CREATED-TIME    PIC 9(6).
    05  MODIFIED-BY     PIC X(10).
    05  MODIFIED-DATE   PIC 9(8).
    05  MODIFIED-TIME   PIC 9(6).

SET-AUDIT-FIELDS.
    ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
    ACCEPT WS-CURRENT-TIME FROM TIME
    MOVE FUNCTION CURRENT-USER TO WS-USERID
    
    IF OPERATION-TYPE = "INSERT"
        MOVE WS-USERID TO CREATED-BY
        MOVE WS-CURRENT-DATE TO CREATED-DATE
        MOVE WS-CURRENT-TIME TO CREATED-TIME
    END-IF
    
    MOVE WS-USERID TO MODIFIED-BY
    MOVE WS-CURRENT-DATE TO MODIFIED-DATE
    MOVE WS-CURRENT-TIME TO MODIFIED-TIME.
```

#### 3. Add Referential Integrity Checks
```cobol
VALIDATE-REFERENCE-DATA.
    * Check if plant exists
    PERFORM CHECK-ZAVOD-EXISTS
    IF NOT ZAVOD-VALID
        DISPLAY "Invalid plant code"
        SET VALIDATION-ERROR TO TRUE
    END-IF
    
    * Check if warehouse exists for plant
    PERFORM CHECK-SKLAD-EXISTS
    IF NOT SKLAD-VALID
        DISPLAY "Invalid warehouse code"
        SET VALIDATION-ERROR TO TRUE
    END-IF
    
    * Check if material exists
    PERFORM CHECK-MATER-EXISTS
    IF NOT MATER-VALID
        DISPLAY "Invalid material number"
        SET VALIDATION-ERROR TO TRUE
    END-IF.
```

#### 4. Modularize Paragraphs
Break down large paragraphs into smaller, single-responsibility units as shown in Priority 1, #2.

### 10.3 Low Priority (Enhancement)

#### 1. Add User Confirmation for Delete
```cobol
CONFIRM-DELETE.
    DISPLAY "Are you sure you want to delete this record? (Y/N)"
    ACCEPT WS-CONFIRMATION
    IF WS-CONFIRMATION = "Y" OR "y"
        SET DELETE-CONFIRMED TO TRUE
    ELSE
        SET DELETE-CANCELLED TO TRUE
    END-IF.
```

#### 2. Add Record Count Display
```cobol
DISPLAY-RECORD-COUNT.
    * Show user how many records for this ZAVOD/SKLAD
    PERFORM COUNT-INVENTORY-RECORDS
    DISPLAY "Total inventory items: " WS-RECORD-COUNT.
```

#### 3. Add Transaction Logging
```cobol
LOG-TRANSACTION.
    ACCEPT WS-TIMESTAMP FROM TIME
    OPEN EXTEND TRANSACTION-LOG
    WRITE TRANS-LOG-RECORD
    CLOSE TRANSACTION-LOG.
```

#### 4. Internationalization
```cobol
* Replace hard-coded Czech messages with message file lookups
DISPLAY-MESSAGE.
    PERFORM GET-MESSAGE-TEXT
    DISPLAY WS-MESSAGE-TEXT.

GET-MESSAGE-TEXT.
    * Read from message file based on language setting
    READ MESSAGE-FILE
        KEY IS WS-LANGUAGE WS-MESSAGE-ID
        INTO WS-MESSAGE-TEXT
    END-READ.
```

### 10.4 Documentation Improvements

1. **Add Program Header**
```cobol
      ******************************************************************
      * PROGRAM NAME: STAPOR                                           *
      * DESCRIPTION:  Inventory State Maintenance                      *
      * AUTHOR:       [Name]                                           *
      * DATE WRITTEN: [Date]                                           *
      * INPUTS:       STAVYW (Display file), STAVY (Database)          *
      * OUTPUTS:      Updated STAVY records                            *
      * CALLED BY:    User menu                                        *
      * CALLS:        None                                             *
      * MODIFICATION HISTORY:                                          *
      *   Date       Programmer    Description                         *
      *   ---------- ------------- ----------------------------------- *
      *   YYYY-MM-DD Initial Name  Initial creation                    *
      ******************************************************************
```

2. **Add Paragraph Documentation**
```cobol
      ******************************************************************
      * ZOBRAZIT-ZADANI                                                *
      * Purpose: Display input screen and retrieve record              *
      * Input:   User entry of ZAVOD, SKLAD, MATER                     *
      * Output:  Record retrieved or not-found indicator set           *
      * Calls:   None                                                  *
      * Called by: Main processing loop                                *
      ******************************************************************
```

3. **Add Field-Level Comments**
```cobol
01  STAVYF0.
    05  ZAVOD       PIC X(02).  * Plant code (01-99)
    05  SKLAD       PIC X(02).  * Warehouse code (01-99)
    05  MATER       PIC X(05).  * Material number (00001-99999)
    05  MNOZ        PIC 9(08).  * Quantity on hand (max 99,999,999)
```

---

## 11. Circular References Analysis

### 11.1 File Dependencies

**Analysis:** No circular file dependencies detected.

```
REF.PF (Reference)
    ↓
STAVY.PF (Database)
    ↓
STAPOR.CBLLE
    ↓
STAVYW.DSPF (Display)
```

**Finding:** Linear dependency chain, no circular references.

### 11.2 Paragraph Call Graph

**Analysis:** Program uses GO TO statements creating loop-back flow but not circular PERFORM chains.

```
UVODNI-AKCE
    ↓
ZOBRAZIT-ZADANI ←─────┐
    ↓                  │
ZNOVU-DRUHA ──────────┘
    ↓
KONEC-PROGRAMU
```

**Finding:** Loop-back via GO TO (lines 96, 102, 107) but not true circular PERFORM recursion. This is acceptable for main processing loop but should be restructured for clarity.

### 11.3 Data Structure Dependencies

**Analysis:** No circular data dependencies.

- STAVYW1-I feeds STAVYF0
- STAVYF0 feeds STAVYW2-O
- STAVYW2-I feeds STAVYF0
- STAVYF0 feeds STAVYW1-O

**Finding:** Bidirectional data flow between screens and database record, but this is normal and expected for CRUD operations. Not a circular dependency issue.

---

## 12. Modernization Path

### 12.1 Short-term Improvements (1-3 months)

1. **Add error handling** without changing program structure
2. **Fix logic bugs** (NAVRAT, ZRUSIT)
3. **Add input validation**
4. **Document existing code**
5. **Add file status variables**

**Effort:** LOW  
**Risk:** LOW  
**Benefit:** HIGH (immediate stability improvements)

### 12.2 Medium-term Improvements (3-6 months)

1. **Restructure control flow** (remove GO TO)
2. **Modularize paragraphs**
3. **Add record locking**
4. **Implement audit trail**
5. **Add referential integrity checks**

**Effort:** MEDIUM  
**Risk:** MEDIUM (requires testing)  
**Benefit:** HIGH (maintainability improvements)

### 12.3 Long-term Improvements (6-12 months)

1. **Migrate to ILE RPG** or modern language
2. **Implement web interface** (replace 5250)
3. **Add REST API** for integration
4. **Implement transaction logging**
5. **Add business intelligence/reporting**

**Effort:** HIGH  
**Risk:** HIGH (major rewrite)  
**Benefit:** VERY HIGH (future-proofing)

---

## 13. Comparison with Similar Programs

Based on the directory listing, similar programs exist:
- **STAOBR.CBLLE** - Likely inventory display program
- **STAOBR2.CBLLE** - Likely variant or enhanced version
- **STAOBR_2.CBLLE** - Another variant

### Recommended Analysis:
1. Compare error handling approaches across programs
2. Identify shared code patterns for library creation
3. Standardize naming conventions
4. Create common copybooks for shared logic

---

## 14. Summary and Action Plan

### 14.1 Program Assessment

| Category | Rating | Justification |
|----------|--------|---------------|
| **Functionality** | ⭐⭐⭐⭐ (4/5) | Provides complete CRUD operations |
| **Code Quality** | ⭐⭐ (2/5) | Uses GO TO, lacks error handling |
| **Maintainability** | ⭐⭐ (2/5) | Large paragraphs, minimal comments |
| **Reliability** | ⭐⭐ (2/5) | No error handling, logic bugs |
| **Security** | ⭐⭐ (2/5) | No locking strategy, no validation |
| **Performance** | ⭐⭐⭐⭐ (4/5) | Efficient file access |
| **Overall** | ⭐⭐⭐ (2.7/5) | **Needs significant improvement** |

### 14.2 Critical Action Items

#### Immediate (Week 1)
- [ ] Add FILE STATUS variables to all file definitions
- [ ] Add error handling for OPEN operations
- [ ] Fix NAVRAT logic bug (line 91)
- [ ] Fix ZRUSIT logic bug (lines 99-103)
- [ ] Document current code with comments

#### Short-term (Month 1)
- [ ] Add input validation for all user entry fields
- [ ] Add error handling for all database operations
- [ ] Add user confirmation for delete operations
- [ ] Create test cases and execute testing
- [ ] Add program header documentation

#### Medium-term (Months 2-3)
- [ ] Restructure ZNOVU-DRUHA paragraph into smaller units
- [ ] Replace GO TO with PERFORM UNTIL structures
- [ ] Add record locking for update operations
- [ ] Implement referential integrity checks
- [ ] Add audit trail fields and logic

#### Long-term (Months 4-6)
- [ ] Complete modularization of all paragraphs
- [ ] Add transaction logging
- [ ] Implement soft delete instead of hard delete
- [ ] Add business rules validation
- [ ] Create user manual and technical documentation

### 14.3 Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Production failure due to no error handling | HIGH | HIGH | Immediate: Add file status checks |
| Data loss from delete bug | MEDIUM | HIGH | Immediate: Fix ZRUSIT logic |
| Lost updates in multi-user | MEDIUM | MEDIUM | Short-term: Add record locking |
| Invalid data entry | HIGH | MEDIUM | Short-term: Add validation |
| Maintenance difficulty | MEDIUM | MEDIUM | Medium-term: Restructure code |

### 14.4 Estimated Effort

| Phase | Effort (Hours) | Resources | Duration |
|-------|---------------|-----------|----------|
| Immediate fixes | 16-24 | 1 developer | 1 week |
| Short-term improvements | 40-60 | 1 developer | 1 month |
| Medium-term improvements | 80-120 | 1 developer + tester | 2-3 months |
| Long-term modernization | 200-400 | Team | 6-12 months |

**Total estimated effort for stabilization: 136-204 hours (17-26 days)**

---

## 15. Conclusion

The STAPOR.CBLLE program is a functional inventory maintenance application that successfully provides CRUD operations for the STAVY (inventory states) file. However, it suffers from several critical issues common in legacy AS/400 COBOL code:

### Strengths:
- ✅ Clear purpose and functionality
- ✅ Proper use of AS/400 display files and database files
- ✅ Efficient indexed file access
- ✅ Good use of level-88 conditions
- ✅ Appropriate use of MOVE CORRESPONDING

### Critical Weaknesses:
- ❌ No error handling or FILE STATUS checking
- ❌ Unstructured control flow (heavy use of GO TO)
- ❌ Logic bugs (NAVRAT, ZRUSIT conditions)
- ❌ No input validation
- ❌ No record locking for updates
- ❌ Monolithic paragraph structure

### Recommendation:
**Priority: HIGH** - This program requires immediate attention to address critical stability and reliability issues. The short-term improvements should be implemented within the next month to prevent potential production issues. Medium-term improvements should follow to enhance maintainability and prepare for long-term modernization.

The program is currently in **maintenance mode** and requires refactoring before any significant functional enhancements should be considered.

---

## Appendices

### Appendix A: File Layouts

#### STAVYF0 Record Layout (from STAVY.PF)
```
Field Name    Type         Length  Decimals  Description
----------    ----         ------  --------  -----------
ZAVOD         CHAR         2                 Plant Code (Key)
SKLAD         CHAR         2                 Warehouse Code (Key)
MATER         CHAR         5                 Material Number (Key)
MNOZ          PACKED-DEC   8       0         Quantity
```

### Appendix B: Screen Layouts

#### STAVYW1 (Input Screen)
```
                    Zadejte údaje:


   Závod:      __
   Sklad:      __
   Material:   _____



F3=Konec                F12=Návrat
```

#### STAVYW2 (Detail Screen)
```
                                                      DD/MM/YY  HH:MM:SS

  Závod:       __
  Sklad:       __
  Material:    _____
  Množství:    ________



F3=Konec        F12=Návrat        F5=Obnova        F23=Zrušit
```

### Appendix C: Glossary

| Czech Term | English Translation | Description |
|------------|-------------------|-------------|
| STAVY | States/Inventory | Inventory state file |
| ZAVOD | Plant | Manufacturing plant/facility code |
| SKLAD | Warehouse | Warehouse/storage location code |
| MATER | Material | Material/item number |
| MNOZ | Quantity | Quantity amount |
| ZADANI | Entry/Input | Data entry screen |
| KONEC | End | Exit/terminate |
| NAVRAT | Return | Return to previous screen |
| OBNOVA | Refresh | Refresh/reload data |
| ZRUSIT | Cancel/Delete | Delete record |

### Appendix D: Related Files

- **STAVY.PF** - Physical file for inventory states
- **STAVYW.DSPF** - Display file for screens
- **REF.PF** - Reference file for field definitions
- **STAVYJ.LF** - Logical file (indexed view of STAVY)
- **STAVYP.PF** - Related physical file (purpose unknown)
- **STAOBR.CBLLE** - Related display program
- **STAOBR2.CBLLE** - Related display program variant

### Appendix E: Change History

| Date | Version | Author | Description |
|------|---------|--------|-------------|
| 2025-01-20 | 1.0 | Analysis Team | Initial comprehensive analysis |

---

**End of Analysis Document**

*Generated: 2025-01-20*  
*Document Version: 1.0*  
*Analysis Tool: Manual Code Review*  
*Reviewer: COBOL Expert Analyst*

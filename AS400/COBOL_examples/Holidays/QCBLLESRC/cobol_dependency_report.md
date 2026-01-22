# COBOL Dependency Analysis Report

**Analysis Date:** 2025-07-21  
**Directory:** `/home/runner/work/Cobol-Demo/Cobol-Demo/AS400/COBOL_examples/Holidays/QCBLLESRC`  
**Analyzer:** COBOL Dependency Subagent

---

## Executive Summary

This analysis examines the dependency structure of two COBOL programs in the Holidays module:
- **CANDAY01.CBLLE** - Canada Day Calculator (main program)
- **TESTCDAY.CBLLE** - Test program for CANDAY01

The codebase demonstrates a **simple, loosely-coupled architecture** with minimal dependencies, representing a well-structured modular design suitable for AS400 ILE COBOL.

---

## 1. Program Inventory

### CANDAY01.CBLLE (Canada Day Calculator)
- **Type:** Main program / Callable module
- **Lines of Code:** 161
- **Purpose:** Calculates the day of the week for Canada Day (July 1st) for any given year
- **Complexity:** Low-Medium
- **Entry Points:** 1 (MAIN-PROCEDURE)

### TESTCDAY.CBLLE (Test Program)
- **Type:** Test harness / Driver program
- **Lines of Code:** 48
- **Purpose:** Automated testing framework for CANDAY01
- **Complexity:** Low
- **Entry Points:** 1 (MAIN-PROCEDURE)

---

## 2. Dependency Analysis

### 2.1 External Dependencies

#### CANDAY01.CBLLE Dependencies:
```
External Dependencies: NONE
├── No COPY statements
├── No CALL statements
└── No external file access (files, databases)

Intrinsic Functions Used:
├── FUNCTION MOD
├── FUNCTION INTEGER-OF-DATE
└── Built-in ACCEPT/DISPLAY for I/O
```

**Analysis:** CANDAY01 is a **self-contained module** with zero external dependencies. This represents excellent modularity and portability.

#### TESTCDAY.CBLLE Dependencies:
```
External Dependencies: CANDAY01 (intended, not implemented)
├── No COPY statements
├── No CALL statements (commented as future implementation)
└── No external file access

Current State: STUB IMPLEMENTATION
```

**Analysis:** TESTCDAY is designed to test CANDAY01 but currently only provides a test framework stub. The intended dependency on CANDAY01 is documented but not yet implemented via CALL statement.

### 2.2 Dependency Graph

```
┌─────────────────┐
│  TESTCDAY       │
│  (Test Driver)  │
└────────┬────────┘
         │
         │ (Intended CALL - Not Implemented)
         ↓
┌─────────────────┐
│  CANDAY01       │
│  (Calculator)   │
└─────────────────┘
         │
         ↓
    [No Dependencies]
```

**Dependency Type:** One-way, hierarchical (test → application)  
**Coupling Level:** Loose (currently none; intended to be procedural CALL)

### 2.3 Data Flow Analysis

#### CANDAY01 Data Flow:
```
Input Sources:
├── WS-INPUT-YEAR (ACCEPT from user)
└── WS-CONTINUE-FLAG (ACCEPT from user)

Internal Data Transformations:
├── WS-INPUT-YEAR → WS-YEAR → WS-DATE-NUMERIC
├── WS-DATE-NUMERIC → FUNCTION INTEGER-OF-DATE → WS-DAY-OF-WEEK
└── WS-DAY-OF-WEEK → WS-DAY-ENTRY(index) → WS-DAY-NAME

Output Destinations:
└── DISPLAY statements (screen output only)
```

**Data Isolation:** All data is contained within WORKING-STORAGE SECTION. No LINKAGE SECTION exists, indicating the program is not currently designed to be called with parameters.

#### TESTCDAY Data Flow:
```
Input Sources:
└── Hard-coded test data (WS-TEST-YEARS array)

Data Processing:
└── WS-TEST-YEAR array iteration via WS-INDEX

Output Destinations:
└── DISPLAY statements (informational only)
```

---

## 3. Copybook Analysis

### 3.1 Current State
**No copybooks are used** in either program.

### 3.2 Potential Copybook Opportunities

The following data structures could be extracted into reusable copybooks:

1. **Day-of-Week Table** (from CANDAY01)
   ```cobol
   Suggested Copybook: DAYNAMES.CPY
   Contains: WS-DAY-TABLE and WS-DAY-NAMES structures
   Reusability: HIGH (useful for any date-related program)
   ```

2. **Date Calculation Fields** (from CANDAY01)
   ```cobol
   Suggested Copybook: DATEUTIL.CPY
   Contains: WS-CANADA-DATE, WS-DATE-NUMERIC, WS-DATE-FORMATTED
   Reusability: MEDIUM (date manipulation programs)
   ```

3. **Error Handling Fields** (from CANDAY01)
   ```cobol
   Suggested Copybook: ERRFLAGS.CPY
   Contains: WS-ERROR-FLAG, WS-CONTINUE-FLAG
   Reusability: MEDIUM (interactive programs)
   ```

---

## 4. Circular Dependency Analysis

### Finding: **NO CIRCULAR DEPENDENCIES**

**Rationale:**
- Only two programs exist in the module
- TESTCDAY is designed to call CANDAY01 (one-way dependency)
- CANDAY01 has no dependencies on TESTCDAY or any other module
- No COPY chains exist (no copybooks used)

**Risk Level:** ✅ **NONE** - No circular dependency risk detected.

---

## 5. Modularity Assessment

### 5.1 CANDAY01 Modularity

| Aspect | Rating | Comments |
|--------|--------|----------|
| Single Responsibility | ⭐⭐⭐⭐⭐ | Excellent - does one thing well |
| Cohesion | ⭐⭐⭐⭐⭐ | High - all code relates to Canada Day calculation |
| Coupling | ⭐⭐⭐⭐⭐ | Zero external coupling |
| Reusability | ⭐⭐⭐ | Medium - hardcoded for interactive use |
| Testability | ⭐⭐⭐ | Medium - requires refactoring for automated testing |

**Strengths:**
- Well-structured with clear section divisions
- Excellent use of PERFORM for procedural flow
- Good error handling and input validation
- Self-documenting code with meaningful variable names

**Weaknesses:**
- Not designed for programmatic calling (no LINKAGE SECTION)
- Interactive I/O makes automated testing difficult
- Tightly coupled to ACCEPT/DISPLAY paradigm

### 5.2 TESTCDAY Modularity

| Aspect | Rating | Comments |
|--------|--------|----------|
| Single Responsibility | ⭐⭐⭐⭐ | Clear purpose as test driver |
| Cohesion | ⭐⭐⭐ | Medium - stub implementation incomplete |
| Coupling | ⭐⭐⭐⭐ | Low - minimal dependencies (by design) |
| Reusability | ⭐⭐ | Low - specific to CANDAY01 testing |
| Testability | N/A | This IS the test program |

---

## 6. Legacy Patterns Identified

### 6.1 Patterns Affecting Dependencies

#### ✅ **Good Legacy Patterns:**

1. **Self-Contained Modules**
   - Location: Both programs
   - Impact: Positive - reduces dependency complexity
   - Pattern: Monolithic but functional

2. **Sectioned Procedures**
   - Location: CANDAY01 (GET-YEAR-INPUT, CALCULATE-DAY-OF-WEEK, etc.)
   - Impact: Positive - enables internal modularity
   - Pattern: Classic COBOL structured programming

3. **Intrinsic Functions**
   - Location: CANDAY01 (FUNCTION INTEGER-OF-DATE, MOD)
   - Impact: Positive - uses modern COBOL features instead of external routines
   - Pattern: ILE COBOL best practice

#### ⚠️ **Legacy Anti-Patterns:**

1. **Interactive I/O in Business Logic**
   - Location: CANDAY01 (ACCEPT/DISPLAY throughout)
   - Impact: Prevents reusability and automated testing
   - Recommendation: Separate I/O from calculation logic
   
   **Suggested Refactoring:**
   ```cobol
   Create two programs:
   - CANDAY01C (Calculator - uses LINKAGE SECTION)
   - CANDAY01I (Interactive wrapper - calls CANDAY01C)
   ```

2. **No Parameter Passing Interface**
   - Location: CANDAY01 (missing LINKAGE SECTION)
   - Impact: Cannot be called by other programs
   - Recommendation: Add USING clause support
   
   **Suggested Enhancement:**
   ```cobol
   PROCEDURE DIVISION USING WS-INPUT-YEAR WS-DAY-NAME.
   ```

3. **Hard-coded Test Data**
   - Location: TESTCDAY (WS-TEST-YEARS with FILLER values)
   - Impact: Limited test coverage flexibility
   - Recommendation: Read test data from external file or database

4. **Stub Test Implementation**
   - Location: TESTCDAY (TEST-YEAR section)
   - Impact: No actual testing occurs
   - Recommendation: Implement actual CALL to CANDAY01C

---

## 7. Recommendations

### 7.1 Immediate Actions (Priority: HIGH)

1. **Refactor CANDAY01 for Testability**
   ```
   Action: Split into two programs
   - CANDAY01C.CBLLE (Calculator core with LINKAGE SECTION)
   - CANDAY01I.CBLLE (Interactive interface)
   
   Benefits:
   - Enables automated testing via TESTCDAY
   - Improves reusability for batch processing
   - Maintains backward compatibility via wrapper
   ```

2. **Complete TESTCDAY Implementation**
   ```
   Action: Add CALL statement to invoke CANDAY01C
   Example:
     CALL 'CANDAY01C' USING WS-TEST-YEAR(WS-INDEX), WS-RESULT-DAY
   
   Benefits:
   - Enables true automated testing
   - Validates program behavior programmatically
   ```

### 7.2 Medium-Term Enhancements (Priority: MEDIUM)

3. **Create Shared Copybooks**
   ```
   Copybooks to Create:
   - DAYNAMES.CPY (day-of-week table)
   - DATEUTIL.CPY (date calculation fields)
   
   Benefits:
   - Promotes code reuse across holiday programs
   - Standardizes date handling
   - Reduces maintenance burden
   ```

4. **Add Error Code Standardization**
   ```
   Action: Create ERRCODES.CPY with standard return codes
   
   Benefits:
   - Consistent error handling across modules
   - Better integration with calling programs
   ```

### 7.3 Long-Term Strategy (Priority: LOW)

5. **Build Holiday Calculation Library**
   ```
   Suggested Programs:
   - CANDAY01C (Canada Day - already exists)
   - USAIND01C (US Independence Day)
   - UKTHNKS01C (UK Thanksgiving)
   - HOLIDAY-LIB (Service program wrapper)
   
   Benefits:
   - Centralized holiday logic
   - Shared by multiple applications
   - ILE service program integration
   ```

6. **External Test Data Management**
   ```
   Action: Replace hard-coded test data with file/DB table
   
   Benefits:
   - Flexible test case management
   - Easier test maintenance
   - Support for regression testing
   ```

---

## 8. Dependency Risk Assessment

### Risk Matrix

| Risk Category | Level | Mitigation Status |
|---------------|-------|-------------------|
| Circular Dependencies | 🟢 NONE | N/A - No risk present |
| Missing Dependencies | 🟢 NONE | All intrinsic functions available |
| Tight Coupling | 🟢 LOW | Programs are independent |
| Copybook Chains | 🟢 NONE | No copybooks used |
| Version Conflicts | 🟢 NONE | No shared resources |
| Unreachable Code | 🟢 NONE | All sections are called |

**Overall Risk Level:** 🟢 **LOW**

---

## 9. Code Quality Metrics

### Dependency Metrics

| Metric | CANDAY01 | TESTCDAY | Ideal Range |
|--------|----------|----------|-------------|
| External CALL count | 0 | 0 (should be 1) | 0-3 |
| COPY statement count | 0 | 0 | 2-5 |
| Intrinsic function count | 2 | 0 | 1-5 |
| Afferent coupling (Ca) | 1* | 0 | Varies |
| Efferent coupling (Ce) | 0 | 1* | Varies |
| Instability (Ce/(Ce+Ca)) | 0.0 | 1.0 | 0.3-0.7 |

\* = Intended dependency, not yet implemented

### Analysis:
- **CANDAY01** is maximally stable (I=0.0) - good for library module
- **TESTCDAY** is maximally unstable (I=1.0) - appropriate for test driver
- Both programs show healthy separation of concerns

---

## 10. Conclusion

The Canada Day Calculator module demonstrates **excellent independence** with minimal dependency complexity. The architecture follows a simple test-driven approach with clear separation between the calculator logic (CANDAY01) and test harness (TESTCDAY).

### Key Strengths:
✅ Zero circular dependencies  
✅ Self-contained modules with clear purposes  
✅ Modern COBOL intrinsic function usage  
✅ Good internal structure with sectioned procedures  
✅ Low coupling, high cohesion design  

### Key Opportunities:
⚠️ CANDAY01 needs LINKAGE SECTION for testability  
⚠️ TESTCDAY requires implementation of actual CALL logic  
⚠️ Potential for copybook creation to support future holiday modules  
⚠️ Interactive I/O should be separated from business logic  

### Dependency Health Score: **8.5/10**

The codebase is in excellent shape from a dependency perspective, with straightforward opportunities for enhancement that would support growth of the holiday calculation subsystem.

---

## Appendix A: Dependency Tree (Full Detail)

```
Holiday Calculation Module
│
├── CANDAY01.CBLLE (Canada Day Calculator)
│   ├── Dependencies: NONE
│   ├── Uses: FUNCTION MOD, FUNCTION INTEGER-OF-DATE
│   ├── I/O: ACCEPT (keyboard), DISPLAY (screen)
│   └── Called By: TESTCDAY (intended)
│
└── TESTCDAY.CBLLE (Test Program)
    ├── Dependencies: CANDAY01 (intended, not implemented)
    ├── Uses: None
    ├── I/O: DISPLAY (screen)
    └── Called By: None (top-level test driver)
```

---

## Appendix B: Refactoring Example

### Current State (CANDAY01):
```cobol
PROCEDURE DIVISION.
MAIN-PROCEDURE.
    PERFORM GET-YEAR-INPUT.
    PERFORM CALCULATE-DAY-OF-WEEK.
```

### Recommended State (CANDAY01C - Callable Version):
```cobol
LINKAGE SECTION.
01 LK-INPUT-YEAR     PIC 9(4).
01 LK-DAY-NAME       PIC X(9).
01 LK-RETURN-CODE    PIC S9(4) COMP.

PROCEDURE DIVISION USING LK-INPUT-YEAR 
                         LK-DAY-NAME 
                         LK-RETURN-CODE.
MAIN-PROCEDURE.
    MOVE LK-INPUT-YEAR TO WS-YEAR.
    PERFORM CALCULATE-DAY-OF-WEEK.
    MOVE WS-DAY-NAME TO LK-DAY-NAME.
    MOVE 0 TO LK-RETURN-CODE.
    GOBACK.
```

### Recommended Test Call (TESTCDAY):
```cobol
01 WS-RETURN-CODE    PIC S9(4) COMP.
01 WS-RESULT-DAY     PIC X(9).

PROCEDURE DIVISION.
TEST-YEAR SECTION.
    CALL 'CANDAY01C' USING WS-TEST-YEAR(WS-INDEX)
                           WS-RESULT-DAY
                           WS-RETURN-CODE.
    IF WS-RETURN-CODE = 0
        DISPLAY 'Test passed: ' WS-RESULT-DAY
    ELSE
        DISPLAY 'Test failed with code: ' WS-RETURN-CODE.
```

---

**Report Generated By:** COBOL Dependency Analysis Subagent  
**Report Version:** 1.0  
**Last Updated:** 2025-07-21

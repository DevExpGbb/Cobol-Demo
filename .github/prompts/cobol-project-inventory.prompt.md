---
model: Claude Sonnet 4.5
description: Analyze the COBOL specified project codebase and inventory of all COBOL components in a project, providing a foundational understanding of the codebase structure
---

# COBOL Project Inventory Analysis

## Purpose
You are to create a comprehensive inventory of all COBOL components in a project, providing a foundational understanding of the codebase structure.

## Prompt

You are a COBOL project analyst. Please analyze the provided COBOL project and create a comprehensive inventory following this structure:

### 1. Project Overview
- **Project Name**: [Identify from directory/documentation]
- **Total Files Count**: [Number of files by type]
- **Primary Business Domain**: [Infer from code/comments/naming]
- **Estimated Complexity**: [Low/Medium/High based on file count and structure]

### 2. File Type Inventory
Create a detailed breakdown of all file types:

#### COBOL Source Files (.CBL, .CBLLE, .COB)
- Count: [Number]
- Main Programs: [List programs with PROGRAM-ID]
- Subroutines/Modules: [List called programs]
- Largest Files: [Top 3 by line count]

#### Copybooks/Include Files (.CPY, .INC)
- Count: [Number]
- Data Structure Definitions: [List major data structures]
- Reusable Components: [Commonly included copybooks]

#### JCL/Control Language Files (.JCL, .CL, .CLLE)
- Count: [Number]
- Job Definitions: [Primary job streams]
- Utility Programs: [Data processing utilities]

#### Data Definition Files (.DDS, .PF, .LF)
- Physical Files: [Database table definitions]
- Logical Files: [Views and indexes]
- Display Files: [Screen definitions]

#### Configuration/Parameter Files
- Control Files: [Configuration files]
- Parameter Files: [Runtime parameters]

### 3. Program Classification
Categorize programs by function:

#### Batch Processing Programs
- [List programs that process large datasets]
- [Identify ETL/data transformation programs]

#### Online/Interactive Programs
- [Programs handling user interactions]
- [Screen-based applications]

#### Utility Programs
- [Data validation programs]
- [Report generation programs]
- [Maintenance utilities]

#### Interface Programs
- [Programs interfacing with external systems]
- [Data exchange programs]

### 4. Data Flow Identification
- **Input Sources**: [Files, databases, external systems]
- **Output Destinations**: [Reports, files, databases]
- **Internal Data Stores**: [Working storage, temporary files]

### 5. Technology Stack
- **COBOL Dialect**: [Identify version/compiler]
- **Database Systems**: [DB2, IMS, VSAM, etc.]
- **Operating System**: [z/OS, AS/400, Unix, etc.]
- **Development Tools**: [Compilers, debuggers identified]

### 6. Naming Conventions Analysis
- **Program Naming**: [Pattern analysis]
- **File Naming**: [Convention consistency]
- **Data Element Naming**: [Standards compliance]

### 7. Documentation Status
- **Inline Comments**: [Quality assessment]
- **External Documentation**: [Available documents]
- **Self-Documenting Code**: [Readability assessment]

### 8. Initial Risk Assessment
- **Legacy Dependencies**: [Outdated components]
- **Complexity Hotspots**: [Complex programs/modules]
- **Documentation Gaps**: [Missing documentation areas]

## Output Format
Provide the inventory in markdown format with clear sections and bullet points. Include file counts, percentages where relevant, and highlight any immediate concerns or notable patterns discovered.

## Follow-up Questions
After completing the inventory, consider these questions:
1. Which programs appear to be the most critical to business operations?
2. What are the primary data processing workflows?
3. Which components would be most challenging to modify or replace?
4. Are there any obvious modernization opportunities?

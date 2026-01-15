---
model: Claude Sonnet 4
description: Create a comprehensive README.md file for a COBOL program with detailed structure and content.
---
# COBOL Program Documentation Generator Prompt

Use this prompt template to create comprehensive README documentation for COBOL programs. Replace the placeholders in [brackets] with program-specific information.

Write the README documentation in the same directory as the COBOL code, in a file named `README.md`. If the file already exists, update the documentation with your new results.

---

## Prompt Template

```
Create a comprehensive README.md file for a COBOL program with the following structure and content:

# [Program Name/Title]

## Overview
[Provide a clear, concise description of what the program does, its purpose, and its target environment (AS400, OpenCOBOL, etc.)]

## Programs

### [MAIN_PROGRAM.CBLLE]
**[Brief Program Description]**

#### Features:
- [List key features and capabilities]
- [Include user interface type (batch, interactive, etc.)]
- [Mention data processing capabilities]
- [Note validation and error handling features]
- [Other notable functionality]

#### Usage:
1. [Step-by-step instructions for running the program]
2. [Input requirements and formats]
3. [Expected outputs and results]
4. [How to exit or continue operations]

#### Sample Output:
```
[Include realistic sample output showing program execution]
[Show typical user interaction]
[Display formatted results]
```

### [ADDITIONAL_PROGRAMS.CBLLE] (if applicable)
**[Description of supporting programs]**

[Brief description of test programs, utilities, or related modules]

## Technical Details

### [Core Algorithm/Process Name]
[Describe the main business logic or computational approach]
- [List key COBOL functions, intrinsics, or techniques used]
- [Explain data processing methods]
- [Detail calculation or transformation logic]

### Key Features:
- **[Feature Category]**: [Description of implementation]
- **[Feature Category]**: [Description of implementation]
- **[Feature Category]**: [Description of implementation]
- **[Feature Category]**: [Description of implementation]

### Data Structures:
- [List important data structures, tables, or arrays]
- [Describe file layouts or record formats]
- [Note working storage elements]
- [Mention control flags or counters]

## [Domain/Business Context] (if applicable)
[Provide relevant business or historical context]
- **[Business Rule/Context]**: [Explanation]
- **[Data Range/Scope]**: [Coverage details]
- **[Practical Application]**: [Real-world usage]

## Compilation
For AS400 environments:
```
[AS400 compilation commands]
```

For Open COBOL environments:
```
[OpenCOBOL compilation commands]
```

## Dependencies
- [List required copybooks, includes, or libraries]
- [Note external files or databases]
- [Mention system requirements]
- [Identify any external programs called]

## Future Enhancements
- [List potential improvements or extensions]
- [Suggest related functionality]
- [Note scalability considerations]
- [Identify integration opportunities]

## Testing
[Describe testing approach and available test programs]

---
**Created**: [Date]  
**Author**: [Author/Team Name]  
**Version**: [Version Number]
```

## Usage Instructions

1. **Analyze the COBOL Program**: Before using this template, examine the source code to understand:
   - Program purpose and business logic
   - Input/output requirements
   - Data structures and processing flow
   - Error handling and validation
   - Key algorithms or calculations

2. **Gather Context Information**:
   - Business domain or application area
   - Target environment (AS400, OpenCOBOL, etc.)
   - Related programs or dependencies
   - Historical or regulatory context

3. **Fill in the Template**:
   - Replace all [bracketed placeholders] with specific information
   - Remove sections that don't apply to your program
   - Add additional sections if needed for complex programs

4. **Create Sample Output**:
   - Run the program if possible to capture real output
   - Create realistic sample data and interactions
   - Show both successful execution and error scenarios

5. **Technical Details**:
   - Extract key COBOL language features used
   - Document important data structures from DATA DIVISION
   - Explain complex business logic from PROCEDURE DIVISION
   - Note any unusual or advanced techniques

## Customization Tips

- **For Batch Programs**: Focus more on file processing, record layouts, and batch control
- **For Interactive Programs**: Emphasize user interface, screen flows, and validation
- **For Utility Programs**: Document parameters, return codes, and integration points
- **For Legacy Programs**: Add historical context, migration considerations, and modernization notes

## Quality Checklist

- [ ] Clear, non-technical overview for business users
- [ ] Step-by-step usage instructions
- [ ] Realistic sample output
- [ ] Technical implementation details for developers
- [ ] Complete compilation instructions
- [ ] Accurate dependency information
- [ ] Meaningful future enhancement suggestions
- [ ] Proper formatting and structure

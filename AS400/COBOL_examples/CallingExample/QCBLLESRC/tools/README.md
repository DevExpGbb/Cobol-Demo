# COBOL Development Tools

This directory contains automation tools and templates to streamline COBOL development and maintenance for the CallingExample project.

## 🔧 Available Tools

### 1. Dependency Scanner (`dependency-scanner.ps1`)
PowerShell script for automated COBOL dependency analysis.

**Features:**
- Scans COBOL source files for program dependencies
- Extracts variable definitions and references
- Generates reports in multiple formats (Markdown, JSON, CSV)
- Creates Mermaid dependency diagrams

**Usage:**
```powershell
# Basic scan with markdown output
.\dependency-scanner.ps1 -SourcePath ".." -OutputFormat "markdown"

# Export to JSON file
.\dependency-scanner.ps1 -SourcePath ".." -OutputFormat "json" -OutputFile "dependencies.json"

# Export to CSV for analysis
.\dependency-scanner.ps1 -SourcePath ".." -OutputFormat "csv" -OutputFile "dependencies.csv"
```

### 2. Variable Validator (`validate-variables.sh`)
Bash script for checking undefined variable references and code quality issues.

**Features:**
- Detects undefined variable references
- Checks for missing periods
- Validates comment alignment
- Identifies inconsistent indentation
- Comprehensive code quality analysis

**Usage:**
```bash
# Validate all COBOL files
./validate-variables.sh *.CBLLE

# Validate specific files
./validate-variables.sh CALLER.CBLLE HERON.CBLLE

# Check single program
./validate-variables.sh HERON.CBLLE
```

**Sample Output:**
```
COBOL Variable Reference Validator
==================================

Validating: CALLER.CBLLE
------------
  ⚠️  UNDEFINED VARIABLE: PLOCHA-DISPLAYED (referenced at lines: 15)
  ❌ Found 1 undefined variable reference(s)

Additional Code Quality Checks:
  ⚠️  Potential missing periods: 2 lines
```

### 3. Program Template (`PROGRAM-TEMPLATE.CBLLE`)
Standardized COBOL program template following AS400 best practices.

**Features:**
- Comprehensive documentation headers
- Standard error handling patterns
- Structured program flow (Initialize → Process → Terminate)
- Debug mode support
- Consistent variable naming conventions

**Usage:**
1. Copy template to new program file
2. Replace placeholders with actual values:
   - `[PROGRAM-NAME]` → Your program name
   - `[DEVELOPER-NAME]` → Your name
   - `[CREATION-DATE]` → Current date
   - `[Brief description]` → Program purpose
3. Add your specific business logic in appropriate sections

## 📖 Documentation

### Build Instructions (`BUILD_INSTRUCTIONS.md`)
Comprehensive guide for compiling and deploying COBOL programs on AS400.

**Includes:**
- Individual and batch compilation commands
- CL program for automated builds
- JCL templates for z/OS
- Makefile for modern development
- Deployment scripts
- Common error solutions
- Performance optimization tips

## 🚀 Quick Start

1. **Validate existing code:**
   ```bash
   cd /path/to/QCBLLESRC
   ./tools/validate-variables.sh *.CBLLE
   ```

2. **Generate dependency report:**
   ```powershell
   cd /path/to/QCBLLESRC
   .\tools\dependency-scanner.ps1 -SourcePath "." -OutputFormat "markdown" -OutputFile "current-dependencies.md"
   ```

3. **Create new program from template:**
   ```bash
   cp tools/PROGRAM-TEMPLATE.CBLLE NEW-PROGRAM.CBLLE
   # Edit NEW-PROGRAM.CBLLE and replace placeholders
   ```

4. **Build programs:**
   ```bash
   # Follow instructions in tools/BUILD_INSTRUCTIONS.md
   ```

## 🎯 Integration with Development Workflow

### Pre-commit Validation
Add to your development process:
```bash
#!/bin/bash
# pre-commit-hook.sh
echo "Running COBOL validation..."
if ./tools/validate-variables.sh *.CBLLE; then
    echo "✅ All validations passed"
    exit 0
else
    echo "❌ Validation failed - please fix issues before committing"
    exit 1
fi
```

### Continuous Integration
Example CI/CD pipeline step:
```yaml
- name: Validate COBOL Code
  run: |
    cd AS400/COBOL_examples/CallingExample/QCBLLESRC
    ./tools/validate-variables.sh *.CBLLE
    
- name: Generate Dependency Report
  run: |
    cd AS400/COBOL_examples/CallingExample/QCBLLESRC
    pwsh -File tools/dependency-scanner.ps1 -SourcePath "." -OutputFormat "json" -OutputFile "build-artifacts/dependencies.json"
```

## 🔄 Tool Enhancement Ideas

### Future Improvements
1. **Performance Profiler**: Tool to analyze COBOL program performance
2. **Copybook Generator**: Extract common data structures into copybooks
3. **Code Metrics Dashboard**: Visual representation of code quality metrics
4. **Migration Assistant**: Help migrate legacy patterns to modern COBOL
5. **Test Generator**: Automatically create unit tests from program specifications
6. **Documentation Generator**: Extract documentation from source code comments

### Contributing
To add new tools or enhance existing ones:
1. Follow the established naming conventions
2. Add comprehensive documentation
3. Include usage examples
4. Test thoroughly with the CallingExample programs
5. Update this README with new tool information

## 📋 Tool Requirements

### Dependencies
- **PowerShell 5.1+** (for dependency-scanner.ps1)
- **Bash** (for validate-variables.sh)
- **AS400/IBM i system** (for build instructions)
- **COBOL compiler** (ILE COBOL recommended)

### Permissions
- Read access to COBOL source files
- Write access for output files
- AS400 system access for compilation and deployment

---

*Last Updated: 2025-07-21*  
*Tool Version: v1.0.0*  
*Supports: AS400 COBOL, ILE COBOL*
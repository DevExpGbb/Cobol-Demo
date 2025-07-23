#!/bin/bash
# validate-variables.sh - Check for undefined variable references in COBOL programs
# Usage: ./validate-variables.sh [file1.CBLLE] [file2.CBLLE] ...
#        ./validate-variables.sh *.CBLLE

echo "COBOL Variable Reference Validator"
echo "=================================="

validate_cobol_variables() {
    local file="$1"
    echo ""
    echo "Validating: $file"
    echo "$(printf '%*s' "${#file}" | tr ' ' '-')"
    
    if [[ ! -f "$file" ]]; then
        echo "ERROR: File not found: $file"
        return 1
    fi
    
    # Extract defined variables (level 01, 77, etc.)
    # Look for lines starting with whitespace, followed by level number, then variable name
    defined_vars=$(grep -E "^\s*(01|77|[0-9]{2})\s+([A-Z0-9-]+)" "$file" | \
                  sed -E 's/^\s*[0-9]{2}\s+([A-Z0-9-]+).*/\1/' | \
                  sort -u)
    
    # Extract referenced variables in various statements
    # MOVE, DISPLAY, COMPUTE, ADD, SUBTRACT, etc.
    referenced_vars=$(grep -iE "(MOVE|DISPLAY|COMPUTE|ADD|SUBTRACT|MULTIPLY|DIVIDE)\s+" "$file" | \
                     grep -oE "\b[A-Z0-9-]+\b" | \
                     grep -E "^[A-Z]" | \
                     sort -u)
    
    # Find undefined references
    undefined_count=0
    for var in $referenced_vars; do
        # Skip COBOL reserved words and keywords
        if [[ "$var" =~ ^(TO|FROM|BY|INTO|GIVING|USING|VALUE|PICTURE|PIC|PACKED-DECIMAL|COMP|DISPLAY|ZERO|ZEROS|SPACE|SPACES|LOW-VALUE|HIGH-VALUE|FUNCTION|SQRT|PROGRAM|CALL|GOBACK|STOP|RUN|IF|ELSE|END-IF|PERFORM|UNTIL|TIMES|VARYING)$ ]]; then
            continue
        fi
        
        # Skip numeric literals and string literals
        if [[ "$var" =~ ^[0-9]+$ ]] || [[ "$var" =~ ^\".*\"$ ]]; then
            continue
        fi
        
        # Check if variable is defined
        if ! echo "$defined_vars" | grep -q "^$var$"; then
            line_nums=$(grep -n "\b$var\b" "$file" | cut -d: -f1 | tr '\n' ',' | sed 's/,$//')
            echo "  ⚠️  UNDEFINED VARIABLE: $var (referenced at lines: $line_nums)"
            ((undefined_count++))
        fi
    done
    
    if [[ $undefined_count -eq 0 ]]; then
        echo "  ✅ No undefined variable references found"
    else
        echo "  ❌ Found $undefined_count undefined variable reference(s)"
    fi
    
    # Additional checks
    echo ""
    echo "Additional Code Quality Checks:"
    
    # Check for missing periods
    missing_periods=$(grep -n "^\s*[A-Z].*[^.]$" "$file" | grep -v "^\s*\*" | wc -l)
    if [[ $missing_periods -gt 0 ]]; then
        echo "  ⚠️  Potential missing periods: $missing_periods lines"
    fi
    
    # Check for inconsistent indentation
    inconsistent_indent=$(grep -n "^[A-Z]" "$file" | grep -v "IDENTIFICATION\|PROGRAM-ID\|WORKING-STORAGE\|PROCEDURE\|LINKAGE" | wc -l)
    if [[ $inconsistent_indent -gt 0 ]]; then
        echo "  ⚠️  Inconsistent indentation: $inconsistent_indent lines"
    fi
    
    # Check for comments alignment (should start in column 7 for fixed format)
    misaligned_comments=$(grep -n "^\s*\*" "$file" | grep -v "^\s\s\s\s\s\s\*" | wc -l)
    if [[ $misaligned_comments -gt 0 ]]; then
        echo "  ⚠️  Misaligned comments: $misaligned_comments lines"
    fi
    
    return $undefined_count
}

# Main execution
total_errors=0
files_processed=0

if [[ $# -eq 0 ]]; then
    # If no arguments provided, scan all .CBLLE files in current directory
    files=(*.CBLLE)
    if [[ ! -f "${files[0]}" ]]; then
        echo "No .CBLLE files found in current directory"
        exit 1
    fi
else
    files=("$@")
fi

for file in "${files[@]}"; do
    validate_cobol_variables "$file"
    error_count=$?
    total_errors=$((total_errors + error_count))
    files_processed=$((files_processed + 1))
done

echo ""
echo "======================================"
echo "Validation Summary:"
echo "  Files processed: $files_processed"
echo "  Total errors found: $total_errors"

if [[ $total_errors -eq 0 ]]; then
    echo "  ✅ All files passed validation!"
    exit 0
else
    echo "  ❌ Issues found that need attention"
    exit 1
fi
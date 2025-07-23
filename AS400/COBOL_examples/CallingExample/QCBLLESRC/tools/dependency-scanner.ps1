# dependency-scanner.ps1 - Automated COBOL Dependency Analysis
# Usage: .\dependency-scanner.ps1 -SourcePath "QCBLLESRC" -OutputFormat "json"

param(
    [string]$SourcePath = ".",
    [string]$OutputFormat = "markdown", # Options: markdown, json, csv
    [string]$OutputFile = ""
)

function Scan-CobolDependencies {
    param([string]$Path)
    
    Write-Host "Scanning COBOL dependencies in: $Path" -ForegroundColor Green
    
    $programs = Get-ChildItem -Path $Path -Filter "*.CBLLE" -Recurse
    $dependencies = @()
    $programInfo = @{}
    
    foreach ($program in $programs) {
        $content = Get-Content $program.FullName
        $programName = ($program.BaseName).ToUpper()
        $lineCount = $content.Length
        
        Write-Host "  Analyzing: $programName ($lineCount lines)" -ForegroundColor Cyan
        
        # Store program info
        $programInfo[$programName] = @{
            FileName = $program.Name
            LineCount = $lineCount
            Path = $program.FullName
        }
        
        # Extract CALL statements
        $calls = $content | Select-String -Pattern 'CALL\s+(PROGRAM\s+)?"([^"]+)"' -AllMatches
        foreach ($call in $calls) {
            $calledProgram = $call.Matches[0].Groups[2].Value
            $dependencies += [PSCustomObject]@{
                Caller = $programName
                Called = $calledProgram.ToUpper()
                Type = if ($call.Matches[0].Groups[1].Value) { "External" } else { "Nested" }
                Line = $call.LineNumber
                Statement = $call.Line.Trim()
            }
        }
        
        # Extract variable definitions
        $variables = $content | Select-String -Pattern '^\s*[0-9]{2}\s+([A-Z0-9-]+)\s+' -AllMatches
        $programInfo[$programName].Variables = @()
        foreach ($var in $variables) {
            $varName = $var.Matches[0].Groups[1].Value
            $programInfo[$programName].Variables += [PSCustomObject]@{
                Name = $varName
                Line = $var.LineNumber
                Definition = $var.Line.Trim()
            }
        }
    }
    
    return @{
        Dependencies = $dependencies
        Programs = $programInfo
        ScanTime = Get-Date
    }
}

function Export-Results {
    param(
        [object]$Results,
        [string]$Format,
        [string]$OutputFile
    )
    
    switch ($Format.ToLower()) {
        "json" {
            $jsonOutput = $Results | ConvertTo-Json -Depth 4
            if ($OutputFile) {
                $jsonOutput | Out-File $OutputFile -Encoding UTF8
                Write-Host "Results exported to: $OutputFile" -ForegroundColor Green
            } else {
                Write-Output $jsonOutput
            }
        }
        "csv" {
            $csvFile = if ($OutputFile) { $OutputFile } else { "dependencies.csv" }
            $Results.Dependencies | Export-Csv $csvFile -NoTypeInformation
            Write-Host "Dependencies exported to: $csvFile" -ForegroundColor Green
        }
        "markdown" {
            $mdOutput = Generate-MarkdownReport $Results
            if ($OutputFile) {
                $mdOutput | Out-File $OutputFile -Encoding UTF8
                Write-Host "Markdown report exported to: $OutputFile" -ForegroundColor Green
            } else {
                Write-Output $mdOutput
            }
        }
        default {
            Write-Error "Unsupported output format: $Format"
        }
    }
}

function Generate-MarkdownReport {
    param([object]$Results)
    
    $report = @"
# COBOL Dependency Analysis Report

**Generated:** $($Results.ScanTime)

## Program Summary

| Program | Lines | Variables | Calls Made |
|---------|-------|-----------|------------|
"@

    foreach ($program in $Results.Programs.Keys) {
        $info = $Results.Programs[$program]
        $callsMade = ($Results.Dependencies | Where-Object { $_.Caller -eq $program }).Count
        $report += "`n| $program | $($info.LineCount) | $($info.Variables.Count) | $callsMade |"
    }

    $report += @"

## Dependencies

| Caller | Called | Type | Line | Statement |
|--------|--------|------|------|-----------|
"@

    foreach ($dep in $Results.Dependencies) {
        $report += "`n| $($dep.Caller) | $($dep.Called) | $($dep.Type) | $($dep.Line) | ``$($dep.Statement)`` |"
    }

    # Generate Mermaid diagram
    $report += @"

## Dependency Diagram

``````mermaid
graph TB
"@

    $nodeId = 65 # Start with 'A'
    $nodeMap = @{}
    
    # Create nodes for all programs
    foreach ($program in $Results.Programs.Keys) {
        $nodeChar = [char]$nodeId
        $nodeMap[$program] = $nodeChar
        $report += "`n    $nodeChar[$program]"
        $nodeId++
    }
    
    # Add dependencies
    foreach ($dep in $Results.Dependencies) {
        $callerNode = $nodeMap[$dep.Caller]
        $calledNode = $nodeMap[$dep.Called]
        if ($calledNode) {
            $report += "`n    $callerNode -->|$($dep.Type)| $calledNode"
        }
    }

    $report += "`n``````"
    
    return $report
}

# Main execution
try {
    $results = Scan-CobolDependencies -Path $SourcePath
    
    Write-Host "`nScan Complete!" -ForegroundColor Green
    Write-Host "  Programs found: $($results.Programs.Count)" -ForegroundColor Yellow
    Write-Host "  Dependencies found: $($results.Dependencies.Count)" -ForegroundColor Yellow
    
    Export-Results -Results $results -Format $OutputFormat -OutputFile $OutputFile
    
} catch {
    Write-Error "Error during scan: $($_.Exception.Message)"
    exit 1
}
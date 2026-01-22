# Product Requirements Document (PRD)

## Title
**Holiday Day-of-Week Calculator - Canada Day Edition**

---

## Introduction

The Holiday Day-of-Week Calculator is a specialized computational utility designed to determine which day of the week a specific holiday falls on for any given year. This initial version focuses on Canada Day (July 1st) and provides both an interactive user interface for individual queries and an automated testing framework to ensure accuracy.

The system is designed to support historical research, event planning, workforce scheduling, and any application requiring advance knowledge of holiday dates. The solution provides accurate day-of-week calculations for a wide temporal range (1600-3000) using industry-standard date calculation algorithms.

---

## Problem Statement

Organizations, event planners, HR departments, and government agencies frequently need to determine when national holidays fall within specific years to:

1. **Plan Events and Celebrations**: Knowing whether a holiday falls on a weekend or weekday affects venue bookings, attendance projections, and resource allocation.

2. **Schedule Workforce**: HR departments need to plan long weekends, shift schedules, and coverage well in advance when holidays fall on specific weekdays.

3. **Financial Planning**: Retail, hospitality, and service industries require advance knowledge of holiday timing for budgeting and inventory management.

4. **Historical Research**: Researchers need to accurately reconstruct calendars for historical analysis and documentation.

5. **Compliance**: Government and regulated industries must ensure accurate holiday observance across multi-year planning horizons.

**Current Challenge**: Manual calendar consultation is time-consuming and error-prone, especially for distant past or future dates. Existing calendar applications often lack the ability to quickly query specific holidays across multiple years or provide contextual information about holiday timing.

---

## Objectives

### Primary Objectives

1. **Accuracy**: Provide mathematically correct day-of-week calculations for Canada Day (July 1st) for any year within the supported range (1600-3000).

2. **Usability**: Enable non-technical users to quickly and easily determine holiday day-of-week information through an intuitive interface.

3. **Performance**: Deliver instant results with minimal computational overhead, supporting both interactive and batch processing modes.

4. **Extensibility**: Design the system architecture to support additional holidays and calendar systems in future releases.

### Secondary Objectives

5. **Educational Value**: Provide contextual information about holiday timing (e.g., weekend vs. weekday implications) to support decision-making.

6. **Quality Assurance**: Include comprehensive automated testing capabilities to ensure ongoing accuracy.

7. **Portability**: Maintain platform independence to support deployment across various operating systems and environments.

---

## Requirements

### Functional Requirements

#### FR1: Date Calculation Engine
- **FR1.1**: System shall calculate the day of the week for July 1st of any year between 1600 and 3000 (inclusive).
- **FR1.2**: System shall use a mathematically proven algorithm (e.g., Zeller's Congruence, integer-based date functions) to ensure accuracy.
- **FR1.3**: System shall correctly handle leap years and century transitions in calculations.
- **FR1.4**: System shall return day names in English: Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday.

#### FR2: Interactive User Interface
- **FR2.1**: System shall provide a console-based or GUI interface for user interaction.
- **FR2.2**: System shall prompt users to enter a year value.
- **FR2.3**: System shall validate user input and reject values outside the range 1600-3000.
- **FR2.4**: System shall display clear error messages for invalid input.
- **FR2.5**: System shall allow users to perform multiple queries in a single session.
- **FR2.6**: System shall provide an option to exit the application gracefully.

#### FR3: Result Presentation
- **FR3.1**: System shall display results in a clear, formatted output including:
  - The holiday name (Canada Day)
  - The date (July 1)
  - The year queried
  - The day of the week
- **FR3.2**: System shall provide contextual information about the result:
  - Whether the holiday falls on a weekend or weekday
  - Special messaging for specific days (Monday, Friday) that affect long weekends
- **FR3.3**: System shall format output for readability with appropriate spacing and visual separators.

#### FR4: Automated Testing Framework
- **FR4.1**: System shall include a test harness capable of running multiple test cases.
- **FR4.2**: Test framework shall support predefined test years including:
  - Recent years (e.g., 2024, 2025, 2026)
  - Historical significant years (e.g., 1867 - Canadian Confederation)
  - Future years for long-term planning (e.g., 2030)
- **FR4.3**: Test framework shall invoke the calculation engine programmatically (batch mode).
- **FR4.4**: Test framework shall validate results against expected outcomes.
- **FR4.5**: Test framework shall report test success/failure status.

#### FR5: Input Validation
- **FR5.1**: System shall validate year input is numeric.
- **FR5.2**: System shall validate year is within supported range (1600-3000).
- **FR5.3**: System shall provide specific error messages for different validation failures.
- **FR5.4**: System shall allow users to retry after invalid input without restarting the application.

### Non-Functional Requirements

#### NFR1: Performance
- **NFR1.1**: Day-of-week calculation shall complete in less than 100 milliseconds on standard hardware.
- **NFR1.2**: System shall support batch processing of at least 100 years per second.
- **NFR1.3**: Interactive response time (from input to display) shall not exceed 500 milliseconds.

#### NFR2: Reliability
- **NFR2.1**: System shall provide 100% accurate results for all dates within the supported range.
- **NFR2.2**: System shall handle edge cases correctly:
  - Leap years (divisible by 4, except century years not divisible by 400)
  - Century boundaries (1700, 1800, 1900, 2000, 2100, etc.)
  - Minimum boundary (year 1600)
  - Maximum boundary (year 3000)
- **NFR2.3**: System shall not crash or terminate abnormally for any valid input.

#### NFR3: Usability
- **NFR3.1**: User interface shall be intuitive enough for first-time users without training.
- **NFR3.2**: Error messages shall be clear and actionable.
- **NFR3.3**: Output shall be readable by non-technical users.
- **NFR3.4**: System shall provide helpful prompts and instructions throughout user interaction.

#### NFR4: Maintainability
- **NFR4.1**: Code shall be modular with clear separation of concerns:
  - Input handling module
  - Calculation engine module
  - Output formatting module
  - Testing framework module
- **NFR4.2**: Calculation algorithm shall be documented with references to mathematical basis.
- **NFR4.3**: System shall use standard libraries and functions where possible.
- **NFR4.4**: Code shall include comments explaining business logic and complex calculations.

#### NFR5: Portability
- **NFR5.1**: System shall avoid platform-specific dependencies where possible.
- **NFR5.2**: Date calculation shall not rely on system clock or locale settings.
- **NFR5.3**: System shall be implementable in multiple programming languages without algorithm changes.

#### NFR6: Extensibility
- **NFR6.1**: Architecture shall support addition of new holidays without modifying core calculation engine.
- **NFR6.2**: System shall support addition of new output formats (e.g., JSON, XML, CSV).
- **NFR6.3**: Design shall accommodate future internationalization requirements (multiple languages).

#### NFR7: Security
- **NFR7.1**: System shall not store sensitive user information.
- **NFR7.2**: System shall validate all input to prevent injection attacks or buffer overflows.
- **NFR7.3**: System shall handle unexpected input gracefully without exposing system internals.

---

## User Stories

### Epic 1: Individual Date Queries

**US1.1: As an event planner, I want to check what day Canada Day falls on in 2026, so I can book venues and plan celebrations accordingly.**

- **Acceptance Criteria**:
  - Given I launch the application
  - When I enter the year 2026
  - Then the system displays "Canada Day (July 1, 2026) falls on a Wednesday"
  - And provides contextual information about weekday implications

**US1.2: As a historical researcher, I want to determine what day Canada Day fell on during the year of Confederation (1867), so I can accurately document the first Canada Day observance.**

- **Acceptance Criteria**:
  - Given I am using the application
  - When I enter the year 1867
  - Then the system correctly calculates and displays the day of the week for July 1, 1867
  - And the result is historically accurate

**US1.3: As an HR manager, I want to check multiple years in one session, so I can plan long-term workforce schedules efficiently.**

- **Acceptance Criteria**:
  - Given I have completed a query for one year
  - When I choose to continue
  - Then the system prompts me for another year without restarting
  - And I can repeat this process as many times as needed

### Epic 2: Input Validation and Error Handling

**US2.1: As a user, I want to receive clear error messages when I enter an invalid year, so I understand what went wrong and how to correct it.**

- **Acceptance Criteria**:
  - Given I enter a year less than 1600 or greater than 3000
  - When the system validates my input
  - Then I see an error message: "Error: Please enter a year between 1600 and 3000"
  - And I am prompted to enter a new year without restarting

**US2.2: As a user, I want the system to prevent me from entering non-numeric values, so I don't receive unexpected errors.**

- **Acceptance Criteria**:
  - Given I enter non-numeric characters
  - When the system processes my input
  - Then I receive an appropriate error message
  - And the system remains stable and operational

### Epic 3: Contextual Information

**US3.1: As an event coordinator, I want to know if Canada Day falls on a weekend, so I can adjust my planning for higher attendance.**

- **Acceptance Criteria**:
  - Given Canada Day falls on Saturday or Sunday
  - When I query that year
  - Then the system displays "Canada Day is on a weekend! Perfect for celebrations!"
  - And I can use this information for event planning

**US3.2: As a business owner, I want to know if Canada Day falls on a Monday or Friday, so I can plan for extended long weekends.**

- **Acceptance Criteria**:
  - Given Canada Day falls on Monday
  - When I query that year
  - Then the system displays "Great way to start the week with a holiday!"
  - And I understand the long weekend implications

### Epic 4: Automated Testing and Quality Assurance

**US4.1: As a QA engineer, I want to run automated tests against multiple years, so I can verify the system's accuracy without manual testing.**

- **Acceptance Criteria**:
  - Given I run the test framework
  - When it processes the predefined test cases (2024, 2025, 2026, 1867, 2030)
  - Then each test executes successfully
  - And I receive a summary of test results

**US4.2: As a developer, I want the test framework to validate results against known correct answers, so I can detect regressions quickly.**

- **Acceptance Criteria**:
  - Given the test framework has expected results defined
  - When it runs calculations
  - Then it compares actual results to expected results
  - And reports any discrepancies clearly

### Epic 5: System Usability

**US5.1: As a first-time user, I want clear instructions and prompts, so I can use the system without referring to documentation.**

- **Acceptance Criteria**:
  - Given I launch the application for the first time
  - When the application starts
  - Then I see a clear title and purpose statement
  - And prompts that explain what input is expected

**US5.2: As a user, I want a clean and organized output format, so I can easily read and understand the results.**

- **Acceptance Criteria**:
  - Given I receive results from a query
  - When the system displays output
  - Then information is formatted with visual separators
  - And key information is highlighted or clearly labeled

---

## Acceptance Criteria

### System-Level Acceptance Criteria

#### AC1: Calculation Accuracy
- **AC1.1**: System correctly calculates day-of-week for all years 1600-3000 (verified through known historical dates and astronomical references).
- **AC1.2**: System produces identical results to verified calendar systems (e.g., perpetual calendar algorithms, astronomical databases).
- **AC1.3**: System correctly handles leap year calculations:
  - Years divisible by 4 are leap years
  - Exception: Years divisible by 100 are not leap years
  - Exception to exception: Years divisible by 400 are leap years
- **AC1.4**: Test cases for years 1600, 1700, 1800, 1900, 2000, 2100, 2400 pass with correct leap year handling.

#### AC2: Input Validation
- **AC2.1**: System rejects year < 1600 with error message.
- **AC2.2**: System rejects year > 3000 with error message.
- **AC2.3**: System handles non-numeric input gracefully without crashing.
- **AC2.4**: System allows retry after invalid input.
- **AC2.5**: Error messages are displayed clearly and guide user to correct input.

#### AC3: User Interface
- **AC3.1**: Application displays title and purpose on startup.
- **AC3.2**: Application prompts user clearly for year input.
- **AC3.3**: Application displays results in formatted, readable manner.
- **AC3.4**: Application provides option to continue or exit after each query.
- **AC3.5**: Application handles both uppercase and lowercase for continue/exit responses (Y/y/N/n).
- **AC3.6**: Application displays exit message when user chooses to quit.

#### AC4: Output Content
- **AC4.1**: Output includes holiday name (Canada Day).
- **AC4.2**: Output includes full date (July 1, YYYY).
- **AC4.3**: Output includes calculated day of the week.
- **AC4.4**: Output includes contextual "Fun Facts" based on day of week:
  - Weekend message for Saturday/Sunday
  - Weekday long weekend message for Monday-Friday
  - Special Monday message
  - Special Friday message

#### AC5: Testing Framework
- **AC5.1**: Test framework executes without manual intervention.
- **AC5.2**: Test framework processes all predefined test years (2024, 2025, 2026, 1867, 2030).
- **AC5.3**: Test framework invokes calculation engine correctly for each test case.
- **AC5.4**: Test framework validates results against expected values.
- **AC5.5**: Test framework reports pass/fail status for each test.
- **AC5.6**: Test framework provides summary upon completion.

#### AC6: Performance
- **AC6.1**: Single calculation completes in < 100ms.
- **AC6.2**: Interactive response (input to output) completes in < 500ms.
- **AC6.3**: Batch processing of 100 years completes in < 1 second.
- **AC6.4**: System remains responsive during all operations.

#### AC7: Reliability
- **AC7.1**: System does not crash for any valid input within range.
- **AC7.2**: System does not crash for invalid input.
- **AC7.3**: System produces consistent results across multiple runs with same input.
- **AC7.4**: System handles edge cases correctly:
  - Year 1600 (minimum boundary)
  - Year 3000 (maximum boundary)
  - Century years (1700, 1800, 1900, 2000, 2100, etc.)
  - Leap years and non-leap years

#### AC8: Modularity (for developers)
- **AC8.1**: Input validation logic is separate from calculation logic.
- **AC8.2**: Calculation engine can be invoked programmatically (batch mode).
- **AC8.3**: Output formatting is separate from calculation logic.
- **AC8.4**: Test framework can invoke calculation engine without user interaction.
- **AC8.5**: Day-of-week lookup (numeric to name) is table-driven and easily modifiable.

---

## Timeline

### Phase 1: Core Development (Weeks 1-3)
**Deliverables:**
- Calculation engine implementation
- Input validation module
- Interactive user interface
- Basic output formatting

**Milestones:**
- Week 1: Complete calculation algorithm and unit tests
- Week 2: Implement user interface and input validation
- Week 3: Integrate components and perform initial testing

### Phase 2: Testing Framework (Weeks 4-5)
**Deliverables:**
- Automated test harness
- Predefined test cases
- Result validation logic
- Test reporting functionality

**Milestones:**
- Week 4: Design and implement test framework architecture
- Week 5: Add test cases and validation, perform integration testing

### Phase 3: Enhancement and Polish (Week 6)
**Deliverables:**
- Contextual output messages
- Enhanced error handling
- User experience improvements
- Documentation

**Milestones:**
- Week 6 Day 1-2: Implement contextual messages and fun facts
- Week 6 Day 3-4: Enhance error messages and user guidance
- Week 6 Day 5: Complete documentation and user guide

### Phase 4: Quality Assurance (Week 7)
**Deliverables:**
- Comprehensive test coverage
- Performance benchmarking
- Edge case validation
- Bug fixes

**Milestones:**
- Week 7 Day 1-2: Execute full test suite and boundary testing
- Week 7 Day 3-4: Performance testing and optimization
- Week 7 Day 5: Final bug fixes and validation

### Phase 5: Release Preparation (Week 8)
**Deliverables:**
- Final testing and validation
- Release documentation
- Deployment package
- User training materials

**Milestones:**
- Week 8 Day 1-2: Final regression testing
- Week 8 Day 3-4: Documentation review and finalization
- Week 8 Day 5: Release approval and deployment

**Total Duration:** 8 weeks from project kickoff to production release

---

## Dependencies

### Technical Dependencies

#### TD1: Date Calculation Libraries
- **Dependency**: Standard library functions for date manipulation (e.g., intrinsic date functions, date conversion utilities)
- **Criticality**: HIGH
- **Mitigation**: If unavailable, implement custom algorithm based on Zeller's Congruence or similar proven method

#### TD2: User Interface Framework
- **Dependency**: Console I/O capabilities or GUI framework (depending on platform)
- **Criticality**: MEDIUM
- **Mitigation**: System can be implemented as command-line tool, desktop application, or web service

#### TD3: Testing Infrastructure
- **Dependency**: Unit testing framework or test runner
- **Criticality**: MEDIUM
- **Mitigation**: Can implement custom test harness if standard framework unavailable

### Data Dependencies

#### DD1: Day-of-Week Lookup Table
- **Dependency**: Accurate mapping of numeric day values (0-6 or 1-7) to day names
- **Criticality**: HIGH
- **Mitigation**: Table is defined within system; no external data source required

#### DD2: Historical Calendar System
- **Dependency**: Gregorian calendar rules (used since October 1582 in Catholic countries)
- **Criticality**: HIGH
- **Mitigation**: Algorithm explicitly implements Gregorian calendar rules; documented assumption that all dates use Gregorian calendar

### External Dependencies

#### ED1: None Required for Core Functionality
- System is self-contained with no external service dependencies
- No database required
- No network connectivity required
- No external API calls required

### Team Dependencies

#### TM1: Development Team
- **Requirement**: 1 senior developer or 2 junior developers
- **Skills**: Proficiency in chosen implementation language, understanding of date algorithms
- **Duration**: Full 8-week timeline

#### TM2: QA Team
- **Requirement**: 1 QA engineer
- **Skills**: Test automation, boundary testing, regression testing
- **Duration**: Weeks 4-8 (50% allocation)

#### TM3: Technical Writer
- **Requirement**: 1 technical writer
- **Skills**: User documentation, API documentation
- **Duration**: Weeks 7-8 (25% allocation)

---

## Risks

### Technical Risks

#### TR1: Date Algorithm Accuracy
- **Risk**: Calculation algorithm may have edge cases that produce incorrect results
- **Probability**: LOW
- **Impact**: HIGH
- **Mitigation Strategies**:
  - Use proven, well-documented algorithms (Zeller's Congruence or standard library functions)
  - Implement comprehensive test cases covering all century boundaries and leap year rules
  - Validate results against multiple independent sources (astronomical databases, perpetual calendars)
  - Include specific tests for known edge cases (1600, 1700, 1800, 1900, 2000, 2100)

#### TR2: Platform-Specific Date Function Behavior
- **Risk**: Different platforms may interpret date functions differently
- **Probability**: MEDIUM
- **Impact**: MEDIUM
- **Mitigation Strategies**:
  - Document which date functions are used and their expected behavior
  - Implement custom algorithm if platform functions prove unreliable
  - Test on multiple platforms during QA phase
  - Maintain unit tests that verify specific date calculations

#### TR3: Integer Overflow for Large/Small Years
- **Risk**: Date calculations involving years at boundaries (1600, 3000) may cause integer overflow
- **Probability**: LOW
- **Impact**: MEDIUM
- **Mitigation Strategies**:
  - Use appropriate integer types (64-bit integers if available)
  - Implement boundary tests specifically for years 1600 and 3000
  - Document numeric range limitations
  - Add overflow detection and error handling

### Functional Risks

#### FR1: Incomplete Test Framework
- **Risk**: Test framework may be implemented as stub without full functionality
- **Probability**: MEDIUM
- **Impact**: MEDIUM
- **Mitigation Strategies**:
  - Clearly define test framework requirements in Phase 2
  - Allocate sufficient time for test framework implementation
  - Prioritize test framework completion before enhancement phase
  - Ensure test framework can actually invoke calculation engine programmatically

#### FR2: User Input Validation Gaps
- **Risk**: Unexpected input types may not be handled properly
- **Probability**: MEDIUM
- **Impact**: LOW
- **Mitigation Strategies**:
  - Implement comprehensive input validation (type checking, range checking)
  - Add try-catch blocks or equivalent error handling
  - Test with fuzzing techniques (random inputs)
  - Ensure graceful degradation for all invalid inputs

### Usability Risks

#### UR1: Unclear Error Messages
- **Risk**: Users may not understand error messages or how to correct errors
- **Probability**: MEDIUM
- **Impact**: LOW
- **Mitigation Strategies**:
  - Review all error messages for clarity
  - Include specific guidance in error messages (e.g., "Please enter a year between 1600 and 3000")
  - Conduct usability testing with non-technical users
  - Iterate on message wording based on feedback

#### UR2: Output Format Not Meeting User Needs
- **Risk**: Output may be too technical or not provide enough context
- **Probability**: LOW
- **Impact**: LOW
- **Mitigation Strategies**:
  - Include user representatives in design reviews
  - Provide contextual "Fun Facts" to add value
  - Format output with clear visual structure
  - Gather feedback during beta testing

### Schedule Risks

#### SR1: Algorithm Implementation Complexity
- **Risk**: Date calculation algorithm may take longer to implement than estimated
- **Probability**: LOW
- **Impact**: MEDIUM
- **Mitigation Strategies**:
  - Use standard library functions where possible (reduces implementation time)
  - Allocate buffer time in Phase 1
  - Have fallback to simpler algorithm if complex implementation fails
  - Consider using proven third-party libraries if permitted

#### SR2: Test Framework Scope Creep
- **Risk**: Test framework requirements may expand beyond initial scope
- **Probability**: MEDIUM
- **Impact**: MEDIUM
- **Mitigation Strategies**:
  - Clearly define MVP requirements for test framework
  - Prioritize automated execution over advanced features
  - Defer enhancements to future releases
  - Maintain strict scope control during Phase 2

### Quality Risks

#### QR1: Insufficient Test Coverage
- **Risk**: Test cases may not cover all scenarios, leading to undiscovered bugs
- **Probability**: MEDIUM
- **Impact**: HIGH
- **Mitigation Strategies**:
  - Define comprehensive test matrix covering:
    - All century boundaries (1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000)
    - All days of week for at least one year
    - Leap years and non-leap years
    - Special leap year rules (divisible by 100 but not 400)
  - Allocate full week for QA (Phase 4)
  - Implement automated regression testing
  - Validate against external calendar sources

#### QR2: Performance Degradation
- **Risk**: System may not meet performance targets on lower-end hardware
- **Probability**: LOW
- **Impact**: LOW
- **Mitigation Strategies**:
  - Use efficient algorithms (O(1) complexity for calculations)
  - Avoid unnecessary loops or data processing
  - Test on minimum specification hardware
  - Optimize hotspot code if necessary

---

## Conclusion

The Holiday Day-of-Week Calculator represents a focused solution to a common planning need: determining when national holidays fall on specific days of the week. By providing accurate calculations for Canada Day (July 1st) across a 1400-year span, the system supports diverse use cases from event planning to historical research.

### Key Success Factors

1. **Mathematical Accuracy**: The system's foundation on proven date calculation algorithms ensures reliable results across all boundary conditions and edge cases.

2. **User-Centric Design**: The interactive interface prioritizes ease of use, clear communication, and helpful contextual information, making the tool accessible to non-technical users.

3. **Quality Assurance**: The integrated automated testing framework ensures ongoing accuracy and supports future enhancements with confidence.

4. **Modular Architecture**: The separation of calculation logic, user interface, and testing components enables future extensibility (additional holidays, output formats, internationalization).

### Strategic Value

This system serves as a prototype for a broader holiday calculation suite. The architecture and design patterns established here can be extended to support:
- Additional Canadian holidays (Victoria Day, Thanksgiving, Remembrance Day)
- International holidays (US Independence Day, UK Bank Holidays, etc.)
- Custom organizational holidays
- Multi-year planning reports
- Integration with calendar and scheduling systems

### Next Steps

Upon approval of this PRD:

1. **Immediate**: Initiate Phase 1 development with focus on calculation engine
2. **Week 4**: Review progress and demonstrate working prototype
3. **Week 6**: Conduct user acceptance testing with stakeholder representatives
4. **Week 8**: Final approval and production release

### Success Metrics

The system will be considered successful when:
- ✅ 100% accuracy on comprehensive test suite (>500 test cases across date range)
- ✅ <100ms calculation time on standard hardware
- ✅ Zero crashes or errors during 1000+ consecutive operations
- ✅ 90%+ user satisfaction score on usability survey
- ✅ Successful integration with automated testing framework
- ✅ Complete documentation package for users and developers

This PRD provides the foundation for building a reliable, maintainable, and extensible holiday calculation system that meets current needs while positioning for future growth.

---

**Document Control**

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2025-01-XX | Product Team | Initial PRD based on existing system analysis |

**Approval Required From:**
- [ ] Product Owner
- [ ] Technical Lead
- [ ] QA Manager
- [ ] Business Stakeholder

**Related Documents:**
- COBOL Program Analysis Report (cobol_analyzer_report.md)
- COBOL Dependency Report (cobol_dependency_report.md)
- Technical Implementation Specification (TBD)
- User Guide (TBD)
- Test Plan (TBD)

---

*This PRD is a living document and may be updated as requirements evolve. All changes must be approved through the formal change control process.*

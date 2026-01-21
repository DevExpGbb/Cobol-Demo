---
name: "PRD Generator Agent"
description: 'Generate a new PRD (Product Requirements Document)'
---

Leveraging any existing documentation, including but not limited to `cobol_analyzer_report.md`, `README.md`, `cobol_dependency_report.md` within a given program/project folder, your goal is to generate a new high level PRD (Product Requirements Document) based on the provided context.  Do not make it COBOL specific in its implementation, language or terminology.  We want to build a PRD document that lists out the product requirements in a clear and concise manner that can be easily understood by all stakeholders and be used as a precursor ro rebuild the product in any other programming language.  We will then use this in a subsequent step to create user stories and acceptance criteria for the new product implementation as needed.

The PRD should include the following sections:

1. **Title**: A clear and concise title for the PRD.
2. **Introduction**: A brief overview of the product or feature being proposed.
3. **Problem Statement**: A detailed description of the problem that the product or feature aims to solve.
4. **Objectives**: Specific goals that the product or feature should achieve.
5. **Requirements**: A list of functional and non-functional requirements that the product or feature must meet.
6. **User Stories**: A set of user stories that describe how different users will interact with the product or feature.
7. **Acceptance Criteria**: Clear criteria that must be met for the product or feature to be considered complete and ready for release.
8. **Timeline**: An estimated timeline for the development and release of the product or feature.
9. **Dependencies**: Any dependencies that the product or feature has on other systems, teams, or technologies.
10. **Risks**: Potential risks associated with the development and release of the product or feature, along with mitigation strategies.
11. **Conclusion**: A summary of the PRD and the next steps.
Please use the following context to generate the PRD:
{{context}}
---
# Product Requirements Document (PRD)
## Title
{{title}}
## Introduction
{{introduction}}
## Problem Statement
{{problem_statement}}
## Objectives
{{objectives}}
## Requirements
{{requirements}}
## User Stories
{{user_stories}}
## Acceptance Criteria
{{acceptance_criteria}}
## Timeline
{{timeline}}
## Dependencies
{{dependencies}}
## Risks
{{risks}}
## Conclusion
{{conclusion}}
---
Please ensure that the PRD is well-structured, clear, and concise. Use bullet points where appropriate, and ensure that each section is detailed enough to provide a comprehensive understanding of the product or feature being proposed. The PRD should be suitable for review by stakeholders and development teams, providing them with all the necessary information to proceed with the project.
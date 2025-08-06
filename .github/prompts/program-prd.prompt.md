---
mode: agent
model: GPT-4.1
description: 'Generate a new PRD (Product Requirements Document)'
---
Your goal is to generate a new PRD (Product Requirements Document) based on the provided context, which would include the program code and any docs in the same directory as the COBOL program folder including but not limited to ["cobol_analyzer_report.md", "cobol_dependency_report.md", "README.md"]. 

Ask additional questions if necessary to clarify the requirements or gather more information about the product or feature being proposed.
The PRD should be comprehensive and cover all aspects of the product or feature, including its purpose, functionality, and any specific requirements or constraints.

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

Write the PRD in Markdown format, ensuring that it is easy to read and navigate. Use headings, subheadings, and lists to organize the content effectively. The final document should be ready for sharing with stakeholders and team members involved in the project.

Write the PRD to the same directory as the COBOL code, in a file named `prd_spec.md`. If the file already exists, update your analysis to it.
---
name: "Spec Doc Generator Agent"
description: 'Generate a new Technical Specification Document for Java-based solutions'
---

Using any existing documentation, including but not limited to a "prd.md" within a given program/project folder, your goal is to generate a comprehensive Technical Specification Document based on the provided context. The Spec Document should include detailed technical specifications that can be used by the development team to implement the features outlined in the PRD.

## Document Structure

The Spec Document should include the following sections:

### 1. **Title & Metadata**
- A clear and concise title for the Spec Document
- Version, Author(s), Date, Status (Draft/Review/Approved)
- Document revision history

### 2. **Executive Summary**
- Brief overview of the technical solution
- Key objectives and scope
- Target audience for this document

### 3. **Technology Stack**
- **Language & Version**: Java version (e.g., Java 17, 21 LTS)
- **Framework**: Primary framework (e.g., Spring Boot 3.x, Quarkus, Jakarta EE)
- **Build Tool**: Maven or Gradle with version
- **Database**: Primary data store (e.g., PostgreSQL, MySQL, MongoDB)
- **Caching**: Caching solution if applicable (e.g., Redis, Caffeine)
- **Messaging**: Message broker if applicable (e.g., Kafka, RabbitMQ)
- **Key Libraries**: Essential dependencies (e.g., Lombok, MapStruct, Jackson)
- **Runtime Environment**: JVM version, containerization (Docker), orchestration (Kubernetes)

### 4. **Architecture Overview**
- High-level system architecture diagram (Mermaid or image)
- Architectural pattern (e.g., Microservices, Modular Monolith, Hexagonal)
- Component interaction diagrams
- External system integrations

### 5. **Data Models**
- Entity-Relationship diagrams
- JPA/Hibernate entity definitions
- Database schema design
- Data validation rules
- Migration strategy (e.g., Flyway, Liquibase)

### 6. **API Specifications**
- RESTful API design following OpenAPI 3.0 specification
- Endpoints with HTTP methods, paths, and descriptions
- Request/Response DTOs with validation constraints
- Authentication & Authorization (e.g., OAuth2, JWT)
- API versioning strategy
- Rate limiting and throttling

### 7. **Component Specifications**
- Package structure and module organization
- Service layer design with interface contracts
- Repository layer patterns
- Dependency injection configuration
- Cross-cutting concerns (logging, transactions, caching)

### 8. **Error Handling**
- Exception hierarchy design
- Global exception handling strategy (@ControllerAdvice)
- Error response format (RFC 7807 Problem Details)
- Logging standards (SLF4J/Logback configuration)
- Monitoring and alerting integration

### 9. **Performance Requirements**
- Response time SLAs per endpoint
- Throughput expectations (requests/second)
- Connection pooling configuration (HikariCP)
- JVM tuning recommendations
- Database query optimization guidelines

### 10. **Security Considerations**
- Authentication mechanism implementation
- Authorization and role-based access control
- Input validation and sanitization
- Secrets management (e.g., HashiCorp Vault, AWS Secrets Manager)
- OWASP Top 10 mitigations
- Dependency vulnerability scanning (OWASP Dependency-Check, Snyk)

### 11. **Testing Strategy**
- Unit testing with JUnit 5 and Mockito
- Integration testing with @SpringBootTest and Testcontainers
- API testing with REST Assured or MockMvc
- Code coverage requirements (e.g., minimum 80%)
- Performance testing approach (JMeter, Gatling)
- Contract testing if applicable (Pact)

### 12. **Deployment Plan**
- Build and packaging (JAR/WAR, Docker image)
- CI/CD pipeline design (GitHub Actions, Jenkins)
- Environment configurations (dev, staging, production)
- Infrastructure as Code (Terraform, Pulumi)
- Health checks and readiness probes
- Rollback procedures

### 13. **Observability**
- Structured logging format
- Metrics collection (Micrometer, Prometheus)
- Distributed tracing (OpenTelemetry, Jaeger)
- Dashboard and alerting setup (Grafana)

### 14. **Conclusion & Next Steps**
- Summary of key technical decisions
- Open questions and assumptions
- Implementation phases and milestones
- Dependencies and blockers

---

## Guidelines

- Keep functional requirements separate from technical implementation details
- Include code snippets and configuration examples where helpful
- Use Mermaid diagrams for architecture and sequence diagrams
- Reference the PRD for business context but focus on the "how" not the "what"
- Ensure all technology choices are justified with rationale

Please use the following context to generate the Spec Document.
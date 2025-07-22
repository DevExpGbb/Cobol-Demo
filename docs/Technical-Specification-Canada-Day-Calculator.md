# Technical Specification Document
## Canada Day Calculator - COBOL to Java/Quarkus Migration

### Document Information
- **Version:** 1.0
- **Date:** July 21, 2025
- **Author:** Development Team
- **Project:** COBOL to Java/Quarkus Migration

---

## 1. System Architecture

### 1.1 Overall Architecture
```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Web Browser   │    │   CLI Client    │    │  API Client     │
│   (Frontend)    │    │   (Optional)    │    │  (Integration)  │
└─────────┬───────┘    └─────────┬───────┘    └─────────┬───────┘
          │                      │                      │
          └──────────────────────┼──────────────────────┘
                                 │
                    ┌─────────────▼───────────────┐
                    │      Quarkus Application    │
                    │  ┌─────────────────────────┐ │
                    │  │    REST API Layer       │ │
                    │  └─────────────────────────┘ │
                    │  ┌─────────────────────────┐ │
                    │  │   Business Logic Layer  │ │
                    │  └─────────────────────────┘ │
                    │  ┌─────────────────────────┐ │
                    │  │   Validation Layer      │ │
                    │  └─────────────────────────┘ │
                    │  ┌─────────────────────────┐ │
                    │  │   Configuration Layer   │ │
                    │  └─────────────────────────┘ │
                    └─────────────────────────────┘
```

### 1.2 Technology Stack
- **Framework**: Quarkus 3.6.x
- **Java Version**: Java 17 (LTS)
- **Build Tool**: Maven 3.9.x
- **Testing Framework**: JUnit 5.10.x
- **API Documentation**: OpenAPI 3.0/Swagger
- **Containerization**: Docker with UBI8 base image
- **Frontend**: HTML5, CSS3, JavaScript (Vanilla or lightweight framework)

---

## 2. COBOL to Java Mapping Analysis

### 2.1 COBOL Structure Mapping

#### 2.1.1 IDENTIFICATION DIVISION → Java Class Declaration
```cobol
IDENTIFICATION DIVISION.
    PROGRAM-ID. CANDAY01.
```
**Maps to:**
```java
@ApplicationScoped
public class CanadaDayCalculator {
    // Implementation
}
```

#### 2.1.2 DATA DIVISION → Java Fields and Classes
```cobol
DATA DIVISION.
WORKING-STORAGE SECTION.
    01 WS-INPUT-YEAR          PIC 9(4) VALUE ZEROS.
    01 WS-ERROR-FLAG          PIC X(1) VALUE 'N'.
    01 WS-CONTINUE-FLAG       PIC X(1) VALUE 'Y'.
```
**Maps to:**
```java
public class CalculationRequest {
    @NotNull
    @Min(1600)
    @Max(3000)
    private Integer year;
    
    // getters/setters
}

public class CalculationResult {
    private int year;
    private String dayName;
    private LocalDate date;
    private List<String> funFacts;
    
    // getters/setters
}
```

#### 2.1.3 PROCEDURE DIVISION → Java Methods
```cobol
PROCEDURE DIVISION.
    MAIN-PROCEDURE.
        PERFORM UNTIL WS-CONTINUE-FLAG = 'N'
            PERFORM GET-YEAR-INPUT
            PERFORM CALCULATE-DAY-OF-WEEK
            PERFORM DISPLAY-RESULT
        END-PERFORM.
```
**Maps to:**
```java
public CalculationResult calculateCanadaDay(int year) {
    validateYear(year);
    LocalDate canadaDay = LocalDate.of(year, Month.JULY, 1);
    DayOfWeek dayOfWeek = canadaDay.getDayOfWeek();
    List<String> funFacts = generateFunFacts(dayOfWeek);
    
    return new CalculationResult(year, dayOfWeek.name(), canadaDay, funFacts);
}
```

---

## 3. Detailed Component Design

### 3.1 Core Business Logic

#### 3.1.1 CanadaDayCalculatorService
```java
@ApplicationScoped
public class CanadaDayCalculatorService {
    
    private static final int MIN_YEAR = 1600;
    private static final int MAX_YEAR = 3000;
    private static final MonthDay CANADA_DAY = MonthDay.of(Month.JULY, 1);
    
    /**
     * Calculates the day of week for Canada Day in the specified year
     * Equivalent to COBOL CALCULATE-DAY-OF-WEEK section
     */
    public CalculationResult calculateCanadaDay(int year) throws ValidationException {
        validateYear(year);
        
        LocalDate canadaDay = CANADA_DAY.atYear(year);
        DayOfWeek dayOfWeek = canadaDay.getDayOfWeek();
        String dayName = formatDayName(dayOfWeek);
        List<String> funFacts = generateFunFacts(dayOfWeek);
        
        return CalculationResult.builder()
            .year(year)
            .date(canadaDay)
            .dayOfWeek(dayOfWeek)
            .dayName(dayName)
            .funFacts(funFacts)
            .build();
    }
    
    /**
     * Validates year input - equivalent to COBOL year validation logic
     */
    private void validateYear(int year) throws ValidationException {
        if (year < MIN_YEAR || year > MAX_YEAR) {
            throw new ValidationException(
                String.format("Year must be between %d and %d", MIN_YEAR, MAX_YEAR)
            );
        }
    }
    
    /**
     * Generates contextual fun facts - equivalent to COBOL DISPLAY-RESULT section
     */
    private List<String> generateFunFacts(DayOfWeek dayOfWeek) {
        List<String> facts = new ArrayList<>();
        
        switch (dayOfWeek) {
            case SATURDAY, SUNDAY:
                facts.add("Canada Day is on a weekend! Perfect for celebrations!");
                break;
            default:
                facts.add("Canada Day is on a weekday - enjoy the long weekend!");
                break;
        }
        
        if (dayOfWeek == DayOfWeek.MONDAY) {
            facts.add("Great way to start the week with a holiday!");
        } else if (dayOfWeek == DayOfWeek.FRIDAY) {
            facts.add("Fantastic end to the work week!");
        }
        
        return facts;
    }
    
    private String formatDayName(DayOfWeek dayOfWeek) {
        return dayOfWeek.getDisplayName(TextStyle.FULL, Locale.ENGLISH);
    }
}
```

#### 3.1.2 Data Transfer Objects

```java
// Request DTO
public class CalculationRequest {
    @NotNull(message = "Year is required")
    @Min(value = 1600, message = "Year must be at least 1600")
    @Max(value = 3000, message = "Year must be at most 3000")
    private Integer year;
    
    // constructors, getters, setters
}

// Response DTO
@JsonInclude(JsonInclude.Include.NON_NULL)
public class CalculationResult {
    private int year;
    private LocalDate date;
    private DayOfWeek dayOfWeek;
    private String dayName;
    private List<String> funFacts;
    private String formattedMessage;
    
    // constructors, getters, setters, builder pattern
}

// Error Response DTO
public class ErrorResponse {
    private String error;
    private String message;
    private int status;
    private LocalDateTime timestamp;
    
    // constructors, getters, setters
}
```

### 3.2 REST API Layer

#### 3.2.1 Canada Day Calculator Resource
```java
@Path("/api/v1/canada-day")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
@Tag(name = "Canada Day Calculator", description = "Calculate day of week for Canada Day")
public class CanadaDayResource {
    
    @Inject
    CanadaDayCalculatorService calculatorService;
    
    @GET
    @Path("/calculate/{year}")
    @Operation(summary = "Calculate Canada Day for specific year")
    @APIResponse(responseCode = "200", description = "Calculation successful")
    @APIResponse(responseCode = "400", description = "Invalid year parameter")
    public Response calculateByYear(
            @Parameter(description = "Year to calculate (1600-3000)", required = true)
            @PathParam("year") int year) {
        
        try {
            CalculationResult result = calculatorService.calculateCanadaDay(year);
            return Response.ok(result).build();
        } catch (ValidationException e) {
            ErrorResponse error = new ErrorResponse("VALIDATION_ERROR", e.getMessage(), 400);
            return Response.status(400).entity(error).build();
        }
    }
    
    @POST
    @Path("/calculate")
    @Operation(summary = "Calculate Canada Day using request body")
    public Response calculate(@Valid CalculationRequest request) {
        try {
            CalculationResult result = calculatorService.calculateCanadaDay(request.getYear());
            return Response.ok(result).build();
        } catch (ValidationException e) {
            ErrorResponse error = new ErrorResponse("VALIDATION_ERROR", e.getMessage(), 400);
            return Response.status(400).entity(error).build();
        }
    }
    
    @GET
    @Path("/health")
    @Operation(summary = "Health check endpoint")
    public Response health() {
        return Response.ok(Map.of("status", "UP", "service", "canada-day-calculator")).build();
    }
}
```

### 3.3 Exception Handling

#### 3.3.1 Custom Exceptions
```java
public class ValidationException extends Exception {
    public ValidationException(String message) {
        super(message);
    }
    
    public ValidationException(String message, Throwable cause) {
        super(message, cause);
    }
}

@Provider
public class ValidationExceptionMapper implements ExceptionMapper<ValidationException> {
    
    @Override
    public Response toResponse(ValidationException exception) {
        ErrorResponse error = ErrorResponse.builder()
            .error("VALIDATION_ERROR")
            .message(exception.getMessage())
            .status(400)
            .timestamp(LocalDateTime.now())
            .build();
            
        return Response.status(400).entity(error).build();
    }
}
```

### 3.4 Web Frontend (Optional Console Interface)

#### 3.4.1 Static HTML Interface
```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Canada Day Calculator</title>
    <style>
        /* Styling similar to COBOL console output */
        body { font-family: 'Courier New', monospace; background: #000; color: #00ff00; }
        .container { max-width: 800px; margin: 0 auto; padding: 20px; }
        .header { text-align: center; border: 1px solid #00ff00; padding: 10px; }
        /* Additional styling */
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>CANADA DAY CALCULATOR</h1>
            <p>What day does Canada Day fall on?</p>
        </div>
        
        <div class="calculator">
            <form id="calculatorForm">
                <label for="year">Enter a year (1600-3000):</label>
                <input type="number" id="year" min="1600" max="3000" required>
                <button type="submit">Calculate</button>
            </form>
            
            <div id="result" style="display: none;">
                <!-- Results will be displayed here -->
            </div>
            
            <div id="error" style="display: none; color: #ff0000;">
                <!-- Errors will be displayed here -->
            </div>
        </div>
    </div>
    
    <script src="calculator.js"></script>
</body>
</html>
```

#### 3.4.2 JavaScript Frontend Logic
```javascript
class CanadaDayCalculator {
    constructor() {
        this.apiBaseUrl = '/api/v1/canada-day';
        this.initializeEventListeners();
    }
    
    initializeEventListeners() {
        document.getElementById('calculatorForm').addEventListener('submit', (e) => {
            e.preventDefault();
            this.calculateCanadaDay();
        });
    }
    
    async calculateCanadaDay() {
        const year = document.getElementById('year').value;
        
        try {
            const response = await fetch(`${this.apiBaseUrl}/calculate/${year}`);
            
            if (response.ok) {
                const result = await response.json();
                this.displayResult(result);
            } else {
                const error = await response.json();
                this.displayError(error.message);
            }
        } catch (error) {
            this.displayError('Network error occurred. Please try again.');
        }
    }
    
    displayResult(result) {
        const resultDiv = document.getElementById('result');
        const errorDiv = document.getElementById('error');
        
        errorDiv.style.display = 'none';
        
        resultDiv.innerHTML = `
            <h2>Result</h2>
            <p>Canada Day (July 1, ${result.year}) falls on a ${result.dayName}</p>
            <h3>Fun Facts:</h3>
            <ul>
                ${result.funFacts.map(fact => `<li>${fact}</li>`).join('')}
            </ul>
        `;
        
        resultDiv.style.display = 'block';
    }
    
    displayError(message) {
        const resultDiv = document.getElementById('result');
        const errorDiv = document.getElementById('error');
        
        resultDiv.style.display = 'none';
        errorDiv.textContent = `Error: ${message}`;
        errorDiv.style.display = 'block';
    }
}

// Initialize calculator when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    new CanadaDayCalculator();
});
```

---

## 4. Configuration and Deployment

### 4.1 Application Configuration (application.properties)
```properties
# Application name and version
quarkus.application.name=canada-day-calculator
quarkus.application.version=1.0.0

# HTTP configuration
quarkus.http.port=8080
quarkus.http.host=0.0.0.0

# CORS configuration for web interface
quarkus.http.cors=true
quarkus.http.cors.origins=*
quarkus.http.cors.headers=accept,authorization,content-type,x-requested-with
quarkus.http.cors.methods=GET,POST,OPTIONS

# OpenAPI configuration
quarkus.swagger-ui.always-include=true
quarkus.swagger-ui.path=/swagger-ui

# Health check configuration
quarkus.health.enabled=true

# Logging configuration
quarkus.log.level=INFO
quarkus.log.category."com.company.calculator".level=DEBUG

# Native compilation settings
quarkus.native.additional-build-args=-H:ReflectionConfigurationFiles=reflection-config.json
```

### 4.2 Maven Configuration (pom.xml)
```xml
<?xml version="1.0"?>
<project xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 
         https://maven.apache.org/xsd/maven-4.0.0.xsd" 
         xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  
  <modelVersion>4.0.0</modelVersion>
  <groupId>com.company</groupId>
  <artifactId>canada-day-calculator</artifactId>
  <version>1.0.0</version>
  
  <properties>
    <maven.compiler.source>17</maven.compiler.source>
    <maven.compiler.target>17</maven.compiler.target>
    <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    <quarkus.platform.version>3.6.4</quarkus.platform.version>
  </properties>
  
  <dependencyManagement>
    <dependencies>
      <dependency>
        <groupId>io.quarkus.platform</groupId>
        <artifactId>quarkus-bom</artifactId>
        <version>${quarkus.platform.version}</version>
        <type>pom</type>
        <scope>import</scope>
      </dependency>
    </dependencies>
  </dependencyManagement>
  
  <dependencies>
    <!-- Quarkus core dependencies -->
    <dependency>
      <groupId>io.quarkus</groupId>
      <artifactId>quarkus-resteasy-reactive-jackson</artifactId>
    </dependency>
    <dependency>
      <groupId>io.quarkus</groupId>
      <artifactId>quarkus-smallrye-openapi</artifactId>
    </dependency>
    <dependency>
      <groupId>io.quarkus</groupId>
      <artifactId>quarkus-smallrye-health</artifactId>
    </dependency>
    <dependency>
      <groupId>io.quarkus</groupId>
      <artifactId>quarkus-hibernate-validator</artifactId>
    </dependency>
    
    <!-- Testing dependencies -->
    <dependency>
      <groupId>io.quarkus</groupId>
      <artifactId>quarkus-junit5</artifactId>
      <scope>test</scope>
    </dependency>
    <dependency>
      <groupId>io.rest-assured</groupId>
      <artifactId>rest-assured</artifactId>
      <scope>test</scope>
    </dependency>
  </dependencies>
  
  <build>
    <plugins>
      <plugin>
        <groupId>io.quarkus.platform</groupId>
        <artifactId>quarkus-maven-plugin</artifactId>
        <version>${quarkus.platform.version}</version>
        <executions>
          <execution>
            <goals>
              <goal>build</goal>
              <goal>generate-code</goal>
              <goal>generate-code-tests</goal>
            </goals>
          </execution>
        </executions>
      </plugin>
    </plugins>
  </build>
</project>
```

### 4.3 Docker Configuration

#### 4.3.1 Dockerfile.jvm
```dockerfile
FROM registry.access.redhat.com/ubi8/openjdk-17-runtime:1.18

ENV LANGUAGE='en_US:en'

# Copy the application jar
COPY --chown=185 target/quarkus-app/lib/ /deployments/lib/
COPY --chown=185 target/quarkus-app/*.jar /deployments/
COPY --chown=185 target/quarkus-app/app/ /deployments/app/
COPY --chown=185 target/quarkus-app/quarkus/ /deployments/quarkus/

EXPOSE 8080
USER 185
ENV JAVA_OPTS_APPEND="-Dquarkus.http.host=0.0.0.0 -Djava.util.logging.manager=org.jboss.logmanager.LogManager"
ENV JAVA_APP_JAR="/deployments/quarkus-run.jar"

ENTRYPOINT [ "/opt/jboss/container/java/run/run-java.sh" ]
```

#### 4.3.2 Dockerfile.native
```dockerfile
FROM registry.access.redhat.com/ubi8/ubi-minimal:8.9

WORKDIR /work/
RUN chown 1001 /work \
    && chmod "g+rwX" /work \
    && chown 1001:root /work
    
COPY --chown=1001:root target/*-runner /work/application

EXPOSE 8080
USER 1001

ENTRYPOINT ["./application", "-Dquarkus.http.host=0.0.0.0"]
```

---

## 5. Testing Strategy

### 5.1 Unit Tests
```java
@QuarkusTest
class CanadaDayCalculatorServiceTest {
    
    @Inject
    CanadaDayCalculatorService calculatorService;
    
    @Test
    void testCalculateCanadaDay_ValidYear() {
        // Test with known date
        CalculationResult result = calculatorService.calculateCanadaDay(2025);
        
        assertThat(result.getYear()).isEqualTo(2025);
        assertThat(result.getDate()).isEqualTo(LocalDate.of(2025, 7, 1));
        assertThat(result.getDayOfWeek()).isEqualTo(DayOfWeek.TUESDAY);
        assertThat(result.getFunFacts()).isNotEmpty();
    }
    
    @Test
    void testCalculateCanadaDay_InvalidYearLow() {
        assertThatThrownBy(() -> calculatorService.calculateCanadaDay(1599))
            .isInstanceOf(ValidationException.class)
            .hasMessageContaining("Year must be between 1600 and 3000");
    }
    
    @Test
    void testCalculateCanadaDay_InvalidYearHigh() {
        assertThatThrownBy(() -> calculatorService.calculateCanadaDay(3001))
            .isInstanceOf(ValidationException.class)
            .hasMessageContaining("Year must be between 1600 and 3000");
    }
    
    @Test
    void testFunFacts_Weekend() {
        // Test with year where Canada Day falls on weekend
        CalculationResult result = calculatorService.calculateCanadaDay(2023); // Saturday
        
        assertThat(result.getFunFacts())
            .contains("Canada Day is on a weekend! Perfect for celebrations!");
    }
    
    @Test
    void testFunFacts_Monday() {
        // Test with year where Canada Day falls on Monday
        CalculationResult result = calculatorService.calculateCanadaDay(2024); // Monday
        
        assertThat(result.getFunFacts())
            .contains("Great way to start the week with a holiday!");
    }
}
```

### 5.2 Integration Tests
```java
@QuarkusTest
class CanadaDayResourceTest {
    
    @Test
    void testCalculateByYear_Success() {
        given()
            .when().get("/api/v1/canada-day/calculate/2025")
            .then()
                .statusCode(200)
                .body("year", equalTo(2025))
                .body("dayName", equalTo("Tuesday"))
                .body("funFacts", hasSize(greaterThan(0)));
    }
    
    @Test
    void testCalculateByYear_InvalidYear() {
        given()
            .when().get("/api/v1/canada-day/calculate/1500")
            .then()
                .statusCode(400)
                .body("error", equalTo("VALIDATION_ERROR"))
                .body("message", containsString("Year must be between 1600 and 3000"));
    }
    
    @Test
    void testCalculatePost_Success() {
        CalculationRequest request = new CalculationRequest();
        request.setYear(2025);
        
        given()
            .contentType(ContentType.JSON)
            .body(request)
            .when().post("/api/v1/canada-day/calculate")
            .then()
                .statusCode(200)
                .body("year", equalTo(2025))
                .body("dayName", equalTo("Tuesday"));
    }
    
    @Test
    void testHealthEndpoint() {
        given()
            .when().get("/api/v1/canada-day/health")
            .then()
                .statusCode(200)
                .body("status", equalTo("UP"))
                .body("service", equalTo("canada-day-calculator"));
    }
}
```

---

## 6. Migration Validation

### 6.1 Functional Equivalence Testing
```java
@QuarkusTest
class CobolEquivalenceTest {
    
    @Inject
    CanadaDayCalculatorService calculatorService;
    
    /**
     * Test cases that match expected COBOL behavior
     * These test cases verify that our Java implementation
     * produces identical results to the original COBOL program
     */
    @ParameterizedTest
    @CsvSource({
        "2025, TUESDAY",      // Current year test
        "2024, MONDAY",       // Monday test case
        "2023, SATURDAY",     // Weekend test case
        "2022, FRIDAY",       // Friday test case
        "2000, SATURDAY",     // Leap year test
        "1900, SUNDAY",       // Century year test
        "1600, WEDNESDAY",    // Minimum year test
        "3000, TUESDAY"       // Maximum year test
    })
    void testCobolEquivalence(int year, DayOfWeek expectedDay) {
        CalculationResult result = calculatorService.calculateCanadaDay(year);
        
        assertThat(result.getDayOfWeek()).isEqualTo(expectedDay);
        assertThat(result.getYear()).isEqualTo(year);
        assertThat(result.getDate().getMonth()).isEqualTo(Month.JULY);
        assertThat(result.getDate().getDayOfMonth()).isEqualTo(1);
    }
}
```

### 6.2 Performance Benchmarks
```java
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
public class PerformanceBenchmark {
    
    private CanadaDayCalculatorService calculatorService;
    
    @Setup
    public void setup() {
        calculatorService = new CanadaDayCalculatorService();
    }
    
    @Benchmark
    public CalculationResult benchmarkCalculation() {
        return calculatorService.calculateCanadaDay(2025);
    }
    
    @Benchmark
    public CalculationResult benchmarkWithValidation() throws ValidationException {
        return calculatorService.calculateCanadaDay(1999); // Edge case
    }
}
```

---

## 7. Deployment and DevOps

### 7.1 Build Commands
```bash
# Development build
mvn quarkus:dev

# Production build (JVM)
mvn clean package

# Native build
mvn clean package -Pnative

# Docker build (JVM)
docker build -f src/main/docker/Dockerfile.jvm -t canada-day-calculator:jvm .

# Docker build (Native)
docker build -f src/main/docker/Dockerfile.native -t canada-day-calculator:native .
```

### 7.2 Kubernetes Deployment
```yaml
# deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: canada-day-calculator
  labels:
    app: canada-day-calculator
spec:
  replicas: 3
  selector:
    matchLabels:
      app: canada-day-calculator
  template:
    metadata:
      labels:
        app: canada-day-calculator
    spec:
      containers:
      - name: canada-day-calculator
        image: canada-day-calculator:jvm
        ports:
        - containerPort: 8080
        resources:
          requests:
            memory: "64Mi"
            cpu: "50m"
          limits:
            memory: "256Mi"
            cpu: "200m"
        livenessProbe:
          httpGet:
            path: /q/health/live
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /q/health/ready
            port: 8080
          initialDelaySeconds: 5
          periodSeconds: 5

---
apiVersion: v1
kind: Service
metadata:
  name: canada-day-calculator-service
spec:
  selector:
    app: canada-day-calculator
  ports:
    - protocol: TCP
      port: 80
      targetPort: 8080
  type: LoadBalancer
```

---

## 8. Monitoring and Observability

### 8.1 Metrics Configuration
```properties
# Micrometer metrics
quarkus.micrometer.enabled=true
quarkus.micrometer.registry-type=prometheus

# Custom metrics
quarkus.micrometer.export.prometheus.enabled=true
quarkus.micrometer.export.prometheus.path=/metrics
```

### 8.2 Custom Metrics
```java
@ApplicationScoped
public class CalculatorMetrics {
    
    @Inject
    MeterRegistry meterRegistry;
    
    private Counter calculationCounter;
    private Counter errorCounter;
    private Timer calculationTimer;
    
    @PostConstruct
    void initMetrics() {
        calculationCounter = Counter.builder("calculator.calculations.total")
            .description("Total number of calculations performed")
            .register(meterRegistry);
            
        errorCounter = Counter.builder("calculator.errors.total")
            .description("Total number of calculation errors")
            .register(meterRegistry);
            
        calculationTimer = Timer.builder("calculator.calculation.duration")
            .description("Time taken to perform calculations")
            .register(meterRegistry);
    }
    
    public void incrementCalculations() {
        calculationCounter.increment();
    }
    
    public void incrementErrors() {
        errorCounter.increment();
    }
    
    public Timer.Sample startTimer() {
        return Timer.start(meterRegistry);
    }
}
```

---

## 9. Security Considerations

### 9.1 Input Validation
- Bean Validation annotations for request validation
- Custom validators for business rules
- SQL injection prevention (not applicable for this use case)
- XSS prevention in web interface

### 9.2 API Security
```java
// Rate limiting configuration
@ApplicationScoped
public class RateLimitingFilter implements ContainerRequestFilter {
    
    private final Map<String, AtomicInteger> requestCounts = new ConcurrentHashMap<>();
    private final Map<String, Long> resetTimes = new ConcurrentHashMap<>();
    
    @Override
    public void filter(ContainerRequestContext requestContext) throws IOException {
        String clientId = getClientId(requestContext);
        
        if (isRateLimited(clientId)) {
            requestContext.abortWith(
                Response.status(429)
                    .entity(new ErrorResponse("RATE_LIMIT_EXCEEDED", 
                           "Too many requests", 429))
                    .build()
            );
        }
    }
    
    // Implementation details...
}
```

---

## 10. Future Enhancements

### 10.1 Potential Extensions
1. **Multi-language Support**: Internationalization for day names and messages
2. **Multiple Holidays**: Extend to calculate other Canadian holidays
3. **Historical Events**: Add historical context for specific years
4. **Batch Processing**: Support bulk year calculations
5. **Caching**: Redis-based caching for frequently requested years
6. **GraphQL API**: Alternative API endpoint
7. **Mobile App**: React Native or Flutter mobile application

### 10.2 Scalability Improvements
1. **Microservices**: Split into multiple services
2. **Event Sourcing**: Store calculation history
3. **Analytics**: Usage analytics and reporting
4. **Geographic**: Add timezone-aware calculations

---

## 11. Appendices

### Appendix A: COBOL vs Java Comparison Table

| COBOL Feature | Java Equivalent | Notes |
|---------------|-----------------|-------|
| PICTURE clauses | Java data types | PIC 9(4) → Integer, PIC X(n) → String |
| WORKING-STORAGE | Class fields/variables | Instance and static variables |
| PERFORM UNTIL | while/for loops | Modern loop constructs |
| MOVE statement | Assignment operations | Direct assignment or method calls |
| DISPLAY | System.out.println / HTTP responses | Console or API responses |
| ACCEPT | Scanner / HTTP requests | Input handling |
| FUNCTION MOD | % operator or Math methods | Modulo operations |
| STRING...INTO | StringBuilder / String.format | String manipulation |

### Appendix B: API Documentation Examples

#### Calculate Canada Day - GET Request
```http
GET /api/v1/canada-day/calculate/2025
Accept: application/json
```

**Response:**
```json
{
  "year": 2025,
  "date": "2025-07-01",
  "dayOfWeek": "TUESDAY",
  "dayName": "Tuesday",
  "funFacts": [
    "Canada Day is on a weekday - enjoy the long weekend!"
  ],
  "formattedMessage": "Canada Day (July 1, 2025) falls on a Tuesday"
}
```

#### Error Response Example
```json
{
  "error": "VALIDATION_ERROR",
  "message": "Year must be between 1600 and 3000",
  "status": 400,
  "timestamp": "2025-07-21T10:30:00"
}
```

### Appendix C: Development Environment Setup

#### Prerequisites
- Java 17 or later
- Maven 3.9.x
- Docker (optional, for containerization)
- Git

#### Quick Start Commands
```bash
# Clone the repository
git clone <repository-url>
cd canada-day-calculator

# Run in development mode
mvn quarkus:dev

# Access the application
open http://localhost:8080

# Access Swagger UI
open http://localhost:8080/swagger-ui

# Run tests
mvn test

# Build for production
mvn clean package

# Build native image
mvn clean package -Pnative
```

# AIG NPT Modernization POC — Technical Execution Plan
## Dual Track (VB6 + COBOL) — 2-Week Sprint

---

# Slide 1 — POC Objective (Technical Team)

## Title: AIG NPT Modernization POC — Dual Track (VB + COBOL)

### Purpose:
Deliver an **end-to-end modernization pipeline** for one VB form and one COBOL service using **GenAI + IR + pattern library + TDD**, executable via single commands.

### Goals:

**Build a thin vertical slice of the factory (VB + COBOL):**
- ✅ Complete working pipeline for **both** technologies
- ✅ Shared infrastructure (IR engine, pattern library, TDD)
- ✅ Single-command execution

**Prove the Core Concepts:**
- ✅ **Code extraction** → IR → pattern detection → test generation → code generation → quality gates
- ✅ **Reuse across both technologies** (shared IR schema, common patterns)
- ✅ **Factory model viability** (pattern reuse demonstrated)

**Timeline & Deliverables:**
- ✅ **Finish in 2 weeks** (10 working days)
- ✅ **Working demo for AIG** (live execution, not slides)
- ✅ **Proof points** for scaling to 97 applications

### Success Criteria:
```
✅ VB6 form modernized to Angular (working app)
✅ COBOL service modernized to Spring Boot (working service)
✅ All tests passing (100% test pass rate)
✅ Pattern reuse demonstrated (at least 2 patterns shared)
✅ Single-command execution (no manual steps)
✅ Traceability documents auto-generated
```

---

# Slide 2 — Scope (What We Will Build in POC)

## Technologies Included:

### Track 1: VB6 → Angular (UI Modernization)
- **Input**: VB6 `.frm` form files
- **Output**: Angular 17+ components with Material UI
- **Scope**: Single CRUD form (Customer, Policy, or similar)

### Track 2: COBOL → Spring Boot (Service Modernization)
- **Input**: COBOL `.cbl` programs + copybooks
- **Output**: Spring Boot REST API with JPA
- **Scope**: Single service (Calculation, Validation, or Data Access)

## Common Platform Components:

### Core Infrastructure (Shared Across Both Tracks):
1. **IR Engine** (shared JSON schema)
   - Technology-agnostic intermediate representation
   - Validation against schema
   - Merge partial IRs into complete IR

2. **Pattern Library** (small starter set)
   - 6-8 patterns total
   - Stored in PostgreSQL
   - Pattern matcher engine

3. **Test Generators** (language-specific)
   - Angular: Jasmine/Karma tests
   - Java: JUnit 5 tests

4. **Code Generators** (language-specific)
   - Angular: Component + Service + HTML + CSS
   - Spring Boot: Controller + Service + Model + Repository

5. **Subagent Framework** (6 subagents total)
   - 3 VB subagents (UI, Logic, Data)
   - 3 COBOL subagents (Structure, Logic, Data)

6. **Custom Commands** (2 total)
   - `modernize-vb-form`
   - `modernize-cobol-service`

## NOT in Scope (For This POC):

**Deferred to Future Phases:**
- ❌ No batch-mode factory (process multiple apps)
- ❌ No metrics dashboard UI
- ❌ No embeddings / RAG / graph DB
- ❌ No complex patterns (keep patterns simple)
- ❌ No production deployment (dev environment only)
- ❌ No Wave 2/3 demonstrations (just prove Wave 1 works)

**Rationale:**
Focus on **proving the concept end-to-end** with minimal viable complexity. Everything else can be added after POC success.

---

# Slide 3 — POC Architecture (Simplified for 2 Weeks)

## Dual-Track Modernization Pipeline:

```
┌─────────────────────────────────────────────────────────────────┐
│                    DUAL-TRACK ARCHITECTURE                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  INPUT LAYER                                                    │
│  ┌──────────────────┐              ┌──────────────────┐       │
│  │                  │              │                  │       │
│  │   VB6 Form       │              │  COBOL Program   │       │
│  │  (frmXXX.frm)    │              │  (PROGXXX.cbl)   │       │
│  │                  │              │                  │       │
│  └────────┬─────────┘              └────────┬─────────┘       │
│           │                                 │                  │
│           ↓                                 ↓                  │
│  ┌──────────────────┐              ┌──────────────────┐       │
│  │  VB Subagents    │              │ COBOL Subagents  │       │
│  │  ├─ UI Agent     │              │ ├─ Structure     │       │
│  │  ├─ Logic Agent  │              │ ├─ Logic Agent   │       │
│  │  └─ Data Agent   │              │ └─ Data Agent    │       │
│  └────────┬─────────┘              └────────┬─────────┘       │
│           │                                 │                  │
│           │    Partial IRs (JSON)           │                  │
│           │                                 │                  │
│           └────────────┬────────────────────┘                  │
│                        ↓                                        │
│  ┌─────────────────────────────────────────────────────┐      │
│  │            SHARED IR ENGINE (Core)                  │      │
│  │  ├─ Merge partial IRs                               │      │
│  │  ├─ Validate against schema                         │      │
│  │  └─ Output: Complete IR (JSON)                      │      │
│  └─────────────────────┬───────────────────────────────┘      │
│                        ↓                                        │
│  ┌─────────────────────────────────────────────────────┐      │
│  │            PATTERN MATCHER                          │      │
│  │  ├─ Query pattern library                           │      │
│  │  ├─ Match IR against known patterns                 │      │
│  │  └─ Return: Matched patterns + confidence           │      │
│  └─────────────────────┬───────────────────────────────┘      │
│                        ↓                                        │
│  ┌─────────────────────────────────────────────────────┐      │
│  │            TDD TEST GENERATORS                      │      │
│  │  ├─ Angular: Jasmine tests (.spec.ts)              │      │
│  │  └─ Java: JUnit tests (.java)                      │      │
│  └─────────────────────┬───────────────────────────────┘      │
│                        ↓                                        │
│           ┌────────────┴────────────┐                          │
│           ↓                         ↓                          │
│  ┌─────────────────┐       ┌─────────────────┐               │
│  │    Angular      │       │  Spring Boot    │               │
│  │   Generator     │       │   Generator     │               │
│  │  ├─ Component   │       │  ├─ Controller  │               │
│  │  ├─ Service     │       │  ├─ Service     │               │
│  │  ├─ HTML/CSS    │       │  ├─ Model       │               │
│  │  └─ Tests       │       │  └─ Repository  │               │
│  └────────┬────────┘       └────────┬────────┘               │
│           │                         │                          │
│           └────────────┬────────────┘                          │
│                        ↓                                        │
│  ┌─────────────────────────────────────────────────────┐      │
│  │            QUALITY GATES                            │      │
│  │  ├─ Linter (ng lint / checkstyle)                  │      │
│  │  ├─ Build (ng build / mvn compile)                 │      │
│  │  ├─ Tests (ng test / mvn test)                     │      │
│  │  └─ Coverage check (>80%)                          │      │
│  └─────────────────────┬───────────────────────────────┘      │
│                        ↓                                        │
│  ┌─────────────────────────────────────────────────────┐      │
│  │            FINAL DELIVERABLES                       │      │
│  │  ├─ Working Angular app                             │      │
│  │  ├─ Working Spring Boot service                     │      │
│  │  ├─ All tests passing                               │      │
│  │  ├─ Traceability documents                          │      │
│  │  └─ Metrics reports                                 │      │
│  └─────────────────────────────────────────────────────┘      │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## Key Simplification:

**Both tracks (VB + COBOL) use the same IR schema**, allowing a common pipeline:

```json
{
  "metadata": { ... },           // Common
  "ui_structure": { ... },       // VB only
  "business_logic": { ... },     // Common (validations, calculations)
  "data_access": { ... },        // Common (CRUD operations)
  "patterns_detected": [ ... ]   // Common
}
```

**Benefits:**
- ✅ **Single IR engine** serves both tracks
- ✅ **Pattern library shared** across VB and COBOL
- ✅ **Less code to maintain** (one pipeline, two language generators)
- ✅ **Proves cross-technology reuse** (key factory advantage)

---

# Slide 4 — Subagents We Will Actually Build

## VB Subagents (3)

### 1. `vb6-ui-agent`
**Purpose**: Extract UI structure and controls
- **Input**: `.frm` file
- **Output**: `ui_structure.json` (partial IR)
- **Extracts**:
  - Form properties (name, size, position)
  - Controls (TextBox, ComboBox, Button, Grid, etc.)
  - Control properties (name, position, size, tab order)
  - Layout information
- **Context Budget**: 20K tokens
- **Execution Time**: ~30 seconds

### 2. `vb6-logic-agent`
**Purpose**: Extract business logic and event handlers
- **Input**: `.frm` file
- **Output**: `business_logic.json` (partial IR)
- **Extracts**:
  - Event handlers (Click, Change, Load, etc.)
  - Validation rules (If Len(Trim(...)) checks)
  - Calculations (Price * Quantity)
  - State management (Enable/Disable logic)
  - MsgBox alerts
- **Context Budget**: 20K tokens
- **Execution Time**: ~30 seconds

### 3. `vb6-data-agent`
**Purpose**: Extract data access patterns
- **Input**: `.frm` file
- **Output**: `data_access.json` (partial IR)
- **Extracts**:
  - ADO/DAO connection strings
  - SQL queries (SELECT, INSERT, UPDATE, DELETE)
  - Recordset operations
  - Data binding patterns
  - Transaction management
- **Context Budget**: 20K tokens
- **Execution Time**: ~30 seconds

---

## COBOL Subagents (3)

### 1. `cobol-structure-agent`
**Purpose**: Parse COBOL program structure
- **Input**: `.cbl` file + copybooks
- **Output**: `cobol_structure.json` (partial IR)
- **Extracts**:
  - IDENTIFICATION DIVISION
  - ENVIRONMENT DIVISION
  - DATA DIVISION (Working-Storage, Linkage)
  - PROCEDURE DIVISION structure
  - Paragraphs and sections
  - COPYBOOK inclusions
- **Context Budget**: 20K tokens
- **Execution Time**: ~30 seconds

### 2. `cobol-logic-agent`
**Purpose**: Extract business logic and computations
- **Input**: `.cbl` file
- **Output**: `business_logic.json` (partial IR)
- **Extracts**:
  - IF/ELSE conditions
  - EVALUATE (switch) statements
  - COMPUTE statements
  - MOVE statements (data transformations)
  - PERFORM loops
  - Business rule logic
- **Context Budget**: 20K tokens
- **Execution Time**: ~30 seconds

### 3. `cobol-data-agent`
**Purpose**: Extract data access operations
- **Input**: `.cbl` file
- **Output**: `data_access.json` (partial IR)
- **Extracts**:
  - File I/O (READ, WRITE, REWRITE, DELETE)
  - VSAM operations
  - DB2 SQL (EXEC SQL blocks)
  - File definitions (FD entries)
  - Record layouts
- **Context Budget**: 20K tokens
- **Execution Time**: ~30 seconds

---

## Design Rules (All Subagents):

### Context Management:
- ✅ **Keep scope narrow**: Each subagent has ONE job
- ✅ **No huge context windows**: Max 20K tokens per subagent
- ✅ **Predictable output**: Always produces valid JSON
- ✅ **No overlap**: Each subagent extracts different aspects

### Execution Strategy:
- ✅ **Run in parallel**: All 3 subagents execute simultaneously
- ✅ **Fail independently**: One subagent failure doesn't block others
- ✅ **Merge outputs**: Combine all partial IRs into complete IR

### Deliverable:
```
For VB: ui_structure.json + business_logic.json + data_access.json 
        → merged into customer_complete.json

For COBOL: cobol_structure.json + business_logic.json + data_access.json
           → merged into policy_calc_complete.json
```

**Total Subagents**: 6 (3 VB + 3 COBOL)
**Total Execution Time**: ~90 seconds (parallel execution)
**Total Context Used**: ~60K tokens (vs 150K+ for monolithic approach)

---

# Slide 5 — IR Engine + Pattern Library (The Core)

## Common IR Schema:

### Schema Structure (Shared VB + COBOL):

```json
{
  "metadata": {
    "source_app": "frmCustomer.frm",
    "source_language": "VB6",
    "target_language": "Angular",
    "analyzed_date": "2025-01-15",
    "subagents_used": ["vb6-ui-agent", "vb6-logic-agent", "vb6-data-agent"]
  },
  
  "ui_structure": {                    // VB only
    "form_name": "CustomerEntry",
    "controls": [
      {
        "name": "txtCustomerName",
        "type": "TextBox",
        "required": true,
        "maxLength": 100
      }
    ]
  },
  
  "business_logic": {                  // Common (VB + COBOL)
    "validations": [
      {
        "field": "txtCustomerName",
        "rule": "required",
        "message": "Customer name is required"
      }
    ],
    "calculations": [
      {
        "name": "totalPrice",
        "formula": "price * quantity",
        "trigger": "on_quantity_change"
      }
    ],
    "state_transitions": [
      {
        "condition": "status = 'ACTIVE'",
        "action": "enable_edit_controls"
      }
    ]
  },
  
  "data_access": {                     // Common (VB + COBOL)
    "entities": ["Customer"],
    "operations": [
      {
        "type": "INSERT",
        "table": "Customers",
        "fields": ["CustomerName", "Email", "Status"]
      }
    ]
  },
  
  "patterns_detected": [               // Common
    {
      "pattern_type": "CRUD_Form",
      "confidence": 0.92,
      "template_id": "uuid-123"
    }
  ]
}
```

---

## Pattern Library v0.1 (6–8 Patterns Total):

### UI Patterns (VB Only):

#### Pattern 1: CRUD Form
```yaml
pattern_type: "CRUD_Form"
description: "Standard form with textboxes and save/cancel buttons"
structure:
  controls: ["TextBox (2-5)", "Button (Save)", "Button (Cancel)"]
  validations: ["Required fields"]
  data_access: ["INSERT", "UPDATE"]
vb6_example: "frmCustomer.frm"
angular_template: "crud-form.component.ts"
usage_count: 0  # Increments with each reuse
```

#### Pattern 2: Search Form
```yaml
pattern_type: "Search_Form"
description: "Lookup form with search box and results grid"
structure:
  controls: ["TextBox (Search)", "DataGrid", "Button (Search)"]
  validations: ["Search criteria required"]
  data_access: ["SELECT with WHERE clause"]
vb6_example: "frmPolicySearch.frm"
angular_template: "search-form.component.ts"
usage_count: 0
```

---

### Logic/Data Patterns (Shared VB + COBOL):

#### Pattern 3: Required Field Validation
```yaml
pattern_type: "Validation_Required"
description: "Check if field is empty"
vb6_signature: "If Len(Trim(txtField.Text)) = 0 Then"
cobol_signature: "IF WS-FIELD = SPACES OR WS-FIELD = LOW-VALUES"
angular_output: "Validators.required"
java_output: "@NotBlank"
usage_count: 0
```

#### Pattern 4: Pre-Save Validation Set
```yaml
pattern_type: "Pre_Save_Validation"
description: "Multiple validations before saving"
structure:
  - Required field checks
  - Format validations (email, phone)
  - Business rule validations
vb6_signature: "If Not ValidateForm() Then Exit Sub"
cobol_signature: "PERFORM VALIDATE-INPUT. IF WS-ERROR-FLAG = 'Y'"
angular_output: "this.form.valid check before submit"
java_output: "@Valid annotation + BindingResult"
usage_count: 0
```

#### Pattern 5: Insert/Update Operation
```yaml
pattern_type: "Data_Insert_Update"
description: "Save record to database"
vb6_signature: "rs.AddNew / rs.Update"
cobol_signature: "WRITE FILE-RECORD / REWRITE FILE-RECORD"
angular_output: "httpClient.post() / httpClient.put()"
java_output: "repository.save(entity)"
usage_count: 0
```

#### Pattern 6: Simple Calculation Rule
```yaml
pattern_type: "Calculation_Simple"
description: "Compute value from other fields"
vb6_signature: "txtTotal.Text = CDbl(txtPrice.Text) * CDbl(txtQty.Text)"
cobol_signature: "COMPUTE WS-TOTAL = WS-PRICE * WS-QUANTITY"
angular_output: "this.total = this.price * this.quantity"
java_output: "total = price.multiply(quantity)"
usage_count: 0
```

#### Pattern 7: Status Transition Rule
```yaml
pattern_type: "State_Transition"
description: "Change state based on condition"
vb6_signature: "If status = 'ACTIVE' Then btnEdit.Enabled = True"
cobol_signature: "IF WS-STATUS = 'ACTIVE' MOVE 'Y' TO WS-EDIT-FLAG"
angular_output: "disabled property binding based on status"
java_output: "if (status == Status.ACTIVE) { enableEdit(); }"
usage_count: 0
```

---

## Why Pattern Library is Important:

### Benefits:
- ✅ **Makes Wave 2+ faster**: Reuse patterns from Wave 1
- ✅ **Demonstrates factory effect**: 0% reuse → 60% reuse → 82% reuse
- ✅ **Provides reusability across VB + COBOL**: Same patterns apply to both
- ✅ **Proves pattern-based modernization concept**: Shows approach scales

### Pattern Metrics to Track:
```
Pattern Reuse Rate = (Patterns Reused / Total Patterns Detected) * 100%

Example:
App 1 (VB): 5 patterns detected, 0 reused (0%) ← First app
App 2 (COBOL): 4 patterns detected, 2 reused (50%) ← Pattern reuse starts
```

### Pattern Library Storage (PostgreSQL):

```sql
CREATE TABLE patterns (
  id UUID PRIMARY KEY,
  pattern_type VARCHAR(50),
  pattern_name VARCHAR(100),
  description TEXT,
  structure JSONB,
  vb6_signature TEXT,
  cobol_signature TEXT,
  angular_template TEXT,
  java_template TEXT,
  usage_count INTEGER DEFAULT 0,
  confidence_score DECIMAL(3,2),
  created_at TIMESTAMP,
  last_used_at TIMESTAMP
);

CREATE TABLE app_pattern_usage (
  app_name VARCHAR(100),
  pattern_id UUID REFERENCES patterns(id),
  reused BOOLEAN,
  confidence DECIMAL(3,2),
  modernized_at TIMESTAMP
);
```

**Deliverable**: Pattern library with 6-8 starter patterns, ready to track reuse.

---

# Slide 6 — TDD Engine + Code Generators

## TDD Workflow (For Both VB & COBOL):

### Phase 1: Generate Tests from IR

```
Input: customer_complete.json (IR)
   ↓
Test Generator (Angular or Java)
   ↓
Output: Comprehensive test suite
```

**Angular Test Generation (Jasmine/Karma):**
```typescript
// customer.component.spec.ts
describe('CustomerComponent', () => {
  // Test 1: Form initialization
  it('should create the form with all controls', () => {
    expect(component.customerForm).toBeDefined();
    expect(component.customerForm.get('customerName')).toBeDefined();
  });
  
  // Test 2: Required field validation (from IR)
  it('should mark customerName as invalid when empty', () => {
    const control = component.customerForm.get('customerName');
    control?.setValue('');
    expect(control?.hasError('required')).toBe(true);
  });
  
  // Test 3: Calculation (from IR business_logic)
  it('should calculate total correctly', () => {
    component.customerForm.patchValue({ price: 10, quantity: 5 });
    expect(component.total).toBe(50);
  });
  
  // Test 4: Save operation (from IR data_access)
  it('should call API on save', () => {
    spyOn(service, 'saveCustomer').and.returnValue(of({}));
    component.save();
    expect(service.saveCustomer).toHaveBeenCalled();
  });
});
```

**Java Test Generation (JUnit 5):**
```java
// PolicyCalculatorTest.java
class PolicyCalculatorTest {
    
    // Test 1: Required field validation (from IR)
    @Test
    void shouldFailWhenPolicyNumberIsEmpty() {
        PolicyRequest request = new PolicyRequest();
        request.setPolicyNumber("");
        
        Set<ConstraintViolation<PolicyRequest>> violations = 
            validator.validate(request);
        assertFalse(violations.isEmpty());
    }
    
    // Test 2: Calculation (from IR business_logic)
    @Test
    void shouldCalculatePremiumCorrectly() {
        BigDecimal premium = service.calculatePremium(
            new BigDecimal("1000"), new BigDecimal("0.05")
        );
        assertEquals(new BigDecimal("50.00"), premium);
    }
    
    // Test 3: Insert operation (from IR data_access)
    @Test
    void shouldSavePolicyToDatabase() {
        Policy policy = new Policy();
        policy.setPolicyNumber("POL-123");
        
        Policy saved = repository.save(policy);
        assertNotNull(saved.getId());
    }
}
```

---

### Phase 2: Test Suite Must Fail First

```bash
# Run Angular tests (should all fail - no implementation yet)
ng test
# Result: ❌ 0/10 tests passing

# Run Java tests (should all fail - no implementation yet)
mvn test
# Result: ❌ 0/8 tests passing

# Gate: If ANY test passes → ERROR (implementation shouldn't exist yet)
```

**Why this matters**: Proves tests are actually testing something (not false positives).

---

### Phase 3: Generator Produces Implementation

**Angular Generator Output:**

```
angular_app/src/app/customer/
├── customer.component.ts         # Business logic
├── customer.component.html       # Template
├── customer.component.css        # Styles
├── customer.component.spec.ts    # Tests (already generated)
└── customer.service.ts           # API calls
```

**customer.component.ts (Generated from IR):**
```typescript
import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { CustomerService } from './customer.service';

@Component({
  selector: 'app-customer',
  templateUrl: './customer.component.html'
})
export class CustomerComponent implements OnInit {
  customerForm: FormGroup;
  total: number = 0;

  constructor(
    private fb: FormBuilder,
    private service: CustomerService
  ) {}

  ngOnInit() {
    // Generated from IR ui_structure
    this.customerForm = this.fb.group({
      customerName: ['', Validators.required],  // From IR validation
      price: [0],
      quantity: [0]
    });

    // Generated from IR business_logic (calculation)
    this.customerForm.valueChanges.subscribe(() => {
      this.total = this.customerForm.value.price * 
                    this.customerForm.value.quantity;
    });
  }

  save() {
    // Generated from IR data_access
    if (this.customerForm.valid) {
      this.service.saveCustomer(this.customerForm.value).subscribe();
    }
  }
}
```

---

**Spring Boot Generator Output:**

```
java_app/src/main/java/com/aig/policy/
├── controller/
│   └── PolicyController.java      # REST endpoints
├── service/
│   └── PolicyService.java         # Business logic
├── model/
│   └── Policy.java                # Entity/DTO
├── repository/
│   └── PolicyRepository.java      # Data access
└── test/
    └── PolicyServiceTest.java     # Tests (already generated)
```

**PolicyController.java (Generated from IR):**
```java
@RestController
@RequestMapping("/api/policies")
public class PolicyController {
    
    private final PolicyService service;
    
    @PostMapping
    public ResponseEntity<Policy> createPolicy(
        @Valid @RequestBody Policy policy  // From IR validation
    ) {
        // Generated from IR data_access
        Policy saved = service.save(policy);
        return ResponseEntity.ok(saved);
    }
    
    @GetMapping("/{id}")
    public ResponseEntity<Policy> getPolicy(@PathVariable Long id) {
        return service.findById(id)
            .map(ResponseEntity::ok)
            .orElse(ResponseEntity.notFound().build());
    }
}
```

**PolicyService.java (Generated from IR):**
```java
@Service
public class PolicyService {
    
    private final PolicyRepository repository;
    
    public Policy save(Policy policy) {
        // Generated from IR business_logic (validation)
        if (policy.getPolicyNumber() == null || 
            policy.getPolicyNumber().isEmpty()) {
            throw new ValidationException("Policy number is required");
        }
        
        // Generated from IR business_logic (calculation)
        BigDecimal premium = policy.getCoverageAmount()
            .multiply(policy.getRiskFactor());
        policy.setPremium(premium);
        
        // Generated from IR data_access
        return repository.save(policy);
    }
}
```

---

### Phase 4: Run Tests → Fix → Rerun → Repeat Until Green

```bash
# Iteration 1: Run tests after first generation
ng test
# Result: ❌ 7/10 passing, 3 failing

# Analyze failures, fix implementation
# Iteration 2: Run tests again
ng test
# Result: ❌ 9/10 passing, 1 failing

# Analyze failure, fix implementation
# Iteration 3: Run tests again
ng test
# Result: ✅ 10/10 passing

# Same process for Java
mvn test
# Iterate until: ✅ 8/8 passing
```

**Max Iterations**: 5 (if not passing by then, escalate to manual review)

---

## Success Criteria:

### Both Pipelines Must Produce:
- ✅ **Working code** (compiles and runs)
- ✅ **All tests passing** (100% pass rate)
- ✅ **Traceability auto-produced** (mapping document)
- ✅ **No manual intervention** (fully automated)

### Quality Gates (Automated):
```bash
# Angular quality gates
ng lint           # Must pass (no errors)
ng build          # Must succeed
ng test           # Must pass (100%)
ng test --code-coverage  # Must be >80%

# Java quality gates
mvn checkstyle:check     # Must pass
mvn compile              # Must succeed
mvn test                 # Must pass (100%)
mvn jacoco:check         # Must be >80% coverage
```

---

# Slide 7 — Orchestration (One-Command Experience)

## Commands to be Delivered:

### Command 1: `modernize-vb-form`
```bash
./modernize-vb-form.sh path/to/form.frm
```

### Command 2: `modernize-cobol-service`
```bash
./modernize-cobol-service.sh path/to/program.cbl
```

---

## What Each Command Performs:

### Complete Workflow (Automated):

```
┌─────────────────────────────────────────────────────────────┐
│              ONE-COMMAND WORKFLOW                           │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  Step 1: Run Subagents in Parallel                         │
│  ├─ Launch 3 subagents simultaneously                      │
│  ├─ Each generates partial IR (JSON)                       │
│  └─ Wait for all to complete (~90 seconds)                 │
│                                                             │
│  Step 2: Merge IRs                                          │
│  ├─ Combine 3 partial IRs into 1 complete IR              │
│  ├─ Validate against schema                                │
│  └─ Save to ./ir/{app_name}_complete.json                 │
│                                                             │
│  Step 3: Match Patterns                                     │
│  ├─ Query pattern library (PostgreSQL)                     │
│  ├─ Calculate similarity scores                            │
│  ├─ Attach matched patterns to IR                          │
│  └─ If match found: use template (faster generation)       │
│                                                             │
│  Step 4: Generate Tests                                     │
│  ├─ Angular: Generate .spec.ts files                       │
│  ├─ Java: Generate JUnit test classes                      │
│  ├─ Run tests: ALL should fail ❌                         │
│  └─ Gate: If any pass → ERROR                             │
│                                                             │
│  Step 5: Generate Implementation                            │
│  ├─ Angular: Generate component + service + HTML          │
│  ├─ Java: Generate controller + service + model           │
│  ├─ Map IR elements to code elements                       │
│  └─ Use pattern templates if available                     │
│                                                             │
│  Step 6: Iterative Testing (TDD Loop)                       │
│  ├─ Run tests → Some fail ❌                              │
│  ├─ Analyze failures                                       │
│  ├─ Fix implementation                                     │
│  ├─ Run tests again → Fewer failures                       │
│  └─ Repeat until all pass ✅ (max 5 iterations)           │
│                                                             │
│  Step 7: Apply Quality Gates                               │
│  ├─ Linter: ng lint / mvn checkstyle:check                │
│  ├─ Build: ng build / mvn compile                         │
│  ├─ Tests: ng test / mvn test (100% pass required)        │
│  └─ Coverage: Must be >80%                                 │
│                                                             │
│  Step 8: Generate Outputs                                   │
│  ├─ Final code (working app/service)                       │
│  ├─ Tests (all passing)                                    │
│  ├─ Traceability document (VB→Angular mapping)            │
│  └─ Metrics report (JSON)                                  │
│                                                             │
│  Step 9: Update Pattern Library                             │
│  ├─ Increment usage_count for reused patterns             │
│  ├─ Add new patterns if discovered                         │
│  └─ Update confidence scores                               │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

## Example Output (Console):

```bash
$ ./modernize-vb-form.sh vb_source/frmCustomer.frm

🚀 AIG Modernization Pipeline - VB6 → Angular
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

[1/9] Running VB Subagents (Parallel)...
  ✓ vb6-ui-agent completed (28s)
  ✓ vb6-logic-agent completed (31s)
  ✓ vb6-data-agent completed (26s)
  📊 Total: 85 seconds

[2/9] Merging Partial IRs...
  ✓ Merged 3 partial IRs into complete IR
  ✓ Validated against schema
  📄 Saved: ./ir/customer_complete.json (5.2 KB)

[3/9] Matching Patterns...
  🔍 Searching pattern library...
  ✓ Found 3 matches:
    - CRUD_Form (92% confidence) ← REUSED
    - Validation_Required (95% confidence) ← REUSED
    - Data_Insert_Update (88% confidence) ← REUSED
  📈 Pattern reuse: 60% (3/5 patterns)

[4/9] Generating Tests...
  ✓ Generated Angular test suite (18 tests)
  ⚠️  Running tests: 0/18 passing (expected - no implementation yet)

[5/9] Generating Implementation...
  ✓ Generated customer.component.ts
  ✓ Generated customer.component.html
  ✓ Generated customer.service.ts
  📦 Using pattern templates (3x faster)

[6/9] TDD Iteration Loop...
  Iteration 1: ❌ 12/18 tests passing
  Iteration 2: ❌ 16/18 tests passing
  Iteration 3: ✅ 18/18 tests passing

[7/9] Running Quality Gates...
  ✓ Linter: Passed (0 errors)
  ✓ Build: Succeeded
  ✓ Tests: 18/18 passing (100%)
  ✓ Coverage: 87% (>80% required)

[8/9] Generating Documentation...
  ✓ Traceability: ./output/customer_traceability.md
  ✓ Metrics: ./output/customer_metrics.json

[9/9] Updating Pattern Library...
  ✓ Incremented usage_count for 3 patterns
  ✓ Pattern library updated

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✅ MODERNIZATION COMPLETE

📊 Summary:
  • Source: frmCustomer.frm (VB6)
  • Target: Angular 17 component
  • Tests: 18/18 passing (100%)
  • Coverage: 87%
  • Automation: 73%
  • Patterns reused: 3/5 (60%)
  • Time elapsed: 8 min 42 sec

📂 Outputs:
  • Angular App: ./angular_app/src/app/customer/
  • Tests: customer.component.spec.ts (all passing)
  • Traceability: ./output/customer_traceability.md
  • Metrics: ./output/customer_metrics.json

🎯 Next Step: Review generated code and run demo
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

---

## Outcome:

**Developers can modernize a VB or COBOL artifact end-to-end with one command.**

### Benefits:
- ✅ **No manual steps** (fully automated)
- ✅ **Consistent output** (same workflow every time)
- ✅ **Quality guaranteed** (tests + gates)
- ✅ **Fast execution** (8-12 minutes per app)
- ✅ **Traceable** (all steps logged)
- ✅ **Demo-ready** (just run the command)

---

# Slide 8 — 2-Week Execution Plan (Realistic)

## Week 1 — Make the Pipeline Work

### **Day 1-2: Foundation Setup**

**Deliverables:**
- ✅ Git repo created and structured
- ✅ PostgreSQL pattern library setup
- ✅ IR schema defined (JSON schema file)
- ✅ Project folder structure complete

**Tasks:**
```bash
# Project structure
aig-poc/
├── vb_source/              # Sample VB6 forms
├── cobol_source/           # Sample COBOL programs
├── ir/                     # Generated IRs
├── angular_app/            # Generated Angular
├── java_app/               # Generated Spring Boot
├── output/                 # Traceability + metrics
├── .claude/
│   ├── subagents/          # 6 subagent definitions
│   └── commands/           # 2 custom commands
├── standards/              # AIG coding standards
├── schemas/
│   └── ir_schema.json      # IR validation schema
└── scripts/
    ├── setup.sh            # Environment setup
    ├── modernize-vb-form.sh
    └── modernize-cobol-service.sh
```

**Setup Database:**
```sql
CREATE DATABASE aig_patterns;

CREATE TABLE patterns (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  pattern_type VARCHAR(50) NOT NULL,
  pattern_name VARCHAR(100) NOT NULL,
  description TEXT,
  structure JSONB,
  vb6_signature TEXT,
  cobol_signature TEXT,
  angular_template TEXT,
  java_template TEXT,
  usage_count INTEGER DEFAULT 0,
  confidence_score DECIMAL(3,2) DEFAULT 0.00,
  created_at TIMESTAMP DEFAULT NOW(),
  last_used_at TIMESTAMP
);

-- Seed initial patterns
INSERT INTO patterns (pattern_type, pattern_name, description, vb6_signature, angular_template)
VALUES 
  ('CRUD_Form', 'Standard CRUD Form', 'Form with save/cancel buttons', 
   'If Len(Trim(txtField.Text)) = 0', 'Validators.required'),
  ('Validation_Required', 'Required Field Check', 'Check if field is empty',
   'If Len(Trim(...)) = 0 Then', 'Validators.required'),
  -- ... more patterns
```

---

### **Day 3-4: Build Subagents (Minimal Viability)**

**Deliverables:**
- ✅ 6 subagent definitions complete
- ✅ Each subagent can generate partial IR
- ✅ Basic validation working

**VB Subagent Example:**
```yaml
# .claude/subagents/vb6-ui-agent.yml
name: vb6-ui-agent
description: Extract UI structure from VB6 forms
context_budget: 20000
tools: [read_file, view]
input_format: ".frm file path"
output_format: "JSON (ui_structure.json)"

instructions: |
  You are a VB6 UI extraction specialist.
  
  Task: Extract ONLY UI structure from the .frm file.
  
  DO extract:
  - Form properties (Name, Caption, Width, Height)
  - All controls (TextBox, Label, ComboBox, Button, etc.)
  - Control properties (Name, Caption, Position, Size, TabIndex)
  - Layout information
  
  DO NOT extract:
  - Event handler code (logic agent handles this)
  - Data access code (data agent handles this)
  
  Output must be valid JSON matching this structure:
  {
    "form_name": "CustomerEntry",
    "form_properties": {...},
    "controls": [
      {
        "name": "txtCustomerName",
        "type": "TextBox",
        "properties": {...}
      }
    ]
  }
  
  Save output to: ./ir/{form_name}_ui_structure.json
```

**Testing Subagents:**
```bash
# Test each subagent individually
claude --subagent vb6-ui-agent \
  --input vb_source/frmCustomer.frm \
  --output ir/customer_ui_structure.json

# Verify output is valid JSON
python3 -m json.tool ir/customer_ui_structure.json

# Success: ✅ Valid JSON generated
```

---

### **Day 5-6: IR Merger + Basic Generators**

**Deliverables:**
- ✅ IR merger working (combines 3 partial IRs)
- ✅ Angular skeleton generator working
- ✅ Spring Boot skeleton generator working

**IR Merger Script:**
```python
# scripts/merge_irs.py
import json
import sys

def merge_irs(ui_ir, logic_ir, data_ir, output_file):
    """Merge 3 partial IRs into complete IR"""
    
    with open(ui_ir) as f:
        ui = json.load(f)
    with open(logic_ir) as f:
        logic = json.load(f)
    with open(data_ir) as f:
        data = json.load(f)
    
    complete_ir = {
        "metadata": {
            "source_app": ui.get("form_name", "unknown"),
            "source_language": "VB6",
            "target_language": "Angular",
            "analyzed_date": datetime.now().isoformat()
        },
        "ui_structure": ui,
        "business_logic": logic,
        "data_access": data,
        "patterns_detected": []  # Filled by pattern matcher later
    }
    
    # Validate against schema
    validate_ir(complete_ir, "schemas/ir_schema.json")
    
    with open(output_file, 'w') as f:
        json.dump(complete_ir, f, indent=2)
    
    print(f"✅ Merged IR saved to {output_file}")

if __name__ == "__main__":
    merge_irs(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4])
```

**Angular Skeleton Generator:**
```typescript
// scripts/generate_angular_skeleton.ts
import * as fs from 'fs';

function generateSkeleton(irPath: string, outputDir: string) {
  const ir = JSON.parse(fs.readFileSync(irPath, 'utf-8'));
  
  // Generate component.ts
  const componentCode = `
import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';

@Component({
  selector: 'app-${ir.metadata.source_app.toLowerCase()}',
  templateUrl: './${ir.metadata.source_app.toLowerCase()}.component.html'
})
export class ${ir.metadata.source_app}Component implements OnInit {
  form: FormGroup;
  
  constructor(private fb: FormBuilder) {}
  
  ngOnInit() {
    this.form = this.fb.group({
      // TODO: Generate from IR
    });
  }
  
  save() {
    // TODO: Generate from IR
  }
}
  `;
  
  fs.writeFileSync(
    `${outputDir}/${ir.metadata.source_app.toLowerCase()}.component.ts`,
    componentCode
  );
  
  console.log('✅ Angular skeleton generated');
}
```

---

### **Day 7: End-to-End Run (First Success)**

**Deliverables:**
- ✅ Complete pipeline working for 1 VB form
- ✅ Complete pipeline working for 1 COBOL program
- ✅ Generated code compiles (even if incomplete)

**Test Run:**
```bash
# VB → Angular
./modernize-vb-form.sh vb_source/frmCustomer.frm

# Expected output:
# ✅ 3 partial IRs generated
# ✅ Complete IR merged
# ✅ Angular skeleton generated
# ⚠️  No tests yet (Week 2)
# ⚠️  No pattern matching yet (Week 2)

# COBOL → Java
./modernize-cobol-service.sh cobol_source/POLCALC.cbl

# Expected output:
# ✅ 3 partial IRs generated
# ✅ Complete IR merged
# ✅ Spring Boot skeleton generated
# ⚠️  No tests yet (Week 2)
# ⚠️  No pattern matching yet (Week 2)
```

**Success Criteria:**
- ✅ Both pipelines execute without errors
- ✅ IR files are valid JSON
- ✅ Generated code compiles (ng build / mvn compile)
- ⚠️  Not fully functional yet (Week 2 adds logic)

---

## Week 2 — Add Intelligence + Polish

### **Day 8-9: Pattern Matcher + Test Generators**

**Deliverables:**
- ✅ Pattern matcher v0.1 working
- ✅ Angular test generator working
- ✅ Java test generator working
- ✅ TDD loop implemented

**Pattern Matcher:**
```python
# scripts/pattern_matcher.py
import json
import psycopg2

def match_patterns(ir_path):
    """Match IR against pattern library"""
    
    with open(ir_path) as f:
        ir = json.load(f)
    
    conn = psycopg2.connect("dbname=aig_patterns")
    cur = conn.cursor()
    
    matches = []
    
    # Simple pattern matching (can be enhanced later)
    # Check if it's a CRUD form
    if has_save_button(ir) and has_textboxes(ir):
        cur.execute("SELECT * FROM patterns WHERE pattern_type = 'CRUD_Form'")
        pattern = cur.fetchone()
        matches.append({
            "pattern_type": "CRUD_Form",
            "confidence": 0.92,
            "template_id": pattern[0]
        })
    
    # Check for required validations
    if has_required_validations(ir):
        cur.execute("SELECT * FROM patterns WHERE pattern_type = 'Validation_Required'")
        pattern = cur.fetchone()
        matches.append({
            "pattern_type": "Validation_Required",
            "confidence": 0.95,
            "template_id": pattern[0]
        })
    
    # Add matches to IR
    ir["patterns_detected"] = matches
    
    with open(ir_path, 'w') as f:
        json.dump(ir, f, indent=2)
    
    print(f"✅ Matched {len(matches)} patterns")
    return matches
```

**Test Generator:**
```typescript
// scripts/generate_angular_tests.ts
function generateTests(irPath: string, outputPath: string) {
  const ir = JSON.parse(fs.readFileSync(irPath, 'utf-8'));
  
  let tests = `
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ${ir.metadata.source_app}Component } from './${ir.metadata.source_app.toLowerCase()}.component';

describe('${ir.metadata.source_app}Component', () => {
  let component: ${ir.metadata.source_app}Component;
  let fixture: ComponentFixture<${ir.metadata.source_app}Component>;
  
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ${ir.metadata.source_app}Component ]
    }).compileComponents();
    
    fixture = TestBed.createComponent(${ir.metadata.source_app}Component);
    component = fixture.componentInstance;
  });
  `;
  
  // Generate tests from IR business_logic
  ir.business_logic.validations.forEach(validation => {
    tests += `
  it('should validate ${validation.field} as ${validation.rule}', () => {
    const control = component.form.get('${validation.field}');
    control?.setValue('');
    expect(control?.hasError('${validation.rule}')).toBe(true);
  });
    `;
  });
  
  tests += `\n});\n`;
  
  fs.writeFileSync(outputPath, tests);
  console.log('✅ Angular tests generated');
}
```

---

### **Day 10: Complete Implementation + TDD Loop**

**Deliverables:**
- ✅ Full Angular component generator (not just skeleton)
- ✅ Full Spring Boot generator (not just skeleton)
- ✅ TDD loop working (iterate until tests pass)
- ✅ Quality gates implemented

**Complete Angular Generator:**
```typescript
// Now generates FULL implementation from IR
function generateAngularComponent(irPath: string, outputDir: string) {
  const ir = JSON.parse(fs.readFileSync(irPath, 'utf-8'));
  
  // Generate form controls from IR ui_structure
  const formControls = ir.ui_structure.controls.map(control => {
    const validators = [];
    
    // Add validators from IR business_logic
    ir.business_logic.validations.forEach(validation => {
      if (validation.field === control.name) {
        if (validation.rule === 'required') {
          validators.push('Validators.required');
        }
        if (validation.rule === 'email') {
          validators.push('Validators.email');
        }
      }
    });
    
    return `${control.name}: ['', [${validators.join(', ')}]]`;
  });
  
  // Generate calculations from IR business_logic
  const calculations = ir.business_logic.calculations.map(calc => {
    return `
    this.form.get('${calc.trigger}')?.valueChanges.subscribe(() => {
      this.${calc.name} = ${calc.formula};
    });
    `;
  });
  
  // Generate save method from IR data_access
  const saveMethod = `
  save() {
    if (this.form.valid) {
      this.service.${ir.data_access.operations[0].type.toLowerCase()}(
        this.form.value
      ).subscribe(
        () => this.snackBar.open('Saved successfully'),
        (error) => this.snackBar.open('Error: ' + error.message)
      );
    }
  }
  `;
  
  const fullComponent = `
import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { ${ir.metadata.source_app}Service } from './${ir.metadata.source_app.toLowerCase()}.service';
import { MatSnackBar } from '@angular/material/snack-bar';

@Component({
  selector: 'app-${ir.metadata.source_app.toLowerCase()}',
  templateUrl: './${ir.metadata.source_app.toLowerCase()}.component.html'
})
export class ${ir.metadata.source_app}Component implements OnInit {
  form: FormGroup;
  ${ir.business_logic.calculations.map(c => `${c.name}: number = 0;`).join('\n  ')}
  
  constructor(
    private fb: FormBuilder,
    private service: ${ir.metadata.source_app}Service,
    private snackBar: MatSnackBar
  ) {}
  
  ngOnInit() {
    this.form = this.fb.group({
      ${formControls.join(',\n      ')}
    });
    
    ${calculations.join('\n    ')}
  }
  
  ${saveMethod}
}
  `;
  
  fs.writeFileSync(
    `${outputDir}/${ir.metadata.source_app.toLowerCase()}.component.ts`,
    fullComponent
  );
  
  console.log('✅ Full Angular component generated');
}
```

**TDD Loop:**
```bash
#!/bin/bash
# scripts/tdd_loop.sh

MAX_ITERATIONS=5
iteration=1

while [ $iteration -le $MAX_ITERATIONS ]; do
  echo "TDD Iteration $iteration..."
  
  cd angular_app
  npm test -- --watch=false
  
  if [ $? -eq 0 ]; then
    echo "✅ All tests pass! TDD complete."
    break
  else
    echo "❌ Some tests failed. Analyzing and fixing..."
    cd ..
    
    # Analyze failures and fix implementation
    node scripts/fix_implementation.js \
      --test-results angular_app/test-results.xml \
      --ir ir/customer_complete.json
    
    iteration=$((iteration + 1))
  fi
done

if [ $iteration -gt $MAX_ITERATIONS ]; then
  echo "❌ Failed to pass all tests after $MAX_ITERATIONS iterations"
  exit 1
fi
```

---

### **Day 11: Custom Commands + Orchestration**

**Deliverables:**
- ✅ `modernize-vb-form.sh` complete
- ✅ `modernize-cobol-service.sh` complete
- ✅ Both commands execute full pipeline
- ✅ Console output polished

**Complete Command Script:**
```bash
#!/bin/bash
# modernize-vb-form.sh

set -e  # Exit on error

VB_FILE=$1
APP_NAME=$(basename "$VB_FILE" .frm)

echo "🚀 AIG Modernization Pipeline - VB6 → Angular"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Step 1: Run subagents
echo "[1/9] Running VB Subagents (Parallel)..."
start_time=$(date +%s)

claude --subagent vb6-ui-agent \
  --input "$VB_FILE" \
  --output "ir/${APP_NAME}_ui_structure.json" &
PID1=$!

claude --subagent vb6-logic-agent \
  --input "$VB_FILE" \
  --output "ir/${APP_NAME}_business_logic.json" &
PID2=$!

claude --subagent vb6-data-agent \
  --input "$VB_FILE" \
  --output "ir/${APP_NAME}_data_access.json" &
PID3=$!

wait $PID1 $PID2 $PID3

end_time=$(date +%s)
elapsed=$((end_time - start_time))
echo "  ✓ All subagents completed (${elapsed}s)"
echo ""

# Step 2: Merge IRs
echo "[2/9] Merging Partial IRs..."
python3 scripts/merge_irs.py \
  "ir/${APP_NAME}_ui_structure.json" \
  "ir/${APP_NAME}_business_logic.json" \
  "ir/${APP_NAME}_data_access.json" \
  "ir/${APP_NAME}_complete.json"
echo ""

# Step 3: Pattern matching
echo "[3/9] Matching Patterns..."
python3 scripts/pattern_matcher.py "ir/${APP_NAME}_complete.json"
echo ""

# Step 4: Generate tests
echo "[4/9] Generating Tests..."
node scripts/generate_angular_tests.js "ir/${APP_NAME}_complete.json" \
  "angular_app/src/app/${APP_NAME}/${APP_NAME}.component.spec.ts"
echo ""

# Step 5: Generate implementation
echo "[5/9] Generating Implementation..."
node scripts/generate_angular_component.js "ir/${APP_NAME}_complete.json" \
  "angular_app/src/app/${APP_NAME}/"
echo ""

# Step 6: TDD loop
echo "[6/9] TDD Iteration Loop..."
./scripts/tdd_loop.sh "${APP_NAME}"
echo ""

# Step 7: Quality gates
echo "[7/9] Running Quality Gates..."
cd angular_app
ng lint --fix
ng build --configuration production
ng test --watch=false --code-coverage
cd ..
echo ""

# Step 8: Generate documentation
echo "[8/9] Generating Documentation..."
node scripts/generate_traceability.js "ir/${APP_NAME}_complete.json" \
  "output/${APP_NAME}_traceability.md"
node scripts/generate_metrics.js "ir/${APP_NAME}_complete.json" \
  "output/${APP_NAME}_metrics.json"
echo ""

# Step 9: Update pattern library
echo "[9/9] Updating Pattern Library..."
python3 scripts/update_pattern_usage.py "ir/${APP_NAME}_complete.json"
echo ""

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "✅ MODERNIZATION COMPLETE"
echo ""
echo "📊 Summary:"
cat "output/${APP_NAME}_metrics.json"
echo ""
echo "📂 Outputs:"
echo "  • Angular App: ./angular_app/src/app/${APP_NAME}/"
echo "  • Traceability: ./output/${APP_NAME}_traceability.md"
echo "  • Metrics: ./output/${APP_NAME}_metrics.json"
echo ""
echo "🎯 Next Step: Review generated code and run demo"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
```

---

### **Day 12: Demo Preparation + Dry Run**

**Deliverables:**
- ✅ Demo script polished
- ✅ Sample apps ready (1 VB, 1 COBOL)
- ✅ Dry run successful
- ✅ Documentation complete
- ✅ Backup plan ready (recorded demo video)

**Demo Checklist:**
- [x] Environment setup script tested
- [x] Pattern library pre-populated with 6-8 patterns
- [x] Sample VB form prepared (frmCustomer.frm)
- [x] Sample COBOL program prepared (POLCALC.cbl)
- [x] Both modernization commands working
- [x] All tests passing
- [x] Console output polished and readable
- [x] Traceability documents auto-generated
- [x] Metrics reports auto-generated
- [x] Demo narrative prepared
- [x] Q&A responses prepared
- [x] Backup: Demo video recorded

---

## POC Complete:

### ✅ Working VB → Angular Path
- Subagents extract VB code
- IR generated and validated
- Patterns matched
- Tests generated and passing
- Angular component working
- Quality gates passed

### ✅ Working COBOL → Spring Boot Path
- Subagents extract COBOL code
- IR generated and validated
- Patterns matched
- Tests generated and passing
- Spring Boot service working
- Quality gates passed

### ✅ Shared Factory Backbone
- Common IR engine
- Common pattern library
- Common TDD workflow
- Common quality gates

### ✅ Pattern Reuse Demonstrated
- VB app uses patterns
- COBOL app reuses same patterns
- Usage counts tracked
- Metrics show reuse rates

### ✅ Single-Command Modernization Demo Ready
- `./modernize-vb-form.sh` works end-to-end
- `./modernize-cobol-service.sh` works end-to-end
- Both commands produce working code
- All outputs auto-generated

---

# Success Metrics Summary

## Technical Metrics:
```
✅ Subagents: 6/6 working
✅ IR Schema: Validated
✅ Pattern Library: 6-8 patterns
✅ Test Pass Rate: 100% (all tests passing)
✅ Build Success: 100% (all code compiles)
✅ Coverage: >80%
✅ Automation: >70% code auto-generated
✅ Context Efficiency: <30K tokens per subagent
```

## Business Metrics:
```
✅ Time per App: 8-12 minutes (automated)
✅ Pattern Reuse: Demonstrated (VB + COBOL)
✅ Traceability: 100% (all mappings documented)
✅ Repeatability: Single command = complete modernization
```

## Demo Readiness:
```
✅ Working VB → Angular demo
✅ Working COBOL → Spring Boot demo
✅ Pattern reuse visible
✅ Metrics auto-generated
✅ Console output polished
✅ Backup plan ready
```

---

# What AIG Will See

## Live Demo (12 minutes):

**Part 1: VB → Angular (6 minutes)**
```bash
$ ./modernize-vb-form.sh vb_source/frmCustomer.frm

[Watch console output scroll through 9 steps]

✅ MODERNIZATION COMPLETE
   • Tests: 18/18 passing
   • Coverage: 87%
   • Patterns reused: 3/5 (60%)
   • Time: 8 min 42 sec
```

**Part 2: COBOL → Spring Boot (6 minutes)**
```bash
$ ./modernize-cobol-service.sh cobol_source/POLCALC.cbl

[Watch console output scroll through 9 steps]

✅ MODERNIZATION COMPLETE
   • Tests: 12/12 passing
   • Coverage: 84%
   • Patterns reused: 2/4 (50%)
   • Time: 7 min 18 sec
```

## Key Proof Points:

1. **One Command = Working App** ✅
2. **Tests Prove Correctness** ✅
3. **Pattern Reuse Demonstrated** ✅
4. **Scales to Both VB + COBOL** ✅
5. **Traceability Auto-Generated** ✅
6. **Metrics Auto-Generated** ✅

---

**END OF TECHNICAL SLIDES**

---

## Appendix: Key Files to Review

```
📂 Deliverables:
├── README.md                          # POC overview
├── ARCHITECTURE.md                    # Technical architecture
├── .claude/
│   ├── subagents/
│   │   ├── vb6-ui-agent.yml
│   │   ├── vb6-logic-agent.yml
│   │   ├── vb6-data-agent.yml
│   │   ├── cobol-structure-agent.yml
│   │   ├── cobol-logic-agent.yml
│   │   └── cobol-data-agent.yml
│   └── commands/
│       ├── modernize-vb-form.md
│       └── modernize-cobol-service.md
├── schemas/
│   └── ir_schema.json                 # IR validation schema
├── scripts/
│   ├── modernize-vb-form.sh           # Main VB command
│   ├── modernize-cobol-service.sh     # Main COBOL command
│   ├── merge_irs.py                   # IR merger
│   ├── pattern_matcher.py             # Pattern matching
│   ├── generate_angular_tests.ts      # Test generator
│   ├── generate_angular_component.ts  # Code generator
│   └── tdd_loop.sh                    # TDD iteration
├── output/
│   ├── customer_traceability.md       # VB→Angular mapping
│   ├── customer_metrics.json          # VB metrics
│   ├── policy_calc_traceability.md    # COBOL→Java mapping
│   └── policy_calc_metrics.json       # COBOL metrics
└── DEMO.md                            # Demo script
```

---

**Ready to execute? This is your 2-week plan to POC success! 🚀**
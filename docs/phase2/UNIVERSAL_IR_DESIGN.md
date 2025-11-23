# Universal IR Schema Design

## Overview

The **Universal Intermediate Representation (Universal IR)** is a language-agnostic schema that standardizes parsed code from multiple legacy languages (VB6, COBOL, PowerBuilder, etc.) into a unified format. This document details the schema design, field definitions, extension rules, and confidence metrics.

## Schema Location

**File**: `src/core/universal_ir_schema.py` (700+ lines)
**Technology**: Pydantic v2 models
**Format**: Python dataclasses with automatic JSON serialization

## Schema Version

```
universal-ir-v1
```

All Universal IR instances include a `schema_version` field to support future schema evolution.

## Top-Level Structure

```python
class UniversalIR(BaseModel):
    """Root Universal IR model"""
    metadata: SourceMetadata
    data_structures: DataStructuresSection
    ui: UISection
    business_logic: BusinessLogicSection
    io_operations: IOOperationsSection
    data_operations: DataOperationsSection
    events: EventsSection
    patterns: List[DetectedPattern]
    external_references: ExternalReferencesSection
    security_issues: List[str]
    repository_mapping: RepositoryMappingSection
    frontend_mapping: FrontendMappingSection
    generation_metadata: GenerationMetadata
    schema_version: str = "universal-ir-v1"
```

## 12 Major Sections

### 1. Metadata Section

**Purpose**: Source file metadata, confidence scores, analysis information

**Schema**:
```python
class SourceMetadata(BaseModel):
    source_language: str                    # "VB6", "COBOL", "PowerBuilder", etc.
    source_file: str                        # "StartForm.frm", "seq.cbl"
    source_lines_of_code: int               # Total LOC
    source_path: Optional[str]              # Full path to source file
    target_framework: str                   # "Angular", "SpringBoot", "React"
    analysis_timestamp: str                 # ISO 8601 timestamp
    analyzer_version: str                   # "1.0.0-langgraph-vb6"
    parser_type: str                        # "langgraph-vb6-subagent"

    # Confidence scores (0.0 - 1.0)
    confidence: float                       # Overall weighted average
    ui_agent_confidence: Optional[float]    # VB6 only
    data_agent_confidence: Optional[float]  # VB6 + COBOL
    logic_agent_confidence: Optional[float] # VB6 + COBOL
    io_agent_confidence: Optional[float]    # COBOL only

    # Complexity assessment
    complexity: str                         # "simple", "medium", "complex"
    complexity_score: Optional[int]         # 1-10 scale

    # Sub-agents used
    subagents_used: List[str]               # ["vb6-ui-agent", "vb6-logic-agent", ...]

    # Language-specific metadata (preserves original IR)
    language_specific: Dict[str, Any]       # {"vb6": {...}, "cobol": {...}}
```

**Field Types**:
- **Required**: `source_language`, `source_file`, `source_lines_of_code`, `target_framework`, `confidence`, `complexity`
- **Optional**: All others

**Example**:
```json
{
  "source_language": "VB6",
  "source_file": "StartForm.frm",
  "source_lines_of_code": 245,
  "target_framework": "Angular",
  "analysis_timestamp": "2025-11-22T10:30:00",
  "analyzer_version": "1.0.0-langgraph-vb6",
  "parser_type": "langgraph-vb6-subagent",
  "confidence": 0.892,
  "ui_agent_confidence": 0.920,
  "data_agent_confidence": 0.875,
  "logic_agent_confidence": 0.880,
  "complexity": "medium",
  "subagents_used": ["vb6-ui-agent", "vb6-logic-agent", "vb6-data-agent"]
}
```

---

### 2. Data Structures Section

**Purpose**: Entities, classes, records, files, copybooks

**Schema**:
```python
class DataStructuresSection(BaseModel):
    entities: List[DataStructure] = []      # Business entities/classes
    files: List[FileDefinition] = []        # COBOL files (FD, SELECT)
    copybooks: List[str] = []               # COBOL copybooks
    data_source: Optional[DataSource]       # Database connection info

class DataStructure(BaseModel):
    name: str                               # Entity name
    type: str                               # "class", "record", "struct"
    fields: List[FieldDefinition]           # Field list
    source: Optional[str]                   # "WORKING-STORAGE", "form controls"
    description: Optional[str]              # Human-readable description
    line_number: Optional[int]              # Source line number

class FieldDefinition(BaseModel):
    name: str                               # Field name
    data_type: str                          # "String", "Integer", "Decimal"
    length: Optional[int]                   # String length
    decimals: Optional[int]                 # Decimal places
    nullable: bool = True                   # Can be null?
    default_value: Optional[str]            # Default value

    # UI-specific
    control_type: Optional[str]             # "TextBox", "ComboBox" (VB6)

    # COBOL-specific
    cobol_picture: Optional[str]            # "PIC X(10)", "PIC 9(5)V99"
    cobol_level: Optional[int]              # 01, 05, 10, etc.

    description: Optional[str]
    line_number: Optional[int]
```

**Example** (VB6 Entity):
```json
{
  "entities": [
    {
      "name": "Customer",
      "type": "class",
      "source": "form controls",
      "fields": [
        {
          "name": "customerId",
          "data_type": "Integer",
          "nullable": false,
          "control_type": "TextBox"
        },
        {
          "name": "customerName",
          "data_type": "String",
          "length": 50,
          "nullable": false,
          "control_type": "TextBox"
        }
      ]
    }
  ]
}
```

**Example** (COBOL Entity):
```json
{
  "entities": [
    {
      "name": "DiagDetails",
      "type": "record",
      "source": "WORKING-STORAGE",
      "fields": [
        {
          "name": "diag-code",
          "data_type": "String",
          "length": 10,
          "cobol_picture": "PIC X(10)",
          "cobol_level": 5
        },
        {
          "name": "diag-count",
          "data_type": "Integer",
          "length": 5,
          "cobol_picture": "PIC 9(5)",
          "cobol_level": 5
        }
      ]
    }
  ]
}
```

---

### 3. UI Section

**Purpose**: Forms, screens, controls (VB6, PowerBuilder)

**Schema**:
```python
class UISection(BaseModel):
    has_ui: bool = False                    # Does source have UI?
    forms: List[UIForm] = []                # Form/screen list

class UIForm(BaseModel):
    name: str                               # Form name
    title: Optional[str]                    # Form caption/title
    type: str                               # "form", "dialog", "mdi"
    width: Optional[int]                    # Form width (twips, pixels)
    height: Optional[int]                   # Form height
    controls: List[UIControl] = []          # Control list
    description: Optional[str]

class UIControl(BaseModel):
    name: str                               # Control ID
    type: str                               # "CommandButton", "TextBox", etc.
    caption: Optional[str]                  # Button text, label text
    text: Optional[str]                     # TextBox value
    enabled: bool = True                    # Enabled state
    visible: bool = True                    # Visible state
    tab_index: Optional[int]                # Tab order

    # Position
    left: Optional[int]
    top: Optional[int]
    width: Optional[int]
    height: Optional[int]

    # Data binding (VB6)
    data_field: Optional[str]               # Bound field name
    data_source: Optional[str]              # Data source name

    description: Optional[str]
```

**Example** (VB6 Form):
```json
{
  "has_ui": true,
  "forms": [
    {
      "name": "StartForm",
      "title": "Customer Entry",
      "type": "form",
      "width": 7095,
      "height": 4680,
      "controls": [
        {
          "name": "txtCustomerName",
          "type": "TextBox",
          "text": "",
          "enabled": true,
          "visible": true,
          "tab_index": 0,
          "left": 1200,
          "top": 600,
          "width": 2400,
          "height": 300
        },
        {
          "name": "cmdSave",
          "type": "CommandButton",
          "caption": "Save",
          "enabled": true,
          "tab_index": 1,
          "left": 1200,
          "top": 3000
        }
      ]
    }
  ]
}
```

**COBOL**: `has_ui: false` (no UI controls)

---

### 4. Business Logic Section

**Purpose**: Procedures, workflows, calculations, error handling

**Schema**:
```python
class BusinessLogicSection(BaseModel):
    procedures: List[Procedure] = []        # Functions, subroutines, paragraphs
    workflows: List[Workflow] = []          # Multi-step processes
    calculations: List[Calculation] = []    # COMPUTE statements
    error_handling: List[ErrorHandler] = [] # Error handlers

class Procedure(BaseModel):
    name: str                               # Procedure name
    type: str                               # "event_handler", "business_logic", "paragraph"
    logic_steps: List[LogicStep] = []       # Step-by-step logic
    description: Optional[str]
    start_line: Optional[int]
    end_line: Optional[int]
    confidence: float = 0.9

class LogicStep(BaseModel):
    step_type: str                          # "assignment", "validation", "call", "compute"
    description: str                        # What this step does
    operation: Optional[str]                # "MOVE", "COMPUTE", "IF"
    target: Optional[str]                   # Target field/variable
    source: Optional[str]                   # Source field/value
    condition: Optional[str]                # IF condition
    code_snippet: Optional[str]             # Original code
    line_number: Optional[int]
    target_equivalent: Optional[str]        # Angular/Spring Boot equivalent
```

**Example** (VB6 Event Handler):
```json
{
  "procedures": [
    {
      "name": "cmdSave_Click",
      "type": "event_handler",
      "logic_steps": [
        {
          "step_type": "validation",
          "description": "Validate customer name is not empty",
          "code_snippet": "If Trim(txtCustomerName.Text) = \"\" Then",
          "line_number": 45,
          "target_equivalent": "this.customerForm.get('name').invalid"
        },
        {
          "step_type": "call",
          "description": "Call save customer method",
          "code_snippet": "SaveCustomer",
          "line_number": 50,
          "target_equivalent": "this.customerService.save(customer)"
        }
      ],
      "start_line": 44,
      "end_line": 52,
      "confidence": 0.92
    }
  ]
}
```

**Example** (COBOL Paragraph):
```json
{
  "procedures": [
    {
      "name": "READ-DIAG-FILE",
      "type": "paragraph",
      "logic_steps": [
        {
          "step_type": "io",
          "description": "Read diagnosis file",
          "operation": "READ",
          "target": "DIAG-FILE",
          "code_snippet": "READ DIAG-FILE INTO DIAG-DETAILS",
          "line_number": 120,
          "target_equivalent": "diagRepository.findAll()"
        }
      ],
      "start_line": 119,
      "end_line": 125,
      "confidence": 0.88
    }
  ]
}
```

---

### 5. I/O Operations Section

**Purpose**: File operations, database operations (COBOL-heavy)

**Schema**:
```python
class IOOperationsSection(BaseModel):
    operations: List[IOOperation] = []              # READ, WRITE, OPEN, CLOSE
    io_patterns: List[IOPattern] = []               # Sequential, indexed, etc.
    repository_recommendations: List[RepositoryRecommendation] = []
    file_status_handling: List[Dict[str, Any]] = [] # File status checks

class IOOperation(BaseModel):
    type: str                               # "READ", "WRITE", "OPEN", "CLOSE"
    entity: str                             # File name or table name
    mode: Optional[str]                     # "INPUT", "OUTPUT", "I-O"
    procedure: Optional[str]                # Which procedure contains this
    into_variable: Optional[str]            # READ ... INTO variable
    from_variable: Optional[str]            # WRITE ... FROM variable
    access_pattern: Optional[str]           # "sequential", "indexed", "relative"
    key_field: Optional[str]                # Key field for indexed access
    error_handling: Optional[str]           # AT END, INVALID KEY
    description: Optional[str]
    line_number: Optional[int]
    target_equivalent: Optional[str]        # Spring Boot equivalent
    confidence: float = 0.9
```

**Example** (COBOL):
```json
{
  "operations": [
    {
      "type": "OPEN",
      "entity": "DIAG-FILE",
      "mode": "INPUT",
      "procedure": "MAIN-PROCESS",
      "description": "Open diagnosis file for reading",
      "line_number": 100,
      "target_equivalent": "@Autowired DiagRepository diagRepository",
      "confidence": 0.95
    },
    {
      "type": "READ",
      "entity": "DIAG-FILE",
      "into_variable": "DIAG-DETAILS",
      "access_pattern": "sequential",
      "error_handling": "AT END SET EOF-FLAG TO TRUE",
      "description": "Read next diagnosis record",
      "line_number": 120,
      "target_equivalent": "diagRepository.findAll().forEach(...)",
      "confidence": 0.90
    }
  ]
}
```

**VB6**: Usually empty (database operations go in `data_operations`)

---

### 6. Data Operations Section

**Purpose**: Database operations (VB6 ADO, recordsets)

**Schema**:
```python
class DataOperationsSection(BaseModel):
    operations: List[DataOperation] = []

class DataOperation(BaseModel):
    operation_type: str                     # "query", "insert", "update", "delete"
    target_table: Optional[str]             # Table name
    sql_query: Optional[str]                # SQL statement
    recordset_name: Optional[str]           # VB6 recordset variable
    description: Optional[str]
    line_number: Optional[int]
```

**Example** (VB6):
```json
{
  "operations": [
    {
      "operation_type": "query",
      "target_table": "Customers",
      "sql_query": "SELECT * FROM Customers WHERE CustomerId = ?",
      "recordset_name": "rsCustomers",
      "description": "Load customer by ID",
      "line_number": 78
    }
  ]
}
```

**COBOL**: Usually empty (file operations go in `io_operations`)

---

### 7. Events Section

**Purpose**: Event handlers (VB6, PowerBuilder)

**Schema**:
```python
class EventsSection(BaseModel):
    handlers: List[EventHandler] = []

class EventHandler(BaseModel):
    event_name: str                         # "cmdSave_Click", "Form_Load"
    control_name: Optional[str]             # "cmdSave"
    handler_code: str                       # Original VB6 code
    description: Optional[str]
    start_line: Optional[int]
    end_line: Optional[int]
```

**Example** (VB6):
```json
{
  "handlers": [
    {
      "event_name": "cmdSave_Click",
      "control_name": "cmdSave",
      "handler_code": "Private Sub cmdSave_Click()\\n  SaveCustomer\\nEnd Sub",
      "description": "Save button click handler",
      "start_line": 44,
      "end_line": 46
    }
  ]
}
```

**COBOL**: Usually empty (no event-driven programming)

---

### 8. Patterns Section

**Purpose**: Detected design patterns

**Schema**:
```python
class DetectedPattern(BaseModel):
    pattern_type: str                       # "CRUD", "VALIDATION", "SEQUENTIAL_PROCESSING"
    pattern_name: str                       # Friendly name
    entities_involved: List[str] = []       # Entity names
    description: Optional[str]
    target_equivalent: Optional[str]        # Angular/Spring Boot equivalent
    confidence: float = 0.9
```

**Example**:
```json
{
  "pattern_type": "CRUD_FORM",
  "pattern_name": "Customer CRUD Operations",
  "entities_involved": ["Customer"],
  "description": "Form with Create, Read, Update, Delete operations",
  "target_equivalent": "Angular Reactive Form + CRUD Service",
  "confidence": 0.92
}
```

---

### 9. External References Section

**Purpose**: External dependencies (classes, modules, DLLs, copybooks)

**Schema**:
```python
class ExternalReferencesSection(BaseModel):
    classes: List[str] = []                 # External class names (VB6)
    modules: List[str] = []                 # Module names
    controls: List[str] = []                # ActiveX controls (VB6)
    dlls: List[str] = []                    # DLL references (VB6)
    copybooks: List[str] = []               # COBOL copybooks
```

**Example** (VB6):
```json
{
  "classes": ["ADODB.Connection", "ADODB.Recordset"],
  "modules": ["modDatabase", "modUtilities"],
  "controls": ["MSComDlg.CommonDialog"],
  "dlls": ["KERNEL32.DLL"]
}
```

**Example** (COBOL):
```json
{
  "copybooks": ["CUSTCOPY", "DIAGCOPY"]
}
```

---

### 10. Security Issues Section

**Purpose**: Detected security vulnerabilities

**Schema**:
```python
security_issues: List[str] = []
```

**Example**:
```json
["SQL injection risk in query: SELECT * FROM Users WHERE id = " + userId]
```

---

### 11. Repository Mapping Section

**Purpose**: Backend data access layer mapping (COBOL → Spring Boot)

**Schema**:
```python
class RepositoryMappingSection(BaseModel):
    mappings: List[RepositoryMapping] = []

class RepositoryMapping(BaseModel):
    entity_name: str                        # "DiagDetails"
    repository_name: str                    # "DiagDetailsRepository"
    repository_type: str                    # "JpaRepository", "CrudRepository"
    operations: List[str] = []              # ["findAll", "save", "findById"]
```

**Example** (COBOL):
```json
{
  "mappings": [
    {
      "entity_name": "DiagDetails",
      "repository_name": "DiagDetailsRepository",
      "repository_type": "JpaRepository",
      "operations": ["findAll", "save", "findById"]
    }
  ]
}
```

**VB6**: Usually empty

---

### 12. Frontend Mapping Section

**Purpose**: UI component mapping (VB6 → Angular)

**Schema**:
```python
class FrontendMappingSection(BaseModel):
    mappings: List[ComponentMapping] = []

class ComponentMapping(BaseModel):
    source_control: str                     # "txtCustomerName"
    source_type: str                        # "TextBox"
    target_component: str                   # "mat-input"
    target_framework: str                   # "Angular"
    props: Dict[str, Any] = {}              # Component properties
    events: Dict[str, Any] = {}             # Event handlers
```

**Example** (VB6):
```json
{
  "mappings": [
    {
      "source_control": "txtCustomerName",
      "source_type": "TextBox",
      "target_component": "mat-input",
      "target_framework": "Angular",
      "props": {
        "label": "Customer Name",
        "value": "",
        "disabled": false
      },
      "events": {}
    },
    {
      "source_control": "cmdSave",
      "source_type": "CommandButton",
      "target_component": "mat-button",
      "target_framework": "Angular",
      "props": {
        "label": "Save",
        "disabled": false
      },
      "events": {
        "click": "onSave()"
      }
    }
  ]
}
```

**COBOL**: Usually empty

---

### 13. Generation Metadata Section

**Purpose**: Code generation hints, complexity scores

**Schema**:
```python
class GenerationMetadata(BaseModel):
    estimated_automation_rate: float = 0.9  # 0.0 - 1.0
    estimated_manual_effort_hours: float = 0.0
    complexity_score: int = 5               # 1-10 scale
    recommended_template: str               # "angular-crud-form", "spring-boot-service"
    generation_notes: List[str] = []        # Human-readable notes
    target_specific_hints: Dict[str, Any] = {}  # Framework-specific hints
```

**Example**:
```json
{
  "estimated_automation_rate": 0.92,
  "estimated_manual_effort_hours": 2.0,
  "complexity_score": 6,
  "recommended_template": "angular-crud-form",
  "generation_notes": [
    "Use Angular Material Dialog for modal behavior",
    "Use Reactive Forms with validation"
  ],
  "target_specific_hints": {
    "angular": {
      "use_material": true,
      "use_reactive_forms": true
    }
  }
}
```

---

## Extension Rules for New Languages

### Adding PowerBuilder Support

1. **Create PowerBuilder Adapter**: `src/adapters/powerbuilder_to_universal_ir.py`
2. **Map PowerBuilder Constructs**:
   - Windows → `ui.forms`
   - DataWindows → `ui.forms` + `data_structures.entities`
   - Scripts → `business_logic.procedures`
   - PowerScript events → `events.handlers`
3. **Store PowerBuilder-specific data**: `metadata.language_specific.powerbuilder`
4. **Update validator**: Add PowerBuilder-specific validation in `_validate_powerbuilder_specific()`

### Language-Specific Fields

Use `language_specific` dict in relevant sections:

```json
{
  "metadata": {
    "language_specific": {
      "powerbuilder": {
        "datawindow_type": "grid",
        "powerscript_version": "12.5"
      }
    }
  }
}
```

---

## Strict vs. Optional Fields

### Required (Strict) Fields

**Metadata**:
- `source_language`, `source_file`, `source_lines_of_code`
- `target_framework`, `confidence`, `complexity`

**Data Structures**:
- `DataStructure.name`, `DataStructure.type`
- `FieldDefinition.name`, `FieldDefinition.data_type`

**Business Logic**:
- `Procedure.name`, `Procedure.type`

**All Sections**: Must be present (can be empty lists)

### Optional Fields

- All `description` fields
- All `line_number` fields
- All `target_equivalent` fields
- Most language-specific fields (`cobol_picture`, `control_type`, etc.)

---

## Confidence Metrics Calculation

### Agent-Level Confidence

Each agent reports confidence (0.0 - 1.0):

**VB6**:
- `ui_agent_confidence`: How confident the UI agent is
- `logic_agent_confidence`: How confident the logic agent is
- `data_agent_confidence`: How confident the data agent is

**COBOL**:
- `data_agent_confidence`: How confident the data agent is
- `logic_agent_confidence`: How confident the logic agent is
- `io_agent_confidence`: How confident the I/O agent is

### Overall Confidence

Weighted average based on complexity:

**VB6**:
```python
confidence = (
    ui_agent_confidence * 0.4 +
    logic_agent_confidence * 0.35 +
    data_agent_confidence * 0.25
)
```

**COBOL**:
```python
confidence = (
    data_agent_confidence * 0.3 +
    logic_agent_confidence * 0.4 +
    io_agent_confidence * 0.3
)
```

### Complexity Assessment

**Simple** (1-3):
- Few controls/entities (< 10)
- Few procedures (< 5)
- Minimal logic

**Medium** (4-7):
- Moderate controls/entities (10-30)
- Moderate procedures (5-15)
- Standard patterns

**Complex** (8-10):
- Many controls/entities (> 30)
- Many procedures (> 15)
- Custom patterns, legacy code

---

## Example: Complete Universal IR (VB6)

```json
{
  "metadata": {
    "source_language": "VB6",
    "source_file": "StartForm.frm",
    "source_lines_of_code": 245,
    "target_framework": "Angular",
    "analysis_timestamp": "2025-11-22T10:30:00",
    "analyzer_version": "1.0.0-langgraph-vb6",
    "parser_type": "langgraph-vb6-subagent",
    "confidence": 0.892,
    "ui_agent_confidence": 0.920,
    "data_agent_confidence": 0.875,
    "logic_agent_confidence": 0.880,
    "complexity": "medium",
    "subagents_used": ["vb6-ui-agent", "vb6-logic-agent", "vb6-data-agent"]
  },
  "data_structures": {
    "entities": [
      {
        "name": "Customer",
        "type": "class",
        "fields": [
          {
            "name": "customerId",
            "data_type": "Integer",
            "nullable": false
          },
          {
            "name": "customerName",
            "data_type": "String",
            "length": 50,
            "nullable": false
          }
        ]
      }
    ]
  },
  "ui": {
    "has_ui": true,
    "forms": [
      {
        "name": "StartForm",
        "title": "Customer Entry",
        "type": "form",
        "controls": [
          {
            "name": "txtCustomerName",
            "type": "TextBox"
          },
          {
            "name": "cmdSave",
            "type": "CommandButton",
            "caption": "Save"
          }
        ]
      }
    ]
  },
  "business_logic": {
    "procedures": [
      {
        "name": "cmdSave_Click",
        "type": "event_handler",
        "logic_steps": [
          {
            "step_type": "validation",
            "description": "Validate customer name"
          }
        ]
      }
    ]
  },
  "io_operations": {},
  "data_operations": {},
  "events": {
    "handlers": [
      {
        "event_name": "cmdSave_Click",
        "control_name": "cmdSave"
      }
    ]
  },
  "patterns": [
    {
      "pattern_type": "CRUD_FORM",
      "pattern_name": "Customer CRUD",
      "confidence": 0.92
    }
  ],
  "external_references": {
    "classes": ["ADODB.Connection"]
  },
  "security_issues": [],
  "repository_mapping": {},
  "frontend_mapping": {
    "mappings": [
      {
        "source_control": "txtCustomerName",
        "source_type": "TextBox",
        "target_component": "mat-input",
        "target_framework": "Angular"
      }
    ]
  },
  "generation_metadata": {
    "estimated_automation_rate": 0.92,
    "recommended_template": "angular-crud-form"
  },
  "schema_version": "universal-ir-v1"
}
```

---

## Validation Rules

See `src/core/universal_ir_validator.py` for complete validation logic.

### Schema Validation (Automatic)

Pydantic automatically validates:
- Required fields present
- Correct data types
- List/dict structures
- Enum values

### Business Rule Validation (Custom)

**Metadata**:
- `source_language` in ["VB6", "COBOL", "PowerBuilder", "Delphi", "AS400"]
- `target_framework` in ["Angular", "SpringBoot", "React", "Vue", ".NET"]
- `confidence` between 0.0 and 1.0
- Low confidence warning if < 0.5

**Data Structures**:
- At least one entity for non-trivial applications
- Field names not empty
- Data types valid

**Business Logic**:
- Procedures have names
- Logic steps have descriptions

**Language-Specific**:
- VB6: Must have `ui.has_ui = true`
- COBOL: Must have `io_operations` or `data_structures.files`

---

## JSON Schema Generation

Generate JSON schema for external tools:

```python
from src.core.universal_ir_schema import UniversalIR
import json

schema = UniversalIR.model_json_schema()
with open('docs/universal_ir_schema.json', 'w') as f:
    json.dump(schema, f, indent=2)
```

Output: `docs/universal_ir_schema.json` (48KB)

---

## Summary

The Universal IR schema provides:

✅ **12 comprehensive sections** covering all aspects of legacy applications
✅ **Language-agnostic design** supporting VB6, COBOL, and future languages
✅ **Pydantic type safety** with automatic validation
✅ **Extension mechanism** via `language_specific` fields
✅ **Confidence metrics** for quality assessment
✅ **Target equivalents** for code generation hints

**Next Steps**:
- See `VB6_ADAPTER.md` for VB6 mapping rules
- See `COBOL_ADAPTER.md` for COBOL mapping rules
- See `WORKFLOW_UPDATES.md` for integration details

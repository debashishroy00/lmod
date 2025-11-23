# COBOL → Universal IR Adapter

## Overview

The **COBOL Adapter** (`src/adapters/cobol_to_universal_ir.py`) transforms COBOL-specific Intermediate Representation into Universal IR format. This adapter enables COBOL batch processing applications to use the unified code generation pipeline for Spring Boot services.

**File**: `src/adapters/cobol_to_universal_ir.py` (420 lines)
**Pattern**: Thin transformation layer (no business logic)
**Principle**: Zero data loss during transformation

## Architecture

```
┌──────────────────────────────────────────────────────────┐
│          COBOL → Universal IR Transformation            │
├──────────────────────────────────────────────────────────┤
│                                                          │
│  COBOL IR (8 sections)                                  │
│  ┌────────────────────┐                                 │
│  │ - metadata         │                                 │
│  │ - data_structures  │ (entities, files, copybooks)    │
│  │ - business_logic   │ (procedures, workflows)         │
│  │ - io_operations    │ (file operations, patterns)     │
│  │ - patterns         │                                 │
│  │ - external_refs    │ (copybooks, calls)              │
│  │ - security_issues  │                                 │
│  │ - generation_meta  │                                 │
│  └────────────────────┘                                 │
│           │                                              │
│           v                                              │
│  ┌───────────────────────────────┐                      │
│  │  COBOLToUniversalIRAdapter    │                      │
│  │  - convert(cobol_ir) → UniversalIR                   │
│  │  - _convert_metadata()        │                      │
│  │  - _convert_data_structures() │                      │
│  │  - _convert_business_logic()  │                      │
│  │  - _convert_io_operations()   │                      │
│  │  - _convert_patterns()        │                      │
│  │  - ... (12 conversion methods)│                      │
│  └───────────────────────────────┘                      │
│           │                                              │
│           v                                              │
│  Universal IR (12 sections)                             │
│  ┌────────────────────┐                                 │
│  │ - metadata         │ (source_language="COBOL")       │
│  │ - ui               │ (empty for COBOL)               │
│  │ - data_structures  │ (entities, files, copybooks)    │
│  │ - business_logic   │ (procedures, workflows, calcs)  │
│  │ - io_operations    │ (file operations, patterns)     │
│  │ - data_operations  │ (empty for COBOL)               │
│  │ - events           │ (empty for COBOL)               │
│  │ - patterns         │ (detected patterns)             │
│  │ - external_refs    │ (copybooks, calls)              │
│  │ - security_issues  │ (detected issues)               │
│  │ - repository_map   │ (Spring Boot repositories)      │
│  │ - frontend_mapping │ (empty for COBOL)               │
│  │ - generation_meta  │ (hints for Spring Boot gen)     │
│  └────────────────────┘                                 │
└──────────────────────────────────────────────────────────┘
```

## Class Structure

```python
class COBOLToUniversalIRAdapter:
    def __init__(self):
        """Initialize adapter"""
        pass

    def convert(self, cobol_ir: Dict[str, Any]) -> UniversalIR:
        """Main conversion entry point"""
        # Convert all 12 sections
        # Return UniversalIR Pydantic model

    # 11 private conversion methods
    def _convert_metadata(self, cobol_metadata: Dict) -> SourceMetadata
    def _convert_data_structures(self, cobol_data: Dict) -> DataStructuresSection
    def _convert_business_logic(self, cobol_logic: Dict) -> BusinessLogicSection
    def _convert_io_operations(self, cobol_io: Dict) -> IOOperationsSection
    def _convert_patterns(self, cobol_patterns: List) -> List[DetectedPattern]
    def _convert_external_references(self, cobol_refs: Dict) -> ExternalReferencesSection
    def _convert_security_issues(self, cobol_security: List) -> List
    def _convert_repository_mapping(self, cobol_io: Dict) -> RepositoryMappingSection
    def _convert_generation_metadata(self, cobol_gen: Dict) -> GenerationMetadata
```

## Mapping Rules

### 1. Metadata Mapping

**COBOL IR → Universal IR**

| COBOL Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `source_file` | `metadata.source_file` | Direct copy |
| `source_lines_of_code` | `metadata.source_lines_of_code` | Direct copy |
| `source_path` | `metadata.source_path` | Direct copy |
| `target_framework` | `metadata.target_framework` | Default: "SpringBoot" |
| `analysis_timestamp` | `metadata.analysis_timestamp` | Direct copy |
| `analyzer_version` | `metadata.analyzer_version` | Direct copy |
| `parser_type` | `metadata.parser_type` | Direct copy |
| `confidence` | `metadata.confidence` | Direct copy |
| `data_agent_confidence` | `metadata.data_agent_confidence` | COBOL-specific |
| `logic_agent_confidence` | `metadata.logic_agent_confidence` | COBOL-specific |
| `io_agent_confidence` | `metadata.io_agent_confidence` | COBOL-specific |
| `complexity` | `metadata.complexity` | Direct copy |
| `complexity_score` | `metadata.complexity_score` | Direct copy |
| `subagents_used` | `metadata.subagents_used` | Direct copy |
| *all COBOL metadata* | `metadata.language_specific.cobol` | Preserve original |

**Hard-coded values**:
- `source_language` = `"COBOL"`
- `target_framework` = `"SpringBoot"` (default)

**Example**:
```json
{
  "metadata": {
    "source_language": "COBOL",
    "source_file": "seq.cbl",
    "source_lines_of_code": 56,
    "target_framework": "SpringBoot",
    "confidence": 0.872,
    "data_agent_confidence": 0.875,
    "logic_agent_confidence": 0.838,
    "io_agent_confidence": 0.913,
    "complexity": "medium",
    "subagents_used": ["cobol-data-agent", "cobol-logic-agent", "cobol-io-agent"],
    "language_specific": {
      "cobol": { /* original COBOL metadata */ }
    }
  }
}
```

---

### 2. Data Structures Mapping

**COBOL WORKING-STORAGE → Universal IR Entities**

**Entity-level mapping**:
| COBOL Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `data_structures.entities[].name` | `data_structures.entities[].name` | Direct copy |
| `data_structures.entities[].type` | `data_structures.entities[].type` | Default: "record" |
| `data_structures.entities[].fields[]` | `data_structures.entities[].fields[]` | Field list |
| `data_structures.entities[].source` | `data_structures.entities[].source` | "WORKING-STORAGE" |
| `data_structures.entities[]._what` | `data_structures.entities[].description` | Agent description |

**Field-level mapping**:
| COBOL Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `fields[].name` | `fields[].name` | Field name |
| `fields[].data_type` | `fields[].data_type` | "String", "Integer", "Decimal" |
| `fields[].length` | `fields[].length` | Field length |
| `fields[].decimals` | `fields[].decimals` | Decimal places (for numeric) |
| `fields[].default_value` | `fields[].default_value` | Default value |
| `fields[].cobol_picture` | `fields[].cobol_picture` | **COBOL-specific**: "PIC X(10)", "PIC 9(5)V99" |
| `fields[].level` | `fields[].cobol_level` | **COBOL-specific**: 01, 05, 10, etc. |
| `fields[]._what` | `fields[].description` | Agent description |
| `fields[]._line` | `fields[].line_number` | Source line number |

**File Definitions** (COBOL-specific):
| COBOL Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `data_structures.cobol_files[].select_name` | `data_structures.files[].select_name` | SELECT name |
| `data_structures.cobol_files[].assign_to` | `data_structures.files[].assign_to` | ASSIGN TO path |
| `data_structures.cobol_files[].organization` | `data_structures.files[].organization` | "SEQUENTIAL", "INDEXED" |
| `data_structures.cobol_files[].access_mode` | `data_structures.files[].access_mode` | "SEQUENTIAL", "DYNAMIC" |
| `data_structures.cobol_files[].record_layout` | `data_structures.files[].record_layout` | FD record name |
| `data_structures.cobol_files[].file_status` | `data_structures.files[].file_status` | Status variable |
| `data_structures.cobol_files[]._what` | `data_structures.files[].description` | Agent description |
| `data_structures.cobol_files[]._select_line` | `data_structures.files[].line_number` | SELECT line number |

**Copybooks**:
| COBOL Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `data_structures.copybooks[]` | `data_structures.copybooks[]` | Direct copy |

**Example**:

**COBOL IR**:
```json
{
  "data_structures": {
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
            "level": 5,
            "_what": "Diagnosis code",
            "_line": 25
          },
          {
            "name": "diag-count",
            "data_type": "Integer",
            "length": 5,
            "cobol_picture": "PIC 9(5)",
            "level": 5,
            "_what": "Diagnosis count",
            "_line": 26
          }
        ],
        "_what": "Diagnosis details record"
      }
    ],
    "cobol_files": [
      {
        "select_name": "DIAG-FILE",
        "assign_to": "diag.dat",
        "organization": "SEQUENTIAL",
        "access_mode": "SEQUENTIAL",
        "record_layout": "DIAG-DETAILS",
        "file_status": "WS-FILE-STATUS",
        "_what": "Diagnosis data file",
        "_select_line": 10
      }
    ],
    "copybooks": ["DIAGCOPY"]
  }
}
```

**Universal IR**:
```json
{
  "data_structures": {
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
            "nullable": true,
            "cobol_picture": "PIC X(10)",
            "cobol_level": 5,
            "description": "Diagnosis code",
            "line_number": 25
          },
          {
            "name": "diag-count",
            "data_type": "Integer",
            "length": 5,
            "nullable": true,
            "cobol_picture": "PIC 9(5)",
            "cobol_level": 5,
            "description": "Diagnosis count",
            "line_number": 26
          }
        ],
        "description": "Diagnosis details record"
      }
    ],
    "files": [
      {
        "select_name": "DIAG-FILE",
        "assign_to": "diag.dat",
        "organization": "SEQUENTIAL",
        "access_mode": "SEQUENTIAL",
        "record_layout": "DIAG-DETAILS",
        "file_status": "WS-FILE-STATUS",
        "description": "Diagnosis data file",
        "line_number": 10
      }
    ],
    "copybooks": ["DIAGCOPY"]
  }
}
```

---

### 3. Business Logic Mapping

**COBOL PROCEDURE DIVISION → Procedures**

**Procedure-level mapping**:
| COBOL Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `business_logic.procedures[].name` | `business_logic.procedures[].name` | Paragraph name |
| `business_logic.procedures[].type` | `business_logic.procedures[].type` | Default: "business_logic" |
| `business_logic.procedures[].logic_steps[]` | `business_logic.procedures[].logic_steps[]` | Logic steps |
| `business_logic.procedures[]._what` | `business_logic.procedures[].description` | Agent description |
| `business_logic.procedures[].start_line` | `business_logic.procedures[].start_line` | Source line |
| `business_logic.procedures[].end_line` | `business_logic.procedures[].end_line` | Source line |
| `business_logic.procedures[].confidence` | `business_logic.procedures[].confidence` | Confidence score |

**Logic Step Mapping**:
| COBOL Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `logic_steps[].step_type` | `logic_steps[].step_type` | "assignment", "io", "compute", etc. |
| `logic_steps[].description` | `logic_steps[].description` | What this step does |
| `logic_steps[].operation` | `logic_steps[].operation` | "MOVE", "COMPUTE", "READ", etc. |
| `logic_steps[].to_field` | `logic_steps[].target` | Target field |
| `logic_steps[].from_field` | `logic_steps[].source` | Source field |
| `logic_steps[].condition` | `logic_steps[].condition` | IF condition |
| `logic_steps[].code_snippet` | `logic_steps[].code_snippet` | Original COBOL code |
| `logic_steps[]._line` | `logic_steps[].line_number` | Source line number |
| `logic_steps[]._spring_boot_equivalent` | `logic_steps[].target_equivalent` | Spring Boot equivalent |

**Workflows**:
| COBOL Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `business_logic.workflows[].name` | `business_logic.workflows[].name` | Workflow name |
| `business_logic.workflows[].entry_point` | `business_logic.workflows[].entry_point` | Entry paragraph |
| `business_logic.workflows[].procedures_called[]` | `business_logic.workflows[].procedures_called[]` | Called paragraphs |
| `business_logic.workflows[]._what` | `business_logic.workflows[].description` | Agent description |
| `business_logic.workflows[].confidence` | `business_logic.workflows[].confidence` | Confidence score |

**Calculations**:
| COBOL Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `business_logic.calculations[].name` | `business_logic.calculations[].name` | Calculation name |
| `business_logic.calculations[].formula` | `business_logic.calculations[].formula` | COMPUTE formula |
| `business_logic.calculations[].inputs[]` | `business_logic.calculations[].inputs[]` | Input fields |
| `business_logic.calculations[].output` | `business_logic.calculations[].output` | Output field |
| `business_logic.calculations[].description` | `business_logic.calculations[].description` | Description |

**Error Handlers**:
| COBOL Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `business_logic.error_handling[].type` | `business_logic.error_handling[].type` | "FILE-STATUS", "AT END", etc. |
| `business_logic.error_handling[].scope` | `business_logic.error_handling[].scope` | "global", "paragraph" |
| `business_logic.error_handling[].handler` | `business_logic.error_handling[].handler` | Handler paragraph |
| `business_logic.error_handling[]._what` | `business_logic.error_handling[].description` | Agent description |
| `business_logic.error_handling[]._line` | `business_logic.error_handling[].line_number` | Source line |
| `business_logic.error_handling[]._spring_boot_equivalent` | `business_logic.error_handling[].target_equivalent` | Spring Boot equivalent |

**Example**:

**COBOL IR**:
```json
{
  "business_logic": {
    "procedures": [
      {
        "name": "READ-DIAG-FILE",
        "type": "paragraph",
        "logic_steps": [
          {
            "step_type": "io",
            "description": "Read diagnosis file",
            "operation": "READ",
            "to_field": "DIAG-DETAILS",
            "from_field": "DIAG-FILE",
            "code_snippet": "READ DIAG-FILE INTO DIAG-DETAILS",
            "_line": 120,
            "_spring_boot_equivalent": "diagRepository.findAll()"
          }
        ],
        "_what": "Read diagnosis records",
        "start_line": 119,
        "end_line": 125,
        "confidence": 0.88
      }
    ]
  }
}
```

**Universal IR**:
```json
{
  "business_logic": {
    "procedures": [
      {
        "name": "READ-DIAG-FILE",
        "type": "business_logic",
        "logic_steps": [
          {
            "step_type": "io",
            "description": "Read diagnosis file",
            "operation": "READ",
            "target": "DIAG-DETAILS",
            "source": "DIAG-FILE",
            "code_snippet": "READ DIAG-FILE INTO DIAG-DETAILS",
            "line_number": 120,
            "target_equivalent": "diagRepository.findAll()"
          }
        ],
        "description": "Read diagnosis records",
        "start_line": 119,
        "end_line": 125,
        "confidence": 0.88
      }
    ]
  }
}
```

---

### 4. I/O Operations Mapping

**COBOL File Operations → I/O Operations**

**Operation-level mapping**:
| COBOL Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `io_operations.operations[].type` | `io_operations.operations[].type` | "OPEN", "READ", "WRITE", "CLOSE" |
| `io_operations.operations[].entity` | `io_operations.operations[].entity` | File name |
| `io_operations.operations[].mode` | `io_operations.operations[].mode` | "INPUT", "OUTPUT", "I-O" |
| `io_operations.operations[].procedure` | `io_operations.operations[].procedure` | Which paragraph |
| `io_operations.operations[].into_variable` | `io_operations.operations[].into_variable` | READ INTO variable |
| `io_operations.operations[].from_variable` | `io_operations.operations[].from_variable` | WRITE FROM variable |
| `io_operations.operations[].access_pattern` | `io_operations.operations[].access_pattern` | "sequential", "indexed" |
| `io_operations.operations[].key_field` | `io_operations.operations[].key_field` | Key field for indexed |
| `io_operations.operations[].error_handling` | `io_operations.operations[].error_handling` | AT END, INVALID KEY |
| `io_operations.operations[]._what` | `io_operations.operations[].description` | Agent description |
| `io_operations.operations[]._line` | `io_operations.operations[].line_number` | Source line |
| `io_operations.operations[]._spring_boot` | `io_operations.operations[].target_equivalent` | Spring Boot equivalent |
| `io_operations.operations[].confidence` | `io_operations.operations[].confidence` | Confidence score |

**I/O Patterns**:
| COBOL Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `io_operations.io_patterns[].pattern_type` | `io_operations.io_patterns[].pattern_type` | "SEQUENTIAL_READ", etc. |
| `io_operations.io_patterns[].pattern_name` | `io_operations.io_patterns[].pattern_name` | Friendly name |
| `io_operations.io_patterns[]._what` | `io_operations.io_patterns[].description` | Agent description |
| `io_operations.io_patterns[]._spring_boot` | `io_operations.io_patterns[].target_equivalent` | Spring Boot equivalent |
| `io_operations.io_patterns[].confidence` | `io_operations.io_patterns[].confidence` | Confidence score |

**Repository Recommendations**:
| COBOL Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `io_operations.repository_recommendations[].entity` | `io_operations.repository_recommendations[].entity` | Entity name |
| `io_operations.repository_recommendations[].repository_type` | `io_operations.repository_recommendations[].repository_type` | "JpaRepository", etc. |
| `io_operations.repository_recommendations[].operations_needed[]` | `io_operations.repository_recommendations[].operations_needed[]` | ["findAll", "save", etc.] |
| `io_operations.repository_recommendations[]._what` | `io_operations.repository_recommendations[].description` | Agent description |
| `io_operations.repository_recommendations[]._spring_boot_interface` | `io_operations.repository_recommendations[].target_interface` | Interface name |
| `io_operations.repository_recommendations[].confidence` | `io_operations.repository_recommendations[].confidence` | Confidence score |

**File Status Handling**:
| COBOL Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `io_operations.file_status_handling[]` | `io_operations.file_status_handling[]` | Direct copy |

**Example**:

**COBOL IR**:
```json
{
  "io_operations": {
    "operations": [
      {
        "type": "OPEN",
        "entity": "DIAG-FILE",
        "mode": "INPUT",
        "procedure": "MAIN-PROCESS",
        "_what": "Open diagnosis file for reading",
        "_line": 100,
        "_spring_boot": "@Autowired DiagRepository diagRepository",
        "confidence": 0.95
      },
      {
        "type": "READ",
        "entity": "DIAG-FILE",
        "into_variable": "DIAG-DETAILS",
        "access_pattern": "sequential",
        "error_handling": "AT END SET EOF-FLAG TO TRUE",
        "_what": "Read next diagnosis record",
        "_line": 120,
        "_spring_boot": "diagRepository.findAll().forEach(...)",
        "confidence": 0.90
      }
    ]
  }
}
```

**Universal IR**:
```json
{
  "io_operations": {
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
}
```

---

### 5. Repository Mapping (COBOL-Specific)

The COBOL adapter generates Spring Boot repository mappings from I/O recommendations.

**Mapping**:
| COBOL Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `io_operations.repository_recommendations[].entity` | `repository_mapping.mappings[].entity_name` | Entity name |
| `io_operations.repository_recommendations[].entity` + "Repository" | `repository_mapping.mappings[].repository_name` | Generated name |
| `io_operations.repository_recommendations[].repository_type` | `repository_mapping.mappings[].repository_type` | "JpaRepository" |
| `io_operations.repository_recommendations[].operations_needed[]` | `repository_mapping.mappings[].operations[]` | Repository methods |

**Example**:

**COBOL IR**:
```json
{
  "io_operations": {
    "repository_recommendations": [
      {
        "entity": "DiagDetails",
        "repository_type": "JpaRepository",
        "operations_needed": ["findAll", "save", "findById"],
        "_spring_boot_interface": "DiagDetailsRepository"
      }
    ]
  }
}
```

**Universal IR**:
```json
{
  "repository_mapping": {
    "mappings": [
      {
        "entity_name": "DiagDetails",
        "repository_name": "DiagDetailsRepository",
        "repository_type": "JpaRepository",
        "operations": ["findAll", "save", "findById"]
      }
    ]
  }
}
```

---

### 6. Patterns Mapping

| COBOL Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `patterns[].pattern_type` | `patterns[].pattern_type` | "SEQUENTIAL_PROCESSING", etc. |
| `patterns[].pattern_name` | `patterns[].pattern_name` | Friendly name |
| `patterns[]._what` | `patterns[].description` | Pattern description |
| `patterns[]._spring_boot` | `patterns[].target_equivalent` | Spring Boot equivalent |
| `patterns[].confidence` | `patterns[].confidence` | Confidence score |

---

### 7. External References Mapping

| COBOL Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `external_references.classes[]` | `external_references.classes[]` | Direct copy |
| `external_references.modules[]` | `external_references.modules[]` | Direct copy |
| `external_references.copybooks[]` | `external_references.copybooks[]` | Direct copy |
| `external_references.controls[]` | `external_references.controls[]` | Direct copy |

---

### 8. Generation Metadata Mapping

| COBOL Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `generation_metadata.estimated_automation_rate` | `generation_metadata.estimated_automation_rate` | 0.0-1.0 |
| `generation_metadata.estimated_manual_effort_hours` | `generation_metadata.estimated_manual_effort_hours` | Hours |
| `generation_metadata.complexity_score` | `generation_metadata.complexity_score` | 1-10 |
| `generation_metadata.recommended_template` | `generation_metadata.recommended_template` | "spring-boot-service" |
| `generation_metadata.generation_notes[]` | `generation_metadata.generation_notes[]` | Implementation notes |
| *all COBOL gen metadata* | `generation_metadata.target_specific_hints.springboot` | Spring Boot-specific hints |

---

## Empty Sections for COBOL

The following Universal IR sections are typically **empty** for COBOL:

1. **ui**: COBOL has no UI (batch processing)
2. **data_operations**: COBOL uses file I/O, not database operations
3. **events**: COBOL is not event-driven
4. **frontend_mapping**: No UI components

Example:
```json
{
  "ui": {"has_ui": false},
  "data_operations": {},
  "events": {},
  "frontend_mapping": {}
}
```

---

## Known Limitations

### 1. Complex COBOL Syntax
- **Limitation**: REDEFINES, OCCURS DEPENDING ON not fully captured in Universal IR
- **Workaround**: Store in `language_specific.cobol`
- **Impact**: Manual review required for complex data structures

### 2. COPY REPLACING
- **Limitation**: Copybook substitutions not expanded
- **Workaround**: List copybooks in `external_references.copybooks[]`
- **Impact**: Generator must handle copybook expansion

### 3. Nested Programs
- **Limitation**: Nested COBOL programs not in Universal IR
- **Workaround**: Parse each program separately
- **Impact**: Cross-program calls need manual mapping

### 4. SORT/MERGE Verbs
- **Limitation**: Complex SORT/MERGE operations simplified
- **Workaround**: Capture in `business_logic.procedures[]` with descriptions
- **Impact**: Manual implementation required for sorting logic

---

## Usage

### Programmatic Usage

```python
from src.adapters.cobol_to_universal_ir import COBOLToUniversalIRAdapter
import json

# Load COBOL IR
with open('output/cobol/seq_ir.json', 'r') as f:
    cobol_ir = json.load(f)

# Convert to Universal IR
adapter = COBOLToUniversalIRAdapter()
universal_ir_model = adapter.convert(cobol_ir)

# Save Universal IR
with open('output/cobol/seq_universal_ir.json', 'w') as f:
    json.dump(universal_ir_model.model_dump(), f, indent=2)
```

### Integrated in Workflow

The COBOL adapter is automatically invoked in the LangGraph workflow via `convert_to_universal_ir_node()` in `src/orchestrator/cobol_langgraph_nodes.py`.

---

## Testing

### Test Files
- **Input**: COBOL IR from `samples/cobol/simple/seq.cbl`
- **Output**: Universal IR in `output/cobol/seq_universal_ir.json`

### Validation
- **Schema**: Pydantic automatic validation
- **Business Rules**: `UniversalIRValidator.validate()`
- **COBOL-Specific**: `_validate_cobol_specific()` checks for I/O operations or files

---

## Summary

The COBOL Adapter successfully transforms COBOL IR to Universal IR with:

✅ **Complete data structure mapping** (WORKING-STORAGE → entities, FD → files)
✅ **File operation mapping** (OPEN/READ/WRITE/CLOSE → io_operations)
✅ **Procedure mapping** (PROCEDURE DIVISION → business_logic.procedures)
✅ **Repository generation** (File operations → Spring Boot repositories)
✅ **COBOL-specific preservation** (PIC clauses, level numbers, file definitions)
✅ **Zero data loss** (original COBOL IR preserved in `language_specific.cobol`)
✅ **Type safety** (Pydantic validation)

**Next**: See `WORKFLOW_UPDATES.md` for integration details.

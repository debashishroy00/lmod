# VB6 → Universal IR Adapter

## Overview

The **VB6 Adapter** (`src/adapters/vb6_to_universal_ir.py`) transforms VB6-specific Intermediate Representation into Universal IR format. This adapter enables VB6 applications to use the unified code generation pipeline.

**File**: `src/adapters/vb6_to_universal_ir.py` (424 lines)
**Pattern**: Thin transformation layer (no business logic)
**Principle**: Zero data loss during transformation

## Architecture

```
┌──────────────────────────────────────────────────────────┐
│           VB6 → Universal IR Transformation             │
├──────────────────────────────────────────────────────────┤
│                                                          │
│  VB6 IR (8 sections)                                    │
│  ┌────────────────────┐                                 │
│  │ - metadata         │                                 │
│  │ - ui               │ (forms, controls)               │
│  │ - business_logic   │ (event handlers, procedures)    │
│  │ - data             │ (models)                        │
│  │ - patterns         │                                 │
│  │ - dependencies     │ (external references)           │
│  │ - generation_hints │                                 │
│  │ - data_operations  │ (recordsets, ADO)               │
│  └────────────────────┘                                 │
│           │                                              │
│           v                                              │
│  ┌───────────────────────────────┐                      │
│  │  VB6ToUniversalIRAdapter      │                      │
│  │  - convert(vb6_ir) → UniversalIR                     │
│  │  - _convert_metadata()        │                      │
│  │  - _convert_ui()              │                      │
│  │  - _convert_business_logic()  │                      │
│  │  - _convert_data_structures() │                      │
│  │  - _convert_patterns()        │                      │
│  │  - ... (12 conversion methods)│                      │
│  └───────────────────────────────┘                      │
│           │                                              │
│           v                                              │
│  Universal IR (12 sections)                             │
│  ┌────────────────────┐                                 │
│  │ - metadata         │ (source_language="VB6")         │
│  │ - ui               │ (UISection with forms, controls)│
│  │ - data_structures  │ (entities from models)          │
│  │ - business_logic   │ (procedures from event handlers)│
│  │ - io_operations    │ (empty for VB6)                 │
│  │ - data_operations  │ (ADO, recordsets)               │
│  │ - events           │ (event handlers)                │
│  │ - patterns         │ (detected patterns)             │
│  │ - external_refs    │ (classes, modules, DLLs)        │
│  │ - security_issues  │ (detected issues)               │
│  │ - repository_map   │ (empty for VB6)                 │
│  │ - frontend_mapping │ (VB6 controls → Angular)        │
│  │ - generation_meta  │ (hints for Angular generator)   │
│  └────────────────────┘                                 │
└──────────────────────────────────────────────────────────┘
```

## Class Structure

```python
class VB6ToUniversalIRAdapter:
    def __init__(self):
        """Initialize adapter"""
        pass

    def convert(self, vb6_ir: Dict[str, Any]) -> UniversalIR:
        """Main conversion entry point"""
        # Convert all 12 sections
        # Return UniversalIR Pydantic model

    # 12 private conversion methods
    def _convert_metadata(self, vb6_metadata: Dict) -> SourceMetadata
    def _convert_ui(self, vb6_ui: Dict) -> UISection
    def _convert_business_logic(self, vb6_logic: Dict) -> BusinessLogicSection
    def _convert_data_structures(self, vb6_data: Dict) -> DataStructuresSection
    def _convert_data_operations(self, vb6_data_ops: Dict) -> DataOperationsSection
    def _convert_events(self, vb6_logic: Dict) -> EventsSection
    def _convert_patterns(self, vb6_patterns: List) -> List[DetectedPattern]
    def _convert_external_references(self, vb6_deps: Dict) -> ExternalReferencesSection
    def _convert_frontend_mapping(self, vb6_ui: Dict) -> FrontendMappingSection
    def _convert_generation_metadata(self, vb6_hints: Dict) -> GenerationMetadata

    # Helper methods
    def _convert_controls(self, vb6_controls: List) -> List[UIControl]
    def _map_vb6_control_to_angular(self, vb6_type: str) -> str
```

## Mapping Rules

### 1. Metadata Mapping

**VB6 IR → Universal IR**

| VB6 Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `source_file` | `metadata.source_file` | Direct copy |
| `source_lines_of_code` | `metadata.source_lines_of_code` | Direct copy |
| `source_path` | `metadata.source_path` | Direct copy |
| `analysis_timestamp` | `metadata.analysis_timestamp` | Direct copy |
| `analyzer_version` | `metadata.analyzer_version` | Direct copy |
| `parser_type` | `metadata.parser_type` | Direct copy |
| `confidence` | `metadata.confidence` | Direct copy |
| `ui_agent_confidence` | `metadata.ui_agent_confidence` | VB6-specific |
| `data_agent_confidence` | `metadata.data_agent_confidence` | Direct copy |
| `logic_agent_confidence` | `metadata.logic_agent_confidence` | Direct copy |
| `complexity` | `metadata.complexity` | Direct copy |
| `complexity_score` | `metadata.complexity_score` | Direct copy |
| `subagents_used` | `metadata.subagents_used` | Direct copy |
| *all VB6 metadata* | `metadata.language_specific.vb6` | Preserve original |

**Hard-coded values**:
- `source_language` = `"VB6"`
- `target_framework` = `"Angular"`

**Example**:
```json
{
  "metadata": {
    "source_language": "VB6",
    "source_file": "StartForm.frm",
    "source_lines_of_code": 245,
    "target_framework": "Angular",
    "confidence": 0.892,
    "ui_agent_confidence": 0.920,
    "data_agent_confidence": 0.875,
    "logic_agent_confidence": 0.880,
    "complexity": "medium",
    "subagents_used": ["vb6-ui-agent", "vb6-logic-agent", "vb6-data-agent"],
    "language_specific": {
      "vb6": { /* original VB6 metadata */ }
    }
  }
}
```

---

### 2. UI Mapping

**VB6 Forms → Universal IR UI**

**Form-level mapping**:
| VB6 Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `ui.form.name` | `ui.forms[0].name` | Form name |
| `ui.form.caption` | `ui.forms[0].title` | Form title |
| `ui.form.width` | `ui.forms[0].width` | Twips |
| `ui.form.height` | `ui.forms[0].height` | Twips |
| `ui.controls[]` | `ui.forms[0].controls[]` | Control list |
| `ui._what` | `ui.forms[0].description` | Agent description |

**Control-level mapping**:
| VB6 Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `controls[].id` | `controls[].name` | Control name |
| `controls[].type` | `controls[].type` | "CommandButton", "TextBox", etc. |
| `controls[].caption` | `controls[].caption` | Button/label text |
| `controls[].text` | `controls[].text` | TextBox value |
| `controls[].enabled` | `controls[].enabled` | Enabled state |
| `controls[].visible` | `controls[].visible` | Visible state |
| `controls[].tab_index` | `controls[].tab_index` | Tab order |
| `controls[].position.left` | `controls[].left` | X position (twips) |
| `controls[].position.top` | `controls[].top` | Y position (twips) |
| `controls[].position.width` | `controls[].width` | Width (twips) |
| `controls[].position.height` | `controls[].height` | Height (twips) |
| `controls[].data_field` | `controls[].data_field` | Data binding |
| `controls[].data_source` | `controls[].data_source` | Data source |
| `controls[]._what` | `controls[].description` | Agent description |

**Example**:

**VB6 IR**:
```json
{
  "ui": {
    "form": {
      "name": "StartForm",
      "caption": "Customer Entry",
      "width": 7095,
      "height": 4680
    },
    "controls": [
      {
        "id": "txtCustomerName",
        "type": "TextBox",
        "text": "",
        "enabled": true,
        "visible": true,
        "tab_index": 0,
        "position": {"left": 1200, "top": 600, "width": 2400, "height": 300}
      }
    ]
  }
}
```

**Universal IR**:
```json
{
  "ui": {
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
          }
        ]
      }
    ]
  }
}
```

---

### 3. Business Logic Mapping

**Event Handlers → Procedures**

VB6 event handlers are converted to `business_logic.procedures[]` with `type="event_handler"`.

| VB6 Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `business_logic.event_handlers[].event_name` | `procedures[].name` | "cmdSave_Click" |
| `business_logic.event_handlers[].control_name` | Stored in `description` | "cmdSave" |
| `business_logic.event_handlers[].logic_steps[]` | `procedures[].logic_steps[]` | Logic steps |
| `business_logic.event_handlers[]._what` | `procedures[].description` | Agent description |
| `business_logic.event_handlers[].confidence` | `procedures[].confidence` | Confidence score |
| `business_logic.event_handlers[].start_line` | `procedures[].start_line` | Source line |
| `business_logic.event_handlers[].end_line` | `procedures[].end_line` | Source line |

**Logic Steps Mapping**:
| VB6 Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `logic_steps[].type` | `logic_steps[].step_type` | "validation", "call", etc. |
| `logic_steps[].action` | `logic_steps[].description` | What this step does |
| `logic_steps[].code` | `logic_steps[].code_snippet` | Original VB6 code |
| `logic_steps[].line` | `logic_steps[].line_number` | Source line number |
| `logic_steps[]._angular_equivalent` | `logic_steps[].target_equivalent` | Angular equivalent |

**Example**:

**VB6 IR**:
```json
{
  "business_logic": {
    "event_handlers": [
      {
        "event_name": "cmdSave_Click",
        "control_name": "cmdSave",
        "logic_steps": [
          {
            "type": "validation",
            "action": "Validate customer name is not empty",
            "code": "If Trim(txtCustomerName.Text) = \"\" Then",
            "line": 45,
            "_angular_equivalent": "this.customerForm.get('name').invalid"
          }
        ],
        "_what": "Save button click handler",
        "confidence": 0.92,
        "start_line": 44,
        "end_line": 52
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
        "name": "cmdSave_Click",
        "type": "event_handler",
        "logic_steps": [
          {
            "step_type": "validation",
            "description": "Validate customer name is not empty",
            "code_snippet": "If Trim(txtCustomerName.Text) = \"\" Then",
            "line_number": 45,
            "target_equivalent": "this.customerForm.get('name').invalid"
          }
        ],
        "description": "Save button click handler",
        "confidence": 0.92,
        "start_line": 44,
        "end_line": 52
      }
    ]
  }
}
```

---

### 4. Data Structures Mapping

**VB6 Models → Entities**

| VB6 Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `data.models[].name` | `data_structures.entities[].name` | Entity name |
| `data.models[].fields[]` | `data_structures.entities[].fields[]` | Field list |
| `data.models[]._what` | `data_structures.entities[].description` | Agent description |

**Field Mapping**:
| VB6 Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `fields[].name` | `fields[].name` | Field name |
| `fields[].type` | `fields[].data_type` | "String", "Integer", etc. |
| `fields[].nullable` | `fields[].nullable` | Can be null? |
| `fields[].default_value` | `fields[].default_value` | Default value |
| `fields[].control_type` | `fields[].control_type` | "TextBox", etc. (VB6-specific) |
| `fields[].description` | `fields[].description` | Field description |

**Example**:

**VB6 IR**:
```json
{
  "data": {
    "models": [
      {
        "name": "Customer",
        "fields": [
          {
            "name": "customerId",
            "type": "Integer",
            "nullable": false,
            "control_type": "TextBox"
          },
          {
            "name": "customerName",
            "type": "String",
            "nullable": false,
            "control_type": "TextBox"
          }
        ],
        "_what": "Customer entity"
      }
    ]
  }
}
```

**Universal IR**:
```json
{
  "data_structures": {
    "entities": [
      {
        "name": "Customer",
        "type": "class",
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
            "nullable": false,
            "control_type": "TextBox"
          }
        ],
        "description": "Customer entity"
      }
    ]
  }
}
```

---

### 5. Events Mapping

**VB6 Event Handlers → Events Section**

| VB6 Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `business_logic.event_handlers[].event_name` | `events.handlers[].event_name` | "cmdSave_Click" |
| `business_logic.event_handlers[].control_name` | `events.handlers[].control_name` | "cmdSave" |
| `business_logic.event_handlers[].code` | `events.handlers[].handler_code` | VB6 code |
| `business_logic.event_handlers[]._what` | `events.handlers[].description` | Agent description |
| `business_logic.event_handlers[].start_line` | `events.handlers[].start_line` | Source line |
| `business_logic.event_handlers[].end_line` | `events.handlers[].end_line` | Source line |

**Example**:

**VB6 IR**:
```json
{
  "business_logic": {
    "event_handlers": [
      {
        "event_name": "cmdSave_Click",
        "control_name": "cmdSave",
        "code": "Private Sub cmdSave_Click()\\n  SaveCustomer\\nEnd Sub",
        "_what": "Save button click handler",
        "start_line": 44,
        "end_line": 46
      }
    ]
  }
}
```

**Universal IR**:
```json
{
  "events": {
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
}
```

---

### 6. Patterns Mapping

| VB6 Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `patterns[].type` | `patterns[].pattern_type` | "CRUD", "VALIDATION", etc. |
| `patterns[].name` | `patterns[].pattern_name` | Friendly name |
| `patterns[].entities` | `patterns[].entities_involved` | Entity names |
| `patterns[].description` | `patterns[].description` | Pattern description |
| `patterns[].angular_equivalent` | `patterns[].target_equivalent` | Angular equivalent |
| `patterns[].confidence` | `patterns[].confidence` | Confidence score |

---

### 7. External References Mapping

| VB6 Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `dependencies.classes[]` | `external_references.classes[]` | External classes (ADODB.Connection) |
| `dependencies.modules[]` | `external_references.modules[]` | VB6 modules |
| `dependencies.controls[]` | `external_references.controls[]` | ActiveX controls |
| `dependencies.dlls[]` | `external_references.dlls[]` | DLL references |

**Example**:

**VB6 IR**:
```json
{
  "dependencies": {
    "classes": ["ADODB.Connection", "ADODB.Recordset"],
    "modules": ["modDatabase", "modUtilities"],
    "controls": ["MSComDlg.CommonDialog"],
    "dlls": ["KERNEL32.DLL"]
  }
}
```

**Universal IR**:
```json
{
  "external_references": {
    "classes": ["ADODB.Connection", "ADODB.Recordset"],
    "modules": ["modDatabase", "modUtilities"],
    "controls": ["MSComDlg.CommonDialog"],
    "dlls": ["KERNEL32.DLL"]
  }
}
```

---

### 8. Frontend Mapping (VB6 Controls → Angular Components)

The adapter automatically generates frontend component mappings from VB6 UI controls.

**Control Type Mapping**:

| VB6 Control | Angular Component | Notes |
|-------------|------------------|-------|
| `CommandButton` | `mat-button` | Material button |
| `TextBox` | `mat-input` | Material input |
| `Label` | `mat-label` | Material label |
| `ComboBox` | `mat-select` | Material select dropdown |
| `ListBox` | `mat-list` | Material list |
| `CheckBox` | `mat-checkbox` | Material checkbox |
| `OptionButton` | `mat-radio-button` | Material radio |
| `Frame` | `mat-card` | Material card |
| `PictureBox` | `img` | HTML image tag |
| *default* | `div` | Generic container |

**Generated Mapping**:
```json
{
  "frontend_mapping": {
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
        "events": {}
      }
    ]
  }
}
```

---

### 9. Generation Metadata Mapping

| VB6 Field | Universal IR Field | Notes |
|-----------|-------------------|-------|
| `generation_hints.automation_rate` | `generation_metadata.estimated_automation_rate` | 0.0-1.0 |
| `generation_hints.manual_effort_hours` | `generation_metadata.estimated_manual_effort_hours` | Hours |
| `generation_hints.complexity_score` | `generation_metadata.complexity_score` | 1-10 |
| `generation_hints.template` | `generation_metadata.recommended_template` | "angular-component" |
| `generation_hints.notes[]` | `generation_metadata.generation_notes[]` | Implementation notes |
| *all VB6 hints* | `generation_metadata.target_specific_hints.angular` | Angular-specific hints |

**Example**:

**VB6 IR**:
```json
{
  "generation_hints": {
    "automation_rate": 0.92,
    "manual_effort_hours": 2.0,
    "complexity_score": 6,
    "template": "angular-crud-form",
    "notes": [
      "Use Angular Material Dialog for modal behavior",
      "Use Reactive Forms with validation"
    ]
  }
}
```

**Universal IR**:
```json
{
  "generation_metadata": {
    "estimated_automation_rate": 0.92,
    "estimated_manual_effort_hours": 2.0,
    "complexity_score": 6,
    "recommended_template": "angular-crud-form",
    "generation_notes": [
      "Use Angular Material Dialog for modal behavior",
      "Use Reactive Forms with validation"
    ],
    "target_specific_hints": {
      "angular": { /* original VB6 hints */ }
    }
  }
}
```

---

## Empty Sections for VB6

The following Universal IR sections are typically **empty** for VB6:

1. **io_operations**: VB6 doesn't use COBOL-style file I/O
2. **repository_mapping**: Backend repositories not applicable for VB6 UI apps

Example:
```json
{
  "io_operations": {},
  "repository_mapping": {}
}
```

---

## Known Limitations

### 1. ADO/Recordset Operations
- **Limitation**: VB6 `data_operations` don't map cleanly to Universal IR
- **Workaround**: Store in `data_operations` section with VB6-specific fields
- **Impact**: Code generators must handle VB6-specific data operations

### 2. ActiveX Controls
- **Limitation**: Custom ActiveX controls may not have Angular equivalents
- **Workaround**: Map to `div` with `external_references.controls[]` listing
- **Impact**: Manual implementation required for custom controls

### 3. VB6 Arrays (Control Arrays)
- **Limitation**: VB6 control arrays (e.g., `txtField(0)`, `txtField(1)`) not in Universal IR
- **Workaround**: Store array info in `language_specific.vb6`
- **Impact**: Generator must detect and handle control arrays

### 4. Complex Event Chains
- **Limitation**: VB6 event chains (Form_Load → LoadData → UpdateUI) not fully captured
- **Workaround**: Store as separate `procedures[]` with descriptions
- **Impact**: Manual workflow review recommended

---

## Usage

### Programmatic Usage

```python
from src.adapters.vb6_to_universal_ir import VB6ToUniversalIRAdapter
import json

# Load VB6 IR
with open('output/vb6/StartForm_ir.json', 'r') as f:
    vb6_ir = json.load(f)

# Convert to Universal IR
adapter = VB6ToUniversalIRAdapter()
universal_ir_model = adapter.convert(vb6_ir)

# Save Universal IR
with open('output/vb6/StartForm_universal_ir.json', 'w') as f:
    json.dump(universal_ir_model.model_dump(), f, indent=2)
```

### Integrated in Workflow

The VB6 adapter is automatically invoked in the LangGraph workflow via `convert_to_universal_ir_node()` in `src/orchestrator/langgraph_nodes.py`.

---

## Testing

### Test Files
- **Input**: VB6 IR from `samples/vb6/start-form/StartForm.frm`
- **Output**: Universal IR in `output/vb6/StartForm_universal_ir.json`

### Validation
- **Schema**: Pydantic automatic validation
- **Business Rules**: `UniversalIRValidator.validate()`
- **VB6-Specific**: `_validate_vb6_specific()` checks for `has_ui=true`

---

## Summary

The VB6 Adapter successfully transforms VB6 IR to Universal IR with:

✅ **Complete UI mapping** (forms, controls → UISection)
✅ **Event handler mapping** (VB6 events → procedures + events)
✅ **Data structure mapping** (models → entities)
✅ **Frontend component mapping** (VB6 controls → Angular Material)
✅ **Zero data loss** (original VB6 IR preserved in `language_specific.vb6`)
✅ **Type safety** (Pydantic validation)

**Next**: See `WORKFLOW_UPDATES.md` for integration details.

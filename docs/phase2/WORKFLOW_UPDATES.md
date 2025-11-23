# LangGraph Workflow Updates (Phase 2)

## Overview

Phase 2 introduces **Universal IR conversion** to both VB6 and COBOL LangGraph workflows. This document details the workflow changes, new nodes, state updates, validation logic, and logging behavior.

## Summary of Changes

### VB6 Workflow
- **New Node**: `convert_to_universal_ir_node` (async)
- **New Edge**: `validate → convert_to_universal_ir → END`
- **State Fields**: Added `universal_ir`, `validation_metrics`
- **Return Value**: Changed from `complete_ir` to `universal_ir`

### COBOL Workflow
- **New Node**: `convert_to_universal_ir_node` (sync)
- **New Edge**: `validate → convert_to_universal_ir → END`
- **State Fields**: Added `universal_ir`, `validation_metrics`
- **Return Value**: Changed from `complete_ir` to `universal_ir`

---

## Updated Workflow Diagrams

### VB6 LangGraph Workflow (Phase 2)

```
┌─────────────────────────────────────────────────────────────┐
│              VB6 LangGraph Workflow (Phase 2)              │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  INPUT: frm_content, source_file                           │
│    │                                                        │
│    v                                                        │
│  ┌──────────┐                                              │
│  │  START   │                                              │
│  └──────────┘                                              │
│       │                                                     │
│       ├──────────────┬──────────────┐                      │
│       │              │              │                      │
│       v              v              v                      │
│  ┌─────────┐   ┌──────────┐   ┌─────────┐                │
│  │   UI    │   │  Logic   │   │  Data   │                │
│  │  Agent  │   │  Agent   │   │  Agent  │                │
│  │ (async) │   │ (async)  │   │ (async) │                │
│  └─────────┘   └──────────┘   └─────────┘                │
│       │              │              │                      │
│       │   ui_ir      │  logic_ir    │  data_ir            │
│       │              │              │                      │
│       └──────────────┴──────────────┘                      │
│                      │                                     │
│                      v                                     │
│             ┌────────────────┐                            │
│             │  Merge Node    │                            │
│             │  (combines 3   │                            │
│             │   partial IRs) │                            │
│             └────────────────┘                            │
│                      │                                     │
│                      │ complete_ir (VB6 IR)               │
│                      v                                     │
│             ┌────────────────┐                            │
│             │ Validate Node  │                            │
│             │ (VB6 schema)   │                            │
│             └────────────────┘                            │
│                      │                                     │
│                      │ ← PHASE 2 ADDITION                 │
│                      v                                     │
│      ┌──────────────────────────────────┐                │
│      │ convert_to_universal_ir_node     │                │
│      │ (async)                          │                │
│      │                                  │                │
│      │ Steps:                           │                │
│      │ 1. VB6ToUniversalIRAdapter       │                │
│      │ 2. UniversalIRValidator          │                │
│      │ 3. Return universal_ir +         │                │
│      │    validation_metrics            │                │
│      └──────────────────────────────────┘                │
│                      │                                     │
│                      │ universal_ir, validation_metrics   │
│                      v                                     │
│             ┌────────────────┐                            │
│             │      END       │                            │
│             │ (return        │                            │
│             │ universal_ir)  │                            │
│             └────────────────┘                            │
│                                                             │
│  OUTPUT: Universal IR JSON (12 sections)                  │
└─────────────────────────────────────────────────────────────┘
```

**Execution Time**: 3-5 seconds (LLM calls dominate, +0.1s for Universal IR)

---

### COBOL LangGraph Workflow (Phase 2)

```
┌─────────────────────────────────────────────────────────────┐
│            COBOL LangGraph Workflow (Phase 2)              │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  INPUT: cobol_content, source_file                         │
│    │                                                        │
│    v                                                        │
│  ┌──────────┐                                              │
│  │  START   │                                              │
│  └──────────┘                                              │
│       │                                                     │
│       ├──────────────┬──────────────┐                      │
│       │              │              │                      │
│       v              v              v                      │
│  ┌─────────┐   ┌──────────┐   ┌─────────┐                │
│  │  Data   │   │  Logic   │   │  I/O    │                │
│  │  Agent  │   │  Agent   │   │  Agent  │                │
│  │  (sync) │   │  (sync)  │   │  (sync) │                │
│  └─────────┘   └──────────┘   └─────────┘                │
│       │              │              │                      │
│       │  data_ir     │  logic_ir    │  io_ir              │
│       │              │              │                      │
│       └──────────────┴──────────────┘                      │
│                      │                                     │
│                      v                                     │
│             ┌────────────────┐                            │
│             │  Merge Node    │                            │
│             │  (combines 3   │                            │
│             │   partial IRs) │                            │
│             └────────────────┘                            │
│                      │                                     │
│                      │ complete_ir (COBOL IR)             │
│                      v                                     │
│             ┌────────────────┐                            │
│             │ Validate Node  │                            │
│             │ (COBOL schema) │                            │
│             └────────────────┘                            │
│                      │                                     │
│                      │ ← PHASE 2 ADDITION                 │
│                      v                                     │
│      ┌──────────────────────────────────┐                │
│      │ convert_to_universal_ir_node     │                │
│      │ (sync)                           │                │
│      │                                  │                │
│      │ Steps:                           │                │
│      │ 1. COBOLToUniversalIRAdapter     │                │
│      │ 2. UniversalIRValidator          │                │
│      │ 3. Return universal_ir +         │                │
│      │    validation_metrics            │                │
│      └──────────────────────────────────┘                │
│                      │                                     │
│                      │ universal_ir, validation_metrics   │
│                      v                                     │
│             ┌────────────────┐                            │
│             │      END       │                            │
│             │ (return        │                            │
│             │ universal_ir)  │                            │
│             └────────────────┘                            │
│                                                             │
│  OUTPUT: Universal IR JSON (12 sections)                  │
└─────────────────────────────────────────────────────────────┘
```

**Execution Time**: <0.1 seconds (pure Python, +0.05s for Universal IR)

---

## New Node: `convert_to_universal_ir_node`

### VB6 Version

**File**: `src/orchestrator/langgraph_nodes.py` (lines 633-728)

**Signature**:
```python
async def convert_to_universal_ir_node(state: VB6State) -> Dict[str, Any]:
    """
    WHAT: Convert VB6 IR to Universal IR (Phase 2)
    WHY: Enable unified code generation from language-agnostic schema
    HOW: Use VB6ToUniversalIRAdapter + UniversalIRValidator
    """
```

**Inputs** (from state):
- `complete_ir`: VB6 IR (dict)

**Outputs** (returned to state):
- `universal_ir`: Universal IR (dict)
- `validation_metrics`: Validation metrics (dict)
- `errors`: List of validation errors (if any)
- `timing`: Execution time for this node

**Process**:
1. Extract `complete_ir` from state
2. Create `VB6ToUniversalIRAdapter` instance
3. Convert: `universal_ir_model = adapter.convert(complete_ir)`
4. Convert Pydantic model to dict: `universal_ir_dict = universal_ir_model.model_dump()`
5. Create `UniversalIRValidator` instance
6. Validate: `validation_result = validator.validate(universal_ir_model)`
7. Extract validation metrics (is_valid, errors, warnings, counts)
8. Return dict with `universal_ir`, `validation_metrics`, `errors`, `timing`

**Error Handling**:
- If `complete_ir` missing: Add error, return early
- If conversion fails: Catch exception, add error, return with timing
- If validation fails: Still return Universal IR, but mark as invalid

**Example Output**:
```python
{
    "universal_ir": { /* 12-section Universal IR dict */ },
    "validation_metrics": {
        "is_valid": True,
        "validation_errors": [],
        "validation_warnings": [],
        "entities_count": 2,
        "procedures_count": 5,
        "ui_controls_count": 12,
        "event_handlers_count": 5,
        "io_operations_count": 0,
        "data_operations_count": 0
    },
    "errors": [],  # Empty if no errors
    "timing": {
        "universal_ir_conversion": 0.08  # seconds
    }
}
```

---

### COBOL Version

**File**: `src/orchestrator/cobol_langgraph_nodes.py` (lines 336-435)

**Signature**:
```python
def convert_to_universal_ir_node(state: COBOLState) -> Dict[str, Any]:
    """
    WHAT: Convert COBOL IR to Universal IR (Phase 2)
    WHY: Enable unified code generation from language-agnostic schema
    HOW: Use COBOLToUniversalIRAdapter + UniversalIRValidator
    """
```

**Inputs** (from state):
- `complete_ir`: COBOL IR (dict)

**Outputs** (returned to state):
- `universal_ir`: Universal IR (dict)
- `validation_metrics`: Validation metrics (dict)
- `errors`: List of validation errors (if any)
- `timing`: Execution time for this node

**Process**:
1. Extract `complete_ir` from state
2. Create `COBOLToUniversalIRAdapter` instance
3. Convert: `universal_ir_model = adapter.convert(complete_ir)`
4. Convert Pydantic model to dict: `universal_ir_dict = universal_ir_model.model_dump()`
5. Create `UniversalIRValidator` instance
6. Validate: `validation_result = validator.validate(universal_ir_model)`
7. Extract validation metrics (is_valid, errors, warnings, counts)
8. Return dict with `universal_ir`, `validation_metrics`, `errors`, `timing`

**Difference from VB6**: Synchronous (not async) because COBOL agents are pure Python

**Example Output**:
```python
{
    "universal_ir": { /* 12-section Universal IR dict */ },
    "validation_metrics": {
        "is_valid": True,
        "validation_errors": [],
        "validation_warnings": [],
        "entities_count": 2,
        "procedures_count": 2,
        "ui_controls_count": 0,
        "event_handlers_count": 0,
        "io_operations_count": 8,
        "data_operations_count": 0
    },
    "errors": [],  # Empty if no errors
    "timing": {
        "universal_ir_conversion": 0.05  # seconds
    }
}
```

---

## State Changes

### VB6State Updates

**File**: `src/orchestrator/langgraph_state.py`

**Added Fields**:
```python
class VB6State(TypedDict):
    # ... existing fields ...

    # ============================================================
    # UNIVERSAL IR (Phase 2 - set by convert_to_universal_ir node)
    # ============================================================
    universal_ir: Optional[Dict[str, Any]]  # Universal IR (language-agnostic schema)
                                             # Replaces language-specific IR for code generation

    validation_metrics: Optional[Dict[str, Any]]  # Validation metrics from Universal IR validator
```

**State Flow**:
1. `source_code`, `source_file` → Input (orchestrator)
2. `ui_ir`, `logic_ir`, `data_ir` → Set by agent nodes (parallel)
3. `complete_ir` → Set by merge node (sequential)
4. `universal_ir`, `validation_metrics` → **Set by convert_to_universal_ir node** (Phase 2)
5. `errors`, `timing` → Accumulated across all nodes

---

### COBOLState Updates

**File**: `src/orchestrator/cobol_langgraph_state.py`

**Added Fields**:
```python
class COBOLState(TypedDict):
    # ... existing fields ...

    # ============================================================
    # UNIVERSAL IR (Phase 2 - set by convert_to_universal_ir node)
    # ============================================================

    universal_ir: Optional[Dict[str, Any]]  # Universal IR (language-agnostic schema)
                                             # Replaces language-specific IR for code generation

    validation_metrics: Optional[Dict[str, Any]]  # Validation metrics from Universal IR validator
```

**State Flow**:
1. `source_code`, `source_file` → Input (orchestrator)
2. `data_ir`, `logic_ir`, `io_ir` → Set by agent nodes (parallel)
3. `complete_ir` → Set by merge node (sequential)
4. `universal_ir`, `validation_metrics` → **Set by convert_to_universal_ir node** (Phase 2)
5. `errors`, `timing` → Accumulated across all nodes

---

## Validation Metrics

### Metrics Captured

Both VB6 and COBOL validation metrics include:

| Metric | Description | VB6 | COBOL |
|--------|-------------|-----|-------|
| `is_valid` | Overall validation result | ✓ | ✓ |
| `validation_errors` | List of error messages | ✓ | ✓ |
| `validation_warnings` | List of warning messages | ✓ | ✓ |
| `entities_count` | Number of data entities | ✓ | ✓ |
| `procedures_count` | Number of procedures | ✓ | ✓ |
| `io_operations_count` | Number of I/O operations | ✓ | ✓ |
| `ui_controls_count` | Number of UI controls | ✓ (12+) | ✓ (0) |
| `event_handlers_count` | Number of event handlers | ✓ (5+) | ✓ (0) |
| `data_operations_count` | Number of data operations | ✓ (ADO) | ✓ (0) |

### Example Validation Metrics

**VB6**:
```json
{
  "is_valid": true,
  "validation_errors": [],
  "validation_warnings": [],
  "entities_count": 2,
  "procedures_count": 5,
  "ui_controls_count": 12,
  "event_handlers_count": 5,
  "io_operations_count": 0,
  "data_operations_count": 3
}
```

**COBOL**:
```json
{
  "is_valid": true,
  "validation_errors": [],
  "validation_warnings": [],
  "entities_count": 2,
  "procedures_count": 2,
  "ui_controls_count": 0,
  "event_handlers_count": 0,
  "io_operations_count": 8,
  "data_operations_count": 0
}
```

---

## Logging Behavior

### VB6 Workflow Logging

**Console Output** (Phase 2):
```
============================================================
🔍 VB6 Parser v2.1.0 (LangGraph Architecture)
============================================================

🔧 Building LangGraph workflow...
✅ LangGraph workflow compiled successfully
   - 3 parallel agent nodes
   - 3 sequential processing nodes (Merge, Validate, Universal IR)
   - Auto state management

📄 Parsing: StartForm.frm
📊 Size: 8245 chars, 245 lines

⚡ Launching agents in parallel via LangGraph...

  🎨 UI Agent: Extracting UI components...
  ✓ UI Agent: Found 12 controls
  ✓ UI Agent: Confidence 92.0%
  ⏱  UI Agent: 1.2s

  ⚙️  Logic Agent: Extracting business logic...
  ✓ Logic Agent: Found 5 event handlers
  ✓ Logic Agent: Confidence 88.0%
  ⏱  Logic Agent: 1.5s

  💾 Data Agent: Extracting data models...
  ✓ Data Agent: Found 2 models
  ✓ Data Agent: Confidence 87.5%
  ⏱  Data Agent: 1.3s

🔗 Merging partial IRs...
✓ Merge complete in 0.1s
📈 Overall confidence: 89.2%

✓ Validating schema compliance...
✓ Schema validation passed

🔄 Converting VB6 IR → Universal IR...
✓ Universal IR conversion complete in 0.08s

🔍 Validating Universal IR...
   Language: VB6
   Target: Angular

  ✓ Pydantic schema validation passed
  📋 Validating metadata section...
  📦 Validating data structures section...
  ⚙️  Validating business logic section...
  📁 Validating I/O operations section...
  🎨 Running VB6-specific validations...
  📐 Calculating validation metrics...

  ✅ Validation PASSED

✓ Universal IR validation: PASSED
📊 Universal IR: 12 controls, 2 entities, 5 procedures, 5 events

⏱  Timing breakdown:
  - ui_agent: 1.2s
  - logic_agent: 1.5s
  - data_agent: 1.3s
  - merge: 0.1s
  - universal_ir_conversion: 0.1s
  - Total: 4.2s

📊 Validation Metrics:
  - Valid: True
  - UI Controls: 12
  - Entities: 2
  - Procedures: 5
  - Event Handlers: 5

🎉 Parsing complete!
   Confidence: 89.2%
   Complexity: medium
```

---

### COBOL Workflow Logging

**Console Output** (Phase 2):
```
============================================================
🔍 COBOL Parser v1.0.0 (LangGraph Architecture)
============================================================

🔧 Building COBOL LangGraph workflow...
✅ COBOL LangGraph workflow compiled successfully
   - 3 parallel agent nodes (Data, Logic, I/O)
   - 3 sequential processing nodes (Merge, Validate, Universal IR)
   - Auto state management
   - Pure Python (no LLM calls)

📄 Parsing: seq.cbl
📊 Size: 1614 chars, 56 lines

⚡ Launching agents in parallel via LangGraph...

  💾 Data Agent: Extracting data structures...
  ✓ Data Agent: Found 3 entities
  ✓ Data Agent: Found 1 file(s)
  ✓ Data Agent: Confidence 87.5%
  ⏱  Data Agent: 0.00s

  ⚙️  Logic Agent: Extracting business logic...
  ✓ Logic Agent: Found 2 procedures
  ✓ Logic Agent: Found 1 workflow(s)
  ✓ Logic Agent: Confidence 83.8%
  ⏱  Logic Agent: 0.00s

  📁 I/O Agent: Extracting file operations...
  ✓ I/O Agent: Found 8 I/O operations
  ✓ I/O Agent: Detected 1 pattern(s)
  ✓ I/O Agent: Recommended 2 repository/ies
  ✓ I/O Agent: Confidence 91.3%
  ⏱  I/O Agent: 0.00s

🔗 Merging partial IRs...
✓ Merge complete in 0.00s
📈 Overall confidence: 87.2%

✓ Validating schema compliance...
✓ Schema validation passed

🔄 Converting COBOL IR → Universal IR...
✓ Universal IR conversion complete in 0.05s

🔍 Validating Universal IR...
   Language: COBOL
   Target: SpringBoot

  ✓ Pydantic schema validation passed
  📋 Validating metadata section...
  📦 Validating data structures section...
  ⚙️  Validating business logic section...
  📁 Validating I/O operations section...
  📊 Running COBOL-specific validations...
  📐 Calculating validation metrics...

  ✅ Validation PASSED

✓ Universal IR validation: PASSED
📊 Universal IR: 2 entities, 2 procedures, 8 I/O ops

⏱  Timing breakdown:
  - data_agent: 0.00s
  - logic_agent: 0.00s
  - io_agent: 0.00s
  - merge: 0.00s
  - universal_ir_conversion: 0.05s
  - Total: 0.05s

📊 Validation Metrics:
  - Valid: True
  - Entities: 2
  - Procedures: 2
  - I/O Operations: 8

🎉 Parsing complete!
   Confidence: 87.2%
   Complexity: medium
```

---

## Error Handling

### Validation Errors

**Scenario**: Universal IR fails validation

**Behavior**:
1. Adapter converts COBOL/VB6 IR → Universal IR
2. Validator detects errors (e.g., missing required field)
3. `validation_metrics.is_valid = False`
4. Errors added to `validation_metrics.validation_errors[]`
5. Node returns Universal IR + validation metrics
6. Workflow continues (does NOT abort)
7. Orchestrator displays validation errors

**Example**:
```
📊 Validation Metrics:
  - Valid: False
  - Entities: 0
  - Procedures: 2
  - I/O Operations: 8
  - Validation Errors: 1
       - metadata.source_file is required
```

### Adapter Errors

**Scenario**: Adapter conversion fails

**Behavior**:
1. Adapter throws exception during conversion
2. Exception caught in `convert_to_universal_ir_node`
3. Error message added to `errors[]` list
4. Timing still recorded
5. Workflow aborts (raises ValueError in orchestrator)

**Example**:
```
⚠️  Errors encountered during parsing:
  - Universal IR conversion failed: KeyError 'metadata'
```

---

## Backward Compatibility

### Phase 1 vs Phase 2 Return Values

**Phase 1** (Before Universal IR):
```python
orchestrator.parse(source_code, source_file)
# Returns: complete_ir (VB6 IR or COBOL IR)
```

**Phase 2** (After Universal IR):
```python
orchestrator.parse(source_code, source_file)
# Returns: universal_ir (Universal IR)
```

### Breaking Change

**Yes, this is a breaking change**. Code generators must be updated to read Universal IR instead of language-specific IR.

**Migration**:
- **Before**: Generator reads `complete_ir['ui']['form']`
- **After**: Generator reads `universal_ir['ui']['forms'][0]`

**Preservation**:
- Original IR preserved in `universal_ir['metadata']['language_specific']['vb6']` or `['cobol']`

---

## Performance Impact

### VB6 Workflow
- **Phase 1**: 3-5 seconds (3 LLM calls)
- **Phase 2**: 3-5.1 seconds (+0.1s for Universal IR)
- **Impact**: ~2% overhead (negligible)

### COBOL Workflow
- **Phase 1**: <0.1 seconds (pure Python)
- **Phase 2**: ~0.15 seconds (+0.05s for Universal IR)
- **Impact**: ~50% overhead (but still sub-second)

---

## Testing

### VB6 Test
```bash
PYTHONPATH=/Users/DR/projects/lmod/src python3 -c "
import asyncio
from pathlib import Path
from orchestrator.langgraph_workflow import LangGraphVB6Orchestrator

async def test():
    frm_file = Path('samples/vb6/start-form/StartForm.frm')
    frm_content = frm_file.read_text()

    orchestrator = LangGraphVB6Orchestrator()
    universal_ir = await orchestrator.parse(frm_content, 'StartForm.frm')

    print(f'Schema Version: {universal_ir[\"schema_version\"]}')
    print(f'Source Language: {universal_ir[\"metadata\"][\"source_language\"]}')

asyncio.run(test())
"
```

### COBOL Test
```bash
PYTHONPATH=/Users/DR/projects/lmod/src python3 -c "
from pathlib import Path
from orchestrator.cobol_langgraph_workflow import LangGraphCOBOLOrchestrator

cobol_file = Path('samples/cobol/simple/seq.cbl')
cobol_source = cobol_file.read_text()

orchestrator = LangGraphCOBOLOrchestrator()
universal_ir = orchestrator.parse(cobol_source, 'seq.cbl')

print(f'Schema Version: {universal_ir[\"schema_version\"]}')
print(f'Source Language: {universal_ir[\"metadata\"][\"source_language\"]}')
"
```

---

## Summary

Phase 2 workflow updates successfully integrate Universal IR conversion with:

✅ **New `convert_to_universal_ir_node`** in both VB6 and COBOL workflows
✅ **State extensions** (`universal_ir`, `validation_metrics`)
✅ **Enhanced validation** with comprehensive metrics
✅ **Detailed logging** for debugging and monitoring
✅ **Error handling** with graceful degradation
✅ **Minimal performance impact** (<5% overhead for VB6, ~50% for COBOL)
✅ **Backward compatibility** via `language_specific` preservation

**Next**: See regression testing results in test output files.

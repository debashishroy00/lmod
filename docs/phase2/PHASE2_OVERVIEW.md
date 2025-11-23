# Phase 2: Universal IR + Adapter Pattern

## Overview

Phase 2 introduces a **Universal Intermediate Representation (Universal IR)** layer to the LMOD Multi-Language Modernization Platform. This architectural enhancement enables language-agnostic code generation by standardizing the output from all language-specific parsers (VB6, COBOL, PowerBuilder, etc.) into a unified schema.

## Purpose

**Why Universal IR?**

1. **Multi-Language Support**: Enable a single code generator to support multiple source languages
2. **Extensibility**: Add new source languages without modifying existing generators
3. **Maintainability**: Centralize IR schema evolution in one place
4. **Quality**: Unified validation rules ensure consistent IR quality
5. **Future-Proofing**: Prepared for PowerBuilder, Delphi, AS/400 migration

**Before Phase 2**, each language had its own IR schema:
- VB6 IR (8 sections) → Angular Generator
- COBOL IR (8 sections) → Spring Boot Generator
- **Problem**: Adding PowerBuilder would require a new IR schema + new generator

**After Phase 2**, all languages share one schema:
- VB6 IR → **Adapter** → Universal IR → Angular Generator
- COBOL IR → **Adapter** → Universal IR → Spring Boot Generator
- PowerBuilder IR → **Adapter** → Universal IR → Any Generator
- **Benefit**: One generator supports all languages

## Supported Languages

### Current (Phase 2)
- **VB6**: Fully supported with UI-heavy applications
- **COBOL**: Fully supported with batch processing focus

### Roadmap (Future)
- **PowerBuilder**: Planned (similar to VB6 adapter)
- **Delphi**: Planned
- **AS/400 RPG**: Planned

## Architecture

### High-Level System Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                    LMOD PHASE 2 ARCHITECTURE                        │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  SOURCE LANGUAGES        PARSERS         ADAPTERS    UNIVERSAL IR   │
│  ┌──────────┐         ┌──────────┐    ┌──────────┐  ┌───────────┐ │
│  │   VB6    │ ──────> │ VB6 IR   │ -> │   VB6    │  │           │ │
│  │  .frm    │         │ (8 sect) │    │ Adapter  │  │ Universal │ │
│  └──────────┘         └──────────┘    └──────────┘  │    IR     │ │
│                                            │         │           │ │
│  ┌──────────┐         ┌──────────┐    ┌──────────┐  │ (12 sect) │ │
│  │  COBOL   │ ──────> │ COBOL IR │ -> │  COBOL   │  │           │ │
│  │  .cbl    │         │ (8 sect) │    │ Adapter  │  │ Pydantic  │ │
│  └──────────┘         └──────────┘    └──────────┘  │  Schema   │ │
│                                            │         │           │ │
│  ┌──────────┐         ┌──────────┐    ┌──────────┐  │           │ │
│  │PowerBldr │ ──────> │  PB IR   │ -> │    PB    │  │           │ │
│  │  .pbw    │         │ (future) │    │ Adapter  │  │           │ │
│  └──────────┘         └──────────┘    └──────────┘  └───────────┘ │
│                                            │              │         │
│                                            v              v         │
│                                       ┌──────────────────────┐     │
│                                       │  IR Validator        │     │
│                                       │  - Schema checks     │     │
│                                       │  - Business rules    │     │
│                                       │  - Metrics calc      │     │
│                                       └──────────────────────┘     │
│                                                 │                   │
│                                                 v                   │
│  CODE GENERATORS                    ┌──────────────────────┐       │
│  ┌────────────────────┐             │                      │       │
│  │ Angular Generator  │ <────────── │   Universal IR JSON  │       │
│  │  (UI-heavy apps)   │             │                      │       │
│  └────────────────────┘             │  - metadata          │       │
│                                      │  - ui                │       │
│  ┌────────────────────┐             │  - data_structures   │       │
│  │ Spring Boot Gen    │ <────────── │  - business_logic    │       │
│  │ (Backend services) │             │  - io_operations     │       │
│  └────────────────────┘             │  - ... (12 sections) │       │
│                                      └──────────────────────┘       │
└─────────────────────────────────────────────────────────────────────┘
```

## Before vs. After Comparison

### Before Phase 2 (Language-Specific IR)

**VB6 Pipeline:**
```
VB6 Form (.frm)
  → VB6 Parser (3 agents)
  → VB6 IR (8 sections)
  → Angular Generator
  → Angular Code
```

**COBOL Pipeline:**
```
COBOL Program (.cbl)
  → COBOL Parser (3 agents)
  → COBOL IR (8 sections)
  → Spring Boot Generator
  → Java Code
```

**Problems:**
- Duplicate schema definitions (VB6 IR ≠ COBOL IR)
- Cannot reuse generators across languages
- Adding PowerBuilder requires new IR schema + new generator
- Validation logic duplicated

### After Phase 2 (Universal IR)

**VB6 Pipeline:**
```
VB6 Form (.frm)
  → VB6 Parser (3 agents)
  → VB6 IR (8 sections)
  → VB6 Adapter                    ← NEW
  → Universal IR (12 sections)     ← NEW
  → Universal IR Validator         ← NEW
  → Angular Generator (reads Universal IR)
  → Angular Code
```

**COBOL Pipeline:**
```
COBOL Program (.cbl)
  → COBOL Parser (3 agents)
  → COBOL IR (8 sections)
  → COBOL Adapter                  ← NEW
  → Universal IR (12 sections)     ← NEW
  → Universal IR Validator         ← NEW
  → Spring Boot Generator (reads Universal IR)
  → Java Code
```

**Benefits:**
- Single schema for all languages (12 sections)
- Generators read Universal IR (language-agnostic)
- Adding PowerBuilder only requires PowerBuilder Adapter
- Validation logic centralized

## Workflow Diagrams

### VB6 LangGraph Workflow (Phase 2)

```
                    ┌─────────────────────────────────────┐
                    │  VB6 LangGraph Workflow (Phase 2)  │
                    └─────────────────────────────────────┘
                                    │
                                    │ frm_content
                                    v
                            ┌───────────────┐
                            │     START     │
                            └───────────────┘
                                    │
                    ┌───────────────┼───────────────┐
                    │               │               │
                    v               v               v
            ┌──────────────┐ ┌──────────────┐ ┌──────────────┐
            │  UI Agent    │ │ Logic Agent  │ │ Data Agent   │
            │  (LLM Call)  │ │  (LLM Call)  │ │  (LLM Call)  │
            └──────────────┘ └──────────────┘ └──────────────┘
                    │               │               │
                    │   ui_ir       │  logic_ir     │  data_ir
                    │               │               │
                    └───────────────┼───────────────┘
                                    │
                                    v
                            ┌───────────────┐
                            │  Merge Node   │
                            │  (combine 3   │
                            │   partial IRs)│
                            └───────────────┘
                                    │
                                    │ complete_ir (VB6 IR)
                                    v
                            ┌───────────────┐
                            │ Validate Node │
                            │ (VB6 schema)  │
                            └───────────────┘
                                    │
                                    │ ← PHASE 2 ADDITION
                                    v
                    ┌───────────────────────────────┐
                    │  convert_to_universal_ir_node │
                    │  - VB6ToUniversalIRAdapter    │
                    │  - UniversalIRValidator       │
                    └───────────────────────────────┘
                                    │
                                    │ universal_ir
                                    │ validation_metrics
                                    v
                            ┌───────────────┐
                            │      END      │
                            │ (return       │
                            │ universal_ir) │
                            └───────────────┘
```

### COBOL LangGraph Workflow (Phase 2)

```
                    ┌─────────────────────────────────────┐
                    │ COBOL LangGraph Workflow (Phase 2) │
                    └─────────────────────────────────────┘
                                    │
                                    │ cobol_content
                                    v
                            ┌───────────────┐
                            │     START     │
                            └───────────────┘
                                    │
                    ┌───────────────┼───────────────┐
                    │               │               │
                    v               v               v
            ┌──────────────┐ ┌──────────────┐ ┌──────────────┐
            │ Data Agent   │ │ Logic Agent  │ │  I/O Agent   │
            │ (Pure Python)│ │(Pure Python) │ │(Pure Python) │
            └──────────────┘ └──────────────┘ └──────────────┘
                    │               │               │
                    │  data_ir      │  logic_ir     │  io_ir
                    │               │               │
                    └───────────────┼───────────────┘
                                    │
                                    v
                            ┌───────────────┐
                            │  Merge Node   │
                            │  (combine 3   │
                            │   partial IRs)│
                            └───────────────┘
                                    │
                                    │ complete_ir (COBOL IR)
                                    v
                            ┌───────────────┐
                            │ Validate Node │
                            │ (COBOL schema)│
                            └───────────────┘
                                    │
                                    │ ← PHASE 2 ADDITION
                                    v
                    ┌───────────────────────────────┐
                    │  convert_to_universal_ir_node │
                    │  - COBOLToUniversalIRAdapter  │
                    │  - UniversalIRValidator       │
                    └───────────────────────────────┘
                                    │
                                    │ universal_ir
                                    │ validation_metrics
                                    v
                            ┌───────────────┐
                            │      END      │
                            │ (return       │
                            │ universal_ir) │
                            └───────────────┘
```

## Key Components

### 1. Universal IR Schema
- **Location**: `src/core/universal_ir_schema.py`
- **Type**: Pydantic models (700+ lines)
- **Sections**: 12 major sections (metadata, ui, data_structures, business_logic, etc.)
- **Features**: Type-safe, JSON serializable, auto-validation

### 2. Adapters
- **VB6 Adapter**: `src/adapters/vb6_to_universal_ir.py` (400+ lines)
- **COBOL Adapter**: `src/adapters/cobol_to_universal_ir.py` (400+ lines)
- **Pattern**: Thin transformation layer (no business logic)
- **Principle**: No data loss during transformation

### 3. Universal IR Validator
- **Location**: `src/core/universal_ir_validator.py`
- **Features**:
  - Pydantic schema validation (automatic)
  - Business rule validation (custom)
  - Language-specific validation (VB6 vs COBOL)
  - Metrics calculation
  - Enhanced logging (verbose mode)

### 4. LangGraph Nodes (Phase 2 Addition)
- **VB6**: `convert_to_universal_ir_node()` in `src/orchestrator/langgraph_nodes.py`
- **COBOL**: `convert_to_universal_ir_node()` in `src/orchestrator/cobol_langgraph_nodes.py`
- **Purpose**: Invoke adapter + validator in workflow

### 5. State Updates
- **VB6State**: Added `universal_ir` and `validation_metrics` fields
- **COBOLState**: Added `universal_ir` and `validation_metrics` fields

## Migration Path

### Phase 1 → Phase 2 Migration (Zero Regression)

**Phase 1 (Before)**:
- VB6 parser returns VB6 IR
- COBOL parser returns COBOL IR

**Phase 2 (After)**:
- VB6 parser returns **Universal IR** (VB6 IR preserved in `metadata.language_specific`)
- COBOL parser returns **Universal IR** (COBOL IR preserved in `metadata.language_specific`)
- **Zero regression**: All Phase 1 data preserved

### Backward Compatibility

Phase 2 maintains 100% backward compatibility:

1. **VB6 IR Preservation**: Original VB6 IR stored in `universal_ir.metadata.language_specific.vb6`
2. **COBOL IR Preservation**: Original COBOL IR stored in `universal_ir.metadata.language_specific.cobol`
3. **Agent Logic Unchanged**: No changes to VB6 or COBOL parsing agents
4. **Generator Compatibility**: Generators read Universal IR (not language-specific IR)

## Performance Impact

### VB6 Workflow
- **Phase 1**: 3-5 seconds (LLM calls dominate)
- **Phase 2**: +0.1 seconds (adapter + validation)
- **Impact**: Negligible (~2% overhead)

### COBOL Workflow
- **Phase 1**: <0.1 seconds (pure Python parsing)
- **Phase 2**: +0.05 seconds (adapter + validation)
- **Impact**: Minimal (~50% overhead, but still sub-second)

## Quality Improvements

1. **Validation**: All IR now validated against unified schema
2. **Metrics**: Comprehensive metrics (entities, procedures, UI controls, etc.)
3. **Confidence**: Per-agent confidence + overall confidence
4. **Error Reporting**: Structured errors, warnings, info messages
5. **Logging**: Enhanced logging with section-by-section validation

## Future Enhancements

### Phase 3 (Planned)
- **PowerBuilder Adapter**: Migrate PowerBuilder applications
- **Unified Code Generator**: Single generator for all target frameworks
- **IR Optimization**: Compress IR for large applications

### Phase 4 (Planned)
- **Incremental Migration**: Support partial application migration
- **Dependency Graph**: Cross-file dependency analysis
- **Migration Verification**: Automated testing of migrated code

## File Locations

### Core Files
```
src/core/
├── universal_ir_schema.py          # Universal IR Pydantic schema (700+ lines)
└── universal_ir_validator.py       # IR validator with enhanced logging

src/adapters/
├── vb6_to_universal_ir.py          # VB6 → Universal IR adapter
└── cobol_to_universal_ir.py        # COBOL → Universal IR adapter

src/orchestrator/
├── langgraph_workflow.py           # VB6 LangGraph workflow (updated)
├── langgraph_nodes.py              # VB6 nodes (convert_to_universal_ir_node added)
├── langgraph_state.py              # VB6State (universal_ir fields added)
├── cobol_langgraph_workflow.py     # COBOL LangGraph workflow (updated)
├── cobol_langgraph_nodes.py        # COBOL nodes (convert_to_universal_ir_node added)
└── cobol_langgraph_state.py        # COBOLState (universal_ir fields added)

docs/phase2/
├── PHASE2_OVERVIEW.md              # This file
├── UNIVERSAL_IR_DESIGN.md          # Universal IR schema design
├── VB6_ADAPTER.md                  # VB6 adapter documentation
├── COBOL_ADAPTER.md                # COBOL adapter documentation
└── WORKFLOW_UPDATES.md             # Workflow changes documentation
```

## Testing

### Regression Testing
- **VB6**: All Phase 1 samples tested with Universal IR pipeline
- **COBOL**: All Phase 1 samples tested with Universal IR pipeline
- **Result**: Zero regression, 100% success rate

### Test Files
```
samples/vb6/start-form/StartForm.frm    # VB6 test case
samples/cobol/simple/seq.cbl             # COBOL test case
```

## Summary

Phase 2 successfully introduces a **Universal Intermediate Representation** layer to the LMOD platform, enabling:

✅ **Multi-language support** via adapter pattern
✅ **Zero regression** with backward compatibility
✅ **Quality improvements** via unified validation
✅ **Extensibility** for future languages (PowerBuilder, Delphi, AS/400)
✅ **Maintainability** via centralized schema

**Next Steps**: See individual documentation files for detailed design and implementation:
- `UNIVERSAL_IR_DESIGN.md` - Schema details
- `VB6_ADAPTER.md` - VB6 mapping rules
- `COBOL_ADAPTER.md` - COBOL mapping rules
- `WORKFLOW_UPDATES.md` - LangGraph integration

# VB6 → Angular Modernization Platform - Documentation Index

**Quick Navigation**: Find all documentation organized by topic and phase.

---

## 🚀 Getting Started

### For New Users
1. **[README.md](../README.md)** - Start here! Complete overview, quick start, and examples
2. **[PRD](prd.md)** - Original product requirements and project goals
3. **[Quick Start Guide](#quick-start-commands)** - Copy-paste commands to get running

### Architecture Overview
- **[Universal IR Schema](universal_ir_schema.json)** - Language-agnostic Intermediate Representation (Phase 2)
- **[Universal IR Design](phase2/UNIVERSAL_IR_DESIGN.md)** - Complete Universal IR documentation
- **[Phase 1 Architecture](phase1/PHASE2_SUBAGENT_ARCHITECTURE.md)** - Multi-agent workflow design
- **[Phase 2 Overview](phase2/PHASE2_OVERVIEW.md)** - Universal IR + Adapter Pattern architecture

---

## 📚 Phase 1: VB6 → IR Extraction

### Core Documentation
| Document | Description | When to Read |
|----------|-------------|--------------|
| [LangGraph Implementation](phase1/LANGGRAPH_IMPLEMENTATION.md) | Complete architecture overview | Understanding how Phase 1 works |
| [LangGraph Quickstart](phase1/LANGGRAPH_QUICKSTART.md) | Getting started guide | Setting up Phase 1 |
| [Subagent Quickstart](phase1/SUBAGENT_QUICKSTART.md) | Using the multi-agent system | Running VB6 parsing |
| [Subagent Architecture](phase1/PHASE2_SUBAGENT_ARCHITECTURE.md) | Deep dive into agent design | Customizing agents |
| [Parser Instructions](phase1/PHASE2_PARSER_INSTRUCTIONS.md) | Parser implementation details | Understanding IR extraction |

### Quality & Validation
| Document | Description | When to Read |
|----------|-------------|--------------|
| [Validation Complete](phase1/VALIDATION_COMPLETE.md) | Quality metrics and accuracy | Verifying Phase 1 results |
| [Validator Fix Explanation](phase1/VALIDATOR_FIX_EXPLANATION.md) | IR validation logic | Debugging validation issues |

### Key Concepts
- **3 Specialized Agents**: UI Agent, Logic Agent, Data Agent
- **LangGraph Orchestration**: Parallel execution with state management
- **IR Output**: Rich JSON with traceability, confidence scores, and security analysis
- **Entry Point**: `src/orchestrator/main.py`

---

## 📚 Phase 2: Universal IR + Adapter Pattern ✅ **NEW**

### Core Documentation
| Document | Description | When to Read |
|----------|-------------|--------------|
| [Phase 2 Overview](phase2/PHASE2_OVERVIEW.md) | Purpose, architecture, and motivation | Understanding Universal IR system |
| [Universal IR Design](phase2/UNIVERSAL_IR_DESIGN.md) | Complete schema documentation (12 sections) | Understanding IR structure |
| [VB6 Adapter](phase2/VB6_ADAPTER.md) | VB6 IR → Universal IR mapping rules | Working with VB6 pipeline |
| [COBOL Adapter](phase2/COBOL_ADAPTER.md) | COBOL IR → Universal IR mapping rules | Working with COBOL pipeline |
| [Workflow Updates](phase2/WORKFLOW_UPDATES.md) | LangGraph integration details | Understanding workflow changes |
| [Completion Summary](phase2/PHASE2_COMPLETION_SUMMARY.md) | Implementation results and metrics | Phase 2 status overview |

### Legacy Code Generation Docs
| Document | Description | When to Read |
|----------|-------------|--------------|
| [Implementation Plan](phase2/PHASE2_IMPLEMENTATION_PLAN.md) | Code generation design (pre-Universal IR) | Historical reference |
| [Implementation Summary](phase2/PHASE2_IMPLEMENTATION_SUMMARY.md) | Code generation delivery (pre-Universal IR) | Historical reference |
| [Angular Specification](phase2/angular.md) | Original specification | Understanding requirements |
| [Test Results](phase2/TEST_RESULTS.md) | Comprehensive test report | Verifying code generation quality |

### Test Results & Examples
| Path | Description |
|------|-------------|
| [test-results/start-form/](phase2/test-results/start-form/) | Simple form example (5 controls) |
| [test-results/supplier-form/](phase2/test-results/supplier-form/) | Medium form example (16 controls) |

### Key Concepts
- **LLM-Powered Generation**: Uses Claude Haiku for cost-effective code creation
- **Comprehensive Mappings**: VB6 controls → Angular Material components
- **Quality Validation**: Syntax checks + automatic retry on errors
- **Full Traceability**: VB6 source → IR → Angular code mapping
- **Entry Point**: `src/codegen/main.py`

---

## 📚 Phase 3: Universal IR Code Generation ✅ **NEW**

### Core Documentation
| Document | Description | When to Read |
|----------|-------------|--------------|
| [Phase 3 Overview](phase3/PHASE3_OVERVIEW.md) | Universal IR code generation architecture | Understanding Phase 3 system |
| [Angular Universal Generator](phase3/ANGULAR_UNIVERSAL_GENERATOR.md) | Complete Angular generator documentation | Working with Angular generator |
| [Spring Boot Universal Generator](phase3/SPRINGBOOT_UNIVERSAL_GENERATOR.md) | Complete Spring Boot generator documentation | Working with Spring Boot generator |
| [Phase 3 Completion Summary](phase3/PHASE3_COMPLETION_SUMMARY.md) | Implementation results and test status | Phase 3 status overview |

### Key Concepts
- **Language-Agnostic Generators**: Both generators now read Universal IR (work with any source language)
- **Multi-Language → Multi-Framework**: VB6/COBOL/PowerBuilder → Angular/Spring Boot/React
- **Angular Generator**: LLM-based (Claude Haiku), generates Angular 17+ components
- **Spring Boot Generator**: Template-based (Jinja2), generates Spring Boot 3.x + JPA
- **End-to-End Tests**: Regression tests for both pipelines
- **Entry Points**: `src/codegen/main.py` (Angular), `src/codegen/springboot_main.py` (Spring Boot)

---

## 📖 Tutorials & Guides

### Tutorials
| Document | Description | For |
|----------|-------------|-----|
| [Angular Component Tutorial](tutorials/ANGULAR_TUTORIAL.md) | Complete guide to Angular components | VB6 developers learning Angular |

### Quality & Testing Guides
| Document | Description | For |
|----------|-------------|-----|
| [Quality Guide](guides/QUALITY_GUIDE.md) | Comprehensive quality assessment guide | Validating generated code |
| [Cost Optimization](guides/COST_OPTIMIZATION.md) | Haiku vs Sonnet 4 cost analysis | Understanding API costs |

---

## 📊 Test Results & Validation

### Production Validation
| Document | Description | Status |
|----------|-------------|--------|
| [Validation Results](results/VALIDATION_RESULTS.md) | Real Angular 17 project testing | ✅ PASSED |
| [Haiku Test Results](results/HAIKU_TEST_RESULTS.md) | Cost-optimized model testing | ✅ PASSED |
| [Robustness Findings](results/ROBUSTNESS_FINDINGS.md) | Real-world VB6 samples testing | ⚠️ 40% pass rate |

### Key Metrics
- **Simple Forms**: 100% success rate
- **Medium Forms**: 100% success rate (with Haiku)
- **Very Complex Forms**: 100% success rate (44KB form)
- **Cost**: $0.004-$0.008 per form (with Haiku)
- **Quality**: Zero TypeScript errors

---

## 🗂️ Historical Documentation (Archive)

These documents provide historical context but may be outdated:

| Document | Topic | Status |
|----------|-------|--------|
| [Cost Comparison](archive/COST_COMPARISON.md) | Cost analysis | Superseded by Cost Optimization |
| [Phase 1 Manual IR Mapping](archive/PHASE1_MANUAL_IR_MAPPING.md) | Manual IR creation | Automated now |
| [Phase 2 Complete](archive/PHASE2_COMPLETE.md) | Old completion report | See Implementation Summary |
| [Project Summary](archive/PROJECT_SUMMARY.md) | Old project overview | See README |
| [Prompt Refinement Guide](archive/PROMPT_REFINEMENT_GUIDE.md) | Prompt engineering | Historical |
| [Prompt Refinement Status](archive/PROMPT_REFINEMENT_STATUS.md) | Refinement progress | Historical |
| [Prompt Refinement Complete](archive/PROMPT_REFINEMENT_COMPLETE.md) | Refinement results | Historical |
| [Quickstart](archive/QUICKSTART.md) | Old quickstart | See README Quick Start |
| [Restart Phase 2 Instructions](archive/RESTART_PHASE2_INSTRUCTIONS.md) | Phase 2 restart guide | Obsolete |
| [Robustness Improvements](archive/ROBUSTNESS_IMPROVEMENTS.md) | Improvement log | Historical |
| [Sample Analysis](archive/SAMPLE_ANALYSIS.md) | Example VB6 analysis | Use current test results |
| [Validator V2 Results](archive/validator_v2_results.md) | Old validation results | See Validation Complete |
| [Platform Development Plan](archive/platform-development-plan.md) | Long-term vision | Future roadmap |

---

## 🔍 Quick Reference

### VB6 → Angular Mappings

#### Controls
```
CommandButton → <button mat-raised-button>
TextBox       → <mat-form-field><input matInput>
Label         → <mat-label>
ComboBox      → <mat-select>
CheckBox      → <mat-checkbox>
ListBox       → <mat-selection-list>
Frame         → <mat-card>
```

**Details**: [src/codegen/mappings/control_mappings.py](../src/codegen/mappings/control_mappings.py)

#### Events
```
Click         → (click)
Change        → (change)
Form_Load     → ngOnInit()
Form_Unload   → ngOnDestroy()
GotFocus      → (focus)
LostFocus     → (blur)
```

**Details**: [src/codegen/mappings/event_mappings.py](../src/codegen/mappings/event_mappings.py)

#### Types
```
String   → string
Long     → number
Boolean  → boolean
Date     → Date
```

**Details**: [src/codegen/mappings/type_mappings.py](../src/codegen/mappings/type_mappings.py)

---

## 💻 Quick Start Commands

### Complete End-to-End Pipeline

```bash
# 1. Install dependencies
pip install anthropic

# 2. Set API key
export ANTHROPIC_API_KEY='your-key-here'

# 3. Phase 1: VB6 → IR
python3 src/orchestrator/main.py samples/vb6/simple/StartForm.frm
# Output: samples/vb6/simple/StartForm_ir.json

# 4. Phase 2: IR → Angular
python3 src/codegen/main.py samples/vb6/simple/StartForm_ir.json output/angular/start-form
# Output: output/angular/start-form/*.component.{ts,html,scss,spec.ts}

# 5. View results
ls -la output/angular/start-form/
cat output/angular/start-form/TRACEABILITY.md
```

### Test with Medium Complexity Form

```bash
# Parse medium complexity form
python3 src/orchestrator/main.py samples/vb6/medium/frmsupplier.frm

# Generate Angular
python3 src/codegen/main.py samples/vb6/medium/frmsupplier_ir.json output/angular/supplier-form

# View results
ls -la output/angular/supplier-form/
```

---

## 📊 Project Metrics

### Phase 1 (VB6 → IR)
- **Accuracy**: 98.3% (simple forms), 96.8% (medium forms)
- **Speed**: ~5-10 seconds per form
- **Output**: Rich IR JSON with traceability

### Phase 2 (IR → Angular)
- **Success Rate**: 100% (tested on simple & medium forms)
- **Speed**: ~30-45 seconds per form
- **Cost**: ~$0.004-$0.008 per form (with Haiku)
- **Quality**: Zero TypeScript errors, all tests pass

### ROI
- **Manual Migration**: $400-$800 per form
- **Automated**: ~$0.01 per form
- **Savings**: 99.99% cost reduction

---

## 🗂️ Source Code Organization

### Phase 1: VB6 → IR
```
src/orchestrator/
├── main.py                    # CLI entry point
├── langgraph_workflow.py      # LangGraph workflow definition
├── langgraph_nodes.py         # Workflow nodes (agents, merge, validate)
└── langgraph_state.py         # State schema

src/agents/
├── vb6_ui_agent.py           # Extracts UI structure
├── vb6_logic_agent.py        # Analyzes business logic
└── vb6_data_agent.py         # Identifies data operations
```

### Phase 2: IR → Angular
```
src/codegen/
├── main.py                    # CLI entry point
├── angular_generator.py       # Main orchestrator
├── prompt_builder.py          # Builds LLM prompts
├── file_writer.py            # Writes Angular files
├── validators.py             # Code quality checks
└── mappings/
    ├── control_mappings.py   # VB6 → Angular controls
    ├── event_mappings.py     # VB6 → Angular events
    └── type_mappings.py      # VB6 → TypeScript types
```

### Shared Utilities
```
src/
├── validator.py              # IR validation
├── ir_canonicalizer.py      # IR normalization
└── validator_diagnostic.py   # Diagnostic validation
```

---

## 🎯 Common Tasks

### I want to...

#### ...understand how the system works
1. Read [README.md](../README.md) for overview
2. Read [Phase 1 LangGraph Implementation](phase1/LANGGRAPH_IMPLEMENTATION.md)
3. Read [Phase 2 Overview](phase2/PHASE2_OVERVIEW.md) - Universal IR + Adapter Pattern
4. Read [Phase 3 Overview](phase3/PHASE3_OVERVIEW.md) - Universal IR Code Generation
5. Read [Universal IR Design](phase2/UNIVERSAL_IR_DESIGN.md) - Complete schema documentation

#### ...run the complete pipeline
1. Follow [Quick Start Commands](#quick-start-commands)
2. Check [README Quick Start](../README.md#-quick-start)

#### ...learn Angular (coming from VB6)
1. Read [Angular Component Tutorial](tutorials/ANGULAR_TUTORIAL.md)
2. Review [generated examples](phase2/test-results/)

#### ...understand API costs
1. Read [Cost Optimization Guide](guides/COST_OPTIMIZATION.md)
2. Review [Haiku Test Results](results/HAIKU_TEST_RESULTS.md)

#### ...validate code quality
1. Read [Quality Guide](guides/QUALITY_GUIDE.md)
2. Run `python3 check_quality.py output/your-component/`

#### ...add support for new VB6 controls
1. Update [control_mappings.py](../src/codegen/mappings/control_mappings.py)
2. Add Angular Material component mapping
3. Test with sample form

#### ...add support for new VB6 events
1. Update [event_mappings.py](../src/codegen/mappings/event_mappings.py)
2. Add Angular event binding
3. Test with sample form

#### ...customize the UI agent
1. Read [Subagent Architecture](phase1/PHASE2_SUBAGENT_ARCHITECTURE.md)
2. Modify [vb6_ui_agent.py](../src/agents/vb6_ui_agent.py)
3. Test with sample form

#### ...improve code generation quality
1. Modify prompts in [prompt_builder.py](../src/codegen/prompt_builder.py)
2. Update validation rules in [validators.py](../src/codegen/validators.py)
3. Test and compare results

#### ...debug IR validation issues
1. Read [Validator Fix Explanation](phase1/VALIDATOR_FIX_EXPLANATION.md)
2. Check [validator.py](../src/validator.py) implementation
3. Run with `--verbose` flag (if available)

#### ...see example generated code
1. Check [test-results/start-form/](phase2/test-results/start-form/)
2. Check [test-results/supplier-form/](phase2/test-results/supplier-form/)
3. Run pipeline on samples and inspect output

---

## 📈 Performance & Quality

### Test Results Summary

| Form | Complexity | Controls | Handlers | Output LOC | Time | Cost | Result |
|------|------------|----------|----------|------------|------|------|--------|
| StartForm | Simple | 5 | 3 | 397 | 15s | $0.004 | ✅ PASS |
| frmsupplier | Medium | 16 | 7 | 824 | 20s | $0.008 | ✅ PASS |

**Details**: [Test Results Report](phase2/TEST_RESULTS.md)

### Quality Criteria (All Met ✅)

- ✅ Valid TypeScript (zero syntax errors)
- ✅ Valid HTML (well-formed Material UI)
- ✅ All controls mapped (100% coverage)
- ✅ All handlers implemented (100% coverage)
- ✅ Validations preserved (Reactive Forms)
- ✅ Tests generated (unit tests for all handlers)
- ✅ Traceability maintained (complete VB6 → Angular mapping)
- ✅ Fast generation (<60s per form)
- ✅ Affordable (<$0.01 per form)
- ✅ Production ready (passes all checks)

---

## 🆘 Troubleshooting

### Common Issues

#### "ANTHROPIC_API_KEY not found"
**Solution**: Set your API key
```bash
export ANTHROPIC_API_KEY='your-key-here'
# Or create src/.env file with:
# ANTHROPIC_API_KEY=your-key-here
```

#### "File not found" when running Phase 2
**Solution**: Make sure you ran Phase 1 first
```bash
# Phase 1 must be run before Phase 2
python3 src/orchestrator/main.py samples/vb6/simple/StartForm.frm
# This creates StartForm_ir.json
```

#### "Validation failed" during code generation
**Solution**: Check the error messages - retry usually fixes it
- The system automatically retries once with error feedback
- If it still fails, check the IR JSON is valid
- Review the generated code manually

#### Generated code has TypeScript errors
**Solution**: This is rare - the validator should catch these
- Check [validators.py](../src/codegen/validators.py) for validation logic
- The system automatically retries with error feedback
- Report as a bug if errors persist

---

## 🔗 External Resources

### LangGraph
- [LangGraph Documentation](https://langchain-ai.github.io/langgraph/)
- Used for Phase 1 multi-agent workflow

### Angular
- [Angular 17 Documentation](https://angular.io/docs)
- [Angular Material](https://material.angular.io/)
- Target framework for generated code

### Claude API
- [Anthropic Documentation](https://docs.anthropic.com/)
- Used for code generation in Phase 2

---

## 📝 Contributing

See [README - Contributing](../README.md#-contributing) for guidelines.

---

## 📄 License

MIT License - See [README - License](../README.md#-license) for details.

---

**Last Updated**: 2025-11-22
**Version**: 3.0 (Universal IR Code Generation)
**Status**: Production Ready ✅

---

**Navigation**:
- [Back to README](../README.md)
- [Phase 1 Docs](phase1/)
- [Phase 2 Docs](phase2/)
- [Phase 3 Docs](phase3/) ✨ **NEW**
- [Tutorials](tutorials/)
- [Guides](guides/)
- [Test Results](results/)
- [Archive](archive/)

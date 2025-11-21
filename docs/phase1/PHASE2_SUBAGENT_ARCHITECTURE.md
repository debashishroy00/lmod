# Phase 2 Subagent Architecture - The Right Way

## What-Why-How: Building for Scale

---

## WHAT We're Building

### Production Subagent System

**NOT:** Monolithic parser for small files  
**YES:** Distributed subagent system for enterprise scale

**Components:**
1. **Orchestrator** - Main controller, launches agents, merges IRs
2. **3 VB6 Subagents** - UI, Logic, Data (specialized)
3. **IR Merger** - Combines partial IRs, resolves conflicts
4. **Validator** - Ensures completeness, quality checks

**Scale targets:**
- Handle 100 LOC to 100,000 LOC files
- Process 650 apps (parallel batches)
- Support 2000+ legacy technologies
- Achieve 95%+ accuracy at scale

---

## WHY This Architecture

### Strategic Requirements (AIG Scale)

**650+ Applications:**
- Can't process sequentially (21+ hours)
- Need parallel processing (2 hours with batching)
- Must handle variety (simple to complex)

**Millions of Lines:**
- Single file may be 50,000+ lines
- Won't fit in one Claude call with prompts
- Need intelligent chunking/splitting

**2000+ Technologies:**
- VB6, COBOL, PowerBuilder, Delphi, etc.
- Each needs specialized agents
- Subagent pattern works for ALL

**Production Quality:**
- 95%+ accuracy required
- Specialized agents achieve this
- Monolithic can't compete

---

## HOW to Build It

### Architecture Layers

```
Layer 1: Orchestrator (Python)
├── Load source file
├── Determine file size / complexity
├── Launch appropriate subagents
├── Manage context windows
├── Merge partial IRs
└── Validate output

Layer 2: Subagent Runtime (Claude API)
├── vb6-ui-agent.yml (specialized prompt)
├── vb6-logic-agent.yml (specialized prompt)
├── vb6-data-agent.yml (specialized prompt)
└── API calls with proper prompts

Layer 3: IR Processing
├── Partial IR validation
├── IR merger (conflict resolution)
├── Schema validation
└── Confidence scoring

Layer 4: Validation & Metrics
├── Compare to golden fixture (if exists)
├── Calculate accuracy per section
├── Measure timing
└── Generate metrics report
```

---

## Implementation Plan

### Step 1: Orchestrator Core (Day 1)

**WHAT to build:**
```python
# src/orchestrator.py

class VB6Orchestrator:
    """
    WHAT: Main controller for subagent system
    WHY: Manages parallel agents, merges results
    HOW: Async API calls, IR merging, validation
    """
    
    def __init__(self, api_key: str):
        self.client = anthropic.Anthropic(api_key=api_key)
        self.agents = {
            'ui': VB6UIAgent(self.client),
            'logic': VB6LogicAgent(self.client),
            'data': VB6DataAgent(self.client)
        }
    
    async def parse(self, frm_content: str) -> IR:
        """
        WHAT: Parse VB6 form using 3 subagents
        WHY: Parallel processing, specialized extraction
        HOW: Launch agents async, merge partial IRs
        """
        # Launch all agents in parallel
        ui_task = self.agents['ui'].extract(frm_content)
        logic_task = self.agents['logic'].extract(frm_content)
        data_task = self.agents['data'].extract(frm_content)
        
        # Wait for all to complete (2 min total)
        ui_ir, logic_ir, data_ir = await asyncio.gather(
            ui_task, logic_task, data_task
        )
        
        # Merge partial IRs
        complete_ir = self.merge_irs({
            'ui': ui_ir,
            'logic': logic_ir,
            'data': data_ir
        })
        
        # Validate completeness
        self.validate(complete_ir)
        
        return complete_ir
```

**WHY this design:**
- Async = parallel execution (fast)
- Specialized agents = higher accuracy
- Modular = easy to extend

**HOW to test:**
```bash
python3 src/orchestrator.py samples/vb6/simple/StartForm.frm
# Should take ~2 minutes (3 API calls)
# Should output complete IR
# Should achieve 95%+ accuracy
```

---

### Step 2: VB6 UI Agent (Day 1)

**WHAT to build:**
```python
# src/agents/vb6_ui_agent.py

class VB6UIAgent:
    """
    WHAT: Extract ONLY UI structure from VB6 forms
    WHY: Specialized = better accuracy on UI elements
    HOW: Claude API with focused prompt
    """
    
    PROMPT = """
    You are a VB6 UI extraction specialist.
    
    WHAT you extract:
    - Form properties (name, caption, size, border)
    - All controls (TextBox, CommandButton, Label, etc.)
    - Control properties (position, size, TabIndex, etc.)
    - Layout information
    
    WHAT you DO NOT extract:
    - Event handler code (Logic Agent's job)
    - Database operations (Data Agent's job)
    - Business rules (Logic Agent's job)
    
    WHY this separation:
    - You focus ONLY on UI structure
    - Other agents handle other concerns
    - Specialization = higher accuracy
    
    HOW to extract:
    1. Find Begin VB.Form ... End
    2. Extract form properties
    3. Find each Begin VB.ControlType ... End
    4. Extract control properties
    5. Sort controls by TabIndex
    6. Output partial IR (ui section only)
    
    OUTPUT FORMAT:
    {
      "ui": {
        "form": {
          "name": "...",
          "caption": "...",
          ...
        },
        "controls": [
          {
            "id": "...",
            "type": "TextBox",
            "tab_index": 0,
            ...
          }
        ],
        "confidence": 0.95
      }
    }
    
    CRITICAL: Output ONLY the ui section.
    DO NOT include metadata, logic, data sections.
    """
    
    async def extract(self, frm_content: str) -> Dict:
        """
        WHAT: Call Claude API with UI-focused prompt
        WHY: Get specialized UI extraction
        HOW: Anthropic SDK with streaming
        """
        message = await self.client.messages.create(
            model="claude-sonnet-4-20250514",
            max_tokens=4000,
            messages=[{
                "role": "user",
                "content": f"{self.PROMPT}\n\nVB6 Form:\n{frm_content}"
            }]
        )
        
        # Parse response (should be JSON with ui section)
        response_text = message.content[0].text
        ui_ir = json.loads(response_text)
        
        return ui_ir
```

**WHY this prompt design:**
- Clear scope (UI only)
- Explicit what NOT to do (avoid overlap)
- Examples embedded (better results)
- Output format specified (JSON schema)

---

### Step 3: VB6 Logic Agent (Day 2)

**WHAT to build:**
```python
# src/agents/vb6_logic_agent.py

class VB6LogicAgent:
    """
    WHAT: Extract ONLY business logic from VB6 forms
    WHY: Specialized = better understanding of intent
    HOW: Claude API with logic-focused prompt
    """
    
    PROMPT = """
    You are a VB6 business logic extraction specialist.
    
    WHAT you extract:
    - Event handlers (Click, Load, etc.)
    - Validation rules (required fields, format checks)
    - Calculations (business rules, formulas)
    - Workflows (multi-step processes)
    - External function calls
    
    WHAT you DO NOT extract:
    - UI structure (UI Agent's job)
    - Database entities (Data Agent's job)
    - Form properties (UI Agent's job)
    
    WHY this separation:
    - You focus ONLY on logic and behavior
    - You understand INTENT not just syntax
    - Specialization = higher accuracy
    
    HOW to extract:
    1. Find all Private Sub / Public Sub
    2. Analyze event handler logic
    3. Identify validation patterns (Len, Trim, IsNumeric)
    4. Extract calculations (COMPUTE, formulas)
    5. Map workflows (Button1 → Action → Button2)
    6. Flag external dependencies
    7. Output partial IR (logic section only)
    
    OUTPUT FORMAT:
    {
      "logic": {
        "event_handlers": [
          {
            "control": "cmdSave",
            "event": "Click",
            "steps": [...],
            ...
          }
        ],
        "validations": [...],
        "calculations": [...],
        "workflows": [...],
        "confidence": 0.92
      }
    }
    
    CRITICAL: 
    - Understand INTENT (not just syntax)
    - Flag implicit validations
    - Identify patterns (CRUD, Search, etc.)
    - Output ONLY logic section
    """
    
    async def extract(self, frm_content: str) -> Dict:
        # Similar to UI Agent but logic-focused
        ...
```

**WHY logic needs specialization:**
- Understanding intent requires reasoning
- Validation patterns vary widely
- Business rules context-dependent
- Generic parser misses nuance

---

### Step 4: VB6 Data Agent (Day 2)

**WHAT to build:**
```python
# src/agents/vb6_data_agent.py

class VB6DataAgent:
    """
    WHAT: Extract ONLY data layer from VB6 forms
    WHY: Specialized = better entity/relationship extraction
    HOW: Claude API with data-focused prompt
    """
    
    PROMPT = """
    You are a VB6 data layer extraction specialist.
    
    WHAT you extract:
    - Business entities (Customer, Invoice, etc.)
    - CRUD operations (Create, Read, Update, Delete)
    - Database queries (SQL SELECT, INSERT, UPDATE)
    - Data relationships (one-to-many, etc.)
    - External data classes
    
    WHAT you DO NOT extract:
    - UI controls (UI Agent's job)
    - Event handlers (Logic Agent's job)
    - Validations (Logic Agent's job)
    
    WHY this separation:
    - You focus ONLY on data and persistence
    - You identify entities not UI forms
    - Specialization = higher accuracy
    
    HOW to extract:
    1. Find ADO/DAO RecordSet operations
    2. Extract SQL queries (literal and dynamic)
    3. Identify business entities (exclude UI forms)
    4. Map CRUD operations to entities
    5. Flag external data classes
    6. Output partial IR (data section only)
    
    OUTPUT FORMAT:
    {
      "data": {
        "entities": [
          {
            "name": "Customer",
            "type": "business_object",
            ...
          }
        ],
        "operations": [
          {
            "type": "READ",
            "entity": "Customer",
            ...
          }
        ],
        "confidence": 0.88
      }
    }
    
    CRITICAL:
    - Exclude UI forms (ClientEdit, CustomerForm)
    - Focus on business entities
    - Extract actual SQL when present
    - Output ONLY data section
    """
    
    async def extract(self, frm_content: str) -> Dict:
        # Similar to UI/Logic but data-focused
        ...
```

**WHY data needs specialization:**
- Entity vs Form distinction critical
- SQL extraction requires care
- Relationships need inference
- CRUD mapping not obvious

---

### Step 5: IR Merger (Day 3)

**WHAT to build:**
```python
# src/merger.py

class IRMerger:
    """
    WHAT: Combine partial IRs into complete IR
    WHY: Each agent outputs one section, need unified IR
    HOW: Merge algorithm with conflict resolution
    """
    
    def merge(self, partial_irs: Dict[str, Dict]) -> IR:
        """
        WHAT: Combine ui, logic, data sections
        WHY: Create complete IR from partials
        HOW: Merge algorithm with validation
        """
        complete_ir = {
            "metadata": self._build_metadata(partial_irs),
            "ui": partial_irs.get('ui', {}),
            "logic": partial_irs.get('logic', {}),
            "data": partial_irs.get('data', {}),
            "patterns": self._detect_patterns(partial_irs),
            "external_references": self._extract_external_refs(partial_irs),
            "security_issues": self._detect_security(partial_irs),
            "generation_metadata": self._calculate_metrics(partial_irs)
        }
        
        # Resolve conflicts (if any)
        complete_ir = self._resolve_conflicts(complete_ir)
        
        # Validate schema
        self._validate_schema(complete_ir)
        
        return complete_ir
    
    def _resolve_conflicts(self, ir: IR) -> IR:
        """
        WHAT: Handle overlapping extractions
        WHY: Agents may extract same thing differently
        HOW: Priority rules, confidence scores
        
        Example conflict:
        - UI Agent says: confidence = 0.95
        - Logic Agent says: confidence = 0.92
        Resolution: Use UI Agent (higher confidence)
        """
        ...
```

**WHY merger is critical:**
- Partial IRs → Complete IR
- Handles overlaps gracefully
- Validates completeness
- Ensures schema compliance

---

### Step 6: Validation & Metrics (Day 3)

**WHAT to build:**
```python
# src/validator.py

class SubagentValidator:
    """
    WHAT: Validate subagent system output
    WHY: Ensure accuracy, measure performance
    HOW: Golden fixture comparison, metrics
    """
    
    def validate(self, actual_ir: IR, expected_ir: IR) -> Report:
        """
        WHAT: Compare subagent output to golden fixture
        WHY: Measure accuracy per agent
        HOW: Section-by-section similarity
        """
        results = {
            'ui': self._compare_sections(
                actual_ir['ui'], 
                expected_ir['ui']
            ),
            'logic': self._compare_sections(
                actual_ir['logic'],
                expected_ir['logic']
            ),
            'data': self._compare_sections(
                actual_ir['data'],
                expected_ir['data']
            )
        }
        
        # Per-agent metrics
        agent_metrics = {
            'ui_agent': {
                'accuracy': results['ui']['similarity'],
                'time': results['ui']['duration'],
                'confidence': actual_ir['ui']['confidence']
            },
            'logic_agent': {
                'accuracy': results['logic']['similarity'],
                'time': results['logic']['duration'],
                'confidence': actual_ir['logic']['confidence']
            },
            'data_agent': {
                'accuracy': results['data']['similarity'],
                'time': results['data']['duration'],
                'confidence': actual_ir['data']['confidence']
            }
        }
        
        return Report(
            overall_accuracy=self._calculate_overall(results),
            agent_metrics=agent_metrics,
            recommendations=self._generate_recommendations(results)
        )
```

**WHY per-agent metrics:**
- Identify which agent needs improvement
- Optimize independently
- Track progress over time

---

## Testing Strategy

### Phase 2A: StartForm.frm (Simple, 99 lines)

**WHAT to test:**
```bash
# Run subagent system
python3 src/orchestrator.py samples/vb6/simple/StartForm.frm

# Expected:
# - 3 API calls (parallel)
# - ~2 minutes total
# - Complete IR output
# - 95%+ accuracy per agent
```

**WHY start simple:**
- Proves subagent pattern works
- Validates IR merger
- Baseline for complex files

---

### Phase 2B: frmsupplier.frm (Medium, 296 lines)

**WHAT to test:**
```bash
# Run on larger file
python3 src/orchestrator.py samples/vb6/medium/frmsupplier.frm

# Expected:
# - Same 3 API calls
# - ~2-3 minutes (more content)
# - Complete IR output
# - 90%+ accuracy (acceptable drop)
```

**WHY test medium:**
- More realistic file size
- Tests agent specialization
- Validates merger with complexity

---

### Phase 2C: Large File Simulation (10,000+ lines)

**WHAT to test:**
```bash
# Create large test file (concatenate multiple forms)
cat samples/vb6/**/*.frm > large_test.frm

# Run subagent system
python3 src/orchestrator.py large_test.frm

# Expected:
# - Automatic chunking if needed
# - Same 3 API calls (may process in chunks)
# - ~3-4 minutes
# - 85%+ accuracy (acceptable for large)
```

**WHY test large:**
- Proves context window management
- Validates chunking strategy
- Prepares for real enterprise code

---

## Success Criteria

### Functional Requirements

✅ **Parse any VB6 form** (100 to 100,000 lines)  
✅ **3 specialized agents** (UI, Logic, Data)  
✅ **Parallel execution** (~2 min for small files)  
✅ **Complete IR output** (all sections populated)  
✅ **Schema compliant** (passes validation)

### Performance Requirements

✅ **Accuracy:** 95%+ per agent (simple files)  
✅ **Accuracy:** 90%+ per agent (medium files)  
✅ **Accuracy:** 85%+ per agent (large files)  
✅ **Speed:** ~2 min (simple), ~4 min (large)  
✅ **Parallel:** 10 apps simultaneously

### Scale Requirements

✅ **Context:** Handle 100,000-line files (chunking)  
✅ **Volume:** Process 650 apps in 2 hours (batching)  
✅ **Technologies:** Extensible to COBOL, PowerBuilder  
✅ **Quality:** Maintain 90%+ accuracy at scale

---

## Implementation Timeline

### Week 1: Core System

**Day 1:**
- Orchestrator core
- VB6 UI Agent
- Test on StartForm.frm

**Day 2:**
- VB6 Logic Agent
- VB6 Data Agent
- Test on StartForm.frm (all agents)

**Day 3:**
- IR Merger
- Validator
- Test on frmsupplier.frm

### Week 2: Scale & Polish

**Day 4:**
- Context window management (chunking)
- Parallel batch processing
- Test on large files

**Day 5:**
- Error handling
- Retry logic
- Documentation

---

## Cost Analysis

### API Costs (Claude Sonnet 4)

**Per file:**
```
3 agents × $15 / 1M input tokens
Average file: 500 lines ≈ 2,000 tokens

Cost per file:
- UI Agent: 2K tokens × $15/1M = $0.03
- Logic Agent: 2K tokens × $15/1M = $0.03  
- Data Agent: 2K tokens × $15/1M = $0.03
Total: ~$0.09 per file
```

**650 files:**
```
650 × $0.09 = $58.50 total
```

**WHY this is acceptable:**
- One-time cost per file
- $58 vs $262K savings = negligible
- Enables 650-app modernization

---

## Deployment

### Requirements

```
- Python 3.9+
- anthropic SDK (pip install anthropic)
- asyncio (stdlib)
- API key (environment variable)
```

### Usage

```bash
# Single file
python3 src/orchestrator.py path/to/Form.frm

# Batch processing
python3 src/batch_processor.py path/to/vb6_forms/

# With golden fixture validation
python3 src/orchestrator.py --validate path/to/Form.frm
```

---

## Key Takeaways

**WHAT we're building:**
- Enterprise-scale subagent system
- Not a prototype, a production platform

**WHY subagents:**
- Handles 100 to 100,000 lines
- Parallel = 10x faster at scale
- Specialized = 95%+ accuracy
- Extensible = all 2000 technologies

**HOW it works:**
- 3 agents (UI, Logic, Data)
- Parallel API calls (2 min)
- IR merger (conflict resolution)
- Validation (golden fixture)

**Bottom line:**
- Built for 650 apps, millions of LOC
- NOT built for 97 simple forms
- Scales to AIG's real requirements
- Production-ready architecture

---

**This is the right way.** 🎯

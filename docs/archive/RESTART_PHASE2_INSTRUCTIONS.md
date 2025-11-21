# Restart Phase 2 - Subagent Architecture

## WHAT We're Doing

**Restarting Phase 2 with correct architecture:**
- ❌ Discard monolithic vb6_parser.py (wrong approach)
- ✅ Build subagent orchestrator + 3 specialized agents
- ✅ Prove scalability (100 to 100,000 LOC)
- ✅ Validate for 650-app factory

---

## WHY We're Restarting

**Strategic imperative:**
- AIG has 650+ apps with millions of LOC
- Single files may be 10,000-50,000 lines
- Monolithic parser won't handle this
- Subagents = production-ready foundation

**Technical necessity:**
- Context window limits (large files)
- Parallel processing (speed at scale)
- Specialization (accuracy at scale)
- Extensibility (2000 technologies)

---

## HOW to Execute (Step-by-Step)

### Give Claude Code These Instructions:

```markdown
**TO CLAUDE CODE:**

We need to restart Phase 2 with the correct architecture.

WHAT went wrong:
- You built a monolithic parser (vb6_parser.py)
- This works for small files but doesn't scale
- AIG has 650+ apps with millions of LOC

WHAT we need instead:
- Subagent orchestrator system
- 3 specialized agents (UI, Logic, Data)
- Parallel API calls to Claude
- IR merger to combine results

WHY subagents:
- Handle any file size (100 to 100,000 lines)
- Process 650 apps in parallel (2 hours vs 21 hours)
- Specialize = 95%+ accuracy per section
- Extend to all 2000 legacy technologies

HOW to build:

**Step 1: Setup (30 min)**
```bash
# Install Anthropic SDK
pip install anthropic

# Set API key
export ANTHROPIC_API_KEY="your-key-here"

# Create structure
mkdir -p src/agents
mkdir -p src/orchestrator
```

**Step 2: Orchestrator Core (2 hours)**
File: `src/orchestrator/vb6_orchestrator.py`

Build async orchestrator that:
1. Takes VB6 form content as input
2. Launches 3 agents in parallel (asyncio.gather)
3. Waits for all to complete (~2 min total)
4. Merges partial IRs into complete IR
5. Validates against IR schema
6. Returns complete IR JSON

Reference: PHASE2_SUBAGENT_ARCHITECTURE.md (Step 1)

**Step 3: VB6 UI Agent (1.5 hours)**
File: `src/agents/vb6_ui_agent.py`

Build specialized UI extraction agent:
- Prompt: Extract ONLY UI (form + controls)
- API call: Claude Sonnet 4
- Output: Partial IR (ui section only)
- Confidence: Self-scored

Reference: PHASE2_SUBAGENT_ARCHITECTURE.md (Step 2)

**Step 4: VB6 Logic Agent (1.5 hours)**
File: `src/agents/vb6_logic_agent.py`

Build specialized logic extraction agent:
- Prompt: Extract ONLY logic (events + validations)
- API call: Claude Sonnet 4
- Output: Partial IR (logic section only)
- Confidence: Self-scored

Reference: PHASE2_SUBAGENT_ARCHITECTURE.md (Step 3)

**Step 5: VB6 Data Agent (1.5 hours)**
File: `src/agents/vb6_data_agent.py`

Build specialized data extraction agent:
- Prompt: Extract ONLY data (entities + operations)
- API call: Claude Sonnet 4
- Output: Partial IR (data section only)
- Confidence: Self-scored

Reference: PHASE2_SUBAGENT_ARCHITECTURE.md (Step 4)

**Step 6: IR Merger (1.5 hours)**
File: `src/orchestrator/ir_merger.py`

Build IR merger:
- Combine partial IRs (ui + logic + data)
- Resolve conflicts (use higher confidence)
- Detect patterns across sections
- Calculate generation metadata
- Validate schema compliance

Reference: PHASE2_SUBAGENT_ARCHITECTURE.md (Step 5)

**Step 7: Validator (1 hour)**
File: `src/validator.py`

Update validator for subagent system:
- Compare per-agent accuracy
- Measure timing per agent
- Track parallel speedup
- Generate per-agent metrics

Reference: PHASE2_SUBAGENT_ARCHITECTURE.md (Step 6)

**Step 8: Testing (2 hours)**

Test suite:
```bash
# Test 1: Simple file (StartForm.frm - 99 lines)
python3 src/orchestrator/main.py samples/vb6/simple/StartForm.frm

# Expected: ~2 min, 95%+ accuracy

# Test 2: Medium file (frmsupplier.frm - 296 lines)
python3 src/orchestrator/main.py samples/vb6/medium/frmsupplier.frm

# Expected: ~2-3 min, 90%+ accuracy

# Test 3: Validate against golden fixture
python3 src/validator.py

# Expected: >= 90% overall, >= 85% per agent
```

**Success Criteria:**
✅ All 3 agents working in parallel
✅ Complete IR output (all sections)
✅ >= 90% accuracy vs golden fixture
✅ ~2 min execution time
✅ Scales to any file size

**Timeline: 12 hours total (2 days)**

Follow What-Why-How model in all code and documentation.
```

---

## Cost Estimate

**Per file:**
- 3 API calls × ~2,000 tokens each = 6,000 tokens
- Claude Sonnet 4: $15 / 1M input tokens
- Cost: $0.09 per file

**For 650 files:**
- 650 × $0.09 = $58.50 total

**For testing Phase 2:**
- 2 files × $0.09 = $0.18

**Very affordable for enterprise modernization!**

---

## What You'll Have After Phase 2

**Architecture:**
```
lmod/
├── src/
│   ├── orchestrator/
│   │   ├── vb6_orchestrator.py   # Main controller
│   │   ├── ir_merger.py           # Combines partial IRs
│   │   └── main.py                # CLI entry point
│   ├── agents/
│   │   ├── vb6_ui_agent.py        # UI specialist
│   │   ├── vb6_logic_agent.py     # Logic specialist
│   │   └── vb6_data_agent.py      # Data specialist
│   └── validator.py                # Golden fixture validator
├── samples/
│   └── vb6/
│       ├── simple/StartForm.frm
│       └── medium/frmsupplier.frm
├── expected-ir/
│   └── StartForm.json              # Golden fixture
└── README.md
```

**Capabilities:**
✅ Parse any VB6 form (100 to 100,000 lines)
✅ 3 specialized agents (UI, Logic, Data)
✅ Parallel execution (~2 min)
✅ >= 90% accuracy
✅ Extensible to COBOL, PowerBuilder
✅ Ready for 650-app factory

---

## Next Steps After Phase 2

**Week 1:** Build subagent system (this work)
**Week 2:** Test on customer code (validate scale)
**Week 3:** Build COBOL agents (dual-track)
**Week 4:** Build Angular generator (end-to-end)
**Week 5:** Demo to AIG (working factory)

---

## Key Messages

**To Team:**
"We're restarting Phase 2 with the correct architecture. Subagents scale to 650 apps with millions of LOC. The monolithic approach was a prototype - now we're building production."

**To AIG:**
"Our platform uses specialized AI agents that work in parallel. This handles your largest applications (50,000+ lines) and scales to 650 apps in 2 hours instead of days."

**To Claude Code:**
"Build production-ready subagent orchestrator. This is not a prototype - it's the foundation for 650-app modernization factory."

---

## Bottom Line

**WHAT:** Rebuild Phase 2 with subagent architecture  
**WHY:** Scales to 650 apps, millions of LOC  
**HOW:** Orchestrator + 3 agents + IR merger  
**WHEN:** Start immediately (2 days to complete)  
**COST:** $0.18 for testing, $58 for 650 apps  

**This is the right foundation for AIG's scale.** 🎯

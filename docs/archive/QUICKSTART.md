# VB6 Parser - Quick Start

## Prerequisites

```bash
python3 --version  # Must be 3.9+
```

## Parse a VB6 Form (One Command)

```bash
python3 src/vb6_parser.py samples/vb6/simple/StartForm.frm
```

**Output**:
```
🔍 VB6 Parser v1.0.0

📄 Input: samples/vb6/simple/StartForm.frm
📊 Lines: 99

⚙️  Parsing...
✅ Parsing complete in 0ms

📄 Output: samples/vb6/simple/StartForm_ir.json

📊 Results:
   Form: StartForm
   Controls: 5
   Event Handlers: 3
   Patterns Detected: 3
   Confidence: 87.2%
   Complexity: simple

🎯 Patterns:
   - Search Form (88%)
   - Modal Dialog (95%)
   - Validation Pattern (92%)

⚠️  Security Issues: 2
   - [medium] On Error Resume Next suppresses all errors, causing silent failures
   - [low] Type conversion without validation - could fail on invalid input

✨ Done!
```

## Validate Parser Accuracy

```bash
python3 src/validate.py
```

**Expected Output**:
```
🔍 VB6 Parser Validator

📄 Actual:   samples/vb6/simple/StartForm_ir.json
📄 Expected: expected-ir/StartForm.json

📊 Section Similarity:

✅ metadata                  100.0% (5/5 matches)
✅ ui                        95.5% (21/22 matches)
✅ logic                     93.3% (14/15 matches)
✅ data                      100.0% (3/3 matches)
✅ patterns                  100.0% (4/4 matches)
✅ external_references       100.0% (3/3 matches)
✅ security_issues           100.0% (3/3 matches)
✅ generation_metadata       100.0% (3/3 matches)

============================================================
Overall Similarity: 96.6% (56/58 matches)
============================================================

✅ SUCCESS: Parser meets >= 90% similarity threshold!
```

## Parse Your Own VB6 Form

```bash
# Place your .frm file anywhere
python3 src/vb6_parser.py /path/to/your/Form.frm

# Output will be: /path/to/your/Form_ir.json
```

## What Gets Parsed?

✅ **Form Properties**: Name, caption, dimensions, border style
✅ **Controls**: TextBox, CommandButton, Label, etc. (sorted by TabIndex)
✅ **Event Handlers**: Click events with full logic analysis
✅ **Validations**: Len(), Trim(), IsNumeric checks
✅ **Patterns**: CRUD, Search, Modal Dialog, Validation
✅ **Security Issues**: On Error Resume Next, unsafe type conversions
✅ **External Refs**: Classes, modules, functions referenced
✅ **Data Entities**: Business objects (excluding UI forms)

## Common Use Cases

### 1. Analyze a Form

```bash
python3 src/vb6_parser.py MyForm.frm
cat MyForm_ir.json | python3 -m json.tool | less
```

### 2. Check Pattern Detection

```bash
python3 src/vb6_parser.py MyForm.frm | grep "Patterns:"
```

### 3. Find Security Issues

```bash
python3 src/vb6_parser.py MyForm.frm | grep -A 10 "Security Issues"
```

### 4. Extract Control Count

```bash
python3 src/vb6_parser.py MyForm.frm | grep "Controls:"
```

## Understanding the IR JSON

The output JSON has this structure:

```json
{
  "metadata": {...},           // Source info, confidence, complexity
  "ui": {                      // User interface
    "form": {...},             // Form properties
    "controls": [...]          // All controls (sorted by TabIndex)
  },
  "logic": {                   // Business logic
    "event_handlers": [...],   // Click, Load, etc.
    "validations": [...],      // Input validation rules
    "workflows": [...]         // Multi-step processes
  },
  "data": {                    // Data layer
    "entities": [...],         // Business objects
    "operations": [...]        // CRUD operations
  },
  "patterns": [...],           // Design patterns detected
  "security_issues": [...],    // Security concerns
  "external_references": {...},// Dependencies
  "generation_metadata": {...} // Automation metrics
}
```

## Troubleshooting

### Parser Errors

```bash
# Check file exists
ls -l MyForm.frm

# Check file encoding (should be UTF-8 or ASCII)
file MyForm.frm

# Try with full path
python3 src/vb6_parser.py "$(pwd)/MyForm.frm"
```

### Low Confidence Scores

- **< 70%**: Complex form with many external dependencies
- **70-85%**: Medium complexity, some manual review needed
- **> 85%**: High confidence, mostly automated

## Next Steps

1. Parse your VB6 forms
2. Review generated IR JSON
3. Check security issues
4. Use IR as input for code generation (coming soon)

## Documentation

- [README.md](README.md) - Full documentation
- [PHASE1_MANUAL_IR_MAPPING.md](PHASE1_MANUAL_IR_MAPPING.md) - How golden fixture was created
- [PHASE2_PARSER_INSTRUCTIONS.md](PHASE2_PARSER_INSTRUCTIONS.md) - Parser implementation details
- [ir-schema-draft.json](ir-schema-draft.json) - Complete IR schema

## Support

For issues or questions, see [README.md](README.md) for architecture details.

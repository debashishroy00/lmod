# Phase 2: Automated VB6 Parser - Implementation Guide

## WHAT We're Building:

**TypeScript Parser** that reads VB6 `.frm` files and extracts structured IR (Intermediate Representation) JSON.

**Input**: `samples/vb6/simple/StartForm.frm` (99 lines of VB6)
**Output**: IR JSON matching `ir-schema-draft.json`
**Validation**: >= 90% similarity to `expected-ir/StartForm.json`

---

## WHY This Architecture:

### Modular Parser Design

**WHAT**: Break parsing into focused functions
**WHY**: VB6 .frm files have clear sections - parse each separately
**HOW**:
- Form properties parser (lines 2-15)
- Controls parser (lines 16-64)
- Event handlers parser (lines 71-98)
- Combine into complete IR

**Benefits**:
- Easy to test each section independently
- Clear separation of concerns
- Reusable for other .frm files

---

## HOW The Parser Works:

### VB6 .frm File Structure:

```
VERSION 5.00                          ← Header (ignore)
Begin VB.Form FormName                ← Form declaration
   Property = Value                   ← Form properties
   Begin VB.ControlType controlName   ← Control declaration
      Property = Value                ← Control properties
   End                                ← End control
End                                   ← End form
Attribute VB_Name = "FormName"        ← Form attributes
Private Sub controlName_Event()       ← Event handler
   ' VB6 code                         ← Logic
End Sub
```

### Parsing Strategy:

**Line-based parsing** with state machine:
1. Read file line by line
2. Track current context (in form? in control? in event handler?)
3. Extract properties based on context
4. Build IR incrementally

---

## Implementation Plan:

### Step 1: Type Definitions

**File**: `src/types/ir.ts`

**WHAT**: TypeScript interfaces matching IR schema
**WHY**: Type safety + IDE autocomplete
**HOW**: Map JSON schema to TypeScript types

```typescript
// Core IR structure
export interface IntermediateRepresentation {
  metadata: Metadata;
  ui: UIStructure;
  logic: Logic;
  data: Data;
  patterns: Pattern[];
  external_references: ExternalReferences;
  security_issues: SecurityIssue[];
  generation_metadata: GenerationMetadata;
}

// Each section has its own interface
export interface UIStructure {
  type: 'form' | 'dialog' | 'mdi_child' | 'web_page' | 'console' | 'none';
  confidence: number;
  form: FormProperties;
  controls: Control[];
  layout: LayoutInfo;
}

// And so on...
```

---

### Step 2: Form Properties Parser

**File**: `src/parsers/vb6-form-parser.ts`

**WHAT**: Extract form-level properties
**WHY**: Captures form name, size, style, startup position
**HOW**: Parse lines between `Begin VB.Form` and first `Begin VB.ControlType`

**Input Lines**:
```vb6
Begin VB.Form StartForm
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Start"
   ClientHeight    =   1500
   ClientWidth     =   4050
   StartUpPosition =   1  'CenterOwner
```

**Output**:
```typescript
{
  form: {
    name: "StartForm",
    caption: "Start",
    width: 4050,
    height: 1500,
    border_style: "FixedDialog",
    start_position: "CenterOwner"
  }
}
```

**Algorithm**:
```typescript
function parseFormProperties(lines: string[]): FormProperties {
  const formNameMatch = lines[0].match(/Begin VB\.Form (\w+)/);
  const name = formNameMatch ? formNameMatch[1] : 'Unknown';

  const properties: any = { name };

  for (const line of lines) {
    if (line.includes('Caption')) {
      properties.caption = extractQuotedValue(line);
    }
    if (line.includes('ClientWidth')) {
      properties.width = extractNumericValue(line);
    }
    // ... more properties
  }

  return properties;
}
```

---

### Step 3: Controls Parser

**File**: `src/parsers/vb6-controls-parser.ts`

**WHAT**: Extract all UI controls (TextBox, Button, Label, etc.)
**WHY**: Controls are the main UI elements users interact with
**HOW**: Find `Begin VB.ControlType` blocks, parse properties until `End`

**Input Block**:
```vb6
Begin VB.TextBox txtID
   Height          =   285
   Left            =   1320
   TabIndex        =   0
   Top             =   360
   Width           =   2535
End
```

**Output**:
```typescript
{
  id: "txtID",
  type: "TextBox",
  position: {
    left: 1320,
    top: 360,
    width: 2535,
    height: 285
  },
  tab_index: 0
}
```

**Algorithm**:
```typescript
function parseControls(lines: string[]): Control[] {
  const controls: Control[] = [];
  let currentControl: Partial<Control> | null = null;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i].trim();

    // Start of control block
    if (line.startsWith('Begin VB.')) {
      const match = line.match(/Begin VB\.(\w+)\s+(\w+)/);
      if (match) {
        currentControl = {
          type: match[1],
          id: match[2],
          position: {},
          properties: {}
        };
      }
    }

    // Control property
    else if (currentControl && line.includes('=')) {
      const [key, value] = line.split('=').map(s => s.trim());

      if (key === 'Caption') {
        currentControl.caption = extractQuotedValue(value);
      } else if (key === 'Height') {
        currentControl.position!.height = parseInt(value);
      }
      // ... more properties
    }

    // End of control block
    else if (line === 'End' && currentControl) {
      controls.push(currentControl as Control);
      currentControl = null;
    }
  }

  return controls;
}
```

---

### Step 4: Event Handler Parser

**File**: `src/parsers/vb6-event-parser.ts`

**WHAT**: Extract event handlers (Click, Load, etc.) and their logic
**WHY**: Captures business logic and user interactions
**HOW**: Find `Private Sub` declarations, extract code until `End Sub`

**Input**:
```vb6
Private Sub cmdOpen_Click()
  Dim objClient As Client
  On Error Resume Next
  Set objClient = GetClient(CLng(txtID.Text))
  If objClient Is Nothing Then
    MsgBox "Client ID not found"
  Else
    ' ... more code
  End If
End Sub
```

**Output**:
```typescript
{
  control_id: "cmdOpen",
  event_type: "Click",
  handler_name: "cmdOpen_Click",
  logic_steps: [
    {
      step_type: "object_creation",
      description: "Declare client variable",
      code_snippet: "Dim objClient As Client"
    },
    {
      step_type: "validation",
      description: "Check if client exists",
      code_snippet: "If objClient Is Nothing Then"
    }
    // ... more steps
  ]
}
```

**Algorithm**:
```typescript
function parseEventHandlers(lines: string[]): EventHandler[] {
  const handlers: EventHandler[] = [];
  let currentHandler: Partial<EventHandler> | null = null;
  let handlerLines: string[] = [];

  for (const line of lines) {
    // Start of handler
    if (line.includes('Private Sub') || line.includes('Public Sub')) {
      const match = line.match(/Sub (\w+)_(\w+)\(\)/);
      if (match) {
        currentHandler = {
          control_id: match[1],
          event_type: match[2],
          handler_name: `${match[1]}_${match[2]}`,
          logic_steps: []
        };
        handlerLines = [];
      }
    }

    // Handler code
    else if (currentHandler && !line.includes('End Sub')) {
      handlerLines.push(line);
    }

    // End of handler
    else if (line.includes('End Sub') && currentHandler) {
      currentHandler.logic_steps = analyzeLogic(handlerLines);
      handlers.push(currentHandler as EventHandler);
      currentHandler = null;
    }
  }

  return handlers;
}

function analyzeLogic(lines: string[]): LogicStep[] {
  const steps: LogicStep[] = [];

  for (const line of lines) {
    const trimmed = line.trim();

    // Detect step types
    if (trimmed.startsWith('Dim')) {
      steps.push({
        step_type: 'object_creation',
        description: 'Variable declaration',
        code_snippet: trimmed
      });
    }
    else if (trimmed.startsWith('If')) {
      steps.push({
        step_type: 'validation',
        description: 'Conditional check',
        code_snippet: trimmed
      });
    }
    else if (trimmed.includes('MsgBox')) {
      steps.push({
        step_type: 'message',
        description: 'Display message to user',
        code_snippet: trimmed
      });
    }
    // ... more patterns
  }

  return steps;
}
```

---

### Step 5: IR Assembly

**File**: `src/parsers/vb6-ir-assembler.ts`

**WHAT**: Combine parsed sections into complete IR
**WHY**: Each parser produces partial data - need to merge
**HOW**: Call all parsers, combine results, add metadata

```typescript
export function assembleIR(
  formProps: FormProperties,
  controls: Control[],
  eventHandlers: EventHandler[],
  sourceFile: string
): IntermediateRepresentation {

  return {
    metadata: {
      source_language: 'VB6',
      source_file: sourceFile,
      source_lines_of_code: countLines(sourceFile),
      target_framework: 'Angular',
      analysis_timestamp: new Date().toISOString(),
      confidence: calculateOverallConfidence({formProps, controls, eventHandlers}),
      complexity: assessComplexity(controls.length, eventHandlers.length)
    },

    ui: {
      type: determineUIType(formProps.border_style),
      confidence: 0.95,  // High for UI - clearly defined
      form: formProps,
      controls: controls,
      layout: analyzeLayout(controls)
    },

    logic: {
      confidence: 0.88,
      event_handlers: eventHandlers,
      validations: extractValidations(eventHandlers),
      workflows: identifyWorkflows(eventHandlers),
      error_handling: findErrorHandling(eventHandlers)
    },

    data: {
      confidence: 0.70,  // Lower - external dependencies
      data_source: { type: 'other', is_external: true },
      entities: inferEntities(eventHandlers),
      operations: extractDataOperations(eventHandlers)
    },

    patterns: detectPatterns({formProps, controls, eventHandlers}),

    external_references: findExternalRefs(eventHandlers),

    security_issues: analyzeSecurityIssues(eventHandlers),

    generation_metadata: {
      estimated_automation_rate: 0.88,
      complexity_score: calculateComplexityScore(controls, eventHandlers)
    }
  };
}
```

---

### Step 6: Main Parser Entry Point

**File**: `src/index.ts`

**WHAT**: Main entry point - orchestrates parsing
**WHY**: User-facing API for parsing VB6 files
**HOW**: Read file, call parsers, output IR JSON

```typescript
import * as fs from 'fs';
import { parseVB6Form } from './parsers/vb6-parser';

function main() {
  const filePath = process.argv[2] || 'samples/vb6/simple/StartForm.frm';

  console.log(`🔍 Parsing VB6 form: ${filePath}`);

  // Read source file
  const sourceCode = fs.readFileSync(filePath, 'utf-8');

  // Parse
  const startTime = Date.now();
  const ir = parseVB6Form(sourceCode, filePath);
  const duration = Date.now() - startTime;

  // Output
  const outputPath = filePath.replace('.frm', '_ir.json');
  fs.writeFileSync(
    outputPath,
    JSON.stringify(ir, null, 2),
    'utf-8'
  );

  console.log(`✅ IR generated in ${duration}ms`);
  console.log(`📄 Output: ${outputPath}`);
  console.log(`📊 Confidence: ${(ir.metadata.confidence * 100).toFixed(1)}%`);
  console.log(`📊 Controls: ${ir.ui.controls.length}`);
  console.log(`📊 Event Handlers: ${ir.logic.event_handlers.length}`);
}

main();
```

---

### Step 7: Similarity Validator

**File**: `src/validate.ts`

**WHAT**: Compare parser output to golden fixture
**WHY**: Measure parser accuracy (need >= 90%)
**HOW**: Deep comparison of JSON structures

```typescript
import * as fs from 'fs';

interface SimilarityReport {
  overall: number;
  sections: {
    metadata: number;
    ui: number;
    logic: number;
    data: number;
    patterns: number;
  };
  mismatches: string[];
}

function calculateSimilarity(
  actual: any,
  expected: any,
  path: string = ''
): SimilarityReport {

  let matches = 0;
  let total = 0;
  const mismatches: string[] = [];

  function compare(a: any, e: any, p: string) {
    // Skip annotation fields (start with _)
    if (p.endsWith('/_comment') || p.endsWith('/_what') ||
        p.endsWith('/_why') || p.endsWith('/_source')) {
      return;
    }

    total++;

    if (typeof a === 'object' && typeof e === 'object') {
      for (const key of Object.keys(e)) {
        if (key.startsWith('_')) continue;  // Skip annotations
        compare(a[key], e[key], `${p}/${key}`);
      }
    } else if (a === e) {
      matches++;
    } else {
      mismatches.push(`${p}: expected "${e}", got "${a}"`);
    }
  }

  compare(actual, expected, '');

  const overall = total > 0 ? matches / total : 0;

  return {
    overall,
    sections: {
      metadata: compareSections(actual.metadata, expected.metadata),
      ui: compareSections(actual.ui, expected.ui),
      logic: compareSections(actual.logic, expected.logic),
      data: compareSections(actual.data, expected.data),
      patterns: compareSections(actual.patterns, expected.patterns)
    },
    mismatches: mismatches.slice(0, 10)  // Top 10 mismatches
  };
}

function main() {
  const actualPath = 'samples/vb6/simple/StartForm_ir.json';
  const expectedPath = 'expected-ir/StartForm.json';

  const actual = JSON.parse(fs.readFileSync(actualPath, 'utf-8'));
  const expected = JSON.parse(fs.readFileSync(expectedPath, 'utf-8'));

  const report = calculateSimilarity(actual, expected);

  console.log('\n📊 Similarity Report:');
  console.log(`Overall: ${(report.overall * 100).toFixed(1)}%`);
  console.log(`\nBy Section:`);
  console.log(`  Metadata: ${(report.sections.metadata * 100).toFixed(1)}%`);
  console.log(`  UI: ${(report.sections.ui * 100).toFixed(1)}%`);
  console.log(`  Logic: ${(report.sections.logic * 100).toFixed(1)}%`);
  console.log(`  Data: ${(report.sections.data * 100).toFixed(1)}%`);
  console.log(`  Patterns: ${(report.sections.patterns * 100).toFixed(1)}%`);

  if (report.mismatches.length > 0) {
    console.log(`\n⚠️  Top Mismatches:`);
    report.mismatches.forEach(m => console.log(`  - ${m}`));
  }

  const passed = report.overall >= 0.90;
  console.log(`\n${passed ? '✅' : '❌'} Result: ${passed ? 'PASS' : 'FAIL'} (need >= 90%)`);

  process.exit(passed ? 0 : 1);
}

main();
```

---

## Success Criteria:

### Phase 2 Complete When:

✅ **Parser runs without errors**
```bash
npm run parse:vb6
# Should output: StartForm_ir.json
```

✅ **Output is valid JSON**
```bash
node -e "JSON.parse(require('fs').readFileSync('samples/vb6/simple/StartForm_ir.json'))"
# Should not throw error
```

✅ **Similarity >= 90%**
```bash
npm run validate
# Should show: Overall: >= 90%
```

✅ **Key fields match**
- Form name: "StartForm"
- 5 controls extracted
- 3 event handlers extracted
- 3 patterns detected

---

## Implementation Timeline:

### Estimated: 2-3 hours

| Task | Time | Cumulative |
|------|------|-----------|
| Setup + types | 20 min | 0:20 |
| Form parser | 20 min | 0:40 |
| Controls parser | 40 min | 1:20 |
| Event handler parser | 40 min | 2:00 |
| IR assembly | 20 min | 2:20 |
| Similarity validator | 20 min | 2:40 |
| Testing + fixes | 30 min | 3:10 |

---

## Testing Strategy:

### Simple Validation (Not TDD):

**WHAT**: One-time comparison to golden fixture
**WHY**: Ensure parser works correctly
**HOW**: Run parser → compare JSON → fix if < 90%

**NOT Doing**:
- ❌ Iterative TDD loops
- ❌ Test generation for Angular code
- ❌ Complex test infrastructure

**Doing**:
- ✅ Parse file
- ✅ Compare to expected
- ✅ Show similarity %
- ✅ Done!

---

## What-Why-How Summary:

### WHAT We're Building:
- TypeScript VB6 parser (5 focused modules)
- Similarity validator (simple JSON comparison)
- Main entry point (user-facing CLI)

### WHY This Architecture:
- Modular (each parser has one job)
- Testable (compare output to golden fixture)
- Reusable (works for any .frm file)
- Fast (no TDD iteration, just parse + validate)

### HOW It Works:
1. Read .frm file
2. Parse form properties (lines 2-15)
3. Parse controls (lines 16-64)
4. Parse event handlers (lines 71-98)
5. Assemble into IR JSON
6. Validate against golden fixture
7. Report similarity score

---

## Next Steps:

**Ready to start implementation?**

I'll build each component following this guide, using What-Why-How explanations for each step.

**Begin with**:
1. Type definitions (`src/types/ir.ts`)
2. Utility functions (`src/utils/extractors.ts`)
3. Form parser (`src/parsers/vb6-form-parser.ts`)
4. ... and so on

**Let's build! 🚀**

# Angular Generator - Universal IR Mapping

**Version**: 3.0
**Status**: ✅ Production Ready
**Entry Point**: `src/codegen/main.py`

---

## Overview

The Angular generator produces **Angular 17+ standalone components** from Universal IR. It uses **Claude Haiku API** (LLM-based generation) to create TypeScript, HTML, SCSS, and unit test files.

### Key Features

- ✅ Reads Universal IR (language-agnostic schema)
- ✅ Generates Angular 17+ code (signals, standalone components, inject())
- ✅ Uses Angular Material components
- ✅ Produces complete component files (.ts, .html, .scss, .spec.ts)
- ✅ Creates traceability report (source → IR → Angular mappings)
- ✅ Validates generated code (syntax checks + automatic retry)
- ✅ Works with any source language (VB6, COBOL, etc.)

---

## Universal IR Sections Used

The Angular generator reads these sections from Universal IR:

### 1. `metadata` (Required)

```json
{
  "metadata": {
    "source_language": "VB6",
    "source_file": "StartForm.frm",
    "target_framework": "Angular",
    "confidence": 0.872,
    "complexity": "simple"
  }
}
```

**Usage**:
- `source_language`: Shown in traceability report
- `source_file`: Shown in traceability report
- `target_framework`: Should be "Angular"
- `confidence`: Shown in summary and traceability
- `complexity`: Shown in generation metadata

### 2. `ui` (Required)

```json
{
  "ui": {
    "forms": [
      {
        "name": "StartForm",
        "type": "Form",
        "width": 4800,
        "height": 3600,
        "controls": [
          {
            "name": "cmdSave",
            "type": "CommandButton",
            "caption": "Save",
            "left": 120,
            "top": 480,
            "width": 1215,
            "height": 375,
            "enabled": true,
            "visible": true,
            "tab_index": 1
          },
          {
            "name": "txtClientID",
            "type": "TextBox",
            "text": "",
            "left": 1800,
            "top": 240,
            "width": 2000,
            "height": 300,
            "max_length": 10,
            "enabled": true,
            "visible": true,
            "tab_index": 0
          }
        ]
      }
    ]
  }
}
```

**Usage**:
- `forms[0].name`: Generates component name (e.g., `StartForm` → `start.component.ts`)
- `forms[0].controls[]`: Maps to Angular Material components
- `forms[0].width/height`: Used for SCSS styling (converted from twips)

### 3. `business_logic` (Required)

```json
{
  "business_logic": {
    "procedures": [
      {
        "name": "cmdSave_Click",
        "type": "event_handler",
        "description": "Saves client data",
        "start_line": 15,
        "end_line": 30,
        "parameters": [],
        "logic_steps": [
          {
            "step_number": 1,
            "description": "Validate client ID is not empty",
            "operation_type": "validation",
            "confidence": 0.95
          },
          {
            "step_number": 2,
            "description": "Call API to save client",
            "operation_type": "api_call",
            "target": "SaveClient",
            "confidence": 0.85
          }
        ]
      }
    ]
  }
}
```

**Usage**:
- `procedures[]`: Generated as TypeScript methods in component class
- `procedures[].name`: Method name (e.g., `cmdSave_Click` → `onSave()`)
- `procedures[].logic_steps[]`: Shown in prompt to LLM for implementation guidance

### 4. `events` (Required)

```json
{
  "events": {
    "handlers": [
      {
        "event_name": "cmdSave_Click",
        "control_name": "cmdSave",
        "event_type": "Click",
        "start_line": 15,
        "end_line": 30
      },
      {
        "event_name": "Form_Load",
        "control_name": "StartForm",
        "event_type": "Load",
        "start_line": 5,
        "end_line": 10
      }
    ]
  }
}
```

**Usage**:
- `handlers[]`: Mapped to Angular event bindings `(click)`, `(change)`, etc.
- Special case: `Form_Load` → `ngOnInit()` lifecycle hook
- Special case: `Form_Unload` → `ngOnDestroy()` lifecycle hook

### 5. `data_structures` (Optional)

```json
{
  "data_structures": {
    "entities": [
      {
        "name": "Client",
        "type": "class",
        "fields": [
          {
            "name": "clientID",
            "data_type": "String",
            "nullable": false
          },
          {
            "name": "clientName",
            "data_type": "String",
            "nullable": true
          }
        ]
      }
    ]
  }
}
```

**Usage**:
- `entities[]`: May generate TypeScript interfaces (if needed)
- `entities[].fields[]`: Used for type hints in component

### 6. `frontend_mapping` (Optional)

```json
{
  "frontend_mapping": {
    "mappings": [
      {
        "source_control": "cmdSave",
        "source_type": "CommandButton",
        "target_component": "button[mat-raised-button]",
        "props": {
          "color": "primary",
          "type": "button"
        }
      },
      {
        "source_control": "txtClientID",
        "source_type": "TextBox",
        "target_component": "mat-form-field>input[matInput]",
        "props": {
          "type": "text",
          "placeholder": "Client ID"
        }
      }
    ]
  }
}
```

**Usage**:
- `mappings[]`: Used in prompt to guide LLM on control → Angular Material mapping
- `mappings[].target_component`: Specific Material component to use
- `mappings[].props`: HTML attributes to set

### 7. `generation_metadata` (Optional)

```json
{
  "generation_metadata": {
    "complexity_score": "simple",
    "estimated_manual_effort_hours": 4,
    "estimated_automation_rate": 0.85,
    "generation_notes": [
      "Form has simple validation logic",
      "Uses standard Material UI components"
    ]
  }
}
```

**Usage**:
- Shown in traceability report
- Written to `GENERATION_METADATA.json`

---

## Control Mapping Table

### UI Controls → Angular Material

| Universal IR Control Type | Angular Material Component | Example |
|---------------------------|----------------------------|---------|
| `CommandButton` | `<button mat-raised-button>` | `<button mat-raised-button (click)="onSave()">Save</button>` |
| `TextBox` | `<mat-form-field><input matInput>` | `<mat-form-field><input matInput [(ngModel)]="clientId"></mat-form-field>` |
| `Label` | `<mat-label>` | `<mat-label>Client ID</mat-label>` |
| `ComboBox` | `<mat-select>` | `<mat-select [(ngModel)]="selected"><mat-option>...</mat-option></mat-select>` |
| `CheckBox` | `<mat-checkbox>` | `<mat-checkbox [(ngModel)]="agreed">I agree</mat-checkbox>` |
| `ListBox` | `<mat-selection-list>` | `<mat-selection-list><mat-list-option>...</mat-list-option></mat-selection-list>` |
| `Frame` | `<mat-card>` | `<mat-card><mat-card-title>Group</mat-card-title></mat-card>` |
| `OptionButton` (Radio) | `<mat-radio-button>` | `<mat-radio-group><mat-radio-button value="1">Option 1</mat-radio-button></mat-radio-group>` |
| `Image` | `<img>` | `<img [src]="imagePath" alt="Image">` |
| `PictureBox` | `<img>` or `<canvas>` | `<img [src]="picturePath" alt="Picture">` |

**Source**: Defined in `src/codegen/prompt_builder.py::_format_detailed_control_mapping()`

### Event Mapping Table

| Universal IR Event Type | Angular Event | Example |
|-------------------------|---------------|---------|
| `Click` | `(click)` | `<button (click)="onClick()">` |
| `Change` | `(change)` | `<input (change)="onChange()">` |
| `DblClick` | `(dblclick)` | `<div (dblclick)="onDoubleClick()">` |
| `GotFocus` | `(focus)` | `<input (focus)="onFocus()">` |
| `LostFocus` | `(blur)` | `<input (blur)="onBlur()">` |
| `KeyPress` | `(keypress)` | `<input (keypress)="onKeyPress($event)">` |
| `Load` (Form_Load) | `ngOnInit()` | Lifecycle hook - implement in component class |
| `Unload` (Form_Unload) | `ngOnDestroy()` | Lifecycle hook - implement in component class |

**Source**: Defined in `src/codegen/prompt_builder.py::_format_detailed_event_mapping()`

### Data Type Mapping Table

| Universal IR Data Type | TypeScript Type | Example |
|------------------------|-----------------|---------|
| `String` | `string` | `clientId: string` |
| `Integer` | `number` | `age: number` |
| `Long` | `number` | `count: number` |
| `Double` | `number` | `price: number` |
| `Boolean` | `boolean` | `isActive: boolean` |
| `Date` | `Date` | `createdAt: Date` |
| `Variant` | `any` | `data: any` (avoid when possible) |

---

## Prompt Structure

The Angular generator uses a comprehensive prompt built by `src/codegen/prompt_builder.py`.

### Prompt Template

```
You are an Angular 17 expert generating production-ready code from Universal Intermediate Representation (IR).

**CRITICAL RULES**:
1. Output ONLY valid TypeScript/HTML/SCSS - no markdown code fences, no explanations
2. Use Angular 17+ features (signals, standalone components, inject())
3. Follow Angular style guide and best practices
4. Include proper TypeScript types (avoid 'any' when possible)
5. Generate passing unit tests
6. Use Angular Material components

**INPUT**: Universal IR (language-agnostic schema)

Source Language: {metadata.source_language}
Target Framework: {metadata.target_framework}
Confidence: {metadata.confidence}

{json.dumps(universal_ir, indent=2)}

**YOUR TASK**: Generate Angular component files for the form "{form_name}"

---

## OUTPUT FILE FORMAT

You MUST output files with these exact separator markers:

=== FILE: {component_name}.component.ts ===
[TypeScript code - no markdown, no code fences]

=== FILE: {component_name}.component.html ===
[HTML code - no markdown, no code fences]

=== FILE: {component_name}.component.scss ===
[SCSS code - no markdown, no code fences]

=== FILE: {component_name}.component.spec.ts ===
[Test code - no markdown, no code fences]

---

## COMPONENT REQUIREMENTS

### 1. TypeScript Component ({component_name}.component.ts)

**Required structure**:
import { Component, signal } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
// Add Angular Material imports based on controls

@Component({
  selector: 'app-{component_name}',
  standalone: true,
  imports: [CommonModule, FormsModule /* add Material modules */],
  templateUrl: './{component_name}.component.html',
  styleUrl: './{component_name}.component.scss'
})
export class {class_name}Component {
  // Component implementation
}

**Implement these event handlers/procedures**:
{_format_procedures(procedures, event_handlers)}

**Use signals for state** (Angular 17 feature):
- Form inputs: `clientId = signal('');`
- Loading states: `isLoading = signal(false);`

### 2. HTML Template ({component_name}.component.html)

**Map these UI controls to Angular Material**:
{_format_control_mappings(controls, frontend_mapping)}

**Layout requirements**:
- Use CSS Grid or Flexbox for layout
- Follow Material Design spacing (8px grid)
- Make responsive where appropriate

### 3. SCSS Styles ({component_name}.component.scss)

**Form dimensions** (from IR):
- Width: {form.width} twips (convert to px: 1 twip ≈ 0.0625px)
- Height: {form.height} twips

**Use Material theming**:
- Colors from theme
- Standard spacing units (8px, 16px, 24px)

### 4. Unit Tests ({component_name}.component.spec.ts)

**Required tests**:
- Component creation test
- One test per event handler/procedure
- Validation tests (if applicable)
- Use Angular TestBed

---

## MAPPING RULES

{_format_detailed_control_mapping()}
{_format_detailed_event_mapping()}
{_format_data_types(entities)}

---

## CODE QUALITY REQUIREMENTS

1. **TypeScript**:
   - Strict typing (no implicit any)
   - Proper access modifiers (private/public)
   - Use readonly where appropriate

2. **HTML**:
   - Semantic markup
   - Accessibility (ARIA labels)
   - Proper event bindings

3. **Tests**:
   - All tests must pass
   - Use descriptive test names
   - Test both success and error cases

4. **Comments**:
   - Add brief comments for complex logic
   - Include traceability comments (source line numbers from IR)

---

Now generate the complete Angular component files.
```

**Source**: `src/codegen/prompt_builder.py::build_angular_generation_prompt()`

---

## Output Files

The Angular generator produces **6 files**:

### 1. `{component-name}.component.ts` (TypeScript Component)

**Example**:
```typescript
import { Component, signal } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatFormFieldModule } from '@angular/material/form-field';

@Component({
  selector: 'app-start',
  standalone: true,
  imports: [
    CommonModule,
    FormsModule,
    MatButtonModule,
    MatInputModule,
    MatFormFieldModule
  ],
  templateUrl: './start.component.html',
  styleUrl: './start.component.scss'
})
export class StartComponent {
  // Signals (Angular 17 feature)
  clientId = signal('');
  isLoading = signal(false);

  // Event handlers
  onSave(): void {
    if (!this.clientId()) {
      alert('Please enter Client ID');
      return;
    }
    // Save logic
    console.log('Saving client:', this.clientId());
  }

  onClose(): void {
    // Close logic
    window.close();
  }

  // Lifecycle hooks
  ngOnInit(): void {
    // Form_Load logic
    console.log('Form loaded');
  }
}
```

### 2. `{component-name}.component.html` (Template)

**Example**:
```html
<div class="start-form">
  <h2>Start Form</h2>

  <mat-form-field>
    <mat-label>Client ID</mat-label>
    <input matInput [(ngModel)]="clientId" placeholder="Enter Client ID">
  </mat-form-field>

  <div class="button-group">
    <button mat-raised-button color="primary" (click)="onSave()">
      Save
    </button>
    <button mat-raised-button (click)="onClose()">
      Close
    </button>
  </div>
</div>
```

### 3. `{component-name}.component.scss` (Styles)

**Example**:
```scss
.start-form {
  padding: 24px;
  max-width: 600px;
  margin: 0 auto;

  h2 {
    margin-bottom: 24px;
  }

  mat-form-field {
    width: 100%;
    margin-bottom: 16px;
  }

  .button-group {
    display: flex;
    gap: 16px;

    button {
      flex: 1;
    }
  }
}
```

### 4. `{component-name}.component.spec.ts` (Unit Tests)

**Example**:
```typescript
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { StartComponent } from './start.component';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';

describe('StartComponent', () => {
  let component: StartComponent;
  let fixture: ComponentFixture<StartComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [StartComponent, BrowserAnimationsModule]
    }).compileComponents();

    fixture = TestBed.createComponent(StartComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should save client when onSave is called with valid ID', () => {
    component.clientId.set('12345');
    spyOn(console, 'log');
    component.onSave();
    expect(console.log).toHaveBeenCalledWith('Saving client:', '12345');
  });

  it('should alert when onSave is called without client ID', () => {
    component.clientId.set('');
    spyOn(window, 'alert');
    component.onSave();
    expect(window.alert).toHaveBeenCalledWith('Please enter Client ID');
  });
});
```

### 5. `TRACEABILITY.md` (Traceability Report)

**Example**:
```markdown
# Traceability Report

**Form**: StartForm
**Source Language**: VB6
**Source File**: StartForm.frm
**Target Framework**: Angular
**Generated**: 4 files
**Confidence**: 87.2%

---

## VB6 Source → Universal IR → Angular Code

### UI Controls

#### txtClientID
- **Source Type**: TextBox
- **Caption**: ""
- **Universal IR Path**: `ui.forms[0].controls[].name == 'txtClientID'`
- **Angular**: Search for `clientid` in template

#### cmdSave
- **Source Type**: CommandButton
- **Caption**: "Save"
- **Universal IR Path**: `ui.forms[0].controls[].name == 'cmdSave'`
- **Angular**: Search for `save` in template

### Event Handlers

#### cmdSave_Click()
- **Control**: cmdSave
- **Source Lines**: 15-30
- **Universal IR Path**: `events.handlers[].event_name == 'cmdSave_Click'`
- **Angular Method**: Search for method in .component.ts

#### Form_Load()
- **Control**: StartForm
- **Source Lines**: 5-10
- **Universal IR Path**: `events.handlers[].event_name == 'Form_Load'`
- **Angular Method**: Search for method in .component.ts

### Frontend Component Mappings

| Source Control | Source Type | Target Component |
|----------------|-------------|------------------|
| txtClientID | TextBox | mat-form-field>input[matInput] |
| cmdSave | CommandButton | button[mat-raised-button] |
| cmdClose | CommandButton | button[mat-raised-button] |
```

### 6. `GENERATION_METADATA.json` (Generation Metadata)

**Example**:
```json
{
  "generated_at": "2025-11-22T10:30:00Z",
  "source_language": "VB6",
  "source_file": "StartForm.frm",
  "target_framework": "Angular",
  "form_name": "StartForm",
  "generated_files": [
    "start.component.ts",
    "start.component.html",
    "start.component.scss",
    "start.component.spec.ts"
  ],
  "file_sizes": {
    "start.component.ts": 1245,
    "start.component.html": 432,
    "start.component.scss": 289,
    "start.component.spec.ts": 876
  },
  "ir_metadata": {
    "source_language": "VB6",
    "source_file": "StartForm.frm",
    "confidence": 0.872,
    "complexity": "simple"
  },
  "statistics": {
    "controls_count": 5,
    "procedures_count": 2,
    "event_handlers_count": 3,
    "entities_count": 1,
    "frontend_mappings_count": 5
  }
}
```

---

## Code Generation Flow

### 1. Load Universal IR

```python
# src/codegen/main.py
with open(ir_file, 'r', encoding='utf-8') as f:
    ir = json.load(f)

# Validate Universal IR structure
required_sections = ['metadata', 'ui', 'business_logic', 'data_structures']
missing_sections = [section for section in required_sections if section not in ir]

if missing_sections:
    print(f"❌ Error: IR file is missing required Universal IR sections: {', '.join(missing_sections)}")
    sys.exit(1)
```

### 2. Build Prompt

```python
# src/codegen/angular_generator.py
from .prompt_builder import build_angular_generation_prompt

prompt = build_angular_generation_prompt(ir)
```

### 3. Call Claude API

```python
# src/codegen/angular_generator.py
response = self.client.messages.create(
    model="claude-3-5-haiku-20241022",
    max_tokens=8192,
    temperature=0.0,  # Deterministic for code generation
    messages=[{
        "role": "user",
        "content": prompt
    }]
)

generated_code = response.content[0].text
```

### 4. Parse Response

```python
# src/codegen/angular_generator.py
files = self._parse_generated_files(generated_code)

# Expected format:
# === FILE: component.ts ===
# [code]
# === FILE: component.html ===
# [code]
```

### 5. Validate Generated Code

```python
# src/codegen/angular_generator.py
from .validators import validate_generated_code

validation_result = validate_generated_code(files)

if not validation_result['valid']:
    # Retry with error feedback
    files = self._retry_with_feedback(ir, prompt, validation_result)
```

### 6. Write Files

```python
# src/codegen/angular_generator.py
from .file_writer import write_angular_files

write_angular_files(files, output_dir, ir)
```

---

## Validation Rules

The Angular generator validates generated code using `src/codegen/validators.py`:

### Syntax Validation

1. **TypeScript Files** (`.ts`, `.spec.ts`)
   - Check balanced braces: `{` matches `}`
   - Check balanced brackets: `[` matches `]`
   - Check balanced parentheses: `(` matches `)`
   - Check for required imports: `@angular/core`, `@Component`

2. **HTML Files** (`.html`)
   - Check balanced tags: `<div>` matches `</div>`
   - Check for common typos: `<mat-form-field>` not `<mat-form-feild>`

3. **SCSS Files** (`.scss`)
   - Check balanced braces: `{` matches `}`
   - Check no syntax errors

### Required Content

1. **Component TypeScript** must have:
   - `@Component` decorator
   - Class definition
   - Imports from `@angular/core`

2. **Unit Tests** must have:
   - `describe()` block
   - At least one `it()` test
   - TestBed configuration

### Retry Logic

If validation fails:
1. Extract error messages
2. Build retry prompt with error feedback
3. Call Claude API again (max 1 retry)
4. If still fails, proceed with warning

---

## Known Limitations

### 1. Form Layout

**Limitation**: Layout is simplified (CSS Grid or Flexbox), not pixel-perfect
**Reason**: VB6 uses absolute positioning (left/top coordinates), Angular uses responsive layouts
**Workaround**: Manual adjustment for complex layouts

### 2. Advanced VB6 Controls

**Limitation**: Some VB6 controls have no direct Angular Material equivalent
**Examples**:
- `MSFlexGrid` → May use `<table mat-table>` (simplified)
- `UpDown` → May use `<input type="number">` (no Material spinner)
- `Animation` → No Angular Material equivalent

**Workaround**: LLM chooses closest Material component or uses plain HTML

### 3. Complex Business Logic

**Limitation**: LLM may simplify complex VB6 logic
**Reason**: LLM has limited context window (8K tokens output)
**Workaround**: Review generated code and refine manually

### 4. Legacy VB6 APIs

**Limitation**: VB6-specific APIs (e.g., `MsgBox`, `DoEvents`) not directly translatable
**Examples**:
- `MsgBox` → `alert()` (basic) or Material Dialog (advanced)
- `DoEvents` → No Angular equivalent (use async/await)

**Workaround**: Manual replacement with Angular equivalents

---

## Performance & Cost

### API Usage

**Model**: Claude 3.5 Haiku (`claude-3-5-haiku-20241022`)

**Token Limits**:
- Input: ~200K tokens (prompt + Universal IR)
- Output: 8,192 tokens (component files)

**Cost** (per form):
- Simple forms (5 controls): ~$0.004
- Medium forms (16 controls): ~$0.008
- Complex forms (40+ controls): ~$0.015

**Speed**:
- API call: 15-30 seconds
- Validation: 1-2 seconds
- File writing: <1 second
- **Total**: ~20-45 seconds per form

### Comparison

| Metric | Manual Migration | Automated (Angular Generator) |
|--------|------------------|-------------------------------|
| Time per form | 4-8 hours | 20-45 seconds |
| Cost per form | $400-$800 (developer time) | $0.004-$0.015 (API cost) |
| Consistency | Variable | High (deterministic prompt) |
| Traceability | Manual documentation | Automatic TRACEABILITY.md |

**ROI**: 99.99% cost reduction

---

## Testing

### Unit Testing Generated Code

```bash
# Navigate to Angular project
cd output/angular/start-form/

# Run Angular tests
ng test

# Expected: All tests pass ✅
```

### End-to-End Testing

```bash
# Run Phase 3 regression tests
python3 tools/run_regression_phase3.py

# Expected output:
# ✓ VB6 Universal IR is valid
# ✓ Angular generator output is valid
# All Phase 3 Regression Tests PASSED ✅
```

---

## Examples

### Example 1: Simple Form (StartForm)

**Input**: `samples/vb6/simple/StartForm_ir.json` (Universal IR)

**Controls**: 5 controls (2 TextBox, 2 CommandButton, 1 Label)

**Output**:
- `start.component.ts` (135 lines)
- `start.component.html` (45 lines)
- `start.component.scss` (30 lines)
- `start.component.spec.ts` (78 lines)
- `TRACEABILITY.md`
- `GENERATION_METADATA.json`

**Result**: ✅ All TypeScript tests pass, zero syntax errors

### Example 2: Medium Form (frmsupplier)

**Input**: `samples/vb6/medium/frmsupplier_ir.json` (Universal IR)

**Controls**: 16 controls (8 TextBox, 4 CommandButton, 2 ComboBox, 2 Label)

**Output**:
- `supplier.component.ts` (287 lines)
- `supplier.component.html` (124 lines)
- `supplier.component.scss` (68 lines)
- `supplier.component.spec.ts` (165 lines)
- `TRACEABILITY.md`
- `GENERATION_METADATA.json`

**Result**: ✅ All TypeScript tests pass, zero syntax errors

---

## Troubleshooting

### Issue: "No forms found in Universal IR"

**Cause**: IR file is missing `ui.forms[]` section

**Solution**: Regenerate Universal IR from source:
```bash
# VB6 → Universal IR
python3 src/orchestrator/main.py samples/vb6/simple/StartForm.frm
```

### Issue: Validation failed

**Cause**: Generated code has syntax errors

**Solution**: The generator automatically retries once. If still fails, check the error messages in console output.

### Issue: Generated code missing features

**Cause**: LLM simplified complex logic

**Solution**: Review generated code and refine manually. The TRACEABILITY.md shows source → IR → Angular mappings for reference.

---

## Related Documentation

- [Phase 3 Overview](PHASE3_OVERVIEW.md)
- [Spring Boot Generator Details](SPRINGBOOT_UNIVERSAL_GENERATOR.md)
- [Phase 3 Completion Summary](PHASE3_COMPLETION_SUMMARY.md)
- [Universal IR Design](../phase2/UNIVERSAL_IR_DESIGN.md)

---

**Last Updated**: 2025-11-22
**Status**: ✅ Production Ready

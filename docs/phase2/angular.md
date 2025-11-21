# Angular Code Generation Specification

## Overview

**WHAT**: Transform VB6 Intermediate Representation (IR) into production-ready Angular 17+ components
**WHY**: Complete the VB6 → IR → Angular modernization pipeline for AIG's 650+ legacy apps
**HOW**: LLM-powered code generation using IR as structured input, producing TypeScript/HTML/CSS with Material UI

---

## Current Pipeline Status

✅ **Phase 1**: VB6 → IR extraction (COMPLETE)
- 3 specialized agents (UI, Logic, Data)
- 98.3% accuracy on simple forms
- 96.8% accuracy on medium complexity forms
- Robust canonicalization + validation

🔄 **Phase 2**: IR → Angular code generation (THIS DOCUMENT)
- Input: Validated IR JSON
- Output: Angular component with tests

---

## Architecture

### Input: Intermediate Representation (IR)

The IR contains three key sections for Angular generation:

```json
{
  "metadata": {
    "original_form_name": "StartForm",
    "target_language": "Angular",
    "analyzed_date": "2025-01-15"
  },
  "ui": {
    "type": "form",
    "form": {
      "name": "StartForm",
      "caption": "Start",
      "width": 4050,
      "height": 1500
    },
    "controls": [
      {
        "name": "cmdClose",
        "type": "CommandButton",
        "caption": "Close",
        "position": { "left": 120, "top": 600 }
      }
    ]
  },
  "logic": {
    "event_handlers": [
      {
        "control_id": "cmdClose",
        "event_type": "Click",
        "handler_name": "cmdClose_Click",
        "logic_steps": [...]
      }
    ],
    "validations": [...]
  },
  "data": {
    "entities": [...],
    "operations": [...]
  }
}
```

### Output: Angular Component Files

For each VB6 form, generate:

```
start-form/
├── start-form.component.ts      # Component class
├── start-form.component.html    # Template
├── start-form.component.scss    # Styles
├── start-form.component.spec.ts # Tests
└── start-form.service.ts        # Data service (if needed)
```

---

## Generation Strategy

### Option 1: Template-Based Generation (Fast, Deterministic)

**WHAT**: Use Jinja2/Mako templates with IR as input
**WHY**: Predictable, fast, easy to debug
**WHEN**: Use for 80% of standard CRUD forms

**Pros**:
- Fast generation (<1s per component)
- Deterministic output
- Easy to version control templates
- No API costs

**Cons**:
- Limited flexibility for complex logic
- Requires template per pattern
- May need fallback for edge cases

### Option 2: LLM-Powered Generation (Flexible, Adaptive)

**WHAT**: Use Claude Sonnet 4 to generate code from IR + instructions
**WHY**: Handles complex logic, edge cases, and variations
**WHEN**: Use for complex forms or when templates don't cover the pattern

**Pros**:
- Handles any complexity
- Adapts to variations
- Can include best practices automatically

**Cons**:
- Slower (~10-30s per component)
- API costs ($0.50-2.00 per form)
- Non-deterministic output (needs validation)

### Recommended: Hybrid Approach

1. **Template-first**: Try template generation for standard patterns
2. **LLM fallback**: Use LLM for complex/non-standard patterns
3. **Quality gates**: Validate generated code with TypeScript compiler + ESLint

---

## Code Generation Prompt (LLM Approach)

```python
def generate_angular_component(ir: Dict[str, Any]) -> Dict[str, str]:
    """
    Generate Angular component from IR using Claude Sonnet 4

    Returns:
        {
            'component.ts': '...',
            'component.html': '...',
            'component.scss': '...',
            'component.spec.ts': '...',
            'service.ts': '...'  # optional
        }
    """

    prompt = f"""You are an Angular 17 expert generating production-ready code from Intermediate Representation (IR).

**CRITICAL RULES**:
1. Output ONLY valid TypeScript/HTML/CSS - no markdown, no explanations
2. Use Angular 17+ features (signals, standalone components, inject())
3. Follow Angular style guide and best practices
4. Include proper TypeScript types (no 'any')
5. Generate passing unit tests

**INPUT**: VB6 Form IR (JSON)
{json.dumps(ir, indent=2)}

**YOUR TASK**: Generate Angular component files

**COMPONENT STRUCTURE** (.component.ts):
```typescript
import {{ Component, signal }} from '@angular/core';
import {{ CommonModule }} from '@angular/common';
import {{ FormsModule }} from '@angular/forms';
import {{ MatButtonModule }} from '@angular/material/button';

@Component({{
  selector: 'app-{form_name}',
  standalone: true,
  imports: [CommonModule, FormsModule, MatButtonModule],
  templateUrl: './{form_name}.component.html',
  styleUrl: './{form_name}.component.scss'
}})
export class {FormName}Component {{
  // Extract from IR.ui.controls
  // Map VB6 controls to Angular Material components

  // Extract from IR.logic.event_handlers
  // Map VB6 event handlers to TypeScript methods

  // Extract from IR.logic.validations
  // Implement form validation
}}
```

**TEMPLATE** (.component.html):
```html
<!-- Extract from IR.ui.form and IR.ui.controls -->
<!-- Use Angular Material components -->
<div class="form-container">
  <h2>{{{{ title }}}}</h2>

  <!-- Map VB6 TextBox → mat-form-field + input -->
  <!-- Map VB6 CommandButton → mat-button -->
  <!-- Map VB6 ComboBox → mat-select -->
</div>
```

**STYLES** (.component.scss):
```scss
// Extract from IR.ui.form (width, height)
// Map VB6 twips to CSS pixels (1 twip = 1/1440 inch = ~0.0625px)
.form-container {{
  width: {{form_width}}px;
  height: {{form_height}}px;
  padding: 16px;
}}
```

**TESTS** (.component.spec.ts):
```typescript
import {{ ComponentFixture, TestBed }} from '@angular/core/testing';
import {{ {FormName}Component }} from './{form_name}.component';

describe('{FormName}Component', () => {{
  let component: {FormName}Component;
  let fixture: ComponentFixture<{FormName}Component>;

  beforeEach(async () => {{
    await TestBed.configureTestingModule({{
      imports: [{FormName}Component]
    }}).compileComponents();

    fixture = TestBed.createComponent({FormName}Component);
    component = fixture.componentInstance;
    fixture.detectChanges();
  }});

  it('should create', () => {{
    expect(component).toBeTruthy();
  }});

  // Extract test cases from IR.logic.event_handlers
  // One test per event handler
}});
```

**MAPPING RULES**:

1. **VB6 Controls → Angular Material**:
   - `CommandButton` → `<button mat-raised-button>`
   - `TextBox` → `<mat-form-field><input matInput></mat-form-field>`
   - `Label` → `<label>` or `<mat-label>`
   - `ComboBox` → `<mat-select>`
   - `CheckBox` → `<mat-checkbox>`
   - `OptionButton` (radio) → `<mat-radio-button>`
   - `ListBox` → `<mat-selection-list>`

2. **VB6 Events → Angular Methods**:
   - `cmdClose_Click()` → `onCloseClick()`
   - `txtID_Change()` → `onIdChange()`
   - `Form_Load()` → `ngOnInit()`
   - `Form_Unload()` → `ngOnDestroy()`

3. **VB6 Data Types → TypeScript**:
   - `String` → `string`
   - `Long`, `Integer` → `number`
   - `Boolean` → `boolean`
   - `Date` → `Date`
   - `Object` → `any` (with TODO to refine)

4. **VB6 Validation → Angular Validators**:
   - `required` → `Validators.required`
   - `numeric` → `Validators.pattern(/^\\d+$/)`
   - `length` → `Validators.maxLength(n)`
   - `custom` → Custom validator function

**DATA SERVICE** (if IR.data.operations exists):
```typescript
import {{ Injectable, inject }} from '@angular/core';
import {{ HttpClient }} from '@angular/common/http';
import {{ Observable }} from 'rxjs';

@Injectable({{ providedIn: 'root' }})
export class {FormName}Service {{
  private http = inject(HttpClient);
  private apiUrl = '/api/{entity}';

  // Extract from IR.data.operations
  // Generate CRUD methods

  get{Entity}(id: number): Observable<{Entity}> {{
    return this.http.get<{Entity}>(`${{this.apiUrl}}/${{id}}`);
  }}
}}
```

**OUTPUT FORMAT**:
Return each file with a clear separator:

```
=== FILE: {form_name}.component.ts ===
[TypeScript code here]

=== FILE: {form_name}.component.html ===
[HTML code here]

=== FILE: {form_name}.component.scss ===
[SCSS code here]

=== FILE: {form_name}.component.spec.ts ===
[Test code here]

=== FILE: {form_name}.service.ts ===
[Service code here - ONLY if IR.data.operations exists]
```

Now generate the Angular component files from the provided IR.
"""

    return prompt
```

---

## VB6 → Angular Mapping Reference

### Control Mapping

| VB6 Control | Angular Material | Notes |
|-------------|------------------|-------|
| `CommandButton` | `<button mat-raised-button>` | Primary action buttons |
| `TextBox` | `<input matInput>` | Wrap in `<mat-form-field>` |
| `Label` | `<mat-label>` or `<label>` | Use `mat-label` in form fields |
| `ComboBox` | `<mat-select>` | Dropdown selection |
| `ListBox` | `<mat-selection-list>` | Multiple selection |
| `CheckBox` | `<mat-checkbox>` | Boolean toggle |
| `OptionButton` | `<mat-radio-button>` | Exclusive selection |
| `Frame` | `<mat-card>` or `<fieldset>` | Grouping container |
| `PictureBox` | `<img>` | Images |
| `Timer` | `setInterval()` | Use RxJS `interval()` |

### Event Mapping

| VB6 Event | Angular Method | Hook |
|-----------|----------------|------|
| `Form_Load` | `ngOnInit()` | Lifecycle hook |
| `Form_Unload` | `ngOnDestroy()` | Lifecycle hook |
| `Control_Click` | `onClick()` | Event binding `(click)` |
| `Control_Change` | `onChange()` | Event binding `(change)` |
| `Control_DblClick` | `onDoubleClick()` | Event binding `(dblclick)` |
| `Control_GotFocus` | `onFocus()` | Event binding `(focus)` |
| `Control_LostFocus` | `onBlur()` | Event binding `(blur)` |
| `Control_KeyPress` | `onKeyPress()` | Event binding `(keypress)` |

### Data Type Mapping

| VB6 Type | TypeScript | Notes |
|----------|------------|-------|
| `String` | `string` | Primitive |
| `Long`, `Integer` | `number` | No separate int type |
| `Single`, `Double` | `number` | All numbers are `number` |
| `Boolean` | `boolean` | Primitive |
| `Date` | `Date` | Built-in object |
| `Currency` | `number` | Use for money values |
| `Object` | `any` or interface | Define interface when possible |
| `Variant` | `any` | Avoid when possible |

---

## Implementation Checklist

### Phase 2.1: Basic Code Generation (Week 1)

- [ ] Create `src/codegen/` directory
- [ ] Implement `angular_generator.py`
  - [ ] Parse IR JSON
  - [ ] Generate component.ts
  - [ ] Generate component.html
  - [ ] Generate component.scss
  - [ ] Generate component.spec.ts
- [ ] Add VB6 → Angular mapping tables
- [ ] Implement file writer with proper formatting
- [ ] Add TypeScript/ESLint validation
- [ ] Test with StartForm.frm IR

### Phase 2.2: Data Services (Week 1-2)

- [ ] Detect data operations in IR
- [ ] Generate service.ts for CRUD operations
- [ ] Inject service into component
- [ ] Add HTTP interceptors for error handling
- [ ] Generate data models/interfaces
- [ ] Add RxJS patterns (observables, subjects)

### Phase 2.3: Quality Gates (Week 2)

- [ ] Run `tsc --noEmit` to validate TypeScript
- [ ] Run `eslint` to check code quality
- [ ] Run generated tests with `ng test`
- [ ] Measure code coverage (target: >80%)
- [ ] Generate traceability report (IR field → Angular code line)

### Phase 2.4: End-to-End Demo (Week 2)

- [ ] Full pipeline: `StartForm.frm` → IR → Angular component
- [ ] Run generated app: `ng serve`
- [ ] Manual QA: verify all controls render
- [ ] Manual QA: verify all event handlers work
- [ ] Document any manual adjustments needed
- [ ] Create demo video for AIG

---

## Success Criteria

**Angular Code Generation Must**:
1. ✅ Generate valid TypeScript (passes `tsc --noEmit`)
2. ✅ Generate valid HTML (no template errors)
3. ✅ All generated tests pass (`ng test`)
4. ✅ Code coverage >= 80%
5. ✅ ESLint passes with 0 errors
6. ✅ App compiles and runs (`ng serve`)
7. ✅ All VB6 controls have Angular equivalents
8. ✅ All VB6 event handlers are implemented
9. ✅ Validation rules are preserved
10. ✅ CRUD operations work (if data layer exists)

**Nice-to-Have**:
- Accessibility (ARIA labels, keyboard navigation)
- Responsive design (mobile-friendly)
- Dark mode support
- Animation/transitions for better UX
- Error boundaries and loading states

---

## Next Steps

1. **Review this spec** with team
2. **Choose approach**: Template-based vs LLM-powered vs Hybrid
3. **Implement `angular_generator.py`** in `src/codegen/`
4. **Test with StartForm.frm** (simplest case)
5. **Test with frmsupplier.frm** (medium complexity)
6. **Iterate based on results**
7. **Prepare AIG demo**

---

## Related Documents

- [prd.md](prd.md) - Original requirements
- [PHASE2_PARSER_INSTRUCTIONS.md](PHASE2_PARSER_INSTRUCTIONS.md) - Parser architecture
- [ir-schema-draft.json](ir-schema-draft.json) - IR structure
- [platform-development-plan.md](platform-development-plan.md) - Overall platform vision

---

**Status**: ✅ Specification Complete
**Next**: Begin implementation of `src/codegen/angular_generator.py`
**Target**: Working Angular demo by end of Week 2

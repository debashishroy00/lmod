"""
Build LLM prompt for Angular code generation from IR

WHAT: Constructs comprehensive prompts for Claude API
WHY: Prompt engineering is critical for quality code generation
HOW: Template-based prompt with IR data and mapping rules
"""

import json
from typing import Dict, Any, List


def build_angular_generation_prompt(ir: Dict[str, Any]) -> str:
    """
    Build comprehensive prompt for Claude to generate Angular component

    Args:
        ir: Complete IR JSON from Phase 1

    Returns:
        Formatted prompt string for Claude API
    """

    form_name = ir['ui']['form']['name']
    controls = ir['ui']['controls']
    event_handlers = ir['logic']['event_handlers']
    validations = ir['logic'].get('validations', [])

    # Generate component name (remove "Form" suffix, convert to kebab-case)
    component_name = form_name.replace('Form', '').lower()
    if not component_name:
        component_name = form_name.lower()

    class_name = form_name

    prompt = f"""You are an Angular 17 expert generating production-ready code from Intermediate Representation (IR).

**CRITICAL RULES**:
1. Output ONLY valid TypeScript/HTML/SCSS - no markdown code fences, no explanations
2. Use Angular 17+ features (signals, standalone components, inject())
3. Follow Angular style guide and best practices
4. Include proper TypeScript types (avoid 'any' when possible)
5. Generate passing unit tests
6. Use Angular Material components

**INPUT**: VB6 Form IR (JSON)

{json.dumps(ir, indent=2)}

**YOUR TASK**: Generate Angular component files for the form "{form_name}"

---

## OUTPUT FILE FORMAT

You MUST output files with these exact separator markers:

```
=== FILE: {component_name}.component.ts ===
[TypeScript code - no markdown, no code fences]

=== FILE: {component_name}.component.html ===
[HTML code - no markdown, no code fences]

=== FILE: {component_name}.component.scss ===
[SCSS code - no markdown, no code fences]

=== FILE: {component_name}.component.spec.ts ===
[Test code - no markdown, no code fences]
```

---

## COMPONENT REQUIREMENTS

### 1. TypeScript Component ({component_name}.component.ts)

**Required structure**:
```typescript
import {{ Component, signal }} from '@angular/core';
import {{ CommonModule }} from '@angular/common';
import {{ FormsModule }} from '@angular/forms';
// Add Angular Material imports based on controls

@Component({{
  selector: 'app-{component_name}',
  standalone: true,
  imports: [CommonModule, FormsModule /* add Material modules */],
  templateUrl: './{component_name}.component.html',
  styleUrl: './{component_name}.component.scss'
}})
export class {class_name}Component {{
  // Component implementation
}}
```

**Implement these event handlers**:
{_format_event_handlers(event_handlers)}

**Use signals for state** (Angular 17 feature):
- Form inputs: `clientId = signal('');`
- Loading states: `isLoading = signal(false);`

### 2. HTML Template ({component_name}.component.html)

**Map these VB6 controls to Angular Material**:
{_format_control_mappings(controls)}

**Layout requirements**:
- Use CSS Grid or Flexbox for layout
- Follow Material Design spacing (8px grid)
- Make responsive where appropriate

### 3. SCSS Styles ({component_name}.component.scss)

**Form dimensions** (from IR):
- Width: {ir['ui']['form'].get('width', 'auto')} twips (convert to px: 1 twip ≈ 0.0625px)
- Height: {ir['ui']['form'].get('height', 'auto')} twips

**Use Material theming**:
- Colors from theme
- Standard spacing units (8px, 16px, 24px)

### 4. Unit Tests ({component_name}.component.spec.ts)

**Required tests**:
- Component creation test
- One test per event handler
- Validation tests (if applicable)
- Use Angular TestBed

---

## MAPPING RULES

### VB6 Control → Angular Material

{_format_detailed_control_mapping()}

### VB6 Events → Angular Events

{_format_detailed_event_mapping()}

### VB6 Validations → Angular Validators

{_format_validation_rules(validations)}

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
   - Include traceability comments (VB6 line numbers from IR)

---

## IMPORTANT REMINDERS

- Do NOT use markdown code fences (```typescript)
- Do NOT add explanatory text outside the files
- Do NOT use placeholder comments like "// rest of code here"
- DO implement complete, working code
- DO use the exact file separator format: `=== FILE: filename ===`

Now generate the complete Angular component files.
"""

    return prompt


def _format_event_handlers(handlers: List[Dict[str, Any]]) -> str:
    """Format event handlers section of prompt"""
    if not handlers:
        return "No event handlers"

    lines = []
    for handler in handlers:
        control_id = handler.get('control_id', 'unknown')
        event_type = handler.get('event_type', 'Unknown')
        handler_name = handler.get('handler_name', 'unknown')
        source_lines = handler.get('_source_lines', 'unknown')

        lines.append(f"- **{handler_name}()** (VB6 lines {source_lines})")

        # Show logic steps
        logic_steps = handler.get('logic_steps', [])
        if logic_steps:
            for step in logic_steps[:3]:  # Show first 3 steps
                desc = step.get('description', '')
                lines.append(f"  - {desc}")
            if len(logic_steps) > 3:
                lines.append(f"  - ... and {len(logic_steps) - 3} more steps")
        lines.append("")

    return "\n".join(lines)


def _format_control_mappings(controls: List[Dict[str, Any]]) -> str:
    """Format control mappings section"""
    if not controls:
        return "No controls"

    lines = []
    for control in controls:
        control_id = control.get('id', 'unknown')
        vb6_type = control.get('type', 'Unknown')
        caption = control.get('caption', '')
        angular_hint = control.get('_type_angular_mapping', 'Unknown')

        lines.append(f"- **{control_id}** ({vb6_type})")
        lines.append(f"  - Caption: \"{caption}\"")
        lines.append(f"  - Angular: {angular_hint}")
        lines.append("")

    return "\n".join(lines)


def _format_detailed_control_mapping() -> str:
    """Format detailed control mapping table"""
    return """
| VB6 Control | Angular Material | Example |
|-------------|------------------|---------|
| CommandButton | `<button mat-raised-button>` | `<button mat-raised-button (click)="onSave()">Save</button>` |
| TextBox | `<mat-form-field><input matInput>` | `<mat-form-field><input matInput [(ngModel)]="name"></mat-form-field>` |
| Label | `<mat-label>` | `<mat-label>Client ID</mat-label>` |
| ComboBox | `<mat-select>` | `<mat-select [(ngModel)]="selected"><mat-option>...</mat-option></mat-select>` |
| CheckBox | `<mat-checkbox>` | `<mat-checkbox [(ngModel)]="agreed">I agree</mat-checkbox>` |
| ListBox | `<mat-selection-list>` | `<mat-selection-list><mat-list-option>...</mat-list-option></mat-selection-list>` |
| Frame | `<mat-card>` | `<mat-card><mat-card-title>Group</mat-card-title></mat-card>` |
"""


def _format_detailed_event_mapping() -> str:
    """Format detailed event mapping table"""
    return """
| VB6 Event | Angular Event | Example |
|-----------|---------------|---------|
| Click | `(click)` | `<button (click)="onClick()">` |
| Change | `(change)` | `<input (change)="onChange()">` |
| DblClick | `(dblclick)` | `<div (dblclick)="onDoubleClick()">` |
| GotFocus | `(focus)` | `<input (focus)="onFocus()">` |
| LostFocus | `(blur)` | `<input (blur)="onBlur()">` |
| KeyPress | `(keypress)` | `<input (keypress)="onKeyPress($event)">` |
| Form_Load | `ngOnInit()` | Lifecycle hook - implement in component class |
| Form_Unload | `ngOnDestroy()` | Lifecycle hook - implement in component class |
"""


def _format_validation_rules(validations: List[Dict[str, Any]]) -> str:
    """Format validation rules section"""
    if not validations:
        return "No validations specified in IR"

    lines = []
    for validation in validations:
        field = validation.get('field', 'unknown')
        rule_type = validation.get('rule_type', 'unknown')
        error_msg = validation.get('error_message', '')
        source_lines = validation.get('_source_lines', 'unknown')

        lines.append(f"- **Field**: {field} (VB6 lines {source_lines})")
        lines.append(f"  - Rule: {rule_type}")
        lines.append(f"  - Error: \"{error_msg}\"")
        lines.append(f"  - Angular: {_map_validation_to_angular(rule_type)}")
        lines.append("")

    return "\n".join(lines)


def _map_validation_to_angular(rule_type: str) -> str:
    """Map VB6 validation rule to Angular validator"""
    mapping = {
        "required": "Validators.required",
        "numeric": "Validators.pattern(/^\\d+$/)",
        "custom": "Custom validator function",
        "database_lookup": "Async validator with service call"
    }
    return mapping.get(rule_type, "Custom validator function")

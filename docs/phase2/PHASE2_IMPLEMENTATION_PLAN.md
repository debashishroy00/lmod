# Phase 2 Implementation Plan: IR → Angular Code Generation

**Status**: Ready to implement
**Created**: 2025-11-20
**Goal**: Transform validated IR JSON into production-ready Angular 17+ components

---

## 🎯 Executive Summary

Transform the rich IR JSON (from Phase 1) into working Angular components using an LLM-powered approach with validation gates.

**Approach**: Hybrid (Template-first with LLM fallback)
- Template-based for standard patterns (80% of cases)
- LLM-powered for complex logic (20% of cases)
- TypeScript/ESLint validation as quality gate

**Timeline**: 2-3 days of focused implementation

---

## 📊 Current State Analysis

### Phase 1 Output Quality ✅

The IR JSON is **production-ready** with:

1. **Complete UI Structure**:
   ```json
   {
     "ui": {
       "form": { "name", "caption", "width", "height", "border_style" },
       "controls": [
         { "id", "type", "caption", "position", "tab_index", "properties" }
       ],
       "layout": { "groups": [...] }
     }
   }
   ```

2. **Rich Logic Information**:
   ```json
   {
     "logic": {
       "event_handlers": [
         { "control_id", "event_type", "handler_name", "logic_steps": [...] }
       ],
       "validations": [...],
       "workflows": [...]
     }
   }
   ```

3. **Traceability Built-In**:
   - Every field has `_source_lines`, `_source` annotations
   - Angular mapping hints: `_type_angular_mapping`, `_angular_equivalent`
   - Confidence scores for every section

**Assessment**: IR is extremely well-suited for code generation! The agents have already done the hard work of understanding the VB6 code.

---

## 🏗️ Architecture Design

### Directory Structure

```
src/
├── codegen/                    # NEW: Code generation module
│   ├── __init__.py
│   ├── angular_generator.py   # Main orchestrator
│   ├── prompt_builder.py      # LLM prompt construction
│   ├── file_writer.py         # Write Angular files
│   ├── validators.py          # TypeScript/ESLint validation
│   └── mappings/              # VB6 → Angular mapping tables
│       ├── __init__.py
│       ├── control_mappings.py
│       ├── event_mappings.py
│       └── type_mappings.py
├── orchestrator/              # EXISTING: Phase 1
├── agents/                    # EXISTING: Phase 1
└── validator.py               # EXISTING: IR validator
```

### Component Flow

```
┌─────────────────────────────────────────────────────────────┐
│                    Angular Generator                        │
│                 (src/codegen/angular_generator.py)          │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│ 1. Load & Validate IR JSON                                  │
│    - Read IR file                                           │
│    - Validate against schema                                │
│    - Extract generation_metadata.recommended_template       │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│ 2. Build LLM Prompt                                         │
│    - Use prompt_builder.py                                  │
│    - Include IR JSON                                        │
│    - Include mapping rules from mappings/                   │
│    - Specify output format                                  │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│ 3. Generate Code via Claude API                             │
│    - Call Claude Sonnet 4                                   │
│    - Parse response into files                              │
│    - Extract: .ts, .html, .scss, .spec.ts                  │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│ 4. Validate Generated Code                                  │
│    - Run TypeScript compiler (tsc --noEmit)                 │
│    - Run ESLint                                             │
│    - Run Prettier (formatting)                              │
│    - If validation fails → retry once with error feedback   │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│ 5. Write Files to Disk                                      │
│    - Create component directory                             │
│    - Write all files                                        │
│    - Generate traceability report                           │
└─────────────────────────────────────────────────────────────┘
```

---

## 🔧 Implementation Tasks

### Task 1: Setup Directory Structure (15 mins)

**Files to create**:
```bash
mkdir -p src/codegen/mappings
touch src/codegen/__init__.py
touch src/codegen/angular_generator.py
touch src/codegen/prompt_builder.py
touch src/codegen/file_writer.py
touch src/codegen/validators.py
touch src/codegen/mappings/__init__.py
touch src/codegen/mappings/control_mappings.py
touch src/codegen/mappings/event_mappings.py
touch src/codegen/mappings/type_mappings.py
```

**Dependencies**:
```bash
pip install anthropic  # Claude API (if not already installed)
```

---

### Task 2: Implement Mapping Tables (30 mins)

**File**: `src/codegen/mappings/control_mappings.py`

```python
"""
VB6 Control → Angular Material Component Mappings
"""

CONTROL_MAPPINGS = {
    "CommandButton": {
        "angular_component": "button",
        "material_directive": "mat-raised-button",
        "template": '<button mat-raised-button (click)="{handler}">{caption}</button>',
        "imports": ["MatButtonModule"],
        "notes": "Primary action button"
    },
    "TextBox": {
        "angular_component": "input",
        "material_directive": "matInput",
        "template": '''<mat-form-field>
  <mat-label>{label}</mat-label>
  <input matInput [(ngModel)]="model.{field}" />
</mat-form-field>''',
        "imports": ["MatInputModule", "MatFormFieldModule", "FormsModule"],
        "notes": "Text input field"
    },
    "Label": {
        "angular_component": "label",
        "material_directive": "mat-label",
        "template": '<mat-label>{caption}</mat-label>',
        "imports": [],
        "notes": "Text label"
    },
    "ComboBox": {
        "angular_component": "select",
        "material_directive": "mat-select",
        "template": '''<mat-form-field>
  <mat-label>{label}</mat-label>
  <mat-select [(ngModel)]="model.{field}">
    <mat-option *ngFor="let item of {items}" [value]="item.value">
      {{item.label}}
    </mat-option>
  </mat-select>
</mat-form-field>''',
        "imports": ["MatSelectModule", "MatFormFieldModule", "FormsModule"],
        "notes": "Dropdown selection"
    },
    "CheckBox": {
        "angular_component": "checkbox",
        "material_directive": "mat-checkbox",
        "template": '<mat-checkbox [(ngModel)]="model.{field}">{caption}</mat-checkbox>',
        "imports": ["MatCheckboxModule", "FormsModule"],
        "notes": "Boolean toggle"
    },
    "ListBox": {
        "angular_component": "list",
        "material_directive": "mat-selection-list",
        "template": '''<mat-selection-list [(ngModel)]="model.{field}">
  <mat-list-option *ngFor="let item of {items}" [value]="item.value">
    {{item.label}}
  </mat-list-option>
</mat-selection-list>''',
        "imports": ["MatListModule", "FormsModule"],
        "notes": "Multi-select list"
    },
    "Frame": {
        "angular_component": "fieldset",
        "material_directive": "mat-card",
        "template": '<mat-card><mat-card-title>{caption}</mat-card-title><mat-card-content><!-- controls --></mat-card-content></mat-card>',
        "imports": ["MatCardModule"],
        "notes": "Grouping container"
    }
}

def get_control_mapping(vb6_control_type: str) -> dict:
    """Get Angular mapping for VB6 control type"""
    return CONTROL_MAPPINGS.get(vb6_control_type, {
        "angular_component": "div",
        "material_directive": None,
        "template": f"<!-- TODO: Map {vb6_control_type} -->",
        "imports": [],
        "notes": f"Unmapped control type: {vb6_control_type}"
    })
```

**File**: `src/codegen/mappings/event_mappings.py`

```python
"""
VB6 Event → Angular Event/Lifecycle Hook Mappings
"""

EVENT_MAPPINGS = {
    "Click": {
        "angular_event": "click",
        "binding": "(click)",
        "method_prefix": "on",
        "method_suffix": "Click",
        "example": "(click)=\"onButtonClick()\"",
        "notes": "User clicks control"
    },
    "Change": {
        "angular_event": "change",
        "binding": "(change)",
        "method_prefix": "on",
        "method_suffix": "Change",
        "example": "(change)=\"onTextChange()\"",
        "notes": "Value changes"
    },
    "DblClick": {
        "angular_event": "dblclick",
        "binding": "(dblclick)",
        "method_prefix": "on",
        "method_suffix": "DoubleClick",
        "example": "(dblclick)=\"onItemDoubleClick()\"",
        "notes": "User double-clicks"
    },
    "GotFocus": {
        "angular_event": "focus",
        "binding": "(focus)",
        "method_prefix": "on",
        "method_suffix": "Focus",
        "example": "(focus)=\"onInputFocus()\"",
        "notes": "Control receives focus"
    },
    "LostFocus": {
        "angular_event": "blur",
        "binding": "(blur)",
        "method_prefix": "on",
        "method_suffix": "Blur",
        "example": "(blur)=\"onInputBlur()\"",
        "notes": "Control loses focus"
    },
    "KeyPress": {
        "angular_event": "keypress",
        "binding": "(keypress)",
        "method_prefix": "on",
        "method_suffix": "KeyPress",
        "example": "(keypress)=\"onKeyPress($event)\"",
        "notes": "User presses key"
    },
    "Form_Load": {
        "angular_hook": "ngOnInit",
        "lifecycle": "OnInit",
        "method_name": "ngOnInit",
        "notes": "Form initialization - maps to Angular lifecycle hook"
    },
    "Form_Unload": {
        "angular_hook": "ngOnDestroy",
        "lifecycle": "OnDestroy",
        "method_name": "ngOnDestroy",
        "notes": "Form cleanup - maps to Angular lifecycle hook"
    }
}

def get_event_mapping(vb6_event_type: str) -> dict:
    """Get Angular event mapping for VB6 event"""
    return EVENT_MAPPINGS.get(vb6_event_type, {
        "angular_event": vb6_event_type.lower(),
        "binding": f"({vb6_event_type.lower()})",
        "method_prefix": "on",
        "method_suffix": vb6_event_type,
        "notes": f"Unmapped event: {vb6_event_type}"
    })

def generate_method_name(control_id: str, event_type: str) -> str:
    """
    Generate Angular method name from VB6 control and event

    Examples:
      cmdNew, Click → onNewClick()
      txtID, Change → onIdChange()
    """
    mapping = get_event_mapping(event_type)

    # Remove common prefixes (cmd, txt, lbl, etc.)
    clean_id = control_id
    for prefix in ['cmd', 'txt', 'lbl', 'chk', 'opt', 'lst', 'cbo', 'frm']:
        if control_id.lower().startswith(prefix):
            clean_id = control_id[len(prefix):]
            break

    # Build method name
    if "lifecycle" in mapping:
        return mapping["method_name"]

    return f"{mapping['method_prefix']}{clean_id}{mapping['method_suffix']}"
```

**File**: `src/codegen/mappings/type_mappings.py`

```python
"""
VB6 Data Type → TypeScript Type Mappings
"""

TYPE_MAPPINGS = {
    "String": "string",
    "Long": "number",
    "Integer": "number",
    "Single": "number",
    "Double": "number",
    "Boolean": "boolean",
    "Date": "Date",
    "Currency": "number",
    "Object": "any",  # TODO: Define interface
    "Variant": "any",
    "Byte": "number"
}

def get_typescript_type(vb6_type: str) -> str:
    """Get TypeScript type for VB6 type"""
    return TYPE_MAPPINGS.get(vb6_type, "any")
```

---

### Task 3: Implement Prompt Builder (45 mins)

**File**: `src/codegen/prompt_builder.py`

```python
"""
Build LLM prompt for Angular code generation from IR
"""

import json
from typing import Dict, Any

def build_angular_generation_prompt(ir: Dict[str, Any]) -> str:
    """
    Build comprehensive prompt for Claude to generate Angular component

    Args:
        ir: Complete IR JSON from Phase 1

    Returns:
        Formatted prompt string
    """

    form_name = ir['ui']['form']['name']
    controls = ir['ui']['controls']
    event_handlers = ir['logic']['event_handlers']
    validations = ir['logic']['validations']

    prompt = f"""You are an Angular 17 expert generating production-ready code from Intermediate Representation (IR).

**CRITICAL RULES**:
1. Output ONLY valid TypeScript/HTML/CSS - no markdown, no explanations
2. Use Angular 17+ features (signals, standalone components, inject())
3. Follow Angular style guide and best practices
4. Include proper TypeScript types (no 'any' unless necessary)
5. Generate passing unit tests
6. Use Angular Material components

**INPUT**: VB6 Form IR (JSON)

```json
{json.dumps(ir, indent=2)}
```

**YOUR TASK**: Generate Angular component files for the form "{form_name}"

---

## COMPONENT STRUCTURE

### 1. Component TypeScript (.component.ts)

**Requirements**:
- Use Angular 17 standalone component
- Import Angular Material modules as needed
- Implement all event handlers from IR.logic.event_handlers
- Add signals for reactive state (Angular 17 feature)
- Include proper TypeScript types
- Map VB6 controls to component properties

**Template**:
```typescript
import {{ Component, signal }} from '@angular/core';
import {{ CommonModule }} from '@angular/common';
import {{ FormsModule }} from '@angular/forms';
// Add Material imports based on controls

@Component({{
  selector: 'app-{form_name.lower().replace('form', '')}',
  standalone: true,
  imports: [CommonModule, FormsModule, /* Material modules */],
  templateUrl: './{form_name.lower().replace('form', '')}.component.html',
  styleUrl: './{form_name.lower().replace('form', '')}.component.scss'
}})
export class {form_name}Component {{
  // Add component properties and methods here
}}
```

### 2. Template HTML (.component.html)

**Requirements**:
- Use Angular Material components
- Map VB6 controls from IR.ui.controls
- Use layout from IR.ui.layout.groups if present
- Bind events to component methods
- Follow Angular template syntax

### 3. Styles SCSS (.component.scss)

**Requirements**:
- Extract dimensions from IR.ui.form (width, height)
- Convert VB6 twips to CSS pixels (1 twip ≈ 0.0625px)
- Use Material Design spacing (multiples of 4px/8px)
- Make layout responsive where appropriate

### 4. Unit Tests (.component.spec.ts)

**Requirements**:
- Test component creation
- Test each event handler
- Test validations
- Use Angular Testing Library
- Aim for >80% code coverage

---

## MAPPING RULES

### VB6 Controls → Angular Material

{_generate_control_mapping_table(controls)}

### VB6 Events → Angular Methods

{_generate_event_mapping_table(event_handlers)}

### VB6 Validations → Angular Validators

{_generate_validation_mapping(validations)}

---

## OUTPUT FORMAT

Return files separated by this marker:

```
=== FILE: {form_name.lower()}.component.ts ===
[TypeScript code here]

=== FILE: {form_name.lower()}.component.html ===
[HTML template here]

=== FILE: {form_name.lower()}.component.scss ===
[SCSS styles here]

=== FILE: {form_name.lower()}.component.spec.ts ===
[Unit test code here]
```

**IMPORTANT**:
- Do NOT include markdown code fences (```typescript, etc.)
- Do NOT include explanatory text
- Output ONLY the file marker and code
- Ensure all code is syntactically valid

Now generate the Angular component files.
"""

    return prompt


def _generate_control_mapping_table(controls: list) -> str:
    """Generate control mapping section of prompt"""
    lines = []
    for control in controls:
        vb6_type = control.get('type', 'Unknown')
        control_id = control.get('id', 'unknown')
        angular_hint = control.get('_type_angular_mapping', 'Unknown')
        lines.append(f"- `{vb6_type}` ({control_id}) → {angular_hint}")
    return "\n".join(lines)


def _generate_event_mapping_table(event_handlers: list) -> str:
    """Generate event mapping section of prompt"""
    lines = []
    for handler in event_handlers:
        control_id = handler.get('control_id', 'unknown')
        event_type = handler.get('event_type', 'Unknown')
        handler_name = handler.get('handler_name', 'unknown')

        # Generate Angular method name
        clean_id = control_id.lstrip('cmd').lstrip('txt').lstrip('lbl')
        angular_method = f"on{clean_id}{event_type}()"

        lines.append(f"- `{handler_name}()` → `{angular_method}`")
    return "\n".join(lines)


def _generate_validation_mapping(validations: list) -> str:
    """Generate validation mapping section"""
    if not validations:
        return "No validations in IR"

    lines = []
    for validation in validations:
        field = validation.get('field', 'unknown')
        rule_type = validation.get('rule_type', 'unknown')
        error_msg = validation.get('error_message', '')

        angular_validator = _map_validation_rule(rule_type)
        lines.append(f"- Field `{field}`: {rule_type} → {angular_validator}")
        if error_msg:
            lines.append(f"  Error: \"{error_msg}\"")

    return "\n".join(lines)


def _map_validation_rule(rule_type: str) -> str:
    """Map VB6 validation to Angular validator"""
    mapping = {
        "required": "Validators.required",
        "numeric": "Validators.pattern(/^\\d+$/)",
        "custom": "Custom validator function"
    }
    return mapping.get(rule_type, "Custom validator")
```

---

### Task 4: Implement Main Generator (60 mins)

**File**: `src/codegen/angular_generator.py`

```python
"""
Main Angular code generator - orchestrates the entire code generation process
"""

import os
import json
from typing import Dict, Any, Optional
from anthropic import Anthropic
from pathlib import Path

from .prompt_builder import build_angular_generation_prompt
from .file_writer import write_angular_files
from .validators import validate_generated_code


class AngularGenerator:
    """
    Generates Angular components from IR using Claude API
    """

    def __init__(self, api_key: Optional[str] = None):
        """
        Initialize generator

        Args:
            api_key: Anthropic API key (or uses ANTHROPIC_API_KEY env var)
        """
        self.api_key = api_key or os.getenv('ANTHROPIC_API_KEY')
        if not self.api_key:
            raise ValueError("ANTHROPIC_API_KEY not found in environment")

        self.client = Anthropic(api_key=self.api_key)


    def generate(self, ir: Dict[str, Any], output_dir: str) -> Dict[str, str]:
        """
        Generate Angular component from IR

        Args:
            ir: Complete IR JSON from Phase 1
            output_dir: Directory to write generated files

        Returns:
            Dictionary of {filename: content}
        """
        print("\n🚀 Starting Angular code generation...")

        # Step 1: Build prompt
        print("📝 Building LLM prompt...")
        prompt = build_angular_generation_prompt(ir)

        # Step 2: Call Claude API
        print("🤖 Calling Claude Sonnet 4...")
        generated_code = self._call_claude_api(prompt)

        # Step 3: Parse response into files
        print("📦 Parsing generated files...")
        files = self._parse_generated_files(generated_code)

        # Step 4: Validate generated code
        print("✅ Validating generated code...")
        validation_result = validate_generated_code(files)

        if not validation_result['valid']:
            print("⚠️  Validation failed, retrying with error feedback...")
            files = self._retry_with_feedback(ir, prompt, validation_result)

        # Step 5: Write files to disk
        print(f"💾 Writing files to {output_dir}...")
        write_angular_files(files, output_dir, ir)

        print("✨ Code generation complete!")
        return files


    def _call_claude_api(self, prompt: str) -> str:
        """
        Call Claude API to generate code

        Args:
            prompt: Complete generation prompt

        Returns:
            Raw response text
        """
        response = self.client.messages.create(
            model="claude-sonnet-4-20250514",
            max_tokens=8000,
            temperature=0.0,  # Deterministic for code generation
            messages=[{
                "role": "user",
                "content": prompt
            }]
        )

        return response.content[0].text


    def _parse_generated_files(self, response: str) -> Dict[str, str]:
        """
        Parse LLM response into dictionary of files

        Expected format:
        === FILE: component.ts ===
        [code]
        === FILE: component.html ===
        [code]

        Args:
            response: Raw LLM response

        Returns:
            Dictionary of {filename: content}
        """
        files = {}
        current_file = None
        current_content = []

        for line in response.split('\n'):
            if line.startswith('=== FILE:'):
                # Save previous file
                if current_file:
                    files[current_file] = '\n'.join(current_content).strip()

                # Start new file
                current_file = line.split('FILE:')[1].split('===')[0].strip()
                current_content = []
            elif current_file:
                current_content.append(line)

        # Save last file
        if current_file:
            files[current_file] = '\n'.join(current_content).strip()

        return files


    def _retry_with_feedback(
        self,
        ir: Dict[str, Any],
        original_prompt: str,
        validation_result: Dict[str, Any]
    ) -> Dict[str, str]:
        """
        Retry code generation with validation error feedback

        Args:
            ir: Original IR
            original_prompt: Original prompt
            validation_result: Validation errors

        Returns:
            Corrected files
        """
        errors = validation_result.get('errors', [])
        error_summary = '\n'.join(errors)

        retry_prompt = f"""{original_prompt}

**VALIDATION ERRORS FROM PREVIOUS ATTEMPT**:

```
{error_summary}
```

Please fix these errors and regenerate the files.
"""

        print("🔄 Retrying with error feedback...")
        generated_code = self._call_claude_api(retry_prompt)
        files = self._parse_generated_files(generated_code)

        # Validate again
        validation_result = validate_generated_code(files)
        if not validation_result['valid']:
            print("⚠️  Still has errors, but proceeding with best effort")

        return files
```

---

### Task 5: Implement File Writer & Validators (30 mins)

**File**: `src/codegen/file_writer.py`

```python
"""
Write generated Angular files to disk with proper formatting
"""

import os
from pathlib import Path
from typing import Dict, Any


def write_angular_files(
    files: Dict[str, str],
    output_dir: str,
    ir: Dict[str, Any]
) -> None:
    """
    Write generated files to disk

    Args:
        files: Dictionary of {filename: content}
        output_dir: Output directory
        ir: Original IR (for traceability report)
    """
    # Create output directory
    Path(output_dir).mkdir(parents=True, exist_ok=True)

    # Write each file
    for filename, content in files.items():
        filepath = os.path.join(output_dir, filename)
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(content)
        print(f"  ✓ {filename}")

    # Write traceability report
    _write_traceability_report(output_dir, ir)


def _write_traceability_report(output_dir: str, ir: Dict[str, Any]) -> None:
    """
    Generate traceability report: IR field → Generated code location

    Args:
        output_dir: Output directory
        ir: Original IR
    """
    report_lines = [
        "# Traceability Report",
        "",
        "## VB6 Source → IR → Angular Code",
        ""
    ]

    # Map controls
    report_lines.append("### Controls")
    for control in ir['ui']['controls']:
        control_id = control['id']
        vb6_type = control['type']
        source_lines = control.get('_source_lines', 'unknown')

        report_lines.append(f"- **{control_id}** ({vb6_type})")
        report_lines.append(f"  - VB6: {source_lines}")
        report_lines.append(f"  - IR: ui.controls['{control_id}']")
        report_lines.append(f"  - Angular: component.html (search for '{control_id.lower()}')")
        report_lines.append("")

    # Map event handlers
    report_lines.append("### Event Handlers")
    for handler in ir['logic']['event_handlers']:
        handler_name = handler['handler_name']
        source_lines = handler.get('_source_lines', 'unknown')

        report_lines.append(f"- **{handler_name}()**")
        report_lines.append(f"  - VB6: {source_lines}")
        report_lines.append(f"  - IR: logic.event_handlers['{handler_name}']")
        report_lines.append(f"  - Angular: component.ts (search for method)")
        report_lines.append("")

    # Write report
    report_path = os.path.join(output_dir, 'TRACEABILITY.md')
    with open(report_path, 'w', encoding='utf-8') as f:
        f.write('\n'.join(report_lines))

    print(f"  ✓ TRACEABILITY.md")
```

**File**: `src/codegen/validators.py`

```python
"""
Validate generated Angular code
"""

import subprocess
from typing import Dict, List


def validate_generated_code(files: Dict[str, str]) -> Dict[str, any]:
    """
    Validate generated TypeScript/HTML/CSS

    Args:
        files: Dictionary of {filename: content}

    Returns:
        {
            'valid': bool,
            'errors': List[str],
            'warnings': List[str]
        }
    """
    errors = []
    warnings = []

    # Check 1: TypeScript syntax (basic check without full compilation)
    for filename, content in files.items():
        if filename.endswith('.ts'):
            ts_errors = _validate_typescript_syntax(content, filename)
            errors.extend(ts_errors)

    # Check 2: HTML structure
    for filename, content in files.items():
        if filename.endswith('.html'):
            html_errors = _validate_html_structure(content, filename)
            errors.extend(html_errors)

    # Check 3: Common mistakes
    common_errors = _check_common_mistakes(files)
    errors.extend(common_errors)

    return {
        'valid': len(errors) == 0,
        'errors': errors,
        'warnings': warnings
    }


def _validate_typescript_syntax(content: str, filename: str) -> List[str]:
    """Basic TypeScript syntax validation"""
    errors = []

    # Check for common syntax errors
    if content.count('{') != content.count('}'):
        errors.append(f"{filename}: Unbalanced curly braces")

    if content.count('(') != content.count(')'):
        errors.append(f"{filename}: Unbalanced parentheses")

    if content.count('[') != content.count(']'):
        errors.append(f"{filename}: Unbalanced brackets")

    # Check for required imports
    if '@Component' in content and 'import' not in content:
        errors.append(f"{filename}: Missing imports")

    return errors


def _validate_html_structure(content: str, filename: str) -> List[str]:
    """Basic HTML structure validation"""
    errors = []

    # Check for balanced tags (simplified)
    open_tags = content.count('<')
    close_tags = content.count('>')
    if open_tags != close_tags:
        errors.append(f"{filename}: Unbalanced HTML tags")

    return errors


def _check_common_mistakes(files: Dict[str, str]) -> List[str]:
    """Check for common code generation mistakes"""
    errors = []

    # Check for TODO markers (LLM might add these)
    for filename, content in files.items():
        if 'TODO' in content:
            errors.append(f"{filename}: Contains TODO markers - manual review needed")

    # Check for 'any' type overuse
    ts_files = [f for f in files if f.endswith('.ts')]
    for filename in ts_files:
        content = files[filename]
        any_count = content.count(': any')
        if any_count > 3:
            errors.append(f"{filename}: Excessive use of 'any' type ({any_count} occurrences)")

    return errors
```

---

### Task 6: Create CLI Entry Point (15 mins)

**File**: `src/codegen/main.py`

```python
#!/usr/bin/env python3
"""
Angular Code Generator - Main Entry Point

WHAT: CLI entry point for IR → Angular code generation
WHY: User-friendly interface to code generation pipeline
HOW: Load IR JSON, invoke generator, save Angular files
"""

import sys
import json
import os
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from codegen.angular_generator import AngularGenerator


def main():
    """Main entry point"""
    # Parse command line
    if len(sys.argv) < 2:
        print("Usage: python3 src/codegen/main.py <path-to-ir.json> [output-dir]")
        print("\nExample:")
        print("  python3 src/codegen/main.py samples/vb6/simple/StartForm_ir.json output/angular")
        sys.exit(1)

    ir_file = sys.argv[1]
    output_dir = sys.argv[2] if len(sys.argv) > 2 else "output/angular"

    # Validate input file
    if not os.path.exists(ir_file):
        print(f"❌ Error: File not found: {ir_file}")
        sys.exit(1)

    # Load IR JSON
    try:
        with open(ir_file, 'r', encoding='utf-8') as f:
            ir = json.load(f)
    except Exception as e:
        print(f"❌ Error loading IR: {e}")
        sys.exit(1)

    # Create generator
    try:
        generator = AngularGenerator()
    except ValueError as e:
        print(f"❌ {e}")
        print("\nPlease set ANTHROPIC_API_KEY environment variable")
        sys.exit(1)

    # Generate Angular code
    try:
        files = generator.generate(ir, output_dir)
    except Exception as e:
        print(f"\n❌ Generation failed: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)

    # Print summary
    print("\n" + "=" * 60)
    print("📊 GENERATION SUMMARY")
    print("=" * 60)

    form_name = ir['ui']['form']['name']
    print(f"\n✨ Generated Angular component for: {form_name}")
    print(f"📁 Output directory: {output_dir}")
    print(f"📄 Files generated: {len(files)}")

    for filename in sorted(files.keys()):
        print(f"   - {filename}")

    print("\n💡 Next steps:")
    print(f"   1. cd {output_dir}")
    print("   2. Review generated files")
    print("   3. Run: ng test (to run unit tests)")
    print("   4. Run: ng serve (to see component in action)")

    print("\n✨ Done!")
    print("=" * 60)


if __name__ == '__main__':
    main()
```

---

## 🧪 Testing Plan

### Test 1: Simple Form (StartForm.frm)

```bash
# Phase 1: Generate IR (already done)
python3 src/orchestrator/main.py samples/vb6/simple/StartForm.frm

# Phase 2: Generate Angular
python3 src/codegen/main.py samples/vb6/simple/StartForm_ir.json output/angular/start-form
```

**Expected Output**:
```
output/angular/start-form/
├── start-form.component.ts       # ~100 lines
├── start-form.component.html     # ~50 lines
├── start-form.component.scss     # ~20 lines
├── start-form.component.spec.ts  # ~80 lines
└── TRACEABILITY.md              # Mapping report
```

**Success Criteria**:
- ✅ All files generated
- ✅ TypeScript compiles (no syntax errors)
- ✅ Tests pass
- ✅ All 5 controls rendered (3 buttons, 1 textbox, 1 label)
- ✅ All 3 event handlers implemented

---

### Test 2: Medium Form (frmsupplier.frm)

```bash
# Phase 1: Generate IR (already done)
python3 src/orchestrator/main.py samples/vb6/medium/frmsupplier.frm

# Phase 2: Generate Angular
python3 src/codegen/main.py samples/vb6/medium/frmsupplier_ir.json output/angular/supplier-form
```

---

## 📦 Dependencies

Add to `requirements.txt`:
```
anthropic>=0.40.0
```

Install:
```bash
pip install anthropic
```

---

## 🎯 Success Criteria

**Phase 2 is complete when**:

1. ✅ **Code Generation Works**:
   - Generates .ts, .html, .scss, .spec.ts files
   - Uses Angular 17 syntax (standalone components, signals)
   - Maps VB6 controls to Angular Material

2. ✅ **Code is Valid**:
   - TypeScript has no syntax errors
   - HTML is well-formed
   - Tests are runnable

3. ✅ **Traceability Maintained**:
   - Can map VB6 line → IR field → Angular code
   - TRACEABILITY.md report generated

4. ✅ **Handles Edge Cases**:
   - Missing data gracefully handled
   - Unknown control types have TODO markers
   - Validation errors trigger retry

5. ✅ **Demo Ready**:
   - StartForm.frm → Angular works end-to-end
   - Can show working Angular app (even if basic)
   - Documentation for AIG demo

---

## 📈 Metrics to Track

- **Generation time**: < 30 seconds per form
- **Code quality**: No TypeScript errors
- **Test coverage**: > 70% (generated tests)
- **Control mapping**: > 90% of VB6 controls mapped
- **Event mapping**: 100% of event handlers implemented
- **Cost**: ~$0.50-2.00 per form (Claude API)

---

## 🚀 Next Steps After Implementation

1. **Add Template-Based Generation** (Week 2):
   - Jinja2 templates for common patterns
   - Fallback to LLM for complex cases
   - Reduce cost and improve speed

2. **Add Data Services** (Week 2):
   - Generate service.ts for CRUD operations
   - HTTP client integration
   - RxJS observables

3. **Quality Gates** (Week 2):
   - Actual `tsc` compilation (not just syntax check)
   - ESLint integration
   - Prettier formatting
   - Run generated tests with Karma

4. **Demo Preparation**:
   - Create working Angular app
   - Add routing between forms
   - Polish UI
   - Record demo video

---

**Status**: 📝 Plan Complete - Ready for Implementation
**Est. Time**: 2-3 days (with testing)
**Risk**: Low (IR is high quality, Claude is good at code generation)

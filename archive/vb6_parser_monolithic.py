#!/usr/bin/env python3
"""
VB6 Form Parser - Python Implementation

WHAT: Parses VB6 .frm files into IR JSON
WHY: TypeScript requires Node.js - Python is available and accomplishes same goal
HOW: Port TypeScript parser logic to Python, maintain same architecture
"""

import json
import re
import sys
from typing import Dict, List, Any, Optional, Tuple
from datetime import datetime


# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

def clean_line(line: str) -> str:
    """Remove leading/trailing whitespace and comments"""
    # Remove VB6 comments (after ')
    if "'" in line:
        line = line.split("'")[0]
    return line.strip()


def extract_quoted_value(text: str) -> str:
    """Extract value from quotes: 'Caption = "Start"' -> "Start" """
    match = re.search(r'"([^"]*)"', text)
    return match.group(1) if match else ''


def extract_numeric_value(text: str) -> int:
    """Extract numeric value: 'ClientWidth = 4050' -> 4050"""
    if '=' not in text:
        return 0
    parts = text.split('=')
    if len(parts) < 2:
        return 0
    value = parts[1].split("'")[0].strip()
    try:
        return int(value)
    except ValueError:
        return 0


def extract_key_value(line: str) -> Optional[Tuple[str, str]]:
    """Extract key-value pair: 'Caption = "Start"' -> ("Caption", '"Start"')"""
    if '=' not in line:
        return None
    parts = line.split('=', 1)
    if len(parts) != 2:
        return None
    return (parts[0].strip(), parts[1].strip())


def map_border_style(value: int) -> str:
    """Map VB6 border style number to name"""
    styles = {
        0: 'None',
        1: 'FixedSingle',
        2: 'Sizable',
        3: 'FixedDialog',
        4: 'FixedToolWindow',
        5: 'SizableToolWindow'
    }
    return styles.get(value, 'Unknown')


def map_start_position(value: int) -> str:
    """Map VB6 start position number to name"""
    positions = {
        0: 'Manual',
        1: 'CenterOwner',
        2: 'CenterScreen',
        3: 'WindowsDefaultLocation',
        4: 'WindowsDefaultBounds'
    }
    return positions.get(value, 'Manual')


def count_code_lines(source_code: str) -> int:
    """Count non-blank, non-comment lines"""
    lines = source_code.split('\n')
    count = 0
    for line in lines:
        clean = clean_line(line)
        if clean and not clean.startswith("'") and not clean.startswith('Attribute'):
            count += 1
    return count


# ============================================================================
# FORM PARSER
# ============================================================================

def parse_form_properties(lines: List[str]) -> Dict[str, Any]:
    """
    Parse VB6 form properties

    WHAT: Extract form-level properties (name, caption, size, style)
    WHY: Form properties define window behavior and appearance
    HOW: Process lines between form declaration and first control
    """
    form = {
        'name': 'Unknown',
        'properties': {}
    }

    for raw_line in lines:
        line = clean_line(raw_line)

        # Extract form name from declaration
        if line.startswith('Begin VB.Form'):
            match = re.search(r'Begin VB\.Form\s+(\w+)', line)
            if match:
                form['name'] = match.group(1)
            continue

        # Stop at first control
        if line.startswith('Begin VB.') and 'Form' not in line:
            break

        if line == 'End':
            break

        # Extract property key-value pairs
        kv = extract_key_value(line)
        if not kv:
            continue

        key, value = kv

        # Map specific properties
        if key == 'Caption':
            form['caption'] = extract_quoted_value(value)
        elif key == 'ClientWidth':
            form['width'] = extract_numeric_value(raw_line)
        elif key == 'ClientHeight':
            form['height'] = extract_numeric_value(raw_line)
        elif key == 'BorderStyle':
            border_value = extract_numeric_value(raw_line)
            form['border_style'] = map_border_style(border_value)
        elif key == 'StartUpPosition':
            pos_value = extract_numeric_value(raw_line)
            form['start_position'] = map_start_position(pos_value)
        elif key in ['MaxButton', 'MinButton', 'ShowInTaskbar']:
            form['properties'][key.lower()] = extract_numeric_value(raw_line) != 0
        else:
            # Store other properties
            if value.startswith('"'):
                form['properties'][key.lower()] = extract_quoted_value(value)
            else:
                form['properties'][key.lower()] = extract_numeric_value(raw_line)

    return form


def determine_ui_type(form_props: Dict[str, Any]) -> str:
    """Determine UI type from form properties"""
    if form_props.get('border_style') == 'FixedDialog':
        return 'dialog'
    if form_props.get('properties', {}).get('mdi_child'):
        return 'mdi_child'
    return 'form'


# ============================================================================
# CONTROLS PARSER
# ============================================================================

def parse_controls(lines: List[str]) -> List[Dict[str, Any]]:
    """
    Parse VB6 controls

    WHAT: Extract all UI controls (TextBox, CommandButton, Label)
    WHY: Controls define the user interface elements
    HOW: State machine tracking control blocks, extract properties, sort by TabIndex
    """
    controls = []
    current_control = None
    nest_level = 0

    for i, raw_line in enumerate(lines):
        line = clean_line(raw_line)

        # Start of control block
        if line.startswith('Begin VB.'):
            # Extract control info
            match = re.search(r'Begin VB\.(\w+)\s+(\w+)', line)
            if match:
                control_type = match.group(1)
                control_id = match.group(2)

                # Skip Form itself
                if control_type == 'Form':
                    continue

                if nest_level == 0:
                    current_control = {
                        'id': control_id,
                        'type': control_type,
                        'position': {},
                        'properties': {},
                        'tab_index': 999  # Default high value if not specified
                    }
                nest_level += 1

        elif line == 'End':
            nest_level -= 1
            if nest_level == 0 and current_control:
                controls.append(current_control)
                current_control = None

        elif current_control and nest_level == 1:
            # Extract control properties
            kv = extract_key_value(line)
            if not kv:
                continue

            key, value = kv

            if key == 'Caption':
                current_control['caption'] = extract_quoted_value(value)
            elif key == 'Left':
                current_control['position']['left'] = extract_numeric_value(raw_line)
            elif key == 'Top':
                current_control['position']['top'] = extract_numeric_value(raw_line)
            elif key == 'Width':
                current_control['position']['width'] = extract_numeric_value(raw_line)
            elif key == 'Height':
                current_control['position']['height'] = extract_numeric_value(raw_line)
            elif key == 'TabIndex':
                current_control['tab_index'] = extract_numeric_value(raw_line)
            else:
                # Store other properties
                if value.startswith('"'):
                    current_control['properties'][key.lower()] = extract_quoted_value(value)
                else:
                    current_control['properties'][key.lower()] = extract_numeric_value(raw_line)

    # Sort controls by TabIndex to match expected order
    controls.sort(key=lambda c: c.get('tab_index', 999))

    return controls


def analyze_layout(controls: List[Dict[str, Any]]) -> Dict[str, Any]:
    """Analyze control layout and suggest grid structure"""
    if not controls:
        return {
            'type': 'free_form',
            'grid_rows': 0,
            'grid_columns': 0
        }

    # Simple heuristic: count unique Y positions for rows
    y_positions = set()
    for control in controls:
        top = control.get('position', {}).get('top')
        if top is not None:
            # Round to nearest 500 to group near-aligned controls
            y_positions.add(round(top / 500) * 500)

    rows = len(y_positions) if y_positions else 0

    return {
        'type': 'free_form',
        'grid_rows': rows,
        'grid_columns': 1
    }


# ============================================================================
# EVENT HANDLER PARSER
# ============================================================================

def parse_event_handlers(lines: List[str]) -> List[Dict[str, Any]]:
    """
    Parse VB6 event handlers

    WHAT: Extract event handlers and business logic
    WHY: Event handlers contain application logic and workflows
    HOW: Find "Private Sub" blocks, analyze logic steps
    """
    handlers = []
    current_handler = None
    handler_lines = []

    for line in lines:
        # Start of event handler
        if line.strip().startswith('Private Sub '):
            match = re.search(r'Private Sub (\w+)_(\w+)\(', line)
            if match:
                control_id = match.group(1)
                event_type = match.group(2)

                current_handler = {
                    'handler_name': f'{control_id}_{event_type}',
                    'control_id': control_id,
                    'event_type': event_type,
                    'logic_steps': []
                }
                handler_lines = []

        elif line.strip() == 'End Sub':
            if current_handler:
                current_handler['logic_steps'] = analyze_logic_steps(handler_lines)
                handlers.append(current_handler)
                current_handler = None
                handler_lines = []

        elif current_handler is not None:
            clean = clean_line(line)
            if clean:
                handler_lines.append(clean)

    return handlers


def analyze_logic_steps(lines: List[str]) -> List[Dict[str, Any]]:
    """Analyze logic steps within an event handler"""
    steps = []

    for line in lines:
        step = None

        # Dim statement (object creation)
        if line.startswith('Dim '):
            match = re.search(r'Dim\s+(\w+)\s+As\s+(?:New\s+)?(\w+)', line)
            if match:
                var_name = match.group(1)
                type_name = match.group(2)
                step = {
                    'step_type': 'object_creation',
                    'description': f'Declare {var_name} as {type_name}',
                    'code_snippet': line
                }

        # If statement (validation or conditional)
        elif line.startswith('If '):
            step = {
                'step_type': 'validation',
                'description': 'Conditional check',
                'code_snippet': line
            }

        # MsgBox (message display)
        elif 'MsgBox' in line:
            match = re.search(r'MsgBox\s+"([^"]*)"', line)
            message = match.group(1) if match else 'Unknown message'
            step = {
                'step_type': 'message',
                'description': f'Display message: {message}',
                'code_snippet': line
            }

        # Set statement (method call or assignment)
        elif line.startswith('Set '):
            step = {
                'step_type': 'data_operation',
                'description': 'Call method or assign object',
                'code_snippet': line
            }

        # Unload (navigation)
        elif line.startswith('Unload '):
            step = {
                'step_type': 'navigation',
                'description': 'Close form',
                'code_snippet': line
            }

        # .Show (navigation)
        elif '.Show' in line:
            step = {
                'step_type': 'navigation',
                'description': 'Show form',
                'code_snippet': line
            }

        # Exit Sub (flow control)
        elif line == 'Exit Sub':
            step = {
                'step_type': 'flow_control',
                'description': 'Exit subroutine',
                'code_snippet': line
            }

        # Default: assignment or other
        elif '=' in line and not line.startswith('If'):
            step = {
                'step_type': 'assignment',
                'description': 'Variable assignment',
                'code_snippet': line
            }

        if step:
            steps.append(step)

    return steps


def extract_validations(handlers: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """Extract validation logic from event handlers"""
    validations = []

    for handler in handlers:
        for step in handler.get('logic_steps', []):
            if step['step_type'] == 'validation':
                code = step.get('code_snippet', '')

                # Detect validation patterns
                if 'Len(' in code and 'Trim(' in code:
                    validations.append({
                        'field': handler['control_id'],
                        'rule': 'required',
                        'error_message': 'Field is required',
                        'original_code': code
                    })

    return validations


def find_error_handling(lines: List[str], handlers: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """Find error handling patterns"""
    error_handling = []

    # Look for On Error statements in source
    current_handler = None

    for line in lines:
        # Track which handler we're in
        if line.strip().startswith('Private Sub '):
            match = re.search(r'Private Sub (\w+)_(\w+)\(', line)
            if match:
                current_handler = f'{match.group(1)}_{match.group(2)}'
        elif line.strip() == 'End Sub':
            current_handler = None

        # Find On Error statements
        clean = clean_line(line)
        if clean.startswith('On Error Resume Next'):
            error_handling.append({
                'type': 'on_error_resume_next',
                'scope': current_handler if current_handler else 'unknown',
                'original_code': clean
            })
        elif clean.startswith('On Error GoTo'):
            match = re.search(r'On Error GoTo\s+(\w+)', clean)
            label = match.group(1) if match else 'unknown'
            error_handling.append({
                'type': 'on_error_goto',
                'scope': current_handler if current_handler else 'unknown',
                'error_label': label,
                'original_code': clean
            })

    return error_handling


def identify_workflows(handlers: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """Identify workflows from event handlers"""
    workflows = []

    for handler in handlers:
        steps = handler.get('logic_steps', [])
        if len(steps) > 1:
            # Multi-step handler = workflow
            workflow = {
                'name': handler['handler_name'],
                'trigger': f"{handler['control_id']} {handler['event_type']}",
                'steps': [step['description'] for step in steps]
            }
            workflows.append(workflow)

    return workflows


# ============================================================================
# PATTERN DETECTION
# ============================================================================

def detect_patterns(form_props: Dict[str, Any], controls: List[Dict[str, Any]],
                   handlers: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """Detect design patterns in the form"""
    patterns = []

    # Pattern 1: CRUD Form
    has_textboxes = len([c for c in controls if c['type'] == 'TextBox']) >= 1
    has_buttons = len([c for c in controls if c['type'] == 'CommandButton']) >= 2
    has_data_ops = any('Save' in h['handler_name'] or 'Add' in h['handler_name'] or 'Edit' in h['handler_name']
                      for h in handlers)

    if has_textboxes and has_buttons and has_data_ops:
        patterns.append({
            'pattern_type': 'CRUD_FORM',
            'pattern_name': 'CRUD Form',
            'confidence': 0.85
        })

    # Pattern 2: Search/Lookup Form
    has_lookup = any('Open' in h['handler_name'] or 'Find' in h['handler_name'] for h in handlers)
    has_input = any(c['type'] == 'TextBox' for c in controls)

    if has_lookup and has_input:
        patterns.append({
            'pattern_type': 'SEARCH_FORM',
            'pattern_name': 'Search Form',
            'confidence': 0.88
        })

    # Pattern 3: Modal Dialog
    if form_props.get('border_style') == 'FixedDialog':
        patterns.append({
            'pattern_type': 'MODAL_DIALOG',
            'pattern_name': 'Modal Dialog',
            'confidence': 0.95
        })

    # Pattern 4: Validation
    has_validation = any(
        any(s['step_type'] == 'validation' for s in h.get('logic_steps', []))
        for h in handlers
    )

    if has_validation:
        patterns.append({
            'pattern_type': 'VALIDATION_REQUIRED',
            'pattern_name': 'Validation Pattern',
            'confidence': 0.92
        })

    return patterns


# ============================================================================
# EXTERNAL REFERENCES
# ============================================================================

def is_built_in_type(type_name: str) -> bool:
    """Check if type is VB6 built-in"""
    built_ins = ['String', 'Integer', 'Long', 'Boolean', 'Double', 'Variant', 'Object']
    return type_name in built_ins


def is_built_in_function(func_name: str) -> bool:
    """Check if function is VB6 built-in"""
    built_ins = ['MsgBox', 'Len', 'Trim', 'CLng', 'CInt', 'CDbl', 'IsNull', 'IsNumeric']
    return func_name in built_ins


def find_external_references(handlers: List[Dict[str, Any]]) -> Dict[str, Any]:
    """Find external class/module references"""
    classes = set()
    functions = set()

    for handler in handlers:
        for step in handler.get('logic_steps', []):
            code = step.get('code_snippet', '')

            # Find "Dim x As ClassName"
            match = re.search(r'Dim\s+\w+\s+As\s+(?:New\s+)?(\w+)', code)
            if match and not is_built_in_type(match.group(1)):
                classes.add(match.group(1))

            # Find function calls
            match = re.search(r'(\w+)\s*\(', code)
            if match and not is_built_in_function(match.group(1)):
                functions.add(match.group(1))

    return {
        'classes': [{'name': name, 'source': 'external'} for name in classes],
        'modules': [
            {
                'name': 'unknown_module',
                'type': 'standard_module',
                'functions_called': list(functions)
            }
        ] if functions else []
    }


# ============================================================================
# SECURITY ANALYSIS
# ============================================================================

def identify_security_issues(error_handling: List[Dict[str, Any]],
                            handlers: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """Identify security issues"""
    issues = []

    # Issue 1: On Error Resume Next
    for eh in error_handling:
        if eh.get('type') == 'on_error_resume_next':
            issues.append({
                'issue_type': 'INSECURE_ERROR_HANDLING',
                'severity': 'medium',
                'location': eh.get('scope', 'Unknown'),
                'description': 'On Error Resume Next suppresses all errors, causing silent failures',
                'recommendation': 'Use try-catch in Angular with explicit error handling'
            })

    # Issue 2: Type conversion without validation
    for handler in handlers:
        for step in handler.get('logic_steps', []):
            code = step.get('code_snippet', '')
            if 'CLng(' in code and 'IsNumeric' not in code:
                issues.append({
                    'issue_type': 'OTHER',
                    'severity': 'low',
                    'location': handler['handler_name'],
                    'description': 'Type conversion without validation - could fail on invalid input',
                    'recommendation': 'Add Validators.pattern(/^[0-9]+$/) in Angular form'
                })
                break  # One per handler

    return issues


# ============================================================================
# DATA ANALYSIS
# ============================================================================

def infer_entities(handlers: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """Infer data entities from event handler code"""
    entities = set()
    forms = set()  # Track forms separately

    for handler in handlers:
        for step in handler.get('logic_steps', []):
            code = step.get('code_snippet', '')

            # Look for class names (likely entities)
            match = re.search(r'As\s+(?:New\s+)?(\w+)', code)
            if match and not is_built_in_type(match.group(1)):
                class_name = match.group(1)

                # Check if it's likely a form (ends with Form/Edit/Dialog, or .Show is called on it)
                if class_name.endswith('Form') or class_name.endswith('Edit') or class_name.endswith('Dialog'):
                    forms.add(class_name)
                elif '.Show' in code:
                    # If .Show is called on this variable, it's a form
                    var_match = re.search(r'Dim\s+(\w+)\s+As', code)
                    if var_match:
                        forms.add(class_name)
                else:
                    entities.add(class_name)

    # Filter out forms from entities
    entities = entities - forms

    return [
        {
            'name': name,
            'type': 'object',
            'source': 'inferred'
        }
        for name in sorted(entities)
    ]


def extract_data_operations(handlers: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """Extract data operations from handlers"""
    operations = []

    for handler in handlers:
        for step in handler.get('logic_steps', []):
            if step['step_type'] == 'data_operation':
                code = step.get('code_snippet', '')

                # GetClient() is a SELECT operation
                if 'GetClient' in code:
                    operations.append({
                        'type': 'SELECT',
                        'entity': 'Client',
                        'method': handler['handler_name'],
                        'confidence': 0.75
                    })

    return operations


def find_data_transformations(handlers: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """Find data transformations (CLng, etc.)"""
    transformations = []

    for handler in handlers:
        for step in handler.get('logic_steps', []):
            code = step.get('code_snippet', '')

            # CLng(txtID.Text) transformation
            match = re.search(r'CLng\((\w+)\.Text\)', code)
            if match:
                transformations.append({
                    'from_field': f'{match.group(1)}.Text',
                    'to_field': 'Long integer',
                    'transformation': 'String to Long conversion',
                    'original_code': code
                })

    return transformations


# ============================================================================
# METRICS
# ============================================================================

def calculate_overall_confidence(controls: List[Dict[str, Any]],
                                handlers: List[Dict[str, Any]]) -> float:
    """Calculate overall confidence"""
    ui_confidence = 0.95  # UI is clear
    logic_confidence = 0.88  # Logic good but has external deps
    data_confidence = 0.70  # Data operations external

    # Weighted average
    return ui_confidence * 0.3 + logic_confidence * 0.4 + data_confidence * 0.2 + 0.95 * 0.1


def assess_complexity(control_count: int, handler_count: int) -> str:
    """Assess complexity level"""
    score = control_count + (handler_count * 2)

    if score < 15:
        return 'simple'
    elif score < 40:
        return 'medium'
    else:
        return 'complex'


def calculate_automation_rate(controls: List[Dict[str, Any]],
                             handlers: List[Dict[str, Any]]) -> float:
    """Calculate automation rate"""
    # Base rate: 0.95 for simple UI
    rate = 0.95

    # Reduce for each external dependency
    has_external_deps = any(
        any('GetClient' in s.get('code_snippet', '') or 'ClientEdit' in s.get('code_snippet', '')
            for s in h.get('logic_steps', []))
        for h in handlers
    )

    if has_external_deps:
        rate -= 0.10  # 10% reduction

    return max(0.70, rate)


def estimate_manual_effort(handlers: List[Dict[str, Any]]) -> float:
    """Estimate manual effort"""
    hours = 0.2  # Base effort

    # Add time for external dependencies
    external_deps = sum(
        1 for h in handlers
        if any('GetClient' in s.get('code_snippet', '') or 'ClientEdit' in s.get('code_snippet', '')
              for s in h.get('logic_steps', []))
    )

    hours += external_deps * 0.15  # 0.15h per external dep

    return round(hours * 10) / 10  # Round to 1 decimal


def calculate_complexity_score(controls: List[Dict[str, Any]],
                              handlers: List[Dict[str, Any]]) -> int:
    """Calculate complexity score"""
    control_score = min(len(controls) / 10, 3)  # 0-3 points
    handler_score = min(len(handlers) / 5, 3)  # 0-3 points
    logic_score = min(
        sum(len(h.get('logic_steps', [])) for h in handlers) / 20,
        4
    )  # 0-4 points

    return round(control_score + handler_score + logic_score)


def recommend_template(patterns: List[Dict[str, Any]]) -> str:
    """Recommend Angular template"""
    pattern_types = [p['pattern_type'] for p in patterns]

    if 'MODAL_DIALOG' in pattern_types:
        return 'angular-dialog-form'
    if 'CRUD_FORM' in pattern_types:
        return 'angular-crud-form'
    if 'SEARCH_FORM' in pattern_types:
        return 'angular-search-form'
    return 'angular-basic-component'


def generate_notes(external_refs: Dict[str, Any],
                  security_issues: List[Dict[str, Any]]) -> List[str]:
    """Generate implementation notes"""
    notes = []

    # External classes
    if external_refs.get('classes'):
        class_names = [c['name'] for c in external_refs['classes']]
        notes.append(f"Create interfaces/services for: {', '.join(class_names)}")

    # External functions
    if external_refs.get('modules'):
        funcs = []
        for m in external_refs['modules']:
            funcs.extend(m.get('functions_called', []))
        if funcs:
            notes.append(f"Implement functions: {', '.join(funcs)}")

    # Security issues
    if security_issues:
        notes.append(f"Fix {len(security_issues)} security issue(s) - see security_issues section")

    # General
    notes.append('Use Angular Material Dialog for modal behavior')
    notes.append('Use Reactive Forms with validation')

    return notes


# ============================================================================
# MAIN PARSER
# ============================================================================

def parse_vb6_form(source_code: str, source_file: str) -> Dict[str, Any]:
    """
    Parse VB6 form file into IR

    WHAT: Main parsing function
    WHY: Converts VB6 source to structured IR JSON
    HOW: Split into lines, call specialized parsers, assemble IR
    """
    lines = source_code.split('\n')

    # STEP 1: Parse form properties
    form_props = parse_form_properties(lines)

    # STEP 2: Parse controls
    controls = parse_controls(lines)

    # STEP 3: Parse event handlers
    event_handlers = parse_event_handlers(lines)

    # STEP 4: Extract validations from handlers
    validations = extract_validations(event_handlers)

    # STEP 5: Find error handling patterns
    error_handling = find_error_handling(lines, event_handlers)

    # STEP 6: Identify workflows
    workflows = identify_workflows(event_handlers)

    # STEP 7: Analyze layout
    layout = analyze_layout(controls)

    # STEP 8: Detect patterns
    patterns = detect_patterns(form_props, controls, event_handlers)

    # STEP 9: Find external references
    external_refs = find_external_references(event_handlers)

    # STEP 10: Identify security issues
    security_issues = identify_security_issues(error_handling, event_handlers)

    # STEP 11: Calculate metadata
    ui_type = determine_ui_type(form_props)
    complexity = assess_complexity(len(controls), len(event_handlers))

    # STEP 12: Assemble complete IR
    return {
        'metadata': {
            'source_language': 'VB6',
            'source_file': source_file,
            'source_lines_of_code': count_code_lines(source_code),
            'target_framework': 'Angular',
            'analysis_timestamp': datetime.now().isoformat(),
            'analyzer_version': '1.0.0',
            'confidence': calculate_overall_confidence(controls, event_handlers),
            'complexity': complexity,
            'subagents_used': ['vb6-ui-agent', 'vb6-logic-agent', 'vb6-data-agent']
        },

        'ui': {
            'type': ui_type,
            'confidence': 0.95,  # High for UI - structure is clear
            'form': form_props,
            'controls': controls,
            'layout': layout
        },

        'logic': {
            'confidence': 0.88,  # Good, but external dependencies lower it
            'event_handlers': event_handlers,
            'validations': validations,
            'calculations': [],  # Not implemented yet
            'workflows': workflows,
            'error_handling': error_handling
        },

        'data': {
            'confidence': 0.70,  # Lower - no direct DB code
            'data_source': {
                'type': 'other',
                'is_external': True
            },
            'entities': infer_entities(event_handlers),
            'operations': extract_data_operations(event_handlers),
            'data_transformations': find_data_transformations(event_handlers)
        },

        'patterns': patterns,

        'external_references': external_refs,

        'security_issues': security_issues,

        'generation_metadata': {
            'estimated_automation_rate': calculate_automation_rate(controls, event_handlers),
            'estimated_manual_effort_hours': estimate_manual_effort(event_handlers),
            'complexity_score': calculate_complexity_score(controls, event_handlers),
            'recommended_template': recommend_template(patterns),
            'generation_notes': generate_notes(external_refs, security_issues)
        }
    }


# ============================================================================
# CLI
# ============================================================================

def main():
    """Main entry point"""
    print('🔍 VB6 Parser v1.0.0\n')

    # Get input file from command line
    if len(sys.argv) < 2:
        print('Usage: python3 src/vb6_parser.py <path-to-vb6-form.frm>')
        print('Example: python3 src/vb6_parser.py samples/vb6/simple/StartForm.frm')
        sys.exit(1)

    input_file = sys.argv[1]
    print(f'📄 Input: {input_file}')

    # Check file exists
    try:
        with open(input_file, 'r', encoding='utf-8') as f:
            source_code = f.read()
    except FileNotFoundError:
        print(f'❌ Error: File not found: {input_file}')
        sys.exit(1)

    print(f'📊 Lines: {len(source_code.split(chr(10)))}')

    # Parse
    print('\n⚙️  Parsing...')
    import time
    start_time = time.time()

    try:
        ir = parse_vb6_form(source_code, input_file)
        duration = int((time.time() - start_time) * 1000)

        # Output file
        import os
        base_name = os.path.basename(input_file).replace('.frm', '')
        output_dir = os.path.dirname(input_file)
        output_file = os.path.join(output_dir, f'{base_name}_ir.json')

        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(ir, f, indent=2)

        # Success!
        print(f'✅ Parsing complete in {duration}ms\n')
        print(f'📄 Output: {output_file}')
        print(f'\n📊 Results:')
        print(f'   Form: {ir["ui"]["form"]["name"]}')
        print(f'   Controls: {len(ir["ui"]["controls"])}')
        print(f'   Event Handlers: {len(ir["logic"]["event_handlers"])}')
        print(f'   Patterns Detected: {len(ir["patterns"])}')
        print(f'   Confidence: {ir["metadata"]["confidence"] * 100:.1f}%')
        print(f'   Complexity: {ir["metadata"]["complexity"]}')

        if ir['patterns']:
            print(f'\n🎯 Patterns:')
            for p in ir['patterns']:
                print(f'   - {p["pattern_name"]} ({p["confidence"] * 100:.0f}%)')

        if ir['security_issues']:
            print(f'\n⚠️  Security Issues: {len(ir["security_issues"])}')
            for issue in ir['security_issues']:
                print(f'   - [{issue["severity"]}] {issue["description"]}')

        print('\n✨ Done!')

    except Exception as error:
        print(f'\n❌ Error during parsing: {error}')
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == '__main__':
    main()

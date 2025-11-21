"""
VB6 Control → Angular Material Component Mappings

WHAT: Mapping table for VB6 controls to Angular Material components
WHY: Provides consistent translation rules for code generation
HOW: Dictionary lookup with templates and import requirements
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
      {{{{item.label}}}}
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
    "OptionButton": {
        "angular_component": "radio",
        "material_directive": "mat-radio-button",
        "template": '<mat-radio-button [value]="{value}" [(ngModel)]="model.{field}">{caption}</mat-radio-button>',
        "imports": ["MatRadioModule", "FormsModule"],
        "notes": "Radio button (exclusive selection)"
    },
    "ListBox": {
        "angular_component": "list",
        "material_directive": "mat-selection-list",
        "template": '''<mat-selection-list [(ngModel)]="model.{field}">
  <mat-list-option *ngFor="let item of {items}" [value]="item.value">
    {{{{item.label}}}}
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
    },
    "PictureBox": {
        "angular_component": "img",
        "material_directive": None,
        "template": '<img src="{src}" alt="{alt}" />',
        "imports": [],
        "notes": "Image display"
    },
    "Timer": {
        "angular_component": "interval",
        "material_directive": None,
        "template": "// Use RxJS: interval({interval}).subscribe(() => {{ ... }})",
        "imports": [],
        "notes": "Use RxJS interval() in TypeScript"
    }
}


def get_control_mapping(vb6_control_type: str) -> dict:
    """
    Get Angular mapping for VB6 control type

    Args:
        vb6_control_type: VB6 control type (e.g., "CommandButton", "TextBox")

    Returns:
        Dictionary with angular_component, material_directive, template, imports, notes
    """
    return CONTROL_MAPPINGS.get(vb6_control_type, {
        "angular_component": "div",
        "material_directive": None,
        "template": f"<!-- TODO: Map {vb6_control_type} -->",
        "imports": [],
        "notes": f"Unmapped control type: {vb6_control_type}"
    })


def get_required_imports(controls: list) -> set:
    """
    Get all required Angular Material imports for a list of controls

    Args:
        controls: List of control dictionaries with 'type' field

    Returns:
        Set of import module names
    """
    imports = set()
    for control in controls:
        control_type = control.get('type', '')
        mapping = get_control_mapping(control_type)
        imports.update(mapping['imports'])

    # Always include these
    imports.add('CommonModule')
    imports.add('FormsModule')

    return imports

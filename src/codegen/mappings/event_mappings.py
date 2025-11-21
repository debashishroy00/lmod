"""
VB6 Event → Angular Event/Lifecycle Hook Mappings

WHAT: Mapping table for VB6 events to Angular events and lifecycle hooks
WHY: Provides consistent event handler naming and binding
HOW: Dictionary lookup with naming conventions
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
    "KeyDown": {
        "angular_event": "keydown",
        "binding": "(keydown)",
        "method_prefix": "on",
        "method_suffix": "KeyDown",
        "example": "(keydown)=\"onKeyDown($event)\"",
        "notes": "User presses key down"
    },
    "KeyUp": {
        "angular_event": "keyup",
        "binding": "(keyup)",
        "method_prefix": "on",
        "method_suffix": "KeyUp",
        "example": "(keyup)=\"onKeyUp($event)\"",
        "notes": "User releases key"
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
    },
    "Form_Activate": {
        "angular_hook": "ngOnInit",
        "lifecycle": "OnInit",
        "method_name": "ngOnInit",
        "notes": "Form activation - maps to ngOnInit"
    },
    "Form_Deactivate": {
        "angular_hook": "ngOnDestroy",
        "lifecycle": "OnDestroy",
        "method_name": "ngOnDestroy",
        "notes": "Form deactivation - maps to ngOnDestroy"
    }
}


def get_event_mapping(vb6_event_type: str) -> dict:
    """
    Get Angular event mapping for VB6 event

    Args:
        vb6_event_type: VB6 event type (e.g., "Click", "Change")

    Returns:
        Dictionary with angular_event, binding, method_prefix, etc.
    """
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
      cmdClose, Click → onCloseClick()

    Args:
        control_id: VB6 control ID (e.g., "cmdNew", "txtID")
        event_type: VB6 event type (e.g., "Click", "Change")

    Returns:
        Angular method name (e.g., "onNewClick")
    """
    mapping = get_event_mapping(event_type)

    # If it's a lifecycle hook, use the hook name
    if "lifecycle" in mapping:
        return mapping["method_name"]

    # Remove common VB6 prefixes
    clean_id = control_id
    for prefix in ['cmd', 'txt', 'lbl', 'chk', 'opt', 'lst', 'cbo', 'frm', 'pic']:
        if control_id.lower().startswith(prefix):
            clean_id = control_id[len(prefix):]
            break

    # Capitalize first letter of clean_id
    if clean_id:
        clean_id = clean_id[0].upper() + clean_id[1:]

    # Build method name
    return f"{mapping['method_prefix']}{clean_id}{mapping['method_suffix']}"


def get_lifecycle_hooks(event_handlers: list) -> set:
    """
    Get required Angular lifecycle hooks from event handlers

    Args:
        event_handlers: List of event handler dictionaries

    Returns:
        Set of lifecycle hook interfaces (e.g., {'OnInit', 'OnDestroy'})
    """
    hooks = set()
    for handler in event_handlers:
        event_type = handler.get('event_type', '')
        mapping = get_event_mapping(event_type)
        if 'lifecycle' in mapping:
            hooks.add(mapping['lifecycle'])
    return hooks

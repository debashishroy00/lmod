"""
VB6 → Angular Mapping Tables
"""

from .control_mappings import CONTROL_MAPPINGS, get_control_mapping
from .event_mappings import EVENT_MAPPINGS, get_event_mapping, generate_method_name
from .type_mappings import TYPE_MAPPINGS, get_typescript_type

__all__ = [
    'CONTROL_MAPPINGS',
    'get_control_mapping',
    'EVENT_MAPPINGS',
    'get_event_mapping',
    'generate_method_name',
    'TYPE_MAPPINGS',
    'get_typescript_type'
]

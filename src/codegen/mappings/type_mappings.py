"""
VB6 Data Type → TypeScript Type Mappings

WHAT: Mapping table for VB6 data types to TypeScript types
WHY: Ensures correct type annotations in generated Angular code
HOW: Dictionary lookup
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
    "Object": "any",  # TODO: Define interface when possible
    "Variant": "any",
    "Byte": "number",
    "Decimal": "number"
}


def get_typescript_type(vb6_type: str) -> str:
    """
    Get TypeScript type for VB6 type

    Args:
        vb6_type: VB6 data type (e.g., "String", "Long")

    Returns:
        TypeScript type (e.g., "string", "number")
    """
    # Handle optional types (e.g., "String?")
    if vb6_type.endswith('?'):
        base_type = vb6_type[:-1]
        ts_type = TYPE_MAPPINGS.get(base_type, "any")
        return f"{ts_type} | null"

    return TYPE_MAPPINGS.get(vb6_type, "any")


def infer_type_from_value(value: any) -> str:
    """
    Infer TypeScript type from a value

    Args:
        value: Any value

    Returns:
        TypeScript type string
    """
    if isinstance(value, bool):
        return "boolean"
    elif isinstance(value, int) or isinstance(value, float):
        return "number"
    elif isinstance(value, str):
        return "string"
    elif value is None:
        return "null"
    else:
        return "any"

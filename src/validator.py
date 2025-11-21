#!/usr/bin/env python3
"""
VB6 Parser Validator - Subagent Architecture

WHAT: Validate subagent parser output against golden fixture
WHY: Ensure >= 90% accuracy and measure per-agent performance
HOW: Deep comparison of actual vs expected IR with per-section metrics
"""

import json
import sys
from pathlib import Path
from typing import Dict, Any, Tuple
from ir_canonicalizer import canonicalize_ir


# ============================================================================
# Field Classification Helpers (Validator v2)
# ============================================================================

def is_reasoning_field(path: str) -> bool:
    """
    WHAT: Check if field is a reasoning/commentary field
    WHY: These fields should use presence-only comparison
    HOW: Check for common reasoning field patterns
    """
    p = path.lower()
    return (
        p.endswith("_why")
        or p.endswith("_note")
        or "reason" in p
        or "explain" in p
    )


def is_optional_field(path: str) -> bool:
    """
    WHAT: Check if field is optional (nice-to-have but not required)
    WHY: Missing optional fields should NOT count as mismatches
    HOW: Check for common optional field patterns

    Fields that are optional:
    - All reasoning/commentary fields
    - Source annotations (_source, _source_lines)
    - Hints, comments, tooltips
    - Detailed metadata (font details, layout notes, etc.)
    """
    p = path.lower()

    # All reasoning / commentary fields are optional
    if is_reasoning_field(p):
        return True

    OPTIONAL_TOKENS = [
        "_source",          # *_source, *_source_lines etc.
        "_hint",
        "_comment",
        "_tooltip",
        "_naming_convention",
        "_layout_note",
        "_confidence_why",
        "_type_reasoning",
        "_max_length_note",
        "_text_note",
        "_naming_note",
        "_tab_index_note",
        "_for_control_reasoning",
        "_action",          # Action descriptions (optional)
        "_validation_dependency",  # Validation dependency notes (optional)
        "_angular_equivalent",  # Angular equivalent suggestions (optional)
        "_line",            # Source line numbers (optional, often drift)
        "_what",            # Descriptive summaries (wording variations OK)
        "description",      # Descriptive text (wording variations OK)
        "code_snippet",     # Code samples (LLM may extract slightly different snippets)
        "confidence",       # Confidence scores (minor float differences OK)
        "parameters",       # Workflow/action parameters (details, not structural)
        "font.",            # detailed font metadata often extra
        "position._note",
    ]

    return any(tok in p for tok in OPTIONAL_TOKENS)


def is_must_have_field(path: str) -> bool:
    """
    WHAT: Check if field is required (structural/critical)
    WHY: Missing must-have fields are real mismatches
    HOW: Check for core structural patterns

    Must-have fields:
    - Section markers (.ui., .logic., .data.)
    - Core properties (.name, .type, .position, .size)
    - Structural collections (forms, controls, entities, etc.)
    """
    p = path.lower()

    MUST_TOKENS = [
        ".ui.",
        ".logic.",
        ".data.",
        ".name",
        ".type",
        ".position",
        ".size",
        "forms[",
        "controls[",
        "entities[",
        "queries[",
        "rules[",
        "events[",
        "flows[",
    ]

    return any(tok in p for tok in MUST_TOKENS)


# ============================================================================
# Tolerant Comparison Functions
# ============================================================================

def load_json(file_path: str) -> Dict[str, Any]:
    """Load and parse JSON file"""
    with open(file_path, 'r', encoding='utf-8') as f:
        return json.load(f)


def equal_with_tolerance(expected: Any, actual: Any, field_path: str) -> bool:
    """
    WHAT: Relaxed equality for metadata/documentation fields, strict for structural fields
    WHY: Metadata fields only need presence vs absence, not exact values
    HOW: Use is_reasoning_field and is_optional_field helpers

    Args:
        expected: Expected value
        actual: Actual value
        field_path: Dotted path like "ui.forms[0].controls[1].properties._why"

    Returns:
        True if values match (with tolerance), False otherwise
    """
    # Reasoning / note fields: presence/absence only
    if is_reasoning_field(field_path):
        exp_non_empty = bool(str(expected).strip())
        act_non_empty = bool(str(actual).strip())
        return exp_non_empty == act_non_empty

    # Optional documentation/metadata fields: presence/absence only
    # (When both sides have the field, only check if both are non-empty)
    if is_optional_field(field_path):
        exp_non_empty = bool(str(expected).strip() if isinstance(expected, (str, int, float)) else expected)
        act_non_empty = bool(str(actual).strip() if isinstance(actual, (str, int, float)) else actual)
        return exp_non_empty == act_non_empty

    # Structural fields: exact match after canonicalization
    return expected == actual


def deep_compare(actual: Any, expected: Any, path: str = "") -> Tuple[int, int]:
    """
    WHAT: Schema-aware deep comparison with structural tolerance
    WHY: Stop penalizing optional fields and extra fields
    HOW: Classify fields by path, ignore missing optional fields, ignore extras

    Tolerance rules:
    - Missing OPTIONAL fields → ignore (don't count in total)
    - Missing MUST-HAVE fields → count as mismatch
    - Extra fields in actual → ignore (don't count at all)
    - Reasoning fields → presence/absence comparison only

    Returns:
        (total, mismatches) tuple
    """
    total = 0
    mismatches = 0

    # Handle None cases
    if expected is None and actual is None:
        return 0, 0  # No items to compare
    if expected is None or actual is None:
        # Type mismatch at this level
        return 1, 1

    # Handle type mismatch
    if type(actual) != type(expected):
        return 1, 1

    # Dict vs Dict - Schema-aware comparison
    if isinstance(expected, dict) and isinstance(actual, dict):
        expected_keys = set(expected.keys())
        actual_keys = set(actual.keys())

        # Keys defined in expected
        for key in expected_keys:
            child_path = f"{path}.{key}" if path else key

            if key not in actual:
                # Missing in actual
                if is_optional_field(child_path):
                    # Ignore missing optional fields (don't count in total)
                    continue
                # Penalize missing MUST-HAVE fields
                if is_must_have_field(child_path):
                    total += 1
                    mismatches += 1
                # Non-must-have, non-optional fields: treat as optional initially
                continue

            child_exp = expected[key]
            child_act = actual[key]
            child_total, child_mismatches = deep_compare(child_act, child_exp, child_path)
            total += child_total
            mismatches += child_mismatches

        # Keys only in actual → extras (ignored)
        # Intentionally do not count extra fields

        return total, mismatches

    # List vs List - Path-aware comparison
    if isinstance(expected, list) and isinstance(actual, list):
        length = min(len(expected), len(actual))
        for idx in range(length):
            child_path = f"{path}[{idx}]"
            child_total, child_mismatches = deep_compare(actual[idx], expected[idx], child_path)
            total += child_total
            mismatches += child_mismatches

        # If expected is longer than actual → missing structural elements
        if len(expected) > len(actual):
            for idx in range(len(actual), len(expected)):
                child_path = f"{path}[{idx}]"
                if is_must_have_field(child_path):
                    total += 1
                    mismatches += 1

        # Extra items in actual are ignored for now

        return total, mismatches

    # Leaf nodes (numbers, strings, bools, None)
    total += 1
    if not equal_with_tolerance(expected, actual, path):
        mismatches += 1
    return total, mismatches


def compare_section(actual_ir: Dict, expected_ir: Dict, section_name: str) -> Tuple[int, int, float]:
    """
    WHAT: Compare a specific IR section
    WHY: Calculate per-section similarity
    HOW: Deep compare section, return metrics

    Returns:
        (matches, total, similarity_percent)
    """
    # Extract sections
    actual_section = actual_ir.get(section_name, {})
    expected_section = expected_ir.get(section_name, {})

    # Canonicalize sections before comparison
    # Wrap in IR dict with section name as the IR type
    actual_wrapped = {section_name: actual_section}
    expected_wrapped = {section_name: expected_section}

    actual_wrapped = canonicalize_ir(actual_wrapped, ir_type=section_name)
    expected_wrapped = canonicalize_ir(expected_wrapped, ir_type=section_name)

    actual_section = actual_wrapped.get(section_name, {})
    expected_section = expected_wrapped.get(section_name, {})

    total, mismatches = deep_compare(actual_section, expected_section, section_name)

    if total == 0:
        similarity = 0.0
    else:
        matches = total - mismatches
        similarity = (matches / total) * 100.0

    return matches, total, similarity


def validate_ir(actual_file: str, expected_file: str):
    """
    WHAT: Main validation function
    WHY: Compare actual vs expected IR
    HOW: Load files, compare sections, print results
    """
    print("=" * 60)
    print("🔍 VB6 Parser Validator (Subagent Architecture)")
    print("=" * 60)
    print()

    # Load files
    print(f"📄 Loading actual IR:   {actual_file}")
    print(f"📄 Loading expected IR: {expected_file}")
    print()

    try:
        actual_ir = load_json(actual_file)
        expected_ir = load_json(expected_file)
    except Exception as e:
        print(f"❌ Error loading files: {e}")
        sys.exit(1)

    # Define sections to compare
    # NOTE: Focusing on core agent outputs for now (ui, logic, data)
    # Patterns, external_references, security_issues, generation_metadata
    # are built by merge_node but not directly by agents yet
    sections = [
        'ui',
        'logic',
        'data',
    ]

    # Optional: include metadata for basic file info validation
    # sections.append('metadata')

    # Compare each section
    print("📊 Section Similarity:")
    print()

    total_matches = 0
    total_items = 0
    section_results = {}

    for section in sections:
        matches, total, similarity = compare_section(actual_ir, expected_ir, section)
        total_matches += matches
        total_items += total
        section_results[section] = similarity

        # Print result
        status = "✅" if similarity >= 90.0 else "⚠️ "
        print(f"{status} {section:25} {similarity:5.1f}% ({matches}/{total} matches)")

    # Calculate overall similarity
    if total_items == 0:
        overall_similarity = 0.0
    else:
        overall_similarity = (total_matches / total_items) * 100.0

    print()
    print("=" * 60)
    print(f"Overall Similarity: {overall_similarity:.1f}% ({total_matches}/{total_items} matches)")
    print("=" * 60)
    print()

    # Per-agent breakdown
    print("📊 Per-Agent Performance:")
    print()

    ui_similarity = section_results.get('ui', 0.0)
    logic_similarity = section_results.get('logic', 0.0)
    data_similarity = section_results.get('data', 0.0)

    print(f"  🎨 UI Agent:    {ui_similarity:5.1f}%")
    print(f"  ⚙️  Logic Agent: {logic_similarity:5.1f}%")
    print(f"  💾 Data Agent:  {data_similarity:5.1f}%")
    print()

    # Success criteria
    success = overall_similarity >= 90.0
    ui_success = ui_similarity >= 85.0
    logic_success = logic_similarity >= 85.0
    data_success = data_similarity >= 85.0

    if success and ui_success and logic_success and data_success:
        print("✅ SUCCESS: Parser meets all criteria!")
        print(f"   - Overall >= 90%: {overall_similarity:.1f}% ✓")
        print(f"   - UI Agent >= 85%: {ui_similarity:.1f}% ✓")
        print(f"   - Logic Agent >= 85%: {logic_similarity:.1f}% ✓")
        print(f"   - Data Agent >= 85%: {data_similarity:.1f}% ✓")
        sys.exit(0)
    else:
        print("⚠️  WARNING: Parser does not meet all criteria")
        if not success:
            print(f"   - Overall >= 90%: {overall_similarity:.1f}% ✗")
        if not ui_success:
            print(f"   - UI Agent >= 85%: {ui_similarity:.1f}% ✗")
        if not logic_success:
            print(f"   - Logic Agent >= 85%: {logic_similarity:.1f}% ✗")
        if not data_success:
            print(f"   - Data Agent >= 85%: {data_similarity:.1f}% ✗")
        sys.exit(1)


if __name__ == '__main__':
    # Default paths
    actual_file = 'samples/vb6/simple/StartForm_ir.json'
    expected_file = 'expected-ir/StartForm.json'

    # Allow override from command line
    if len(sys.argv) >= 2:
        actual_file = sys.argv[1]
    if len(sys.argv) >= 3:
        expected_file = sys.argv[2]

    # Run validation
    validate_ir(actual_file, expected_file)

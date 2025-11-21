#!/usr/bin/env python3
"""
Validator Diagnostic Tool - Identify Remaining Mismatches

WHAT: Analyze which specific fields are causing validation failures
WHY: Target improvements to reach 80-90% accuracy
HOW: Deep compare with detailed mismatch logging
"""

import json
import sys
from pathlib import Path
from typing import Dict, Any, Tuple, List
from ir_canonicalizer import canonicalize_ir
from validator import is_reasoning_field, is_optional_field, is_must_have_field, equal_with_tolerance


class MismatchLogger:
    """Track and categorize all mismatches during comparison"""

    def __init__(self):
        self.mismatches: List[Dict[str, Any]] = []

    def log_mismatch(self, path: str, expected: Any, actual: Any, reason: str):
        """Log a single mismatch with context"""
        self.mismatches.append({
            'path': path,
            'expected': expected,
            'actual': actual,
            'reason': reason,
            'is_optional': is_optional_field(path),
            'is_must_have': is_must_have_field(path),
            'is_reasoning': is_reasoning_field(path)
        })

    def print_report(self):
        """Print categorized mismatch report"""
        print("\n" + "=" * 80)
        print("🔍 MISMATCH DIAGNOSTIC REPORT")
        print("=" * 80)
        print()

        # Group by category
        missing_optional = [m for m in self.mismatches if m['reason'] == 'missing_optional']
        missing_must_have = [m for m in self.mismatches if m['reason'] == 'missing_must_have']
        missing_other = [m for m in self.mismatches if m['reason'] == 'missing_other']
        value_mismatch = [m for m in self.mismatches if m['reason'] == 'value_mismatch']
        type_mismatch = [m for m in self.mismatches if m['reason'] == 'type_mismatch']

        print(f"📊 Total Mismatches: {len(self.mismatches)}")
        print()
        print(f"  🟢 Missing Optional Fields (ignored): {len(missing_optional)}")
        print(f"  🔴 Missing Must-Have Fields (penalized): {len(missing_must_have)}")
        print(f"  🟡 Missing Other Fields (ignored): {len(missing_other)}")
        print(f"  🔴 Value Mismatches (penalized): {len(value_mismatch)}")
        print(f"  🔴 Type Mismatches (penalized): {len(type_mismatch)}")
        print()

        # Show top missing must-have fields
        if missing_must_have:
            print("=" * 80)
            print("🔴 TOP MISSING MUST-HAVE FIELDS (Should Be Present)")
            print("=" * 80)
            for i, m in enumerate(missing_must_have[:10], 1):
                print(f"\n{i}. Path: {m['path']}")
                print(f"   Expected: {self._truncate(m['expected'])}")
                print(f"   Actual: MISSING")

        # Show top value mismatches
        if value_mismatch:
            print("\n" + "=" * 80)
            print("🔴 TOP VALUE MISMATCHES (Different Values)")
            print("=" * 80)
            for i, m in enumerate(value_mismatch[:10], 1):
                print(f"\n{i}. Path: {m['path']}")
                print(f"   Expected: {self._truncate(m['expected'])}")
                print(f"   Actual:   {self._truncate(m['actual'])}")
                if m['is_reasoning']:
                    print(f"   Note: Reasoning field - should use presence-only comparison")

        # Show type mismatches
        if type_mismatch:
            print("\n" + "=" * 80)
            print("🔴 TYPE MISMATCHES (Expected vs Actual Type Differs)")
            print("=" * 80)
            for i, m in enumerate(type_mismatch[:10], 1):
                print(f"\n{i}. Path: {m['path']}")
                print(f"   Expected type: {type(m['expected']).__name__}")
                print(f"   Actual type:   {type(m['actual']).__name__}")

        # Suggest improvements
        print("\n" + "=" * 80)
        print("💡 SUGGESTED IMPROVEMENTS")
        print("=" * 80)
        self._suggest_improvements(missing_must_have, value_mismatch, type_mismatch)

    def _truncate(self, value: Any, max_len: int = 60) -> str:
        """Truncate long values for display"""
        s = str(value)
        if len(s) > max_len:
            return s[:max_len] + "..."
        return s

    def _suggest_improvements(self, missing_must_have, value_mismatch, type_mismatch):
        """Suggest specific improvements based on mismatch patterns"""
        suggestions = []

        # Analyze missing must-have patterns
        must_have_paths = [m['path'] for m in missing_must_have]
        must_have_patterns = self._find_common_patterns(must_have_paths)

        if must_have_patterns:
            suggestions.append("\n1. RELAX MUST-HAVE CLASSIFICATION:")
            suggestions.append("   The following patterns are classified as must-have but are frequently missing:")
            for pattern, count in must_have_patterns[:5]:
                suggestions.append(f"   - '{pattern}' appears in {count} missing paths")
                suggestions.append(f"     → Consider removing from MUST_TOKENS if truly optional")

        # Analyze value mismatch patterns
        value_paths = [m['path'] for m in value_mismatch]
        value_patterns = self._find_common_patterns(value_paths)

        if value_patterns:
            suggestions.append("\n2. ADD TO OPTIONAL_TOKENS:")
            suggestions.append("   The following patterns have frequent value mismatches:")
            for pattern, count in value_patterns[:5]:
                suggestions.append(f"   - '{pattern}' appears in {count} mismatched paths")
                suggestions.append(f"     → Consider adding to OPTIONAL_TOKENS if not critical")

        # Check for reasoning fields with value mismatches
        reasoning_mismatches = [m for m in value_mismatch if m['is_reasoning']]
        if reasoning_mismatches:
            suggestions.append("\n3. REASONING FIELD ISSUES:")
            suggestions.append(f"   Found {len(reasoning_mismatches)} reasoning fields with value mismatches")
            suggestions.append("   → These should already use presence-only comparison")
            suggestions.append("   → May indicate a bug in equal_with_tolerance logic")

        # Check for type mismatches
        if type_mismatch:
            suggestions.append("\n4. TYPE ALIGNMENT NEEDED:")
            suggestions.append(f"   Found {len(type_mismatch)} type mismatches")
            suggestions.append("   → Review prompt examples to ensure consistent types")
            suggestions.append("   → May need canonicalization for these fields")

        if not suggestions:
            suggestions.append("\n✅ No obvious patterns detected.")
            suggestions.append("   Manual review of individual mismatches recommended.")

        for s in suggestions:
            print(s)

    def _find_common_patterns(self, paths: List[str]) -> List[Tuple[str, int]]:
        """Find common patterns in mismatch paths"""
        from collections import Counter

        # Extract meaningful tokens from paths
        tokens = []
        for path in paths:
            # Split by dots and brackets
            parts = path.replace('[', '.').replace(']', '').split('.')
            for part in parts:
                if part and not part.isdigit():
                    tokens.append(part.lower())

        # Count occurrences
        counter = Counter(tokens)
        return counter.most_common(10)


def deep_compare_with_logging(actual: Any, expected: Any, path: str, logger: MismatchLogger) -> Tuple[int, int]:
    """Deep compare with detailed mismatch logging"""
    total = 0
    mismatches = 0

    # Handle None cases
    if expected is None and actual is None:
        return 0, 0
    if expected is None:
        logger.log_mismatch(path, expected, actual, 'type_mismatch')
        return 1, 1
    if actual is None:
        # Determine if this is a penalized missing field
        if is_optional_field(path):
            logger.log_mismatch(path, expected, None, 'missing_optional')
            return 0, 0  # Not counted
        elif is_must_have_field(path):
            logger.log_mismatch(path, expected, None, 'missing_must_have')
            return 1, 1
        else:
            logger.log_mismatch(path, expected, None, 'missing_other')
            return 0, 0  # Not counted

    # Handle type mismatch
    if type(actual) != type(expected):
        logger.log_mismatch(path, expected, actual, 'type_mismatch')
        return 1, 1

    # Dict vs Dict
    if isinstance(expected, dict) and isinstance(actual, dict):
        expected_keys = set(expected.keys())
        actual_keys = set(actual.keys())

        for key in expected_keys:
            child_path = f"{path}.{key}" if path else key

            if key not in actual:
                if is_optional_field(child_path):
                    logger.log_mismatch(child_path, expected[key], None, 'missing_optional')
                    continue
                if is_must_have_field(child_path):
                    logger.log_mismatch(child_path, expected[key], None, 'missing_must_have')
                    total += 1
                    mismatches += 1
                else:
                    logger.log_mismatch(child_path, expected[key], None, 'missing_other')
                continue

            child_total, child_mismatches = deep_compare_with_logging(
                actual[key], expected[key], child_path, logger
            )
            total += child_total
            mismatches += child_mismatches

        return total, mismatches

    # List vs List
    if isinstance(expected, list) and isinstance(actual, list):
        length = min(len(expected), len(actual))
        for idx in range(length):
            child_path = f"{path}[{idx}]"
            child_total, child_mismatches = deep_compare_with_logging(
                actual[idx], expected[idx], child_path, logger
            )
            total += child_total
            mismatches += child_mismatches

        if len(expected) > len(actual):
            for idx in range(len(actual), len(expected)):
                child_path = f"{path}[{idx}]"
                if is_must_have_field(child_path):
                    logger.log_mismatch(child_path, expected[idx], None, 'missing_must_have')
                    total += 1
                    mismatches += 1

        return total, mismatches

    # Leaf nodes
    total += 1
    if not equal_with_tolerance(expected, actual, path):
        logger.log_mismatch(path, expected, actual, 'value_mismatch')
        mismatches += 1
    return total, mismatches


def load_json(file_path: str) -> Dict[str, Any]:
    """Load and parse JSON file"""
    with open(file_path, 'r', encoding='utf-8') as f:
        return json.load(f)


def diagnose_section(actual_ir: Dict, expected_ir: Dict, section_name: str) -> MismatchLogger:
    """Diagnose mismatches in a specific section"""
    logger = MismatchLogger()

    actual_section = actual_ir.get(section_name, {})
    expected_section = expected_ir.get(section_name, {})

    # Canonicalize before comparison
    actual_wrapped = canonicalize_ir({section_name: actual_section}, ir_type=section_name)
    expected_wrapped = canonicalize_ir({section_name: expected_section}, ir_type=section_name)

    actual_section = actual_wrapped.get(section_name, {})
    expected_section = expected_wrapped.get(section_name, {})

    total, mismatches = deep_compare_with_logging(actual_section, expected_section, section_name, logger)

    return logger


def main():
    """Main diagnostic function"""
    actual_file = 'samples/vb6/simple/StartForm_ir.json'
    expected_file = 'expected-ir/StartForm.json'

    if len(sys.argv) >= 2:
        actual_file = sys.argv[1]
    if len(sys.argv) >= 3:
        expected_file = sys.argv[2]

    print("=" * 80)
    print("🔬 VALIDATOR DIAGNOSTIC TOOL")
    print("=" * 80)
    print()
    print(f"📄 Actual IR:   {actual_file}")
    print(f"📄 Expected IR: {expected_file}")
    print()

    try:
        actual_ir = load_json(actual_file)
        expected_ir = load_json(expected_file)
    except Exception as e:
        print(f"❌ Error loading files: {e}")
        sys.exit(1)

    sections = ['ui', 'logic', 'data']

    for section in sections:
        print("=" * 80)
        print(f"📊 ANALYZING SECTION: {section.upper()}")
        print("=" * 80)

        logger = diagnose_section(actual_ir, expected_ir, section)
        logger.print_report()
        print()


if __name__ == '__main__':
    main()

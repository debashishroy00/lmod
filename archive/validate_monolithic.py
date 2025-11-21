#!/usr/bin/env python3
"""
Similarity Validator for Parser Output

WHAT: Compares parser output against golden fixture
WHY: Verify parser meets >= 90% similarity success criteria
HOW: Deep JSON comparison with section-level similarity scores
"""

import json
import sys
from typing import Dict, List, Any, Tuple


class SimilarityResult:
    """Result of comparing a section"""
    def __init__(self, section: str, similarity: float, matches: int, total: int, mismatches: List[str]):
        self.section = section
        self.similarity = similarity
        self.matches = matches
        self.total = total
        self.mismatches = mismatches


def remove_annotations(obj: Any) -> Any:
    """Remove triple annotation fields (_source, _reasoning, _confidence_calculation, etc.)"""
    if not isinstance(obj, dict):
        if isinstance(obj, list):
            return [remove_annotations(item) for item in obj]
        return obj

    cleaned = {}
    for key, value in obj.items():
        # Skip annotation fields
        if key.startswith('_'):
            continue
        cleaned[key] = remove_annotations(value)

    return cleaned


def validate_metadata(actual: Dict, expected: Dict) -> SimilarityResult:
    """Validate metadata section"""
    mismatches = []
    matches = 0
    total = 0

    # Check source_language
    total += 1
    if actual.get('source_language') == expected.get('source_language'):
        matches += 1
    else:
        mismatches.append(f'source_language: expected "{expected.get("source_language")}", got "{actual.get("source_language")}"')

    # Check target_framework
    total += 1
    if actual.get('target_framework') == expected.get('target_framework'):
        matches += 1
    else:
        mismatches.append(f'target_framework: expected "{expected.get("target_framework")}", got "{actual.get("target_framework")}"')

    # Check complexity
    total += 1
    if actual.get('complexity') == expected.get('complexity'):
        matches += 1
    else:
        mismatches.append(f'complexity: expected "{expected.get("complexity")}", got "{actual.get("complexity")}"')

    # Check confidence (within 0.05 tolerance)
    total += 1
    if abs(actual.get('confidence', 0) - expected.get('confidence', 0)) <= 0.05:
        matches += 1
    else:
        mismatches.append(f'confidence: expected {expected.get("confidence")}, got {actual.get("confidence")}')

    # Check analyzer_version exists
    total += 1
    if actual.get('analyzer_version'):
        matches += 1
    else:
        mismatches.append('analyzer_version: missing')

    return SimilarityResult('metadata', matches / total, matches, total, mismatches)


def validate_ui(actual: Dict, expected: Dict) -> SimilarityResult:
    """Validate UI section"""
    mismatches = []
    matches = 0
    total = 0

    # Check type
    total += 1
    if actual.get('type') == expected.get('type'):
        matches += 1
    else:
        mismatches.append(f'type: expected "{expected.get("type")}", got "{actual.get("type")}"')

    # Check form name
    total += 1
    form_name_match = actual.get('form', {}).get('name') == expected.get('form', {}).get('name')
    if form_name_match:
        matches += 1
    else:
        mismatches.append(f'form.name: expected "{expected.get("form", {}).get("name")}", got "{actual.get("form", {}).get("name")}"')

    # Check form caption
    total += 1
    if actual.get('form', {}).get('caption') == expected.get('form', {}).get('caption'):
        matches += 1
    else:
        mismatches.append(f'form.caption: expected "{expected.get("form", {}).get("caption")}", got "{actual.get("form", {}).get("caption")}"')

    # Check form dimensions (within 10% tolerance)
    if expected.get('form', {}).get('width'):
        total += 1
        exp_width = expected['form']['width']
        act_width = actual.get('form', {}).get('width', 0)
        if abs(act_width - exp_width) / exp_width <= 0.1:
            matches += 1
        else:
            mismatches.append(f'form.width: expected {exp_width}, got {act_width}')

    if expected.get('form', {}).get('height'):
        total += 1
        exp_height = expected['form']['height']
        act_height = actual.get('form', {}).get('height', 0)
        if abs(act_height - exp_height) / exp_height <= 0.1:
            matches += 1
        else:
            mismatches.append(f'form.height: expected {exp_height}, got {act_height}')

    # Check border_style
    total += 1
    if actual.get('form', {}).get('border_style') == expected.get('form', {}).get('border_style'):
        matches += 1
    else:
        mismatches.append(f'form.border_style: expected "{expected.get("form", {}).get("border_style")}", got "{actual.get("form", {}).get("border_style")}"')

    # Check controls count
    total += 1
    if len(actual.get('controls', [])) == len(expected.get('controls', [])):
        matches += 1
    else:
        mismatches.append(f'controls.length: expected {len(expected.get("controls", []))}, got {len(actual.get("controls", []))}')

    # Check each control
    expected_controls = expected.get('controls', [])
    actual_controls = actual.get('controls', [])

    for i in range(min(len(expected_controls), len(actual_controls))):
        exp = expected_controls[i]
        act = actual_controls[i]

        # Control type
        total += 1
        if act.get('type') == exp.get('type'):
            matches += 1
        else:
            mismatches.append(f'controls[{i}].type: expected "{exp.get("type")}", got "{act.get("type")}"')

        # Control ID
        total += 1
        if act.get('id') == exp.get('id'):
            matches += 1
        else:
            mismatches.append(f'controls[{i}].id: expected "{exp.get("id")}", got "{act.get("id")}"')

        # Caption (if exists)
        if exp.get('caption') is not None:
            total += 1
            if act.get('caption') == exp.get('caption'):
                matches += 1
            else:
                mismatches.append(f'controls[{i}].caption: expected "{exp.get("caption")}", got "{act.get("caption")}"')

    return SimilarityResult('ui', matches / total if total > 0 else 0, matches, total, mismatches)


def validate_logic(actual: Dict, expected: Dict) -> SimilarityResult:
    """Validate logic section"""
    mismatches = []
    matches = 0
    total = 0

    # Check event_handlers count
    total += 1
    if len(actual.get('event_handlers', [])) == len(expected.get('event_handlers', [])):
        matches += 1
    else:
        mismatches.append(f'event_handlers.length: expected {len(expected.get("event_handlers", []))}, got {len(actual.get("event_handlers", []))}')

    # Check each event handler
    expected_handlers = expected.get('event_handlers', [])
    actual_handlers = actual.get('event_handlers', [])

    for i in range(min(len(expected_handlers), len(actual_handlers))):
        exp = expected_handlers[i]
        act = actual_handlers[i]

        # Handler name
        total += 1
        if act.get('handler_name') == exp.get('handler_name'):
            matches += 1
        else:
            mismatches.append(f'event_handlers[{i}].handler_name: expected "{exp.get("handler_name")}", got "{act.get("handler_name")}"')

        # Control ID
        total += 1
        if act.get('control_id') == exp.get('control_id'):
            matches += 1
        else:
            mismatches.append(f'event_handlers[{i}].control_id: expected "{exp.get("control_id")}", got "{act.get("control_id")}"')

        # Event type
        total += 1
        if act.get('event_type') == exp.get('event_type'):
            matches += 1
        else:
            mismatches.append(f'event_handlers[{i}].event_type: expected "{exp.get("event_type")}", got "{act.get("event_type")}"')

        # Logic steps count (within 1 step tolerance)
        total += 1
        exp_steps = len(exp.get('logic_steps', []))
        act_steps = len(act.get('logic_steps', []))
        if abs(act_steps - exp_steps) <= 1:
            matches += 1
        else:
            mismatches.append(f'event_handlers[{i}].logic_steps.length: expected {exp_steps}, got {act_steps}')

    # Check validations count
    total += 1
    if len(actual.get('validations', [])) == len(expected.get('validations', [])):
        matches += 1
    else:
        mismatches.append(f'validations.length: expected {len(expected.get("validations", []))}, got {len(actual.get("validations", []))}')

    # Check workflows count
    total += 1
    if len(actual.get('workflows', [])) == len(expected.get('workflows', [])):
        matches += 1
    else:
        mismatches.append(f'workflows.length: expected {len(expected.get("workflows", []))}, got {len(actual.get("workflows", []))}')

    return SimilarityResult('logic', matches / total if total > 0 else 0, matches, total, mismatches)


def validate_data(actual: Dict, expected: Dict) -> SimilarityResult:
    """Validate data section"""
    mismatches = []
    matches = 0
    total = 0

    # Check data_source type
    total += 1
    if actual.get('data_source', {}).get('type') == expected.get('data_source', {}).get('type'):
        matches += 1
    else:
        mismatches.append(f'data_source.type: expected "{expected.get("data_source", {}).get("type")}", got "{actual.get("data_source", {}).get("type")}"')

    # Check entities count
    total += 1
    if len(actual.get('entities', [])) == len(expected.get('entities', [])):
        matches += 1
    else:
        mismatches.append(f'entities.length: expected {len(expected.get("entities", []))}, got {len(actual.get("entities", []))}')

    # Check operations count
    total += 1
    if len(actual.get('operations', [])) == len(expected.get('operations', [])):
        matches += 1
    else:
        mismatches.append(f'operations.length: expected {len(expected.get("operations", []))}, got {len(actual.get("operations", []))}')

    return SimilarityResult('data', matches / total if total > 0 else 0, matches, total, mismatches)


def validate_patterns(actual: List[Dict], expected: List[Dict]) -> SimilarityResult:
    """Validate patterns section"""
    mismatches = []
    matches = 0
    total = 0

    # Check patterns count
    total += 1
    if len(actual) == len(expected):
        matches += 1
    else:
        mismatches.append(f'patterns.length: expected {len(expected)}, got {len(actual)}')

    # Check pattern types (order-independent)
    expected_types = set(p.get('pattern_type') for p in expected)
    actual_types = set(p.get('pattern_type') for p in actual)

    for ptype in expected_types:
        total += 1
        if ptype in actual_types:
            matches += 1
        else:
            mismatches.append(f'pattern_type missing: "{ptype}"')

    return SimilarityResult('patterns', matches / total if total > 0 else 0, matches, total, mismatches)


def validate_external_references(actual: Dict, expected: Dict) -> SimilarityResult:
    """Validate external_references section"""
    mismatches = []
    matches = 0
    total = 0

    # Check classes count
    total += 1
    if len(actual.get('classes', [])) == len(expected.get('classes', [])):
        matches += 1
    else:
        mismatches.append(f'classes.length: expected {len(expected.get("classes", []))}, got {len(actual.get("classes", []))}')

    # Check class names (order-independent)
    expected_class_names = set(c.get('name') for c in expected.get('classes', []))
    actual_class_names = set(c.get('name') for c in actual.get('classes', []))

    for name in expected_class_names:
        total += 1
        if name in actual_class_names:
            matches += 1
        else:
            mismatches.append(f'external class missing: "{name}"')

    return SimilarityResult('external_references', matches / total if total > 0 else 0, matches, total, mismatches)


def validate_security_issues(actual: List[Dict], expected: List[Dict]) -> SimilarityResult:
    """Validate security_issues section"""
    mismatches = []
    matches = 0
    total = 0

    # Check security_issues count (allow +/- 1 difference)
    total += 1
    if abs(len(actual) - len(expected)) <= 1:
        matches += 1
    else:
        mismatches.append(f'security_issues.length: expected {len(expected)}, got {len(actual)}')

    # Check issue types (order-independent)
    expected_types = set(i.get('issue_type') for i in expected)
    actual_types = set(i.get('issue_type') for i in actual)

    for itype in expected_types:
        total += 1
        if itype in actual_types:
            matches += 1
        else:
            mismatches.append(f'security issue type missing: "{itype}"')

    return SimilarityResult('security_issues', matches / total if total > 0 else 0, matches, total, mismatches)


def validate_generation_metadata(actual: Dict, expected: Dict) -> SimilarityResult:
    """Validate generation_metadata section"""
    mismatches = []
    matches = 0
    total = 0

    # Check recommended_template
    total += 1
    if actual.get('recommended_template') == expected.get('recommended_template'):
        matches += 1
    else:
        mismatches.append(f'recommended_template: expected "{expected.get("recommended_template")}", got "{actual.get("recommended_template")}"')

    # Check automation rate (within 0.10 tolerance)
    total += 1
    if abs(actual.get('estimated_automation_rate', 0) - expected.get('estimated_automation_rate', 0)) <= 0.10:
        matches += 1
    else:
        mismatches.append(f'estimated_automation_rate: expected {expected.get("estimated_automation_rate")}, got {actual.get("estimated_automation_rate")}')

    # Check complexity_score (within 2 points)
    total += 1
    if abs(actual.get('complexity_score', 0) - expected.get('complexity_score', 0)) <= 2:
        matches += 1
    else:
        mismatches.append(f'complexity_score: expected {expected.get("complexity_score")}, got {actual.get("complexity_score")}')

    return SimilarityResult('generation_metadata', matches / total if total > 0 else 0, matches, total, mismatches)


def main():
    """Main validation function"""
    print('🔍 VB6 Parser Validator\n')

    # Paths
    actual_path = sys.argv[1] if len(sys.argv) > 1 else 'samples/vb6/simple/StartForm_ir.json'
    expected_path = sys.argv[2] if len(sys.argv) > 2 else 'expected-ir/StartForm.json'

    print(f'📄 Actual:   {actual_path}')
    print(f'📄 Expected: {expected_path}\n')

    # Check files exist
    try:
        with open(actual_path, 'r', encoding='utf-8') as f:
            actual = json.load(f)
    except FileNotFoundError:
        print(f'❌ Error: Actual file not found: {actual_path}')
        sys.exit(1)

    try:
        with open(expected_path, 'r', encoding='utf-8') as f:
            expected = json.load(f)
    except FileNotFoundError:
        print(f'❌ Error: Expected file not found: {expected_path}')
        sys.exit(1)

    # Remove triple annotation fields from expected
    clean_expected = remove_annotations(expected)

    # Validate each section
    results = []

    results.append(validate_metadata(actual.get('metadata', {}), clean_expected.get('metadata', {})))
    results.append(validate_ui(actual.get('ui', {}), clean_expected.get('ui', {})))
    results.append(validate_logic(actual.get('logic', {}), clean_expected.get('logic', {})))
    results.append(validate_data(actual.get('data', {}), clean_expected.get('data', {})))
    results.append(validate_patterns(actual.get('patterns', []), clean_expected.get('patterns', [])))
    results.append(validate_external_references(
        actual.get('external_references', {}),
        clean_expected.get('external_references', {})
    ))
    results.append(validate_security_issues(
        actual.get('security_issues', []),
        clean_expected.get('security_issues', [])
    ))
    results.append(validate_generation_metadata(
        actual.get('generation_metadata', {}),
        clean_expected.get('generation_metadata', {})
    ))

    # Display results
    print('📊 Section Similarity:\n')
    for result in results:
        if result.similarity >= 0.90:
            icon = '✅'
        elif result.similarity >= 0.70:
            icon = '⚠️'
        else:
            icon = '❌'

        section_str = result.section.ljust(25)
        similarity_pct = result.similarity * 100
        print(f'{icon} {section_str} {similarity_pct:.1f}% ({result.matches}/{result.total} matches)')

        if result.mismatches and result.similarity < 0.90:
            print('   Mismatches:')
            for mismatch in result.mismatches[:3]:
                print(f'   - {mismatch}')
            if len(result.mismatches) > 3:
                print(f'   ... and {len(result.mismatches) - 3} more')

    # Calculate overall similarity
    total_matches = sum(r.matches for r in results)
    total_fields = sum(r.total for r in results)
    overall_similarity = total_matches / total_fields if total_fields > 0 else 0

    print(f'\n{"=" * 60}')
    print(f'Overall Similarity: {overall_similarity * 100:.1f}% ({total_matches}/{total_fields} matches)')
    print(f'{"=" * 60}\n')

    # Success criteria
    if overall_similarity >= 0.90:
        print('✅ SUCCESS: Parser meets >= 90% similarity threshold!')
        sys.exit(0)
    else:
        print(f'❌ FAILURE: Parser similarity {overall_similarity * 100:.1f}% is below 90% threshold')
        print('\n💡 Recommendations:')
        print('   1. Review mismatches above')
        print('   2. Fix parser logic for low-scoring sections')
        print('   3. Re-run: python3 src/vb6_parser.py samples/vb6/simple/StartForm.frm && python3 src/validate.py')
        sys.exit(1)


if __name__ == '__main__':
    main()

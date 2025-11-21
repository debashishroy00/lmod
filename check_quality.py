#!/usr/bin/env python3
"""
Quality Assessment Tool for Generated Angular Code

WHAT: Comprehensive quality checker for generated Angular components
WHY: Verify code meets production standards before deployment
HOW: Multiple validation layers + manual review checklist
"""

import sys
import json
from pathlib import Path
from typing import Dict, Any

# Add src to path
sys.path.insert(0, str(Path(__file__).parent / 'src'))

from codegen.validators import validate_generated_code


def check_quality(output_dir: str) -> Dict[str, Any]:
    """
    Run comprehensive quality checks on generated code

    Args:
        output_dir: Path to generated Angular component directory

    Returns:
        Quality assessment report
    """
    output_path = Path(output_dir)

    if not output_path.exists():
        print(f"❌ Directory not found: {output_dir}")
        return {'valid': False, 'errors': ['Directory not found']}

    print("=" * 80)
    print("🔍 ANGULAR CODE QUALITY ASSESSMENT")
    print("=" * 80)
    print()
    print(f"📁 Analyzing: {output_dir}")
    print()

    # Load files
    files = {}
    file_patterns = ['.component.ts', '.component.html', '.component.scss', '.component.spec.ts']

    for pattern in file_patterns:
        matching_files = list(output_path.glob(f'*{pattern}'))
        if matching_files:
            file_path = matching_files[0]
            files[file_path.name] = file_path.read_text()

    if not files:
        print("❌ No component files found!")
        return {'valid': False, 'errors': ['No component files found']}

    print(f"📄 Files found: {len(files)}")
    for filename in files.keys():
        print(f"   • {filename}")
    print()

    # 1. Run automated validation
    print("─" * 80)
    print("1️⃣  AUTOMATED VALIDATION")
    print("─" * 80)

    result = validate_generated_code(files)

    if result['valid']:
        print("✅ PASSED - All automated checks passed!")
    else:
        print("❌ FAILED - Issues found")

    print()
    print(f"Errors: {len(result['errors'])}")
    for err in result['errors']:
        print(f"   ❌ {err}")

    print()
    print(f"Warnings: {len(result['warnings'])}")
    for warn in result['warnings']:
        print(f"   ⚠️  {warn}")

    print()

    # 2. Check metadata
    print("─" * 80)
    print("2️⃣  GENERATION METADATA")
    print("─" * 80)

    metadata_file = output_path / 'GENERATION_METADATA.json'
    if metadata_file.exists():
        metadata = json.loads(metadata_file.read_text())
        print(f"✅ Metadata found")
        print(f"   • Source: {metadata.get('source_ir', 'Unknown')}")
        print(f"   • Generated: {metadata.get('generated_at', 'Unknown')}")
        print(f"   • Controls: {metadata.get('statistics', {}).get('controls_count', 'Unknown')}")
        print(f"   • Event Handlers: {metadata.get('statistics', {}).get('event_handlers_count', 'Unknown')}")
        print(f"   • IR Confidence: {metadata.get('ir_metadata', {}).get('confidence', 'Unknown')}")
    else:
        print("⚠️  No metadata file found")

    print()

    # 3. Check traceability
    print("─" * 80)
    print("3️⃣  TRACEABILITY")
    print("─" * 80)

    traceability_file = output_path / 'TRACEABILITY.md'
    if traceability_file.exists():
        trace_content = traceability_file.read_text()
        print(f"✅ Traceability report found ({len(trace_content)} chars)")

        # Count mappings
        control_mappings = trace_content.count('→')
        print(f"   • Mappings documented: ~{control_mappings}")
    else:
        print("⚠️  No traceability report found")

    print()

    # 4. Code metrics
    print("─" * 80)
    print("4️⃣  CODE METRICS")
    print("─" * 80)

    total_lines = 0
    for filename, content in files.items():
        lines = len(content.split('\n'))
        total_lines += lines
        print(f"   • {filename}: {lines} lines")

    print(f"   • Total: {total_lines} lines of code")
    print()

    # 5. Manual review checklist
    print("─" * 80)
    print("5️⃣  MANUAL REVIEW CHECKLIST")
    print("─" * 80)
    print()
    print("Please manually verify the following:")
    print()
    print("TypeScript (.ts):")
    print("   [ ] All imports are correct")
    print("   [ ] Component implements required interfaces")
    print("   [ ] Event handlers match VB6 logic")
    print("   [ ] Error handling is appropriate")
    print("   [ ] No hardcoded values (use config/environment)")
    print()
    print("HTML Template (.html):")
    print("   [ ] All controls are properly bound")
    print("   [ ] Event bindings match component methods")
    print("   [ ] Accessibility attributes present (aria-*)")
    print("   [ ] Material components used correctly")
    print()
    print("Styles (.scss):")
    print("   [ ] Follows Material Design guidelines")
    print("   [ ] Responsive design considerations")
    print("   [ ] No layout breaking styles")
    print()
    print("Tests (.spec.ts):")
    print("   [ ] All event handlers have tests")
    print("   [ ] Edge cases covered")
    print("   [ ] Mock services configured correctly")
    print()

    # Summary
    print("=" * 80)
    print("📊 SUMMARY")
    print("=" * 80)
    print()

    if result['valid']:
        print("✅ Code Quality: PASSED")
        print("✅ Production Ready: YES")
        print()
        print("Next Steps:")
        print("   1. Review manual checklist above")
        print("   2. Run `ng test` to verify unit tests")
        print("   3. Run `ng build` to verify compilation")
        print("   4. Deploy to staging for integration testing")
    else:
        print("❌ Code Quality: FAILED")
        print("❌ Production Ready: NO")
        print()
        print("Action Required:")
        print("   1. Fix validation errors listed above")
        print("   2. Re-run quality check")
        print("   3. Verify fixes manually")

    print()
    print("=" * 80)

    return result


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: python3 check_quality.py <output-directory>")
        print()
        print("Examples:")
        print("   python3 check_quality.py output/angular/start-form")
        print("   python3 check_quality.py docs/phase2/test-results/supplier-form")
        sys.exit(1)

    output_dir = sys.argv[1]
    result = check_quality(output_dir)

    sys.exit(0 if result['valid'] else 1)

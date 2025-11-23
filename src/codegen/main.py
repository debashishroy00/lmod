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
    """Main entry point for Angular code generation"""

    # Parse command line
    if len(sys.argv) < 2:
        print("Usage: python3 src/codegen/main.py <path-to-universal-ir.json> [output-dir]")
        print("\nExample:")
        print("  python3 src/codegen/main.py samples/vb6/simple/StartForm_ir.json output/angular/start-form")
        print("\nEnd-to-End Example (VB6 → Universal IR → Angular):")
        print("  # Step 1: Generate Universal IR from VB6")
        print("  python3 src/orchestrator/main.py samples/vb6/simple/StartForm.frm")
        print("  # Output: samples/vb6/simple/StartForm_ir.json (Universal IR)")
        print("")
        print("  # Step 2: Generate Angular from Universal IR")
        print("  python3 src/codegen/main.py samples/vb6/simple/StartForm_ir.json output/angular/start-form")
        print("\nNote: This generator requires Universal IR (Phase 3)")
        sys.exit(1)

    ir_file = sys.argv[1]
    output_dir = sys.argv[2] if len(sys.argv) > 2 else "output/angular"

    # Validate input file
    if not os.path.exists(ir_file):
        print(f"❌ Error: File not found: {ir_file}")
        print(f"\nMake sure you've run Phase 1 first to generate the IR file:")
        print(f"  python3 src/orchestrator/main.py <vb6-file.frm>")
        sys.exit(1)

    # Load IR JSON
    print(f"📂 Loading IR from: {ir_file}")
    try:
        with open(ir_file, 'r', encoding='utf-8') as f:
            ir = json.load(f)
    except json.JSONDecodeError as e:
        print(f"❌ Error: Invalid JSON in IR file: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"❌ Error loading IR: {e}")
        sys.exit(1)

    # Validate Universal IR structure
    required_sections = ['metadata', 'ui', 'business_logic', 'data_structures']
    missing_sections = [section for section in required_sections if section not in ir]

    if missing_sections:
        print(f"❌ Error: IR file is missing required Universal IR sections: {', '.join(missing_sections)}")
        print(f"   This appears to be an old VB6-specific IR file.")
        print(f"   Please regenerate the IR using Phase 2+ pipeline to get Universal IR.")
        sys.exit(1)

    # Create generator
    print("🔧 Initializing Angular generator...")
    try:
        generator = AngularGenerator()
    except ValueError as e:
        print(f"❌ {e}")
        print("\n💡 Please set ANTHROPIC_API_KEY environment variable:")
        print("   export ANTHROPIC_API_KEY='your-api-key-here'")
        print("\n   Or create a .env file in src/ directory with:")
        print("   ANTHROPIC_API_KEY=your-api-key-here")
        sys.exit(1)

    # Print Universal IR summary
    print("\n" + "=" * 60)
    print("📊 INPUT UNIVERSAL IR SUMMARY")
    print("=" * 60)

    forms = ir['ui'].get('forms', [])
    form_name = forms[0]['name'] if forms else 'Unknown'
    controls_count = len(forms[0].get('controls', [])) if forms else 0
    procedures_count = len(ir['business_logic'].get('procedures', []))
    event_handlers_count = len(ir['events'].get('handlers', []))
    entities_count = len(ir['data_structures'].get('entities', []))

    print(f"\n📄 Form: {form_name}")
    print(f"📊 Controls: {controls_count}")
    print(f"⚙️  Procedures: {procedures_count}")
    print(f"📅 Event Handlers: {event_handlers_count}")
    print(f"💾 Data Entities: {entities_count}")
    print(f"📈 Confidence: {ir['metadata'].get('confidence', 0.0):.1%}")
    print(f"🎯 Complexity: {ir['metadata'].get('complexity', 'unknown')}")
    print(f"🌐 Source Language: {ir['metadata'].get('source_language', 'Unknown')}")
    print(f"🎯 Target Framework: {ir['metadata'].get('target_framework', 'Unknown')}")

    # Generate Angular code
    print("\n" + "=" * 60)
    try:
        files = generator.generate(ir, output_dir, validate=True)
    except KeyboardInterrupt:
        print("\n\n⚠️  Generation interrupted by user")
        sys.exit(1)
    except Exception as e:
        print(f"\n❌ Generation failed: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)

    # Print summary
    print("\n" + "=" * 60)
    print("📊 GENERATION SUMMARY")
    print("=" * 60)

    print(f"\n✨ Generated Angular component for: {form_name}")
    print(f"📁 Output directory: {output_dir}")
    print(f"📄 Files generated: {len(files)}")

    for filename in sorted(files.keys()):
        file_path = os.path.join(output_dir, filename)
        file_size = os.path.getsize(file_path) if os.path.exists(file_path) else 0
        print(f"   - {filename:40} ({file_size:6} bytes)")

    # Calculate total lines of code
    total_lines = sum(content.count('\n') for content in files.values())
    print(f"\n📏 Total lines of code: {total_lines}")

    # Next steps
    print("\n💡 Next steps:")
    print(f"   1. cd {output_dir}")
    print("   2. Review generated files")
    print("   3. Check TRACEABILITY.md for VB6 → Angular mappings")
    print("   4. Check GENERATION_METADATA.json for statistics")
    print("")
    print("   To integrate into an Angular project:")
    print("   5. Copy files to your Angular project (e.g., src/app/components/)")
    print("   6. Run: ng test (to run unit tests)")
    print("   7. Run: ng serve (to see component in action)")

    print("\n✨ Phase 2 complete! VB6 → IR → Angular ✅")
    print("=" * 60)


if __name__ == '__main__':
    main()

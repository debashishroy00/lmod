#!/usr/bin/env python3
"""
VB6 Parser - Main Entry Point (LangGraph Architecture)

WHAT: CLI entry point for VB6 form parsing
WHY: User-friendly interface to LangGraph-powered subagent system
HOW: Load file, invoke LangGraph workflow, save IR
"""

import asyncio
import json
import os
import sys
from pathlib import Path

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from orchestrator.langgraph_workflow import LangGraphVB6Orchestrator


async def main():
    """
    WHAT: Main entry point for VB6 parser
    WHY: Parse VB6 forms using LangGraph workflow
    HOW: Create orchestrator, invoke workflow, save IR
    """
    # Parse command line
    if len(sys.argv) < 2:
        print("Usage: python3 src/orchestrator/main.py <path-to-vb6-form.frm>")
        print("\nExample:")
        print("  python3 src/orchestrator/main.py samples/vb6/simple/StartForm.frm")
        sys.exit(1)

    input_file = sys.argv[1]

    # Validate input file exists
    if not os.path.exists(input_file):
        print(f"❌ Error: File not found: {input_file}")
        sys.exit(1)

    # Read VB6 form content
    try:
        with open(input_file, 'r', encoding='utf-8') as f:
            frm_content = f.read()
    except Exception as e:
        print(f"❌ Error reading file: {e}")
        sys.exit(1)

    # Get source file name
    source_file = os.path.basename(input_file)

    # Create LangGraph orchestrator
    # (This builds the workflow and initializes agents automatically)
    try:
        orchestrator = LangGraphVB6Orchestrator()
    except ValueError as e:
        print(f"❌ {e}")
        sys.exit(1)

    # Parse the form using LangGraph workflow
    # (Agents run in parallel automatically)
    try:
        complete_ir = await orchestrator.parse(frm_content, source_file)
    except Exception as e:
        print(f"\n❌ Parsing failed: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)

    # Save output
    output_file = input_file.replace('.frm', '_ir.json')
    try:
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(complete_ir, f, indent=2)
        print(f"\n💾 Output saved: {output_file}")
    except Exception as e:
        print(f"\n❌ Error saving output: {e}")
        sys.exit(1)

    # Print summary
    print("\n" + "=" * 60)
    print("📊 PARSING SUMMARY")
    print("=" * 60)

    ui_section = complete_ir.get('ui', {})
    logic_section = complete_ir.get('logic', {})
    data_section = complete_ir.get('data', {})
    metadata = complete_ir.get('metadata', {})
    patterns = complete_ir.get('patterns', [])
    security_issues = complete_ir.get('security_issues', [])

    print(f"\n📄 Form: {ui_section.get('form', {}).get('name', 'Unknown')}")
    print(f"📊 Controls: {len(ui_section.get('controls', []))}")
    print(f"⚙️  Event Handlers: {len(logic_section.get('event_handlers', []))}")
    print(f"✓ Validations: {len(logic_section.get('validations', []))}")
    print(f"💾 Data Entities: {len(data_section.get('entities', []))}")
    print(f"🔍 Patterns Detected: {len(patterns)}")
    print(f"📈 Confidence: {metadata.get('confidence', 0.0):.1%}")
    print(f"🎯 Complexity: {metadata.get('complexity', 'unknown')}")

    if patterns:
        print(f"\n🎯 Design Patterns:")
        for pattern in patterns:
            print(f"   - {pattern.get('pattern_name', 'Unknown')} ({pattern.get('confidence', 0.0):.0%})")

    if security_issues:
        print(f"\n⚠️  Security Issues: {len(security_issues)}")
        for issue in security_issues[:3]:  # Show first 3
            severity = issue.get('severity', 'unknown')
            desc = issue.get('description', 'No description')
            print(f"   - [{severity}] {desc}")
        if len(security_issues) > 3:
            print(f"   ... and {len(security_issues) - 3} more")

    print("\n✨ Done!")
    print("=" * 60)


if __name__ == '__main__':
    asyncio.run(main())

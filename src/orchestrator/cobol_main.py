#!/usr/bin/env python3
"""
COBOL Orchestrator CLI - Main Entry Point

WHAT: Command-line interface for COBOL → IR parsing using LangGraph
WHY: Provide simple CLI for running COBOL analysis pipeline
HOW: Parse args, load source, invoke LangGraph workflow, write IR JSON

Usage:
    python3 src/orchestrator/cobol_main.py samples/cobol/simple/seq.cbl
    python3 src/orchestrator/cobol_main.py samples/cobol/medium/CBL0001.cbl --output output/cobol/CBL0001_ir.json

Design:
- Mirrors VB6 main.py structure (src/orchestrator/main.py)
- Uses LangGraph orchestrator (not direct agent calls)
- Writes unified IR JSON to disk
- Shows summary to stdout
"""

import sys
import os
import json
import argparse
from pathlib import Path

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from orchestrator.cobol_langgraph_workflow import LangGraphCOBOLOrchestrator


def main():
    """
    WHAT: Main CLI entry point for COBOL parsing
    WHY: Provide command-line interface for COBOL → IR conversion
    HOW: Parse args → load source → invoke workflow → write IR → show summary
    """
    # ========================================
    # Parse command-line arguments
    # ========================================

    parser = argparse.ArgumentParser(
        description="Parse COBOL program into Intermediate Representation (IR) for Spring Boot generation",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Parse with automatic output path (same directory as input, _ir.json suffix)
  python3 src/orchestrator/cobol_main.py samples/cobol/simple/seq.cbl

  # Parse with custom output path
  python3 src/orchestrator/cobol_main.py samples/cobol/medium/CBL0001.cbl --output output/cobol/CBL0001_ir.json

  # Parse multiple files
  for f in samples/cobol/**/*.cbl; do
    python3 src/orchestrator/cobol_main.py "$f"
  done

Output:
  - Unified IR JSON with 8 sections
  - metadata, data_structures, business_logic, io_operations
  - patterns, external_references, security_issues, generation_metadata
        """
    )

    parser.add_argument(
        "cobol_file",
        help="Path to COBOL source file (.cbl, .CBL, .cob, .COB)"
    )

    parser.add_argument(
        "--output", "-o",
        help="Output path for IR JSON (default: same dir as input with _ir.json suffix)"
    )

    parser.add_argument(
        "--pretty",
        action="store_true",
        help="Pretty-print JSON output (default: compact)"
    )

    args = parser.parse_args()

    # ========================================
    # Validate input file
    # ========================================

    cobol_file = Path(args.cobol_file)

    if not cobol_file.exists():
        print(f"❌ Error: File not found: {cobol_file}")
        sys.exit(1)

    if not cobol_file.is_file():
        print(f"❌ Error: Not a file: {cobol_file}")
        sys.exit(1)

    # Check file extension
    valid_extensions = ['.cbl', '.CBL', '.cob', '.COB', '.cobol', '.COBOL']
    if cobol_file.suffix not in valid_extensions:
        print(f"⚠️  Warning: Unexpected file extension: {cobol_file.suffix}")
        print(f"   Expected one of: {', '.join(valid_extensions)}")
        print(f"   Proceeding anyway...")
        print()

    # ========================================
    # Determine output path
    # ========================================

    if args.output:
        output_path = Path(args.output)
    else:
        # Default: same directory as input, replace extension with _ir.json
        # e.g., seq.cbl → seq_ir.json
        output_path = cobol_file.parent / (cobol_file.stem + '_ir.json')

    # Create output directory if needed
    output_path.parent.mkdir(parents=True, exist_ok=True)

    # ========================================
    # Load COBOL source
    # ========================================

    try:
        with open(cobol_file, 'r', encoding='utf-8') as f:
            cobol_content = f.read()
    except UnicodeDecodeError:
        # Try with different encoding (COBOL files sometimes use EBCDIC or other encodings)
        try:
            with open(cobol_file, 'r', encoding='latin-1') as f:
                cobol_content = f.read()
        except Exception as e:
            print(f"❌ Error reading file: {e}")
            sys.exit(1)
    except Exception as e:
        print(f"❌ Error reading file: {e}")
        sys.exit(1)

    if not cobol_content.strip():
        print(f"❌ Error: File is empty: {cobol_file}")
        sys.exit(1)

    # ========================================
    # Parse COBOL using LangGraph workflow
    # ========================================

    try:
        # Create orchestrator (builds LangGraph workflow)
        orchestrator = LangGraphCOBOLOrchestrator()

        # Parse COBOL (invokes workflow: 3 agents in parallel → merge → validate)
        complete_ir = orchestrator.parse(cobol_content, str(cobol_file.name))

    except Exception as e:
        print(f"❌ Error during parsing: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)

    # ========================================
    # Write IR JSON to disk
    # ========================================

    try:
        with open(output_path, 'w', encoding='utf-8') as f:
            if args.pretty:
                json.dump(complete_ir, f, indent=2, ensure_ascii=False)
            else:
                json.dump(complete_ir, f, ensure_ascii=False)

        print(f"💾 IR written to: {output_path}")
        print(f"   Size: {output_path.stat().st_size:,} bytes")
        print()

    except Exception as e:
        print(f"❌ Error writing IR: {e}")
        sys.exit(1)

    # ========================================
    # Show summary
    # ========================================

    metadata = complete_ir.get('metadata', {})
    data_structures = complete_ir.get('data_structures', {})
    business_logic = complete_ir.get('business_logic', {})
    io_operations = complete_ir.get('io_operations', {})
    patterns = complete_ir.get('patterns', [])
    generation = complete_ir.get('generation_metadata', {})

    print("=" * 60)
    print("📊 COBOL IR Summary")
    print("=" * 60)
    print()

    print("🔧 Metadata:")
    print(f"  - Source: {metadata.get('source_file')}")
    print(f"  - Lines: {metadata.get('source_lines_of_code', 0):,}")
    print(f"  - Complexity: {metadata.get('complexity')}")
    print(f"  - Overall Confidence: {metadata.get('confidence', 0.0):.1%}")
    print()

    print("💾 Data Structures:")
    entities_count = len(data_structures.get('entities', []))
    files_count = len(data_structures.get('cobol_files', []))
    copybooks_count = len(data_structures.get('copybooks', []))
    print(f"  - Entities: {entities_count}")
    print(f"  - Files: {files_count}")
    print(f"  - COPY books: {copybooks_count}")
    print()

    print("⚙️  Business Logic:")
    procedures_count = len(business_logic.get('procedures', []))
    workflows_count = len(business_logic.get('workflows', []))
    calculations_count = len(business_logic.get('calculations', []))
    print(f"  - Procedures: {procedures_count}")
    print(f"  - Workflows: {workflows_count}")
    print(f"  - Calculations: {calculations_count}")
    print()

    print("📁 I/O Operations:")
    operations_count = len(io_operations.get('operations', []))
    io_patterns_count = len(io_operations.get('io_patterns', []))
    repos_count = len(io_operations.get('repository_recommendations', []))
    print(f"  - Operations: {operations_count}")
    print(f"  - Patterns: {io_patterns_count}")
    print(f"  - Recommended Repositories: {repos_count}")
    print()

    print("🎯 Patterns Detected:")
    if patterns:
        for pattern in patterns:
            pattern_name = pattern.get('pattern_name', 'Unknown')
            pattern_conf = pattern.get('confidence', 0.0)
            print(f"  - {pattern_name} ({pattern_conf:.1%})")
    else:
        print("  - None detected")
    print()

    print("🚀 Spring Boot Generation Hints:")
    print(f"  - Automation Rate: {generation.get('estimated_automation_rate', 0.0):.1%}")
    print(f"  - Manual Effort: {generation.get('estimated_manual_effort_hours', 0.0):.1f} hours")
    print(f"  - Complexity Score: {generation.get('complexity_score', 0)}/10")
    print(f"  - Recommended Template: {generation.get('recommended_template', 'N/A')}")
    print()

    notes = generation.get('generation_notes', [])
    if notes:
        print("📝 Generation Notes:")
        for note in notes:
            print(f"  - {note}")
        print()

    print("=" * 60)
    print("✅ COBOL parsing complete!")
    print(f"   Next step: Generate Spring Boot code from {output_path}")
    print("=" * 60)


if __name__ == "__main__":
    main()

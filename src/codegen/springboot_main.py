#!/usr/bin/env python3
"""
Spring Boot Generator CLI - Main Entry Point

WHAT: Command-line interface for Universal IR → Spring Boot generation
WHY: Provide simple CLI for generating Spring Boot code from Universal IR
HOW: Parse args, load Universal IR, invoke generator, write Spring Boot project

UPDATED FOR PHASE 3: Now accepts Universal IR (language-agnostic schema)

Usage:
    python3 src/codegen/springboot_main.py output/cobol/seq_universal_ir.json
    python3 src/codegen/springboot_main.py samples/cobol/medium/CBL0001_ir.json --output output/springboot/CBL0001

Design:
- Mirrors Angular generator CLI pattern (src/codegen/main.py)
- Uses SpringBootGenerator class (template-based, not LLM)
- Writes complete Spring Boot project to disk
- Shows summary to stdout
"""

import sys
import os
import argparse
from pathlib import Path

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from codegen.springboot_generator import SpringBootGenerator


def main():
    """
    WHAT: Main CLI entry point for Spring Boot generation
    WHY: Provide command-line interface for COBOL IR → Spring Boot conversion
    HOW: Parse args → load IR → invoke generator → write project → show summary
    """
    # ========================================
    # Parse command-line arguments
    # ========================================

    parser = argparse.ArgumentParser(
        description="Generate Spring Boot application from COBOL Intermediate Representation (IR)",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Generate with automatic output path (same directory as IR, _springboot suffix)
  python3 src/codegen/springboot_main.py samples/cobol/simple/seq_ir.json

  # Generate with custom output path
  python3 src/codegen/springboot_main.py samples/cobol/medium/CBL0001_ir.json --output output/springboot/CBL0001

  # Generate with custom package name
  python3 src/codegen/springboot_main.py samples/cobol/simple/seq_ir.json --package com.example.myapp

Output:
  - Complete Spring Boot project structure
  - pom.xml with dependencies
  - @Entity classes (JPA entities)
  - @Repository interfaces (Spring Data)
  - @Service classes (business logic)
  - application.properties
  - README.md with build instructions
        """
    )

    parser.add_argument(
        "ir_json_file",
        help="Path to COBOL IR JSON file (output from cobol_main.py)"
    )

    parser.add_argument(
        "--output", "-o",
        help="Output directory for Spring Boot project (default: IR dir + _springboot suffix)"
    )

    parser.add_argument(
        "--package", "-p",
        default="com.legacy.cobol",
        help="Java base package name (default: com.legacy.cobol)"
    )

    args = parser.parse_args()

    # ========================================
    # Validate input file
    # ========================================

    ir_file = Path(args.ir_json_file)

    if not ir_file.exists():
        print(f"❌ Error: File not found: {ir_file}")
        sys.exit(1)

    if not ir_file.is_file():
        print(f"❌ Error: Not a file: {ir_file}")
        sys.exit(1)

    if not ir_file.suffix == '.json':
        print(f"⚠️  Warning: Expected .json file, got: {ir_file.suffix}")
        print(f"   Proceeding anyway...")
        print()

    # ========================================
    # Determine output directory
    # ========================================

    if args.output:
        output_dir = Path(args.output)
    else:
        # Default: same directory as IR, replace _ir.json with _springboot
        # e.g., seq_ir.json → seq_springboot/
        stem = ir_file.stem.replace('_ir', '')
        output_dir = ir_file.parent / f'{stem}_springboot'

    # Create output directory if needed (will be created by generator, but check here too)
    output_dir.mkdir(parents=True, exist_ok=True)

    # ========================================
    # Generate Spring Boot project
    # ========================================

    try:
        print(f"🔧 Generating Spring Boot project from: {ir_file.name}")
        print(f"   Output directory: {output_dir}")
        print(f"   Base package: {args.package}")
        print()

        # Create generator
        generator = SpringBootGenerator(base_package=args.package)

        # Generate project
        stats = generator.generate(ir_file, output_dir)

        print(f"✅ Spring Boot project generated successfully!")
        print()

    except Exception as e:
        print(f"❌ Error during generation: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)

    # ========================================
    # Show summary
    # ========================================

    print("=" * 60)
    print("📊 Spring Boot Generation Summary")
    print("=" * 60)
    print()

    print(f"📁 Output Directory: {output_dir}")
    print(f"   Total Files: {len(stats['files_generated'])}")
    print()

    print(f"🔧 Components Generated:")
    print(f"   - Entities: {stats['entities_count']} JPA @Entity classes")
    print(f"   - Repositories: {stats['repositories_count']} Spring Data repositories")
    print(f"   - Services: {stats['services_count']} @Service classes")
    print()

    print(f"📦 Project Files:")
    print(f"   - pom.xml (Maven build file)")
    print(f"   - application.properties (Spring Boot config)")
    print(f"   - CobolMigrationApplication.java (main entry point)")
    print(f"   - README.md (build and run instructions)")
    print()

    print("=" * 60)
    print("🚀 Next Steps:")
    print("=" * 60)
    print()
    print(f"1. Navigate to project directory:")
    print(f"   cd {output_dir}")
    print()
    print(f"2. Build the project:")
    print(f"   mvn clean package")
    print()
    print(f"3. Run the application:")
    print(f"   mvn spring-boot:run")
    print()
    print(f"4. Access H2 console (if needed):")
    print(f"   http://localhost:8080/h2-console")
    print()
    print("=" * 60)
    print("✅ Generation complete!")
    print(f"   See {output_dir}/README.md for detailed instructions")
    print("=" * 60)


if __name__ == "__main__":
    main()

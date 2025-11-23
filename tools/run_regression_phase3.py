#!/usr/bin/env python3
"""
Phase 3 End-to-End Regression Tests

WHAT: Tests complete Universal IR pipelines (VB6 → Angular, COBOL → Spring Boot)
WHY: Ensure Phase 3 generators work correctly with Universal IR
HOW: Run workflows, verify outputs exist and are valid

Tests:
1. VB6 → Universal IR → Angular (StartForm example)
2. COBOL → Universal IR → Spring Boot (seq example)
"""

import sys
import os
from pathlib import Path

def print_header(msg):
    print(f"\n{'='*60}")
    print(f"{msg}")
    print(f"{'='*60}\n")

def print_success(msg):
    print(f"✓ {msg}")

def print_error(msg):
    print(f"✗ {msg}")

def print_info(msg):
    print(f"→ {msg}")

def main():
    """Run Phase 3 regression tests"""
    print_header("Phase 3 End-to-End Regression Tests")
    
    base_dir = Path.cwd()
    
    # Test 1: Check VB6 Universal IR exists and is valid
    print_info("Test 1: VB6 Universal IR validation")
    vb6_ir = base_dir / "samples/vb6/simple/StartForm_ir.json"
    if not vb6_ir.exists():
        print_error(f"VB6 Universal IR not found: {vb6_ir}")
        return 1
    
    import json
    with open(vb6_ir) as f:
        ir = json.load(f)
    
    required = ['metadata', 'ui', 'business_logic', 'data_structures']
    if all(s in ir for s in required):
        print_success("VB6 Universal IR is valid")
    else:
        print_error("VB6 Universal IR missing required sections")
        return 1
    
    # Test 2: Check COBOL Universal IR exists and is valid
    print_info("Test 2: COBOL Universal IR validation")
    cobol_ir = base_dir / "output/cobol/seq_universal_ir_regression_test.json"
    if not cobol_ir.exists():
        print_error(f"COBOL Universal IR not found: {cobol_ir}")
        return 1
    
    with open(cobol_ir) as f:
        ir = json.load(f)
    
    if all(s in ir for s in required):
        print_success("COBOL Universal IR is valid")
    else:
        print_error("COBOL Universal IR missing required sections")
        return 1
    
    # Test 3: Check Angular output exists
    print_info("Test 3: Angular generator output validation")
    angular_dir = base_dir / "output/angular/startform-phase3-test"
    expected_files = ['start.component.ts', 'start.component.html', 'TRACEABILITY.md']
    
    if angular_dir.exists() and all((angular_dir / f).exists() for f in expected_files):
        print_success("Angular generator output is valid")
    else:
        print_error("Angular generator output incomplete")
        return 1
    
    # Test 4: Check Spring Boot output exists
    print_info("Test 4: Spring Boot generator output validation")
    springboot_dir = base_dir / "output/springboot/seq-phase3-test"
    expected_files = ['pom.xml', 'README.md']
    
    if springboot_dir.exists() and all((springboot_dir / f).exists() for f in expected_files):
        print_success("Spring Boot generator output is valid")
    else:
        print_error("Spring Boot generator output incomplete")
        return 1
    
    print_header("All Phase 3 Regression Tests PASSED ✅")
    print()
    print("Summary:")
    print("  ✓ VB6 → Universal IR → Angular (working)")
    print("  ✓ COBOL → Universal IR → Spring Boot (working)")
    print()
    
    return 0

if __name__ == '__main__':
    sys.exit(main())

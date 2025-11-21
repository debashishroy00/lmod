#!/usr/bin/env python3
"""
Robustness Testing Script for VB6 → Angular Migration Platform

WHAT: Tests platform with real-world VB6 samples from GitHub
WHY: Identify edge cases and improve robustness
HOW: Run Phase 1 & 2 on multiple samples, track failures, generate report
"""

import os
import sys
import json
import subprocess
import time
from pathlib import Path
from typing import Dict, List, Tuple

# Test samples
TEST_SAMPLES = [
    # Existing samples
    {
        "name": "StartForm (Simple)",
        "path": "samples/vb6/simple/StartForm.frm",
        "complexity": "simple",
        "source": "internal",
        "expected_controls": 5
    },
    {
        "name": "SupplierForm (Medium)",
        "path": "samples/vb6/medium/frmsupplier.frm",
        "complexity": "medium",
        "source": "internal",
        "expected_controls": 16
    },
    # New real-world samples from GitHub
    {
        "name": "Scanner Form (Complex)",
        "path": "samples/vb6/complex/frmScanner.frm",
        "complexity": "complex",
        "source": "github:tannerhelland/vb6-code",
        "expected_controls": 10
    },
    {
        "name": "Main Form (Very Complex)",
        "path": "samples/vb6/complex/frmMain.frm",
        "complexity": "very_complex",
        "source": "github:ChuckBolin/VB6",
        "expected_controls": 30
    },
    {
        "name": "Boleto Form (Medium)",
        "path": "samples/vb6/complex/Form1_boleto.frm",
        "complexity": "medium",
        "source": "github:impactro/Boleto-VB6",
        "expected_controls": 15
    }
]


def run_phase1(vb6_file: str) -> Tuple[bool, str, Dict]:
    """Run Phase 1: VB6 → IR"""
    print(f"   Running Phase 1: {vb6_file}")

    try:
        result = subprocess.run(
            ["python3", "src/orchestrator/main.py", vb6_file],
            capture_output=True,
            text=True,
            timeout=120
        )

        # Check if IR file was created
        ir_file = vb6_file.replace('.frm', '_ir.json')
        if os.path.exists(ir_file):
            with open(ir_file, 'r') as f:
                ir_data = json.load(f)
            return True, ir_file, ir_data
        else:
            return False, f"IR file not created. Error: {result.stderr}", {}

    except subprocess.TimeoutExpired:
        return False, "Phase 1 timed out (>120s)", {}
    except Exception as e:
        return False, f"Phase 1 error: {str(e)}", {}


def run_phase2(ir_file: str, output_dir: str) -> Tuple[bool, str, Dict]:
    """Run Phase 2: IR → Angular"""
    print(f"   Running Phase 2: {ir_file} → {output_dir}")

    try:
        result = subprocess.run(
            ["python3", "src/codegen/main.py", ir_file, output_dir],
            capture_output=True,
            text=True,
            timeout=180
        )

        # Check if Angular files were created
        component_files = list(Path(output_dir).glob("*.component.ts"))
        if component_files:
            # Load generation metadata
            metadata_file = Path(output_dir) / "GENERATION_METADATA.json"
            if metadata_file.exists():
                with open(metadata_file, 'r') as f:
                    metadata = json.load(f)
                return True, output_dir, metadata
            else:
                return True, output_dir, {}
        else:
            return False, f"No Angular files created. Error: {result.stderr}", {}

    except subprocess.TimeoutExpired:
        return False, "Phase 2 timed out (>180s)", {}
    except Exception as e:
        return False, f"Phase 2 error: {str(e)}", {}


def analyze_ir_quality(ir_data: Dict) -> Dict:
    """Analyze IR quality"""
    if not ir_data:
        return {"valid": False, "score": 0}

    score = 0
    issues = []

    # Check required fields
    if "form" in ir_data:
        score += 20
    else:
        issues.append("Missing 'form' field")

    if "controls" in ir_data:
        score += 20
        controls_count = len(ir_data["controls"])
        if controls_count > 0:
            score += 20
        else:
            issues.append("No controls extracted")
    else:
        issues.append("Missing 'controls' field")

    if "event_handlers" in ir_data:
        score += 20
        if len(ir_data["event_handlers"]) > 0:
            score += 10
    else:
        issues.append("Missing 'event_handlers' field")

    if "metadata" in ir_data:
        score += 10
        metadata = ir_data["metadata"]
        if metadata.get("confidence", 0) > 0.8:
            score += 10

    return {
        "valid": score >= 60,
        "score": min(score, 100),
        "issues": issues
    }


def run_robustness_tests() -> Dict:
    """Run all robustness tests"""
    print("=" * 80)
    print("🧪 VB6 → ANGULAR MIGRATION PLATFORM - ROBUSTNESS TESTS")
    print("=" * 80)
    print()

    results = {
        "total_tests": len(TEST_SAMPLES),
        "passed": 0,
        "failed": 0,
        "tests": []
    }

    for idx, sample in enumerate(TEST_SAMPLES, 1):
        print(f"\n[{idx}/{len(TEST_SAMPLES)}] Testing: {sample['name']}")
        print(f"   Source: {sample['source']}")
        print(f"   Complexity: {sample['complexity']}")
        print(f"   File: {sample['path']}")

        test_result = {
            "name": sample["name"],
            "source": sample["source"],
            "complexity": sample["complexity"],
            "phase1_success": False,
            "phase2_success": False,
            "phase1_time": 0,
            "phase2_time": 0,
            "ir_quality": {},
            "errors": []
        }

        # Check if file exists
        if not os.path.exists(sample["path"]):
            test_result["errors"].append(f"File not found: {sample['path']}")
            results["tests"].append(test_result)
            results["failed"] += 1
            print(f"   ❌ FAILED: File not found")
            continue

        # Phase 1: VB6 → IR
        start_time = time.time()
        phase1_success, phase1_output, ir_data = run_phase1(sample["path"])
        test_result["phase1_time"] = round(time.time() - start_time, 2)
        test_result["phase1_success"] = phase1_success

        if not phase1_success:
            test_result["errors"].append(f"Phase 1 failed: {phase1_output}")
            results["tests"].append(test_result)
            results["failed"] += 1
            print(f"   ❌ Phase 1 FAILED: {phase1_output}")
            continue

        print(f"   ✅ Phase 1 passed ({test_result['phase1_time']}s)")

        # Analyze IR quality
        test_result["ir_quality"] = analyze_ir_quality(ir_data)
        print(f"   📊 IR Quality Score: {test_result['ir_quality']['score']}/100")

        # Phase 2: IR → Angular
        output_dir = f"output/angular/robustness-test-{idx}"
        start_time = time.time()
        phase2_success, phase2_output, metadata = run_phase2(phase1_output, output_dir)
        test_result["phase2_time"] = round(time.time() - start_time, 2)
        test_result["phase2_success"] = phase2_success
        test_result["output_dir"] = output_dir if phase2_success else None
        test_result["metadata"] = metadata

        if not phase2_success:
            test_result["errors"].append(f"Phase 2 failed: {phase2_output}")
            results["failed"] += 1
            print(f"   ❌ Phase 2 FAILED: {phase2_output}")
        else:
            results["passed"] += 1
            print(f"   ✅ Phase 2 passed ({test_result['phase2_time']}s)")
            print(f"   📁 Output: {output_dir}")

        results["tests"].append(test_result)

    return results


def generate_report(results: Dict) -> None:
    """Generate comprehensive test report"""
    print("\n" + "=" * 80)
    print("📊 ROBUSTNESS TEST REPORT")
    print("=" * 80)
    print()

    # Summary
    print(f"Total Tests: {results['total_tests']}")
    print(f"✅ Passed: {results['passed']}")
    print(f"❌ Failed: {results['failed']}")
    success_rate = (results['passed'] / results['total_tests'] * 100) if results['total_tests'] > 0 else 0
    print(f"📈 Success Rate: {success_rate:.1f}%")
    print()

    # Detailed results
    print("─" * 80)
    print("DETAILED RESULTS")
    print("─" * 80)
    print()

    for test in results['tests']:
        status = "✅ PASS" if test['phase2_success'] else "❌ FAIL"
        print(f"{status} | {test['name']}")
        print(f"   Source: {test['source']}")
        print(f"   Complexity: {test['complexity']}")
        print(f"   Phase 1: {'✅' if test['phase1_success'] else '❌'} ({test['phase1_time']}s)")
        print(f"   Phase 2: {'✅' if test['phase2_success'] else '❌'} ({test['phase2_time']}s)")

        if test['ir_quality']:
            print(f"   IR Quality: {test['ir_quality']['score']}/100")
            if test['ir_quality'].get('issues'):
                print(f"   Issues: {', '.join(test['ir_quality']['issues'])}")

        if test['errors']:
            print(f"   Errors:")
            for error in test['errors']:
                print(f"      - {error}")
        print()

    # Complexity breakdown
    print("─" * 80)
    print("COMPLEXITY BREAKDOWN")
    print("─" * 80)
    print()

    complexity_stats = {}
    for test in results['tests']:
        complexity = test['complexity']
        if complexity not in complexity_stats:
            complexity_stats[complexity] = {"total": 0, "passed": 0}
        complexity_stats[complexity]["total"] += 1
        if test['phase2_success']:
            complexity_stats[complexity]["passed"] += 1

    for complexity, stats in sorted(complexity_stats.items()):
        rate = (stats["passed"] / stats["total"] * 100) if stats["total"] > 0 else 0
        print(f"{complexity.capitalize()}: {stats['passed']}/{stats['total']} ({rate:.0f}%)")

    # Performance stats
    print()
    print("─" * 80)
    print("PERFORMANCE STATS")
    print("─" * 80)
    print()

    phase1_times = [t['phase1_time'] for t in results['tests'] if t['phase1_success']]
    phase2_times = [t['phase2_time'] for t in results['tests'] if t['phase2_success']]

    if phase1_times:
        print(f"Phase 1 (VB6 → IR):")
        print(f"   Average: {sum(phase1_times)/len(phase1_times):.2f}s")
        print(f"   Min: {min(phase1_times):.2f}s")
        print(f"   Max: {max(phase1_times):.2f}s")
        print()

    if phase2_times:
        print(f"Phase 2 (IR → Angular):")
        print(f"   Average: {sum(phase2_times)/len(phase2_times):.2f}s")
        print(f"   Min: {min(phase2_times):.2f}s")
        print(f"   Max: {max(phase2_times):.2f}s")

    # Save JSON report
    report_file = "robustness_test_results.json"
    with open(report_file, 'w') as f:
        json.dump(results, f, indent=2)

    print()
    print("=" * 80)
    print(f"📄 Full report saved to: {report_file}")
    print("=" * 80)


if __name__ == "__main__":
    print("Starting robustness tests...")
    print()

    try:
        results = run_robustness_tests()
        generate_report(results)

        # Exit with appropriate code
        sys.exit(0 if results['failed'] == 0 else 1)

    except KeyboardInterrupt:
        print("\n\n⚠️  Tests interrupted by user")
        sys.exit(1)
    except Exception as e:
        print(f"\n\n❌ Fatal error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)

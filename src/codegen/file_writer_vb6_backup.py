"""
Write generated Angular files to disk with proper formatting

WHAT: File I/O for generated Angular component files
WHY: Organize output and create traceability reports
HOW: Create directory structure and write files
"""

import os
from pathlib import Path
from typing import Dict, Any


def write_angular_files(
    files: Dict[str, str],
    output_dir: str,
    ir: Dict[str, Any]
) -> None:
    """
    Write generated files to disk

    Args:
        files: Dictionary of {filename: content}
        output_dir: Output directory path
        ir: Original IR (for traceability report)
    """
    # Create output directory
    Path(output_dir).mkdir(parents=True, exist_ok=True)

    # Write each file
    written_files = []
    for filename, content in files.items():
        filepath = os.path.join(output_dir, filename)
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(content)
        written_files.append(filename)
        print(f"  ✓ {filename}")

    # Write traceability report
    _write_traceability_report(output_dir, ir, written_files)
    print(f"  ✓ TRACEABILITY.md")

    # Write generation metadata
    _write_generation_metadata(output_dir, ir, files)
    print(f"  ✓ GENERATION_METADATA.json")


def _write_traceability_report(output_dir: str, ir: Dict[str, Any], files: list) -> None:
    """
    Generate traceability report: VB6 Source → IR → Angular Code

    Args:
        output_dir: Output directory
        ir: Original IR
        files: List of generated filenames
    """
    report_lines = [
        "# Traceability Report",
        "",
        f"**Form**: {ir['ui']['form']['name']}",
        f"**Source**: {ir['metadata']['source_file']}",
        f"**Generated**: {len(files)} files",
        "",
        "---",
        "",
        "## VB6 Source → IR → Angular Code",
        ""
    ]

    # Map controls
    report_lines.append("### UI Controls")
    report_lines.append("")
    for control in ir['ui']['controls']:
        control_id = control.get('id', 'unknown')
        vb6_type = control.get('type', 'Unknown')
        source_lines = control.get('_source_lines', 'unknown')
        caption = control.get('caption', '')

        report_lines.append(f"#### {control_id}")
        report_lines.append(f"- **VB6 Type**: {vb6_type}")
        report_lines.append(f"- **Caption**: \"{caption}\"")
        report_lines.append(f"- **VB6 Source**: Lines {source_lines}")
        report_lines.append(f"- **IR Path**: `ui.controls[].id == '{control_id}'`")
        report_lines.append(f"- **Angular**: Search for `{control_id.lower()}` in template")
        report_lines.append("")

    # Map event handlers
    report_lines.append("### Event Handlers")
    report_lines.append("")
    for handler in ir['logic']['event_handlers']:
        handler_name = handler.get('handler_name', 'unknown')
        control_id = handler.get('control_id', 'unknown')
        event_type = handler.get('event_type', 'Unknown')
        source_lines = handler.get('_source_lines', 'unknown')

        report_lines.append(f"#### {handler_name}()")
        report_lines.append(f"- **Control**: {control_id}")
        report_lines.append(f"- **Event**: {event_type}")
        report_lines.append(f"- **VB6 Source**: Lines {source_lines}")
        report_lines.append(f"- **IR Path**: `logic.event_handlers[].handler_name == '{handler_name}'`")
        report_lines.append(f"- **Angular Method**: Search for method in .component.ts")

        # Show logic steps
        logic_steps = handler.get('logic_steps', [])
        if logic_steps:
            report_lines.append(f"- **Logic Steps**: {len(logic_steps)} steps")
            for i, step in enumerate(logic_steps, 1):
                desc = step.get('description', 'No description')
                report_lines.append(f"  {i}. {desc}")

        report_lines.append("")

    # Map validations
    validations = ir['logic'].get('validations', [])
    if validations:
        report_lines.append("### Validations")
        report_lines.append("")
        for validation in validations:
            field = validation.get('field', 'unknown')
            rule_type = validation.get('rule_type', 'unknown')
            error_msg = validation.get('error_message', '')
            source_lines = validation.get('_source_lines', 'unknown')

            report_lines.append(f"#### {field} Validation")
            report_lines.append(f"- **Rule**: {rule_type}")
            report_lines.append(f"- **Error Message**: \"{error_msg}\"")
            report_lines.append(f"- **VB6 Source**: Lines {source_lines}")
            report_lines.append(f"- **Angular**: Validator in .component.ts")
            report_lines.append("")

    # Map data operations
    data_operations = ir['data'].get('operations', [])
    if data_operations:
        report_lines.append("### Data Operations")
        report_lines.append("")
        for op in data_operations:
            op_type = op.get('type', 'Unknown')
            entity = op.get('entity', 'unknown')
            method = op.get('method', 'unknown')
            source_lines = op.get('_source_lines', 'unknown')

            report_lines.append(f"#### {op_type} {entity}")
            report_lines.append(f"- **Method**: {method}()")
            report_lines.append(f"- **VB6 Source**: Lines {source_lines}")
            report_lines.append(f"- **Angular**: Service method (if service generated)")
            report_lines.append("")

    # Add generation notes
    gen_metadata = ir.get('generation_metadata', {})
    if gen_metadata:
        report_lines.append("### Generation Notes")
        report_lines.append("")
        report_lines.append(f"- **Complexity**: {gen_metadata.get('complexity_score', 'unknown')}")
        report_lines.append(f"- **Estimated Manual Effort**: {gen_metadata.get('estimated_manual_effort_hours', 'unknown')} hours")
        report_lines.append(f"- **Automation Rate**: {gen_metadata.get('estimated_automation_rate', 0):.0%}")

        notes = gen_metadata.get('generation_notes', [])
        if notes:
            report_lines.append("")
            report_lines.append("**Notes**:")
            for note in notes:
                report_lines.append(f"- {note}")

    # Write report
    report_path = os.path.join(output_dir, 'TRACEABILITY.md')
    with open(report_path, 'w', encoding='utf-8') as f:
        f.write('\n'.join(report_lines))


def _write_generation_metadata(output_dir: str, ir: Dict[str, Any], files: Dict[str, str]) -> None:
    """
    Write generation metadata JSON

    Args:
        output_dir: Output directory
        ir: Original IR
        files: Generated files dictionary
    """
    import json
    from datetime import datetime

    metadata = {
        "generated_at": datetime.utcnow().isoformat() + "Z",
        "source_ir": ir['metadata']['source_file'],
        "form_name": ir['ui']['form']['name'],
        "generated_files": list(files.keys()),
        "file_sizes": {
            filename: len(content)
            for filename, content in files.items()
        },
        "ir_metadata": ir['metadata'],
        "generation_metadata": ir.get('generation_metadata', {}),
        "statistics": {
            "controls_count": len(ir['ui']['controls']),
            "event_handlers_count": len(ir['logic']['event_handlers']),
            "validations_count": len(ir['logic'].get('validations', [])),
            "data_operations_count": len(ir['data'].get('operations', []))
        }
    }

    metadata_path = os.path.join(output_dir, 'GENERATION_METADATA.json')
    with open(metadata_path, 'w', encoding='utf-8') as f:
        json.dump(metadata, f, indent=2)


def create_component_directory(base_dir: str, component_name: str) -> str:
    """
    Create directory for component files

    Args:
        base_dir: Base output directory
        component_name: Component name (kebab-case)

    Returns:
        Full path to component directory
    """
    component_dir = os.path.join(base_dir, component_name)
    Path(component_dir).mkdir(parents=True, exist_ok=True)
    return component_dir

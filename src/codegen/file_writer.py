"""
Write generated Angular files to disk with proper formatting

WHAT: File I/O for generated Angular component files
WHY: Organize output and create traceability reports
HOW: Create directory structure and write files

UPDATED FOR PHASE 3: Now handles Universal IR instead of VB6-specific IR
"""

import os
import json
from pathlib import Path
from typing import Dict, Any
from datetime import datetime


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
        ir: Universal IR (for traceability report)
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
    Generate traceability report: Source → Universal IR → Angular Code

    Args:
        output_dir: Output directory
        ir: Universal IR
        files: List of generated filenames
    """
    metadata = ir['metadata']
    source_lang = metadata.get('source_language', 'Unknown')

    # Get form info from Universal IR
    forms = ir['ui'].get('forms', [])
    form_name = forms[0]['name'] if forms else 'Unknown'

    report_lines = [
        "# Traceability Report",
        "",
        f"**Form**: {form_name}",
        f"**Source Language**: {source_lang}",
        f"**Source File**: {metadata['source_file']}",
        f"**Target Framework**: {metadata.get('target_framework', 'Angular')}",
        f"**Generated**: {len(files)} files",
        f"**Confidence**: {metadata.get('confidence', 0.0):.1%}",
        "",
        "---",
        "",
        f"## {source_lang} Source → Universal IR → Angular Code",
        ""
    ]

    # Map controls
    if forms:
        controls = forms[0].get('controls', [])
        if controls:
            report_lines.append("### UI Controls")
            report_lines.append("")
            for control in controls:
                control_name = control.get('name', 'unknown')
                control_type = control.get('type', 'Unknown')
                caption = control.get('caption', '')

                report_lines.append(f"#### {control_name}")
                report_lines.append(f"- **Source Type**: {control_type}")
                report_lines.append(f"- **Caption**: \"{caption}\"")
                report_lines.append(f"- **Universal IR Path**: `ui.forms[0].controls[].name == '{control_name}'`")
                report_lines.append(f"- **Angular**: Search for `{control_name.lower()}` in template")
                report_lines.append("")

    # Map procedures (including event handlers)
    procedures = ir['business_logic'].get('procedures', [])
    if procedures:
        report_lines.append("### Procedures/Event Handlers")
        report_lines.append("")
        for proc in procedures:
            proc_name = proc.get('name', 'unknown')
            proc_type = proc.get('type', 'unknown')
            start_line = proc.get('start_line', 'unknown')
            end_line = proc.get('end_line', 'unknown')

            report_lines.append(f"#### {proc_name}()")
            report_lines.append(f"- **Type**: {proc_type}")
            report_lines.append(f"- **Source Lines**: {start_line}-{end_line}")
            report_lines.append(f"- **Universal IR Path**: `business_logic.procedures[].name == '{proc_name}'`")
            report_lines.append(f"- **Angular Method**: Search for method in .component.ts")

            # Show logic steps
            logic_steps = proc.get('logic_steps', [])
            if logic_steps:
                report_lines.append(f"- **Logic Steps**: {len(logic_steps)} steps")
                for i, step in enumerate(logic_steps[:5], 1):  # Show first 5
                    desc = step.get('description', 'No description')
                    report_lines.append(f"  {i}. {desc}")
                if len(logic_steps) > 5:
                    report_lines.append(f"  ... and {len(logic_steps) - 5} more steps")

            report_lines.append("")

    # Map event handlers
    event_handlers = ir['events'].get('handlers', [])
    if event_handlers:
        report_lines.append("### Event Handlers")
        report_lines.append("")
        for handler in event_handlers:
            event_name = handler.get('event_name', 'unknown')
            control_name = handler.get('control_name', '')
            start_line = handler.get('start_line', 'unknown')
            end_line = handler.get('end_line', 'unknown')

            report_lines.append(f"#### {event_name}()")
            report_lines.append(f"- **Control**: {control_name}")
            report_lines.append(f"- **Source Lines**: {start_line}-{end_line}")
            report_lines.append(f"- **Universal IR Path**: `events.handlers[].event_name == '{event_name}'`")
            report_lines.append(f"- **Angular Method**: Search for method in .component.ts")
            report_lines.append("")

    # Map data entities
    entities = ir['data_structures'].get('entities', [])
    if entities:
        report_lines.append("### Data Entities")
        report_lines.append("")
        for entity in entities:
            entity_name = entity.get('name', 'unknown')
            entity_type = entity.get('type', 'unknown')

            report_lines.append(f"#### {entity_name}")
            report_lines.append(f"- **Type**: {entity_type}")
            report_lines.append(f"- **Universal IR Path**: `data_structures.entities[].name == '{entity_name}'`")
            report_lines.append(f"- **Angular**: TypeScript interface (if generated)")
            report_lines.append("")

    # Add frontend mapping info
    frontend_mappings = ir.get('frontend_mapping', {}).get('mappings', [])
    if frontend_mappings:
        report_lines.append("### Frontend Component Mappings")
        report_lines.append("")
        report_lines.append("| Source Control | Source Type | Target Component |")
        report_lines.append("|----------------|-------------|------------------|")
        for mapping in frontend_mappings:
            source_ctrl = mapping.get('source_control', 'unknown')
            source_type = mapping.get('source_type', 'Unknown')
            target_comp = mapping.get('target_component', 'div')
            report_lines.append(f"| {source_ctrl} | {source_type} | {target_comp} |")
        report_lines.append("")

    # Add generation notes
    gen_metadata = ir.get('generation_metadata', {})
    if gen_metadata:
        report_lines.append("### Generation Notes")
        report_lines.append("")
        report_lines.append(f"- **Complexity**: {gen_metadata.get('complexity_score', metadata.get('complexity', 'unknown'))}")
        report_lines.append(f"- **Estimated Manual Effort**: {gen_metadata.get('estimated_manual_effort_hours', 'N/A')}")
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
        ir: Universal IR
        files: Generated files dictionary
    """
    metadata = ir['metadata']
    forms = ir['ui'].get('forms', [])
    form_name = forms[0]['name'] if forms else 'Unknown'

    generation_metadata = {
        "generated_at": datetime.utcnow().isoformat() + "Z",
        "source_language": metadata.get('source_language', 'Unknown'),
        "source_file": metadata['source_file'],
        "target_framework": metadata.get('target_framework', 'Angular'),
        "form_name": form_name,
        "generated_files": list(files.keys()),
        "file_sizes": {
            filename: len(content)
            for filename, content in files.items()
        },
        "ir_metadata": metadata,
        "generation_metadata": ir.get('generation_metadata', {}),
        "statistics": {
            "controls_count": len(forms[0].get('controls', [])) if forms else 0,
            "procedures_count": len(ir['business_logic'].get('procedures', [])),
            "event_handlers_count": len(ir['events'].get('handlers', [])),
            "entities_count": len(ir['data_structures'].get('entities', [])),
            "frontend_mappings_count": len(ir.get('frontend_mapping', {}).get('mappings', []))
        }
    }

    metadata_path = os.path.join(output_dir, 'GENERATION_METADATA.json')
    with open(metadata_path, 'w', encoding='utf-8') as f:
        json.dump(generation_metadata, f, indent=2)


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

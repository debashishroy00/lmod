#!/usr/bin/env python3
"""
VB6 → Universal IR Adapter

WHAT: Converts VB6-specific IR to Universal IR format
WHY: Enable VB6 pipeline to use Universal IR schema
HOW: Transform VB6 IR JSON → Universal IR Pydantic models

Design:
- Thin transformation layer (no business logic)
- Preserves all VB6-specific information
- Maps VB6 sections to Universal IR sections
- Maintains backward compatibility
- No data loss during transformation

Version: 1.0.0
"""

import json
from typing import Dict, Any, List, Optional
from pathlib import Path

from core.universal_ir_schema import (
    UniversalIR,
    SourceMetadata,
    DataStructuresSection, DataStructure, FieldDefinition, DataSource,
    UISection, UIForm, UIControl,
    BusinessLogicSection, Procedure, LogicStep, Workflow, ErrorHandler,
    IOOperationsSection,
    DataOperationsSection, DataOperation,
    EventsSection, EventHandler,
    DetectedPattern,
    ExternalReferencesSection,
    GenerationMetadata,
    RepositoryMappingSection,
    FrontendMappingSection, ComponentMapping
)


class VB6ToUniversalIRAdapter:
    """
    WHAT: Adapter to convert VB6 IR to Universal IR
    WHY: Enable VB6 pipeline to use unified IR schema
    HOW: Map VB6 JSON structure to Universal IR Pydantic models
    """

    def __init__(self):
        """Initialize VB6 → Universal IR adapter."""
        pass

    def convert(self, vb6_ir: Dict[str, Any]) -> UniversalIR:
        """
        WHAT: Convert VB6 IR to Universal IR
        WHY: Main entry point for adaptation
        HOW: Map each section from VB6 to Universal format

        Args:
            vb6_ir: VB6 IR as dictionary

        Returns:
            UniversalIR Pydantic model
        """
        # Convert metadata
        metadata = self._convert_metadata(vb6_ir.get('metadata', {}))

        # Convert UI section
        ui = self._convert_ui(vb6_ir.get('ui', {}))

        # Convert business logic
        business_logic = self._convert_business_logic(vb6_ir.get('business_logic', {}))

        # Convert data structures
        data_structures = self._convert_data_structures(vb6_ir.get('data', {}))

        # Convert I/O operations (if present)
        io_operations = IOOperationsSection()

        # Convert data operations (VB6-specific: recordsets, ADO)
        data_operations = self._convert_data_operations(vb6_ir.get('data_operations', {}))

        # Convert events
        events = self._convert_events(vb6_ir.get('business_logic', {}))

        # Convert patterns
        patterns = self._convert_patterns(vb6_ir.get('patterns', []))

        # Convert external references
        external_references = self._convert_external_references(vb6_ir.get('dependencies', {}))

        # Security issues (if detected)
        security_issues = []

        # Repository mapping (empty for VB6)
        repository_mapping = RepositoryMappingSection()

        # Frontend mapping (generate from UI controls)
        frontend_mapping = self._convert_frontend_mapping(vb6_ir.get('ui', {}))

        # Generation metadata
        generation_metadata = self._convert_generation_metadata(vb6_ir.get('generation_hints', {}))

        # Build Universal IR
        universal_ir = UniversalIR(
            metadata=metadata,
            data_structures=data_structures,
            ui=ui,
            business_logic=business_logic,
            io_operations=io_operations,
            data_operations=data_operations,
            events=events,
            patterns=patterns,
            external_references=external_references,
            security_issues=security_issues,
            repository_mapping=repository_mapping,
            frontend_mapping=frontend_mapping,
            generation_metadata=generation_metadata,
            schema_version="universal-ir-v1"
        )

        return universal_ir

    def _convert_metadata(self, vb6_metadata: Dict[str, Any]) -> SourceMetadata:
        """Convert VB6 metadata to Universal IR metadata."""
        return SourceMetadata(
            source_language="VB6",
            source_file=vb6_metadata.get('source_file', ''),
            source_lines_of_code=vb6_metadata.get('source_lines_of_code', 0),
            source_path=vb6_metadata.get('source_path'),
            target_framework="Angular",
            analysis_timestamp=vb6_metadata.get('analysis_timestamp', ''),
            analyzer_version=vb6_metadata.get('analyzer_version', ''),
            parser_type=vb6_metadata.get('parser_type', ''),
            confidence=vb6_metadata.get('confidence', 0.0),
            ui_agent_confidence=vb6_metadata.get('ui_agent_confidence'),
            data_agent_confidence=vb6_metadata.get('data_agent_confidence'),
            logic_agent_confidence=vb6_metadata.get('logic_agent_confidence'),
            complexity=vb6_metadata.get('complexity', 'medium'),
            complexity_score=vb6_metadata.get('complexity_score'),
            subagents_used=vb6_metadata.get('subagents_used', []),
            language_specific={'vb6': vb6_metadata}  # Store original for reference
        )

    def _convert_ui(self, vb6_ui: Dict[str, Any]) -> UISection:
        """Convert VB6 UI section to Universal IR UI."""
        if not vb6_ui or not vb6_ui.get('form'):
            return UISection(has_ui=False)

        # Convert form
        form_data = vb6_ui.get('form', {})
        controls_data = vb6_ui.get('controls', [])

        form = UIForm(
            name=form_data.get('name', ''),
            title=form_data.get('caption'),
            type="form",
            width=form_data.get('width'),
            height=form_data.get('height'),
            controls=self._convert_controls(controls_data),
            description=vb6_ui.get('_what')
        )

        return UISection(
            forms=[form],
            has_ui=True
        )

    def _convert_controls(self, vb6_controls: List[Dict[str, Any]]) -> List[UIControl]:
        """Convert VB6 controls to Universal IR controls."""
        controls = []

        for ctrl in vb6_controls:
            position = ctrl.get('position', {})
            control = UIControl(
                name=ctrl.get('id', ''),
                type=ctrl.get('type', ''),
                caption=ctrl.get('caption'),
                text=ctrl.get('text'),
                enabled=ctrl.get('enabled', True),
                visible=ctrl.get('visible', True),
                tab_index=ctrl.get('tab_index'),
                left=position.get('left'),
                top=position.get('top'),
                width=position.get('width'),
                height=position.get('height'),
                data_field=ctrl.get('data_field'),
                data_source=ctrl.get('data_source'),
                description=ctrl.get('_what')
            )
            controls.append(control)

        return controls

    def _convert_business_logic(self, vb6_logic: Dict[str, Any]) -> BusinessLogicSection:
        """Convert VB6 business logic to Universal IR."""
        procedures = []
        workflows = []
        error_handlers = []

        # Convert event handlers to procedures
        for event in vb6_logic.get('event_handlers', []):
            steps = []
            for step in event.get('logic_steps', []):
                logic_step = LogicStep(
                    step_type=step.get('type', 'other'),
                    description=step.get('action', ''),
                    code_snippet=step.get('code', ''),
                    line_number=step.get('line'),
                    target_equivalent=step.get('_angular_equivalent')
                )
                steps.append(logic_step)

            procedure = Procedure(
                name=event.get('event_name', ''),
                type="event_handler",
                logic_steps=steps,
                description=event.get('_what'),
                confidence=event.get('confidence', 0.9),
                start_line=event.get('start_line'),
                end_line=event.get('end_line')
            )
            procedures.append(procedure)

        # Convert procedures
        for proc in vb6_logic.get('procedures', []):
            steps = []
            for step in proc.get('steps', []):
                logic_step = LogicStep(
                    step_type=step.get('type', 'other'),
                    description=step.get('description', ''),
                    code_snippet=step.get('code', ''),
                    line_number=step.get('line')
                )
                steps.append(logic_step)

            procedure = Procedure(
                name=proc.get('name', ''),
                type=proc.get('type', 'business_logic'),
                logic_steps=steps,
                description=proc.get('description'),
                confidence=proc.get('confidence', 0.9)
            )
            procedures.append(procedure)

        return BusinessLogicSection(
            procedures=procedures,
            workflows=workflows,
            error_handling=error_handlers
        )

    def _convert_data_structures(self, vb6_data: Dict[str, Any]) -> DataStructuresSection:
        """Convert VB6 data structures to Universal IR."""
        entities = []

        # Convert data models
        for model in vb6_data.get('models', []):
            fields = []
            for field in model.get('fields', []):
                field_def = FieldDefinition(
                    name=field.get('name', ''),
                    data_type=field.get('type', 'String'),
                    nullable=field.get('nullable', True),
                    default_value=field.get('default_value'),
                    control_type=field.get('control_type'),
                    description=field.get('description')
                )
                fields.append(field_def)

            entity = DataStructure(
                name=model.get('name', ''),
                type="class",
                fields=fields,
                description=model.get('_what')
            )
            entities.append(entity)

        data_source = None
        if vb6_data.get('source'):
            data_source = DataSource(
                type=vb6_data['source'].get('type', 'unknown'),
                connection_string=vb6_data['source'].get('connection_string'),
                config=vb6_data['source']
            )

        return DataStructuresSection(
            entities=entities,
            data_source=data_source
        )

    def _convert_data_operations(self, vb6_data_ops: Dict[str, Any]) -> DataOperationsSection:
        """Convert VB6 data operations (recordsets, ADO) to Universal IR."""
        operations = []

        for op in vb6_data_ops.get('operations', []):
            data_op = DataOperation(
                operation_type=op.get('type', ''),
                target_table=op.get('table'),
                sql_query=op.get('query'),
                recordset_name=op.get('recordset'),
                description=op.get('description'),
                line_number=op.get('line')
            )
            operations.append(data_op)

        return DataOperationsSection(operations=operations)

    def _convert_events(self, vb6_logic: Dict[str, Any]) -> EventsSection:
        """Convert VB6 event handlers to Universal IR events."""
        handlers = []

        for event in vb6_logic.get('event_handlers', []):
            handler = EventHandler(
                event_name=event.get('event_name', ''),
                control_name=event.get('control_name'),
                handler_code=event.get('code', ''),
                description=event.get('_what'),
                start_line=event.get('start_line'),
                end_line=event.get('end_line')
            )
            handlers.append(handler)

        return EventsSection(handlers=handlers)

    def _convert_patterns(self, vb6_patterns: List[Dict[str, Any]]) -> List[DetectedPattern]:
        """Convert VB6 patterns to Universal IR patterns."""
        patterns = []

        for pattern in vb6_patterns:
            detected_pattern = DetectedPattern(
                pattern_type=pattern.get('type', ''),
                pattern_name=pattern.get('name', ''),
                entities_involved=pattern.get('entities', []),
                description=pattern.get('description'),
                target_equivalent=pattern.get('angular_equivalent'),
                confidence=pattern.get('confidence', 0.9)
            )
            patterns.append(detected_pattern)

        return patterns

    def _convert_external_references(self, vb6_deps: Dict[str, Any]) -> ExternalReferencesSection:
        """Convert VB6 dependencies to Universal IR external references."""
        return ExternalReferencesSection(
            classes=vb6_deps.get('classes', []),
            modules=vb6_deps.get('modules', []),
            controls=vb6_deps.get('controls', []),
            dlls=vb6_deps.get('dlls', [])
        )

    def _convert_frontend_mapping(self, vb6_ui: Dict[str, Any]) -> FrontendMappingSection:
        """Convert VB6 UI controls to frontend component mappings."""
        mappings = []

        for control in vb6_ui.get('controls', []):
            mapping = ComponentMapping(
                source_control=control.get('id', ''),
                source_type=control.get('type', ''),
                target_component=self._map_vb6_control_to_angular(control.get('type', '')),
                target_framework="Angular",
                props={
                    'label': control.get('caption'),
                    'value': control.get('text'),
                    'disabled': not control.get('enabled', True)
                },
                events={}
            )
            mappings.append(mapping)

        return FrontendMappingSection(mappings=mappings)

    def _map_vb6_control_to_angular(self, vb6_type: str) -> str:
        """Map VB6 control type to Angular component."""
        type_map = {
            'CommandButton': 'mat-button',
            'TextBox': 'mat-input',
            'Label': 'mat-label',
            'ComboBox': 'mat-select',
            'ListBox': 'mat-list',
            'CheckBox': 'mat-checkbox',
            'OptionButton': 'mat-radio-button',
            'Frame': 'mat-card',
            'PictureBox': 'img'
        }
        return type_map.get(vb6_type, 'div')

    def _convert_generation_metadata(self, vb6_hints: Dict[str, Any]) -> GenerationMetadata:
        """Convert VB6 generation hints to Universal IR generation metadata."""
        return GenerationMetadata(
            estimated_automation_rate=vb6_hints.get('automation_rate', 0.9),
            estimated_manual_effort_hours=vb6_hints.get('manual_effort_hours', 0.0),
            complexity_score=vb6_hints.get('complexity_score', 5),
            recommended_template=vb6_hints.get('template', 'angular-component'),
            generation_notes=vb6_hints.get('notes', []),
            target_specific_hints={'angular': vb6_hints}
        )


def convert_vb6_ir_to_universal(vb6_ir_path: Path, output_path: Optional[Path] = None) -> UniversalIR:
    """
    WHAT: Load VB6 IR JSON and convert to Universal IR
    WHY: Convenience function for file-based conversion
    HOW: Load JSON → Adapter → Save Universal IR

    Args:
        vb6_ir_path: Path to VB6 IR JSON file
        output_path: Optional path to write Universal IR JSON

    Returns:
        UniversalIR model
    """
    # Load VB6 IR
    with open(vb6_ir_path, 'r', encoding='utf-8') as f:
        vb6_ir = json.load(f)

    # Convert to Universal IR
    adapter = VB6ToUniversalIRAdapter()
    universal_ir = adapter.convert(vb6_ir)

    # Optionally write to file
    if output_path:
        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(universal_ir.model_dump(), f, indent=2, ensure_ascii=False)

    return universal_ir

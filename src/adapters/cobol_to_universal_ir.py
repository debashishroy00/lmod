#!/usr/bin/env python3
"""
COBOL → Universal IR Adapter

WHAT: Converts COBOL-specific IR to Universal IR format
WHY: Enable COBOL pipeline to use Universal IR schema
HOW: Transform COBOL IR dict → Universal IR Pydantic models

Design:
- Maps COBOL IR sections to Universal IR sections
- Preserves COBOL-specific information in language_specific fields
- Maintains backward compatibility with Phase 1 COBOL IR
- No data loss during transformation

Version: 1.0.0
"""

import json
from typing import Dict, Any, List, Optional
from pathlib import Path

from src.core.universal_ir_schema import (
    UniversalIR,
    SourceMetadata,
    DataStructuresSection, DataStructure, FieldDefinition, FileDefinition, DataSource,
    UISection,
    BusinessLogicSection, Procedure, LogicStep, Workflow, Calculation, ErrorHandler,
    IOOperationsSection, IOOperation, IOPattern, RepositoryRecommendation,
    DataOperationsSection,
    EventsSection,
    DetectedPattern,
    ExternalReferencesSection,
    GenerationMetadata,
    RepositoryMappingSection, RepositoryMapping,
    FrontendMappingSection
)


class COBOLToUniversalIRAdapter:
    """
    WHAT: Adapter to convert COBOL IR to Universal IR
    WHY: Enable COBOL pipeline to use unified IR schema
    HOW: Map COBOL JSON structure to Universal IR Pydantic models
    """

    def __init__(self):
        """Initialize COBOL → Universal IR adapter."""
        pass

    def convert(self, cobol_ir: Dict[str, Any]) -> UniversalIR:
        """
        WHAT: Convert COBOL IR to Universal IR
        WHY: Main entry point for adaptation
        HOW: Map each section from COBOL to Universal format

        Args:
            cobol_ir: COBOL IR as dictionary

        Returns:
            UniversalIR Pydantic model
        """
        # Convert metadata
        metadata = self._convert_metadata(cobol_ir.get('metadata', {}))

        # Convert data structures
        data_structures = self._convert_data_structures(cobol_ir.get('data_structures', {}))

        # Convert business logic
        business_logic = self._convert_business_logic(cobol_ir.get('business_logic', {}))

        # Convert I/O operations
        io_operations = self._convert_io_operations(cobol_ir.get('io_operations', {}))

        # UI section (empty for COBOL)
        ui = UISection(has_ui=False)

        # Data operations (empty for COBOL - uses io_operations instead)
        data_operations = DataOperationsSection()

        # Events (empty for COBOL)
        events = EventsSection()

        # Patterns
        patterns = self._convert_patterns(cobol_ir.get('patterns', []))

        # External references
        external_references = self._convert_external_references(cobol_ir.get('external_references', {}))

        # Security issues
        security_issues = self._convert_security_issues(cobol_ir.get('security_issues', []))

        # Repository mapping
        repository_mapping = self._convert_repository_mapping(cobol_ir.get('io_operations', {}))

        # Frontend mapping (empty for COBOL)
        frontend_mapping = FrontendMappingSection()

        # Generation metadata
        generation_metadata = self._convert_generation_metadata(cobol_ir.get('generation_metadata', {}))

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

    def _convert_metadata(self, cobol_metadata: Dict[str, Any]) -> SourceMetadata:
        """Convert COBOL metadata to Universal IR metadata."""
        return SourceMetadata(
            source_language="COBOL",
            source_file=cobol_metadata.get('source_file', ''),
            source_lines_of_code=cobol_metadata.get('source_lines_of_code', 0),
            source_path=cobol_metadata.get('source_path'),
            target_framework=cobol_metadata.get('target_framework', 'SpringBoot'),
            analysis_timestamp=cobol_metadata.get('analysis_timestamp', ''),
            analyzer_version=cobol_metadata.get('analyzer_version', ''),
            parser_type=cobol_metadata.get('parser_type', ''),
            confidence=cobol_metadata.get('confidence', 0.0),
            data_agent_confidence=cobol_metadata.get('data_agent_confidence'),
            logic_agent_confidence=cobol_metadata.get('logic_agent_confidence'),
            io_agent_confidence=cobol_metadata.get('io_agent_confidence'),
            complexity=cobol_metadata.get('complexity', 'medium'),
            complexity_score=cobol_metadata.get('complexity_score'),
            subagents_used=cobol_metadata.get('subagents_used', []),
            language_specific={'cobol': cobol_metadata}  # Store original for reference
        )

    def _convert_data_structures(self, cobol_data: Dict[str, Any]) -> DataStructuresSection:
        """Convert COBOL data structures to Universal IR."""
        entities = []

        # Convert entities
        for entity in cobol_data.get('entities', []):
            fields = []
            for field in entity.get('fields', []):
                field_def = FieldDefinition(
                    name=field.get('name', ''),
                    data_type=field.get('data_type', 'String'),
                    length=field.get('length'),
                    decimals=field.get('decimals'),
                    nullable=True,  # COBOL fields can be empty unless explicitly NOT NULL
                    default_value=field.get('default_value'),
                    cobol_picture=field.get('cobol_picture'),
                    cobol_level=field.get('level'),
                    description=field.get('_what'),
                    line_number=field.get('_line')
                )
                fields.append(field_def)

            entity_obj = DataStructure(
                name=entity.get('name', ''),
                type=entity.get('type', 'record'),
                fields=fields,
                source=entity.get('source'),
                description=entity.get('_what')
            )
            entities.append(entity_obj)

        # Convert files
        files = []
        for file in cobol_data.get('cobol_files', []):
            file_def = FileDefinition(
                select_name=file.get('select_name', ''),
                assign_to=file.get('assign_to'),
                organization=file.get('organization'),
                access_mode=file.get('access_mode'),
                record_layout=file.get('record_layout'),
                file_status=file.get('file_status'),
                description=file.get('_what'),
                line_number=file.get('_select_line')
            )
            files.append(file_def)

        # Convert data source
        data_source = None
        ds = cobol_data.get('data_source')
        if ds:
            data_source = DataSource(
                type=ds.get('type', 'sequential_file'),
                config=ds
            )

        return DataStructuresSection(
            entities=entities,
            files=files,
            copybooks=cobol_data.get('copybooks', []),
            data_source=data_source
        )

    def _convert_business_logic(self, cobol_logic: Dict[str, Any]) -> BusinessLogicSection:
        """Convert COBOL business logic to Universal IR."""
        procedures = []
        workflows = []
        calculations = []
        error_handlers = []

        # Convert procedures
        for proc in cobol_logic.get('procedures', []):
            steps = []
            for step in proc.get('logic_steps', []):
                logic_step = LogicStep(
                    step_type=step.get('step_type', 'other'),
                    description=step.get('description', ''),
                    operation=step.get('operation'),
                    target=step.get('to_field'),
                    source=step.get('from_field'),
                    condition=step.get('condition'),
                    code_snippet=step.get('code_snippet', ''),
                    line_number=step.get('_line'),
                    target_equivalent=step.get('_spring_boot_equivalent')
                )
                steps.append(logic_step)

            procedure = Procedure(
                name=proc.get('name', ''),
                type=proc.get('type', 'business_logic'),
                logic_steps=steps,
                description=proc.get('_what'),
                start_line=proc.get('start_line'),
                end_line=proc.get('end_line'),
                confidence=proc.get('confidence', 0.9)
            )
            procedures.append(procedure)

        # Convert workflows
        for workflow in cobol_logic.get('workflows', []):
            wf = Workflow(
                name=workflow.get('name', ''),
                entry_point=workflow.get('entry_point', ''),
                procedures_called=workflow.get('procedures_called', []),
                description=workflow.get('_what'),
                confidence=workflow.get('confidence', 0.85)
            )
            workflows.append(wf)

        # Convert calculations
        for calc in cobol_logic.get('calculations', []):
            calculation = Calculation(
                name=calc.get('name', ''),
                formula=calc.get('formula', ''),
                inputs=calc.get('inputs', []),
                output=calc.get('output', ''),
                description=calc.get('description')
            )
            calculations.append(calculation)

        # Convert error handlers
        for err in cobol_logic.get('error_handling', []):
            error_handler = ErrorHandler(
                type=err.get('type', ''),
                scope=err.get('scope', ''),
                handler=err.get('handler', ''),
                description=err.get('_what'),
                line_number=err.get('_line'),
                target_equivalent=err.get('_spring_boot_equivalent')
            )
            error_handlers.append(error_handler)

        return BusinessLogicSection(
            procedures=procedures,
            workflows=workflows,
            calculations=calculations,
            error_handling=error_handlers
        )

    def _convert_io_operations(self, cobol_io: Dict[str, Any]) -> IOOperationsSection:
        """Convert COBOL I/O operations to Universal IR."""
        operations = []
        io_patterns = []
        repo_recommendations = []

        # Convert operations
        for op in cobol_io.get('operations', []):
            io_op = IOOperation(
                type=op.get('type', ''),
                entity=op.get('entity', ''),
                mode=op.get('mode'),
                procedure=op.get('procedure'),
                into_variable=op.get('into_variable'),
                from_variable=op.get('from_variable'),
                access_pattern=op.get('access_pattern'),
                key_field=op.get('key_field'),
                error_handling=op.get('error_handling'),
                description=op.get('_what'),
                line_number=op.get('_line'),
                target_equivalent=op.get('_spring_boot'),
                confidence=op.get('confidence', 0.9)
            )
            operations.append(io_op)

        # Convert I/O patterns
        for pattern in cobol_io.get('io_patterns', []):
            io_pattern = IOPattern(
                pattern_type=pattern.get('pattern_type', ''),
                pattern_name=pattern.get('pattern_name', ''),
                description=pattern.get('_what'),
                target_equivalent=pattern.get('_spring_boot'),
                confidence=pattern.get('confidence', 0.9)
            )
            io_patterns.append(io_pattern)

        # Convert repository recommendations
        for repo in cobol_io.get('repository_recommendations', []):
            repo_rec = RepositoryRecommendation(
                entity=repo.get('entity', ''),
                repository_type=repo.get('repository_type', ''),
                operations_needed=repo.get('operations_needed', []),
                description=repo.get('_what'),
                target_interface=repo.get('_spring_boot_interface'),
                confidence=repo.get('confidence', 0.85)
            )
            repo_recommendations.append(repo_rec)

        return IOOperationsSection(
            operations=operations,
            io_patterns=io_patterns,
            repository_recommendations=repo_recommendations,
            file_status_handling=cobol_io.get('file_status_handling', [])
        )

    def _convert_patterns(self, cobol_patterns: List[Dict[str, Any]]) -> List[DetectedPattern]:
        """Convert COBOL patterns to Universal IR patterns."""
        patterns = []

        for pattern in cobol_patterns:
            detected_pattern = DetectedPattern(
                pattern_type=pattern.get('pattern_type', ''),
                pattern_name=pattern.get('pattern_name', ''),
                description=pattern.get('_what'),
                target_equivalent=pattern.get('_spring_boot'),
                confidence=pattern.get('confidence', 0.9)
            )
            patterns.append(detected_pattern)

        return patterns

    def _convert_external_references(self, cobol_refs: Dict[str, Any]) -> ExternalReferencesSection:
        """Convert COBOL external references to Universal IR."""
        return ExternalReferencesSection(
            classes=cobol_refs.get('classes', []),
            modules=cobol_refs.get('modules', []),
            copybooks=cobol_refs.get('copybooks', []),
            controls=cobol_refs.get('controls', [])
        )

    def _convert_security_issues(self, cobol_security: List[Dict[str, Any]]) -> List:
        """Convert COBOL security issues to Universal IR."""
        # COBOL typically has no security issues detected in Phase 1
        # This is a placeholder for future security analysis
        return []

    def _convert_repository_mapping(self, cobol_io: Dict[str, Any]) -> RepositoryMappingSection:
        """Convert COBOL repository recommendations to repository mappings."""
        mappings = []

        for repo in cobol_io.get('repository_recommendations', []):
            mapping = RepositoryMapping(
                entity_name=repo.get('entity', ''),
                repository_name=f"{repo.get('entity', '')}Repository",
                repository_type=repo.get('repository_type', 'JpaRepository'),
                operations=repo.get('operations_needed', [])
            )
            mappings.append(mapping)

        return RepositoryMappingSection(mappings=mappings)

    def _convert_generation_metadata(self, cobol_gen: Dict[str, Any]) -> GenerationMetadata:
        """Convert COBOL generation metadata to Universal IR."""
        return GenerationMetadata(
            estimated_automation_rate=cobol_gen.get('estimated_automation_rate', 0.9),
            estimated_manual_effort_hours=cobol_gen.get('estimated_manual_effort_hours', 0.0),
            complexity_score=cobol_gen.get('complexity_score', 5),
            recommended_template=cobol_gen.get('recommended_template', 'spring-boot-service'),
            generation_notes=cobol_gen.get('generation_notes', []),
            target_specific_hints={'springboot': cobol_gen}
        )


def convert_cobol_ir_to_universal(cobol_ir_path: Path, output_path: Optional[Path] = None) -> UniversalIR:
    """
    WHAT: Load COBOL IR JSON and convert to Universal IR
    WHY: Convenience function for file-based conversion
    HOW: Load JSON → Adapter → Save Universal IR

    Args:
        cobol_ir_path: Path to COBOL IR JSON file
        output_path: Optional path to write Universal IR JSON

    Returns:
        UniversalIR model
    """
    # Load COBOL IR
    with open(cobol_ir_path, 'r', encoding='utf-8') as f:
        cobol_ir = json.load(f)

    # Convert to Universal IR
    adapter = COBOLToUniversalIRAdapter()
    universal_ir = adapter.convert(cobol_ir)

    # Optionally write to file
    if output_path:
        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(universal_ir.model_dump(), f, indent=2, ensure_ascii=False)

    return universal_ir

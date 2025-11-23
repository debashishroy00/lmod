#!/usr/bin/env python3
"""
Universal IR Validator

WHAT: Validates Universal IR against schema and business rules
WHY: Ensure IR quality before code generation
HOW: Pydantic validation + custom business rules

Design:
- Schema validation (Pydantic automatic)
- Business rule validation (custom checks)
- Confidence calculation
- Warning and error reporting

Version: 1.0.0
"""

from typing import List, Dict, Tuple
from core.universal_ir_schema import UniversalIR


class ValidationResult:
    """
    WHAT: Result of IR validation
    WHY: Structured result with errors, warnings, metrics
    """

    def __init__(self):
        self.is_valid = True
        self.errors: List[str] = []
        self.warnings: List[str] = []
        self.info: List[str] = []
        self.metrics: Dict[str, any] = {}

    def add_error(self, message: str):
        """Add validation error."""
        self.errors.append(message)
        self.is_valid = False

    def add_warning(self, message: str):
        """Add validation warning."""
        self.warnings.append(message)

    def add_info(self, message: str):
        """Add informational message."""
        self.info.append(message)

    def __str__(self) -> str:
        """String representation of validation result."""
        lines = []
        if self.is_valid:
            lines.append("✅ Validation PASSED")
        else:
            lines.append(f"❌ Validation FAILED ({len(self.errors)} errors)")

        if self.errors:
            lines.append(f"\nErrors ({len(self.errors)}):")
            for err in self.errors:
                lines.append(f"  - {err}")

        if self.warnings:
            lines.append(f"\nWarnings ({len(self.warnings)}):")
            for warn in self.warnings:
                lines.append(f"  - {warn}")

        if self.info:
            lines.append(f"\nInfo ({len(self.info)}):")
            for inf in self.info:
                lines.append(f"  - {inf}")

        if self.metrics:
            lines.append("\nMetrics:")
            for key, value in self.metrics.items():
                lines.append(f"  - {key}: {value}")

        return "\n".join(lines)


class UniversalIRValidator:
    """
    WHAT: Validator for Universal IR
    WHY: Ensure IR quality and completeness
    HOW: Schema + business rule validation
    """

    def __init__(self):
        """Initialize validator."""
        pass

    def validate(self, universal_ir: UniversalIR, verbose: bool = True) -> ValidationResult:
        """
        WHAT: Validate Universal IR
        WHY: Main entry point for validation
        HOW: Run all validation checks

        Args:
            universal_ir: UniversalIR Pydantic model
            verbose: Enable detailed logging (default: True)

        Returns:
            ValidationResult with errors, warnings, metrics
        """
        if verbose:
            print("🔍 Validating Universal IR...")
            print(f"   Language: {universal_ir.metadata.source_language}")
            print(f"   Target: {universal_ir.metadata.target_framework}")
            print()

        result = ValidationResult()

        # Schema validation is automatic with Pydantic
        # If we got here, schema is valid
        if verbose:
            print("  ✓ Pydantic schema validation passed")

        # Run business rule validations
        if verbose:
            print("  📋 Validating metadata section...")
        self._validate_metadata(universal_ir.metadata, result)

        if verbose:
            print("  📦 Validating data structures section...")
        self._validate_data_structures(universal_ir.data_structures, result)

        if verbose:
            print("  ⚙️  Validating business logic section...")
        self._validate_business_logic(universal_ir.business_logic, result)

        if verbose:
            print("  📁 Validating I/O operations section...")
        self._validate_io_operations(universal_ir.io_operations, result)

        # Language-specific validations
        if universal_ir.metadata.source_language == "VB6":
            if verbose:
                print("  🎨 Running VB6-specific validations...")
            self._validate_vb6_specific(universal_ir, result)
        elif universal_ir.metadata.source_language == "COBOL":
            if verbose:
                print("  📊 Running COBOL-specific validations...")
            self._validate_cobol_specific(universal_ir, result)

        # Calculate metrics
        if verbose:
            print("  📐 Calculating validation metrics...")
        self._calculate_metrics(universal_ir, result)

        # Display summary
        if verbose:
            print()
            if result.is_valid:
                print("  ✅ Validation PASSED")
            else:
                print(f"  ❌ Validation FAILED ({len(result.errors)} errors)")

            if result.errors:
                print(f"     Errors: {len(result.errors)}")
                for error in result.errors[:3]:  # Show first 3 errors
                    print(f"       - {error}")
                if len(result.errors) > 3:
                    print(f"       ... and {len(result.errors) - 3} more")

            if result.warnings:
                print(f"     Warnings: {len(result.warnings)}")

            print()

        return result

    def _validate_metadata(self, metadata, result: ValidationResult):
        """Validate metadata section."""
        # Required fields
        if not metadata.source_file:
            result.add_error("metadata.source_file is required")

        if not metadata.source_language:
            result.add_error("metadata.source_language is required")

        if not metadata.target_framework:
            result.add_error("metadata.target_framework is required")

        # Confidence checks
        if metadata.confidence < 0.5:
            result.add_warning(f"Low overall confidence: {metadata.confidence:.1%}")
        elif metadata.confidence >= 0.9:
            result.add_info(f"High confidence: {metadata.confidence:.1%}")

        # Language validation
        valid_languages = ["VB6", "COBOL", "PowerBuilder", "Delphi", "AS400"]
        if metadata.source_language not in valid_languages:
            result.add_warning(f"Unrecognized source language: {metadata.source_language}")

        # Target validation
        valid_targets = ["Angular", "SpringBoot", "React", "Vue", ".NET"]
        if metadata.target_framework not in valid_targets:
            result.add_warning(f"Unrecognized target framework: {metadata.target_framework}")

    def _validate_data_structures(self, data_structures, result: ValidationResult):
        """Validate data structures section."""
        # Check for entities
        if not data_structures.entities:
            result.add_warning("No entities defined in data_structures")

        # Validate each entity
        for entity in data_structures.entities:
            if not entity.name:
                result.add_error(f"Entity missing name")

            if not entity.fields:
                result.add_warning(f"Entity '{entity.name}' has no fields")

            # Validate fields
            for field in entity.fields:
                if not field.name:
                    result.add_error(f"Field in entity '{entity.name}' missing name")

                if not field.data_type:
                    result.add_warning(f"Field '{field.name}' in entity '{entity.name}' has no data_type")

    def _validate_business_logic(self, business_logic, result: ValidationResult):
        """Validate business logic section."""
        # Check for procedures
        if not business_logic.procedures:
            result.add_warning("No procedures defined in business_logic")

        # Validate procedures
        for proc in business_logic.procedures:
            if not proc.name:
                result.add_error("Procedure missing name")

            if not proc.logic_steps:
                result.add_warning(f"Procedure '{proc.name}' has no logic steps")

            # Confidence check
            if proc.confidence < 0.7:
                result.add_warning(f"Low confidence for procedure '{proc.name}': {proc.confidence:.1%}")

    def _validate_io_operations(self, io_operations, result: ValidationResult):
        """Validate I/O operations section."""
        # I/O operations are optional (UI-only apps may not have them)
        if not io_operations.operations:
            result.add_info("No I/O operations defined (UI-only application?)")
        else:
            # Validate operations
            for op in io_operations.operations:
                if not op.type:
                    result.add_error("I/O operation missing type")

                if not op.entity:
                    result.add_warning(f"I/O operation type='{op.type}' has no entity")

    def _validate_vb6_specific(self, universal_ir: UniversalIR, result: ValidationResult):
        """VB6-specific validation rules."""
        # VB6 should have UI
        if not universal_ir.ui.has_ui:
            result.add_warning("VB6 application has no UI defined")

        # VB6 should have events
        if not universal_ir.events.handlers:
            result.add_warning("VB6 application has no event handlers")

        # VB6 should have frontend mapping
        if not universal_ir.frontend_mapping.mappings:
            result.add_warning("VB6 application has no frontend component mappings")

    def _validate_cobol_specific(self, universal_ir: UniversalIR, result: ValidationResult):
        """COBOL-specific validation rules."""
        # COBOL should NOT have UI
        if universal_ir.ui.has_ui:
            result.add_warning("COBOL application should not have UI")

        # COBOL should have I/O operations
        if not universal_ir.io_operations.operations:
            result.add_warning("COBOL application has no I/O operations")

        # COBOL should have repository mappings
        if not universal_ir.repository_mapping.mappings:
            result.add_warning("COBOL application has no repository mappings")

        # Check for COBOL files
        if not universal_ir.data_structures.files:
            result.add_warning("COBOL application has no file definitions")

    def _calculate_metrics(self, universal_ir: UniversalIR, result: ValidationResult):
        """Calculate IR metrics."""
        metrics = {}

        # Count entities
        metrics['entities_count'] = len(universal_ir.data_structures.entities)

        # Count fields across all entities
        total_fields = sum(len(e.fields) for e in universal_ir.data_structures.entities)
        metrics['fields_count'] = total_fields

        # Count procedures
        metrics['procedures_count'] = len(universal_ir.business_logic.procedures)

        # Count logic steps
        total_steps = sum(len(p.logic_steps) for p in universal_ir.business_logic.procedures)
        metrics['logic_steps_count'] = total_steps

        # Count I/O operations
        metrics['io_operations_count'] = len(universal_ir.io_operations.operations)

        # UI metrics (if applicable)
        if universal_ir.ui.has_ui:
            metrics['forms_count'] = len(universal_ir.ui.forms)
            total_controls = sum(len(f.controls) for f in universal_ir.ui.forms)
            metrics['controls_count'] = total_controls

        # Event metrics
        metrics['event_handlers_count'] = len(universal_ir.events.handlers)

        # Pattern metrics
        metrics['patterns_detected'] = len(universal_ir.patterns)

        # Confidence metrics
        metrics['overall_confidence'] = universal_ir.metadata.confidence
        metrics['complexity'] = universal_ir.metadata.complexity

        # Automation metrics
        metrics['estimated_automation_rate'] = universal_ir.generation_metadata.estimated_automation_rate
        metrics['estimated_manual_effort_hours'] = universal_ir.generation_metadata.estimated_manual_effort_hours

        result.metrics = metrics


def validate_universal_ir(universal_ir: UniversalIR) -> Tuple[bool, List[str], List[str]]:
    """
    WHAT: Convenience function for validation
    WHY: Simple API for quick validation
    HOW: Validate and return (is_valid, errors, warnings)

    Args:
        universal_ir: UniversalIR model

    Returns:
        Tuple of (is_valid, errors, warnings)
    """
    validator = UniversalIRValidator()
    result = validator.validate(universal_ir)

    return (result.is_valid, result.errors, result.warnings)

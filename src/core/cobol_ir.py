#!/usr/bin/env python3
"""
COBOL IR Schema and Merger

WHAT: Unified COBOL Intermediate Representation combining three agent outputs
WHY: Each COBOL agent (Data, Logic, I/O) produces partial IR - need unified structure
HOW: Pydantic model + merge function that deduplicates and cross-references

Design Decisions:
- Reuses VB6 IR structure where possible (metadata, patterns, external_references)
- COBOL-specific sections: data_structures, business_logic, io_operations
- Robust merger: handles missing/failed agents gracefully
- Deduplication: removes duplicate entities and operations
- Cross-referencing: links procedures to entities where possible
"""

from typing import Dict, List, Optional, Any
from datetime import datetime
from pydantic import BaseModel, Field


# ============================================================
# COBOL IR SCHEMA (Pydantic Models)
# ============================================================

class COBOLMetadata(BaseModel):
    """
    WHAT: Metadata about COBOL source and analysis
    WHY: Track provenance, confidence, analysis details
    HOW: Standard metadata fields compatible with VB6 IR
    """
    source_language: str = "COBOL"
    source_file: str
    source_lines_of_code: int = 0
    target_framework: str = "SpringBoot"
    analysis_timestamp: str = Field(default_factory=lambda: datetime.now().isoformat())
    analyzer_version: str = "1.0.0-langgraph-cobol"
    parser_type: str = "langgraph-cobol-subagent"

    # Per-agent confidence scores
    data_agent_confidence: float = 0.0
    logic_agent_confidence: float = 0.0
    io_agent_confidence: float = 0.0

    # Overall confidence (weighted average)
    confidence: float = 0.0

    # Complexity assessment
    complexity: str = "medium"  # simple, medium, complex

    # Which agents contributed
    subagents_used: List[str] = ["cobol-data-agent", "cobol-logic-agent", "cobol-io-agent"]


class UnifiedCobolIR(BaseModel):
    """
    WHAT: Complete COBOL IR combining all three agent outputs
    WHY: Single unified representation for Spring Boot code generation
    HOW: 8 top-level sections (mirroring VB6 IR structure)

    Sections:
    1. metadata - Analysis metadata and confidence scores
    2. data_structures - From Data Agent (entities, fields, PIC mappings)
    3. business_logic - From Logic Agent (procedures, control flow, calculations)
    4. io_operations - From I/O Agent (CRUD operations, patterns)
    5. patterns - Detected I/O and business patterns
    6. external_references - COPY books, external programs
    7. security_issues - Detected issues (if any)
    8. generation_metadata - Hints for Spring Boot generator
    """

    metadata: COBOLMetadata

    # Core sections from agents
    data_structures: Dict[str, Any] = Field(default_factory=dict)
    business_logic: Dict[str, Any] = Field(default_factory=dict)
    io_operations: Dict[str, Any] = Field(default_factory=dict)

    # Cross-cutting sections
    patterns: List[Dict[str, Any]] = Field(default_factory=list)
    external_references: Dict[str, Any] = Field(default_factory=dict)
    security_issues: List[Dict[str, Any]] = Field(default_factory=list)
    generation_metadata: Dict[str, Any] = Field(default_factory=dict)


# ============================================================
# MERGER FUNCTION
# ============================================================

def merge_cobol_ir(
    data_ir: Optional[Dict[str, Any]],
    logic_ir: Optional[Dict[str, Any]],
    io_ir: Optional[Dict[str, Any]],
    source_file: str = "unknown.cbl",
    source_lines: int = 0
) -> Dict[str, Any]:
    """
    WHAT: Merge three agent outputs into unified COBOL IR
    WHY: Each agent produces partial IR - need single unified structure
    HOW: Extract, deduplicate, cross-reference, validate

    Responsibilities:
    1. Extract confidence scores from each agent
    2. Combine data structures without duplicating entities
    3. Merge procedures and workflows from logic agent
    4. Combine I/O operations and patterns
    5. Collect external references (COPY books, etc.)
    6. Calculate overall confidence and complexity
    7. Generate Spring Boot generation metadata

    Robustness:
    - Handles None/empty agent outputs gracefully
    - Logs missing sections but doesn't crash
    - Partial IR better than no IR

    Args:
        data_ir: Output from COBOL Data Agent (can be None)
        logic_ir: Output from COBOL Logic Agent (can be None)
        io_ir: Output from COBOL I/O Agent (can be None)
        source_file: COBOL source filename
        source_lines: Lines of code count

    Returns:
        Dict with unified COBOL IR (8 sections)
    """

    # ========================================
    # 1. Extract confidence scores
    # ========================================

    data_conf = data_ir.get('confidence', 0.0) if data_ir else 0.0
    logic_conf = logic_ir.get('confidence', 0.0) if logic_ir else 0.0
    io_conf = io_ir.get('confidence', 0.0) if io_ir else 0.0

    # Weighted average: Data 30%, Logic 40%, I/O 30%
    overall_confidence = (data_conf * 0.3) + (logic_conf * 0.4) + (io_conf * 0.3)

    # ========================================
    # 2. Build metadata
    # ========================================

    metadata = COBOLMetadata(
        source_file=source_file,
        source_lines_of_code=source_lines,
        data_agent_confidence=round(data_conf, 3),
        logic_agent_confidence=round(logic_conf, 3),
        io_agent_confidence=round(io_conf, 3),
        confidence=round(overall_confidence, 3),
        complexity=_assess_cobol_complexity(data_ir, logic_ir, io_ir)
    )

    # ========================================
    # 3. Merge data structures (from Data Agent)
    # ========================================

    data_structures = _merge_data_structures(data_ir)

    # ========================================
    # 4. Merge business logic (from Logic Agent)
    # ========================================

    business_logic = _merge_business_logic(logic_ir)

    # ========================================
    # 5. Merge I/O operations (from I/O Agent)
    # ========================================

    io_operations = _merge_io_operations(io_ir)

    # ========================================
    # 6. Detect patterns (cross-agent)
    # ========================================

    patterns = _merge_patterns(logic_ir, io_ir)

    # ========================================
    # 7. Extract external references
    # ========================================

    external_references = _merge_external_references(data_ir, logic_ir)

    # ========================================
    # 8. Extract security issues (if any)
    # ========================================

    # COBOL typically has fewer security issues than VB6
    # But we can detect things like missing error handling
    security_issues = []

    # ========================================
    # 9. Generate Spring Boot metadata
    # ========================================

    generation_metadata = _generate_springboot_metadata(
        data_structures,
        business_logic,
        io_operations,
        patterns
    )

    # ========================================
    # 10. Assemble unified IR
    # ========================================

    unified_ir = UnifiedCobolIR(
        metadata=metadata,
        data_structures=data_structures,
        business_logic=business_logic,
        io_operations=io_operations,
        patterns=patterns,
        external_references=external_references,
        security_issues=security_issues,
        generation_metadata=generation_metadata
    )

    # Convert to dict for JSON serialization
    return unified_ir.model_dump()


# ============================================================
# HELPER FUNCTIONS
# ============================================================

def _assess_cobol_complexity(
    data_ir: Optional[Dict],
    logic_ir: Optional[Dict],
    io_ir: Optional[Dict]
) -> str:
    """
    WHAT: Assess COBOL program complexity
    WHY: Affects code generation strategy
    HOW: Count entities, procedures, I/O operations

    Returns: "simple", "medium", or "complex"
    """
    # Count entities
    entity_count = 0
    if data_ir and data_ir.get('entities'):
        entity_count = len(data_ir['entities'])

    # Count procedures
    procedure_count = 0
    if logic_ir and logic_ir.get('procedures'):
        procedure_count = len(logic_ir['procedures'])

    # Count I/O operations
    io_count = 0
    if io_ir and io_ir.get('operations'):
        io_count = len(io_ir['operations'])

    # Calculate score
    score = (entity_count * 2) + (procedure_count * 3) + (io_count * 1)

    if score < 20:
        return "simple"
    elif score < 50:
        return "medium"
    else:
        return "complex"


def _merge_data_structures(data_ir: Optional[Dict]) -> Dict[str, Any]:
    """
    WHAT: Extract and deduplicate data structures from Data Agent
    WHY: Data Agent may return duplicate entities (bug in Week 1)
    HOW: Use set to deduplicate by entity name, keep first occurrence

    Returns: Dict with entities, cobol_files, copybooks, data_source
    """
    if not data_ir:
        return {
            "entities": [],
            "cobol_files": [],
            "copybooks": [],
            "data_source": {}
        }

    # Deduplicate entities by name (fix for Week 1 bug)
    entities = data_ir.get('entities', [])
    seen_names = set()
    unique_entities = []

    for entity in entities:
        name = entity.get('name')
        if name and name not in seen_names:
            seen_names.add(name)
            unique_entities.append(entity)

    return {
        "entities": unique_entities,
        "cobol_files": data_ir.get('cobol_files', []),
        "copybooks": data_ir.get('copybooks', []),
        "data_source": data_ir.get('data_source', {})
    }


def _merge_business_logic(logic_ir: Optional[Dict]) -> Dict[str, Any]:
    """
    WHAT: Extract business logic from Logic Agent
    WHY: Contains procedures, workflows, calculations, error handling
    HOW: Direct extraction - Logic Agent output is already well-structured

    Returns: Dict with procedures, workflows, calculations, error_handling
    """
    if not logic_ir:
        return {
            "procedures": [],
            "workflows": [],
            "calculations": [],
            "error_handling": []
        }

    return {
        "procedures": logic_ir.get('procedures', []),
        "workflows": logic_ir.get('workflows', []),
        "calculations": logic_ir.get('calculations', []),
        "error_handling": logic_ir.get('error_handling', [])
    }


def _merge_io_operations(io_ir: Optional[Dict]) -> Dict[str, Any]:
    """
    WHAT: Extract I/O operations from I/O Agent
    WHY: Contains CRUD operations, patterns, repository recommendations
    HOW: Direct extraction + organize by entity

    Returns: Dict with operations, patterns, repositories, file_status_handling
    """
    if not io_ir:
        return {
            "operations": [],
            "io_patterns": [],
            "repository_recommendations": [],
            "file_status_handling": []
        }

    return {
        "operations": io_ir.get('operations', []),
        "io_patterns": io_ir.get('io_patterns', []),
        "repository_recommendations": io_ir.get('repository_patterns', []),
        "file_status_handling": io_ir.get('file_status_handling', [])
    }


def _merge_patterns(
    logic_ir: Optional[Dict],
    io_ir: Optional[Dict]
) -> List[Dict[str, Any]]:
    """
    WHAT: Merge patterns from Logic and I/O agents
    WHY: Both agents detect patterns - combine without duplicating
    HOW: Combine lists, deduplicate by pattern_type

    Returns: List of unique patterns
    """
    patterns = []
    seen_types = set()

    # Add I/O patterns (higher priority - more specific)
    if io_ir and io_ir.get('io_patterns'):
        for pattern in io_ir['io_patterns']:
            pattern_type = pattern.get('pattern_type')
            if pattern_type and pattern_type not in seen_types:
                seen_types.add(pattern_type)
                patterns.append(pattern)

    # Add logic patterns if not already present
    if logic_ir:
        # Logic agent doesn't have a patterns field yet
        # But could be added in future
        pass

    return patterns


def _merge_external_references(
    data_ir: Optional[Dict],
    logic_ir: Optional[Dict]
) -> Dict[str, Any]:
    """
    WHAT: Extract external references (COPY books, external programs)
    WHY: Needed for dependency analysis
    HOW: Combine copybooks from Data Agent with any external calls from Logic Agent

    Returns: Dict with classes, modules, copybooks, controls
    """
    external_refs = {
        "classes": [],
        "modules": [],
        "copybooks": [],
        "controls": []
    }

    # Add COPY books from Data Agent
    if data_ir and data_ir.get('copybooks'):
        external_refs["copybooks"] = data_ir['copybooks']

    # Could add CALL statement analysis from Logic Agent in future
    # For now, COBOL external references are primarily COPY books

    return external_refs


def _generate_springboot_metadata(
    data_structures: Dict,
    business_logic: Dict,
    io_operations: Dict,
    patterns: List[Dict]
) -> Dict[str, Any]:
    """
    WHAT: Generate metadata for Spring Boot code generator
    WHY: Provides hints and estimates for code generation
    HOW: Calculate automation rate, manual effort, recommend patterns

    Returns: Dict with generation hints and estimates
    """
    # Count entities (for @Entity classes)
    entity_count = len(data_structures.get('entities', []))

    # Count procedures (for @Service methods)
    procedure_count = len(business_logic.get('procedures', []))

    # Count repository recommendations
    repo_count = len(io_operations.get('repository_recommendations', []))

    # Calculate automation rate
    # COBOL → Spring Boot is typically very automatable
    base_rate = 0.90

    # Adjust based on patterns
    if any(p.get('pattern_type') == 'SEQUENTIAL_READ_LOOP' for p in patterns):
        base_rate += 0.02
    if any(p.get('pattern_type') == 'MASTER_UPDATE' for p in patterns):
        base_rate += 0.02

    automation_rate = min(base_rate, 0.95)

    # Estimate manual effort (hours)
    # Base: 1 hour, +0.5 per entity, +0.3 per procedure
    manual_effort = 1.0 + (entity_count * 0.5) + (procedure_count * 0.3)
    manual_effort = round(manual_effort * 10) / 10

    # Calculate complexity score (0-10)
    complexity_score = min(
        (entity_count * 0.5) + (procedure_count * 0.3) + (repo_count * 0.2),
        10
    )
    complexity_score = round(complexity_score)

    # Recommend template
    has_batch = any(p.get('pattern_type') == 'SEQUENTIAL_READ_LOOP' for p in patterns)
    has_crud = any(p.get('pattern_type') == 'MASTER_UPDATE' for p in patterns)

    if has_batch:
        template = "spring-batch-job"
    elif has_crud:
        template = "spring-boot-crud-service"
    else:
        template = "spring-boot-service"

    # Generate implementation notes
    notes = []

    if entity_count > 0:
        notes.append(f"Generate {entity_count} @Entity class(es)")

    if repo_count > 0:
        notes.append(f"Generate {repo_count} JPA Repository interface(s)")

    if procedure_count > 0:
        notes.append(f"Generate {procedure_count} @Service method(s)")

    if data_structures.get('copybooks'):
        copybook_count = len(data_structures['copybooks'])
        notes.append(f"Analyze {copybook_count} COPY book(s) for shared structures")

    if has_batch:
        notes.append("Use Spring Batch for file processing loops")

    notes.append("Use JPA for database operations (replace file I/O)")
    notes.append("Use @Transactional for data consistency")

    return {
        "estimated_automation_rate": automation_rate,
        "estimated_manual_effort_hours": manual_effort,
        "complexity_score": complexity_score,
        "recommended_template": template,
        "generation_notes": notes
    }


# ============================================================
# VALIDATION
# ============================================================

def validate_cobol_ir(ir: Dict[str, Any]) -> List[str]:
    """
    WHAT: Validate unified COBOL IR structure
    WHY: Ensure all required sections present before code generation
    HOW: Check for 8 required top-level keys

    Args:
        ir: Unified COBOL IR dictionary

    Returns:
        List of validation errors (empty if valid)
    """
    required_sections = [
        'metadata',
        'data_structures',
        'business_logic',
        'io_operations',
        'patterns',
        'external_references',
        'security_issues',
        'generation_metadata'
    ]

    errors = []

    for section in required_sections:
        if section not in ir:
            errors.append(f"Missing required section: {section}")

    # Validate metadata has required fields
    if 'metadata' in ir:
        metadata = ir['metadata']
        required_metadata = ['source_language', 'source_file', 'confidence']

        for field in required_metadata:
            if field not in metadata:
                errors.append(f"Missing required metadata field: {field}")

    return errors

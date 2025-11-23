#!/usr/bin/env python3
"""
Universal IR Schema - Language-Agnostic Intermediate Representation

WHAT: Unified IR schema for all source languages (VB6, COBOL, PowerBuilder, Delphi, AS/400)
WHY: Enable consistent code generation across multiple source → target combinations
HOW: Pydantic models with 12 sections, superset of VB6 and COBOL IR needs

Design Principles:
1. Language-agnostic core sections
2. Extensible for future languages
3. Backward compatible with VB6 and COBOL IR
4. Type-safe with Pydantic validation
5. JSON-serializable for tooling integration

Version: 1.0.0
Schema Version: universal-ir-v1
"""

from typing import List, Dict, Optional, Any, Literal
from pydantic import BaseModel, Field
from datetime import datetime


# ============================================================================
# Section 1: Metadata
# ============================================================================

class SourceMetadata(BaseModel):
    """
    Source file and analysis metadata.
    Applies to all languages.
    """
    source_language: str = Field(..., description="Source language: VB6, COBOL, PowerBuilder, Delphi, AS400")
    source_file: str = Field(..., description="Original source file name")
    source_lines_of_code: int = Field(default=0, description="Lines of code in source file")
    source_path: Optional[str] = Field(None, description="Full path to source file")

    target_framework: str = Field(..., description="Target framework: Angular, SpringBoot, React, .NET")

    analysis_timestamp: str = Field(default_factory=lambda: datetime.now().isoformat())
    analyzer_version: str = Field(default="1.0.0-universal-ir")
    parser_type: str = Field(default="universal-parser")

    # Confidence scores (if using multi-agent approach)
    confidence: float = Field(default=0.0, ge=0.0, le=1.0)
    ui_agent_confidence: Optional[float] = Field(None, ge=0.0, le=1.0)
    data_agent_confidence: Optional[float] = Field(None, ge=0.0, le=1.0)
    logic_agent_confidence: Optional[float] = Field(None, ge=0.0, le=1.0)
    io_agent_confidence: Optional[float] = Field(None, ge=0.0, le=1.0)

    # Complexity metrics
    complexity: Literal["simple", "medium", "complex", "very_complex"] = Field(default="medium")
    complexity_score: Optional[int] = Field(None, ge=0, le=10)

    # Agent tracking
    subagents_used: List[str] = Field(default_factory=list)

    # Language-specific metadata extensions
    language_specific: Dict[str, Any] = Field(default_factory=dict)


# ============================================================================
# Section 2: Data Structures
# ============================================================================

class FieldDefinition(BaseModel):
    """
    Universal field definition for records, classes, entities.
    """
    name: str
    data_type: str  # String, Integer, BigDecimal, Boolean, Date, Custom
    length: Optional[int] = None
    decimals: Optional[int] = None
    nullable: bool = True
    default_value: Optional[str] = None

    # Validation constraints
    min_value: Optional[Any] = None
    max_value: Optional[Any] = None
    pattern: Optional[str] = None  # Regex pattern

    # UI-specific (for VB6, PowerBuilder)
    control_type: Optional[str] = None  # TextBox, ComboBox, Label, etc.
    label: Optional[str] = None

    # Database-specific (for COBOL, AS/400)
    column_name: Optional[str] = None
    primary_key: bool = False
    foreign_key: Optional[str] = None
    indexed: bool = False

    # COBOL-specific
    cobol_picture: Optional[str] = None
    cobol_level: Optional[int] = None

    # Metadata
    description: Optional[str] = None
    line_number: Optional[int] = None


class DataStructure(BaseModel):
    """
    Universal data structure (record, class, type, entity).
    """
    name: str
    type: Literal["record", "class", "type", "entity", "variable", "constant"]
    fields: List[FieldDefinition] = Field(default_factory=list)

    # Source context
    source: Optional[str] = None  # FILE SECTION, CLASS MODULE, TYPE definition
    parent_structure: Optional[str] = None

    # Database mapping
    table_name: Optional[str] = None
    schema_name: Optional[str] = None

    # Metadata
    description: Optional[str] = None
    start_line: Optional[int] = None
    end_line: Optional[int] = None


class FileDefinition(BaseModel):
    """
    File definition (COBOL files, VB6 data files).
    """
    select_name: str
    assign_to: Optional[str] = None
    organization: Optional[str] = None  # SEQUENTIAL, INDEXED, RELATIVE
    access_mode: Optional[str] = None  # SEQUENTIAL, RANDOM, DYNAMIC
    record_layout: Optional[str] = None
    file_status: Optional[str] = None

    # Metadata
    description: Optional[str] = None
    line_number: Optional[int] = None


class DataSource(BaseModel):
    """
    Data source configuration (database, file, API).
    """
    type: str  # database, sequential_file, indexed_file, api, memory
    connection_string: Optional[str] = None
    provider: Optional[str] = None

    # Additional config
    config: Dict[str, Any] = Field(default_factory=dict)


class DataStructuresSection(BaseModel):
    """
    All data structures in the source program.
    """
    entities: List[DataStructure] = Field(default_factory=list)
    files: List[FileDefinition] = Field(default_factory=list)
    copybooks: List[str] = Field(default_factory=list)  # COBOL COPY, VB6 includes
    data_source: Optional[DataSource] = None


# ============================================================================
# Section 3: UI Components
# ============================================================================

class UIControl(BaseModel):
    """
    UI control definition (VB6, PowerBuilder, Delphi forms).
    """
    name: str
    type: str  # TextBox, Button, Label, ComboBox, Grid, etc.

    # Properties
    caption: Optional[str] = None
    text: Optional[str] = None
    enabled: bool = True
    visible: bool = True
    tab_index: Optional[int] = None

    # Layout
    left: Optional[int] = None
    top: Optional[int] = None
    width: Optional[int] = None
    height: Optional[int] = None

    # Data binding
    data_field: Optional[str] = None
    data_source: Optional[str] = None

    # Validation
    required: bool = False
    validation_pattern: Optional[str] = None

    # Child controls (for containers)
    controls: List['UIControl'] = Field(default_factory=list)

    # Metadata
    description: Optional[str] = None


class UIForm(BaseModel):
    """
    UI form/window definition.
    """
    name: str
    title: Optional[str] = None
    type: Literal["form", "dialog", "mdi_child", "mdi_parent"] = "form"

    # Layout
    width: Optional[int] = None
    height: Optional[int] = None
    startup_position: Optional[str] = None

    # Controls
    controls: List[UIControl] = Field(default_factory=list)

    # Metadata
    description: Optional[str] = None


class UISection(BaseModel):
    """
    All UI components (VB6 forms, PowerBuilder windows).
    Not applicable for COBOL.
    """
    forms: List[UIForm] = Field(default_factory=list)
    has_ui: bool = False


# ============================================================================
# Section 4: Business Logic
# ============================================================================

class LogicStep(BaseModel):
    """
    Single step in business logic (assignment, call, condition, loop).
    """
    step_type: str  # data_operation, method_call, conditional, loop, calculation, etc.
    description: str

    # Operation details
    operation: Optional[str] = None
    target: Optional[str] = None
    source: Optional[str] = None
    condition: Optional[str] = None

    # Code reference
    code_snippet: Optional[str] = None
    line_number: Optional[int] = None

    # Target framework equivalent
    target_equivalent: Optional[str] = None


class Procedure(BaseModel):
    """
    Procedure, method, function, paragraph.
    """
    name: str
    type: str  # initialization, data_read, data_write, calculation, business_logic, cleanup
    logic_steps: List[LogicStep] = Field(default_factory=list)

    # Signature
    parameters: List[Dict[str, Any]] = Field(default_factory=list)
    return_type: Optional[str] = None

    # Source context
    start_line: Optional[int] = None
    end_line: Optional[int] = None

    # Metadata
    description: Optional[str] = None
    confidence: float = Field(default=0.9, ge=0.0, le=1.0)


class Workflow(BaseModel):
    """
    Workflow or process flow.
    """
    name: str
    entry_point: str
    procedures_called: List[str] = Field(default_factory=list)

    # Flow control
    parallel_execution: bool = False
    error_handling: Optional[str] = None

    # Metadata
    description: Optional[str] = None
    confidence: float = Field(default=0.85, ge=0.0, le=1.0)


class Calculation(BaseModel):
    """
    Business calculation or formula.
    """
    name: str
    formula: str
    inputs: List[str] = Field(default_factory=list)
    output: str

    # Metadata
    description: Optional[str] = None
    line_number: Optional[int] = None


class ErrorHandler(BaseModel):
    """
    Error handling logic.
    """
    type: str  # try_catch, on_error, at_end_clause, etc.
    scope: str
    handler: str

    # Metadata
    description: Optional[str] = None
    line_number: Optional[int] = None
    target_equivalent: Optional[str] = None


class BusinessLogicSection(BaseModel):
    """
    All business logic in the source program.
    """
    procedures: List[Procedure] = Field(default_factory=list)
    workflows: List[Workflow] = Field(default_factory=list)
    calculations: List[Calculation] = Field(default_factory=list)
    error_handling: List[ErrorHandler] = Field(default_factory=list)


# ============================================================================
# Section 5: I/O Operations
# ============================================================================

class IOOperation(BaseModel):
    """
    I/O operation (file, database, API).
    """
    type: str  # OPEN, CLOSE, READ, WRITE, DELETE, UPDATE
    entity: str
    mode: Optional[str] = None  # INPUT, OUTPUT, I-O, EXTEND

    # Operation details
    procedure: Optional[str] = None
    into_variable: Optional[str] = None
    from_variable: Optional[str] = None
    access_pattern: Optional[str] = None  # SEQUENTIAL, RANDOM, DYNAMIC
    key_field: Optional[str] = None

    # Error handling
    error_handling: Optional[str] = None

    # Metadata
    description: Optional[str] = None
    line_number: Optional[int] = None
    target_equivalent: Optional[str] = None
    confidence: float = Field(default=0.9, ge=0.0, le=1.0)


class IOPattern(BaseModel):
    """
    Detected I/O pattern.
    """
    pattern_type: str  # SEQUENTIAL_READ, BATCH_WRITE, CRUD, etc.
    pattern_name: str
    operations: List[str] = Field(default_factory=list)

    # Metadata
    description: Optional[str] = None
    target_equivalent: Optional[str] = None
    confidence: float = Field(default=0.9, ge=0.0, le=1.0)


class RepositoryRecommendation(BaseModel):
    """
    Repository pattern recommendation for target framework.
    """
    entity: str
    repository_type: str  # JpaRepository, ReadOnlyRepository, CustomRepository
    operations_needed: List[str] = Field(default_factory=list)

    # Metadata
    description: Optional[str] = None
    target_interface: Optional[str] = None
    confidence: float = Field(default=0.85, ge=0.0, le=1.0)


class IOOperationsSection(BaseModel):
    """
    All I/O operations in the source program.
    """
    operations: List[IOOperation] = Field(default_factory=list)
    io_patterns: List[IOPattern] = Field(default_factory=list)
    repository_recommendations: List[RepositoryRecommendation] = Field(default_factory=list)
    file_status_handling: List[Dict[str, Any]] = Field(default_factory=list)


# ============================================================================
# Section 6: Data Operations (VB6-specific, database interactions)
# ============================================================================

class DataOperation(BaseModel):
    """
    Data operation (VB6 recordset, ADO, DAO).
    """
    operation_type: str  # SELECT, INSERT, UPDATE, DELETE, OPEN_RECORDSET
    target_table: Optional[str] = None
    sql_query: Optional[str] = None
    recordset_name: Optional[str] = None

    # Metadata
    description: Optional[str] = None
    line_number: Optional[int] = None


class DataOperationsSection(BaseModel):
    """
    VB6-specific data operations (recordsets, ADO, DAO).
    """
    operations: List[DataOperation] = Field(default_factory=list)


# ============================================================================
# Section 7: Control Flow & Events
# ============================================================================

class EventHandler(BaseModel):
    """
    Event handler (VB6, PowerBuilder, Delphi).
    """
    event_name: str
    control_name: Optional[str] = None
    handler_code: str

    # Signature
    parameters: List[Dict[str, Any]] = Field(default_factory=list)

    # Metadata
    description: Optional[str] = None
    start_line: Optional[int] = None
    end_line: Optional[int] = None


class EventsSection(BaseModel):
    """
    All event handlers in the source program.
    """
    handlers: List[EventHandler] = Field(default_factory=list)


# ============================================================================
# Section 8: Patterns
# ============================================================================

class DetectedPattern(BaseModel):
    """
    Detected design pattern or coding pattern.
    """
    pattern_type: str
    pattern_name: str
    entities_involved: List[str] = Field(default_factory=list)

    # Metadata
    description: Optional[str] = None
    target_equivalent: Optional[str] = None
    confidence: float = Field(default=0.9, ge=0.0, le=1.0)


# ============================================================================
# Section 9: External References
# ============================================================================

class ExternalReferencesSection(BaseModel):
    """
    External dependencies (classes, modules, DLLs, APIs).
    """
    classes: List[str] = Field(default_factory=list)
    modules: List[str] = Field(default_factory=list)
    copybooks: List[str] = Field(default_factory=list)
    controls: List[str] = Field(default_factory=list)
    dlls: List[str] = Field(default_factory=list)
    apis: List[str] = Field(default_factory=list)


# ============================================================================
# Section 10: Security Issues
# ============================================================================

class SecurityIssue(BaseModel):
    """
    Detected security issue or vulnerability.
    """
    severity: Literal["low", "medium", "high", "critical"]
    issue_type: str  # SQL_INJECTION, XSS, HARDCODED_CREDENTIALS, etc.
    description: str
    location: Optional[str] = None
    line_number: Optional[int] = None

    # Remediation
    recommendation: Optional[str] = None


# ============================================================================
# Section 11: Repository Mapping
# ============================================================================

class RepositoryMapping(BaseModel):
    """
    Mapping of entities to repository interfaces.
    """
    entity_name: str
    repository_name: str
    repository_type: str
    operations: List[str] = Field(default_factory=list)

    # Custom queries
    custom_queries: List[Dict[str, str]] = Field(default_factory=list)


class RepositoryMappingSection(BaseModel):
    """
    Repository pattern mappings for code generation.
    """
    mappings: List[RepositoryMapping] = Field(default_factory=list)


# ============================================================================
# Section 12: Frontend Mapping (for UI frameworks)
# ============================================================================

class ComponentMapping(BaseModel):
    """
    Mapping of UI controls to frontend components.
    """
    source_control: str
    source_type: str
    target_component: str
    target_framework: str  # Angular, React, Vue

    # Props/bindings
    props: Dict[str, Any] = Field(default_factory=dict)
    events: Dict[str, str] = Field(default_factory=dict)


class FrontendMappingSection(BaseModel):
    """
    Frontend component mappings for UI frameworks.
    """
    mappings: List[ComponentMapping] = Field(default_factory=list)


# ============================================================================
# Section 13: Generation Metadata
# ============================================================================

class GenerationMetadata(BaseModel):
    """
    Code generation hints and metadata.
    """
    estimated_automation_rate: float = Field(default=0.9, ge=0.0, le=1.0)
    estimated_manual_effort_hours: float = Field(default=0.0, ge=0.0)
    complexity_score: int = Field(default=5, ge=0, le=10)
    recommended_template: str = Field(default="default")
    generation_notes: List[str] = Field(default_factory=list)

    # Target-specific hints
    target_specific_hints: Dict[str, Any] = Field(default_factory=dict)


# ============================================================================
# UNIVERSAL IR - Root Model
# ============================================================================

class UniversalIR(BaseModel):
    """
    Universal Intermediate Representation - Language-Agnostic IR

    Version: 1.0.0
    Schema: universal-ir-v1

    Supports: VB6, COBOL, PowerBuilder, Delphi, AS/400
    Targets: Angular, Spring Boot, React, .NET, Vue
    """

    # Required sections
    metadata: SourceMetadata
    data_structures: DataStructuresSection = Field(default_factory=DataStructuresSection)
    business_logic: BusinessLogicSection = Field(default_factory=BusinessLogicSection)
    io_operations: IOOperationsSection = Field(default_factory=IOOperationsSection)

    # Optional sections (language-specific)
    ui: UISection = Field(default_factory=UISection)
    data_operations: DataOperationsSection = Field(default_factory=DataOperationsSection)
    events: EventsSection = Field(default_factory=EventsSection)

    # Analysis sections
    patterns: List[DetectedPattern] = Field(default_factory=list)
    external_references: ExternalReferencesSection = Field(default_factory=ExternalReferencesSection)
    security_issues: List[SecurityIssue] = Field(default_factory=list)

    # Generation sections
    repository_mapping: RepositoryMappingSection = Field(default_factory=RepositoryMappingSection)
    frontend_mapping: FrontendMappingSection = Field(default_factory=FrontendMappingSection)
    generation_metadata: GenerationMetadata = Field(default_factory=GenerationMetadata)

    # Schema version
    schema_version: str = Field(default="universal-ir-v1")

    class Config:
        json_schema_extra = {
            "example": {
                "metadata": {
                    "source_language": "COBOL",
                    "source_file": "seq.cbl",
                    "target_framework": "SpringBoot",
                    "confidence": 0.87
                },
                "schema_version": "universal-ir-v1"
            }
        }


# Update forward references
UIControl.model_rebuild()

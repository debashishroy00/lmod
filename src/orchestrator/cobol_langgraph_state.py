#!/usr/bin/env python3
"""
LangGraph State Definition for COBOL Parsing Workflow

WHAT: Type-safe state schema for COBOL parsing workflow
WHY: LangGraph requires explicit state definition for type safety
HOW: TypedDict with annotations for automatic state merging

Design Pattern:
- Mirrors VB6 state structure (src/orchestrator/langgraph_state.py)
- Three agent outputs → merged IR → validation
- Robust error handling with accumulated errors
- Timing metrics for performance monitoring
"""

from typing import TypedDict, Optional, Annotated, List, Dict, Any
import operator


class COBOLState(TypedDict):
    """
    WHAT: Shared state across all agents in the COBOL parsing workflow
    WHY: LangGraph automatically manages and merges this state between nodes
    HOW: TypedDict with operator annotations for list/dict accumulation

    Flow:
        1. Input fields set by orchestrator on workflow invocation
        2. Agent outputs set by individual agent nodes (run in parallel)
        3. Merge output set by merge node (runs after agents complete)
        4. Validation result set by validate node
        5. Errors/timing accumulated across all nodes

    State Management:
    - Simple fields (str, int): Last write wins
    - Annotated[list, operator.add]: Append to list
    - Annotated[dict, operator.or_]: Merge dicts
    """

    # ============================================================
    # INPUT (set by orchestrator on workflow invocation)
    # ============================================================

    source_code: str           # COBOL source code
    source_file: str           # Filename (e.g., "seq.cbl", "CBL0001.cbl")

    # ============================================================
    # AGENT OUTPUTS (set by individual agent nodes - parallel execution)
    # ============================================================

    data_ir: Optional[Dict[str, Any]]      # Partial IR from Data Agent
                                            # Contains: entities, cobol_files, copybooks, data_source

    logic_ir: Optional[Dict[str, Any]]     # Partial IR from Logic Agent
                                            # Contains: procedures, workflows, calculations, error_handling

    io_ir: Optional[Dict[str, Any]]        # Partial IR from I/O Agent
                                            # Contains: operations, io_patterns, repository_patterns

    # ============================================================
    # MERGE OUTPUT (set by merge node - sequential after agents)
    # ============================================================

    complete_ir: Optional[Dict[str, Any]]  # Complete unified IR with 8 sections
                                            # Sections: metadata, data_structures, business_logic,
                                            #           io_operations, patterns, external_references,
                                            #           security_issues, generation_metadata

    # ============================================================
    # METADATA (calculated/accumulated)
    # ============================================================

    confidence: Optional[float]     # Overall confidence (0.0-1.0) - weighted average
    complexity: Optional[str]       # Complexity level: "simple", "medium", "complex"

    # ============================================================
    # ERROR HANDLING (accumulated using operator.add)
    # ============================================================
    # Annotated[list, operator.add] means: append to list, don't replace
    # Each node can add errors without overwriting previous ones
    errors: Annotated[List[str], operator.add]

    # ============================================================
    # TIMING (accumulated for performance measurement)
    # ============================================================
    # Annotated with operator.or_ to merge dicts from parallel nodes
    # Example: {"data_agent": 0.5, "logic_agent": 0.8, "io_agent": 0.6, "merge": 0.1}
    timing: Annotated[Dict[str, float], operator.or_]

#!/usr/bin/env python3
"""
LangGraph State Definition for VB6 Parsing Workflow

WHAT: Type-safe state schema for VB6 parsing workflow
WHY: LangGraph requires explicit state definition for type safety
HOW: TypedDict with annotations for automatic state merging
"""

from typing import TypedDict, Optional, Annotated, List, Dict, Any
import operator


class VB6State(TypedDict):
    """
    WHAT: Shared state across all agents in the VB6 parsing workflow
    WHY: LangGraph automatically manages and merges this state between nodes
    HOW: TypedDict with operator annotations for list accumulation

    Flow:
        1. Input fields set by orchestrator
        2. Agent outputs set by individual agent nodes
        3. Merge output set by merge node
        4. Errors/timing accumulated across all nodes
    """

    # ============================================================
    # INPUT (set by orchestrator on workflow invocation)
    # ============================================================
    source_code: str           # VB6 form source code
    source_file: str           # Filename (e.g., "StartForm.frm")

    # ============================================================
    # AGENT OUTPUTS (set by individual agent nodes)
    # ============================================================
    ui_ir: Optional[Dict[str, Any]]      # Partial IR from UI Agent
    logic_ir: Optional[Dict[str, Any]]   # Partial IR from Logic Agent
    data_ir: Optional[Dict[str, Any]]    # Partial IR from Data Agent

    # ============================================================
    # MERGE OUTPUT (set by merge node)
    # ============================================================
    complete_ir: Optional[Dict[str, Any]]  # Complete IR with all 8 sections

    # ============================================================
    # UNIVERSAL IR (Phase 2 - set by convert_to_universal_ir node)
    # ============================================================
    universal_ir: Optional[Dict[str, Any]]  # Universal IR (language-agnostic schema)
                                             # Replaces language-specific IR for code generation

    validation_metrics: Optional[Dict[str, Any]]  # Validation metrics from Universal IR validator

    # ============================================================
    # METADATA (accumulated/calculated)
    # ============================================================
    confidence: Optional[float]     # Overall confidence (0.0-1.0)
    complexity: Optional[str]       # Complexity level (simple/medium/complex)

    # ============================================================
    # ERROR HANDLING (accumulated using operator.add)
    # ============================================================
    # Annotated[list, operator.add] means: append to list, don't replace
    errors: Annotated[List[str], operator.add]

    # ============================================================
    # TIMING (accumulated for performance measurement)
    # ============================================================
    # Annotated with operator.or_ to merge dicts from parallel nodes
    timing: Annotated[Dict[str, float], operator.or_]  # {"ui_agent": 1.2, "logic_agent": 1.5, ...}

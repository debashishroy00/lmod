#!/usr/bin/env python3
"""
LangGraph Node Functions for COBOL Parsing Workflow

WHAT: Node functions for COBOL LangGraph workflow
WHY: Each node receives state, calls agent, returns state updates
HOW: Wrap existing COBOL agents in LangGraph-compatible node functions

Design Pattern:
- Mirrors VB6 node structure (src/orchestrator/langgraph_nodes.py)
- Reuses existing COBOL agent classes (no duplication)
- Robust error handling with retry logic
- Timing metrics for each node
- No LLM calls - agents are pure Python parsers
"""

import time
from typing import Dict, Any

# Import state definition
from orchestrator.cobol_langgraph_state import COBOLState

# Import existing COBOL agent classes (reuse them!)
from agents.cobol.cobol_data_agent import COBOLDataAgent
from agents.cobol.cobol_logic_agent import COBOLLogicAgent
from agents.cobol.cobol_io_agent import COBOLIOAgent

# Import COBOL IR merger
from core.cobol_ir import merge_cobol_ir, validate_cobol_ir


# ============================================================
# AGENT INITIALIZATION (no LLM - pure Python parsers)
# ============================================================

def initialize_cobol_agents():
    """
    WHAT: Initialize all 3 COBOL agents
    WHY: Create agent instances for workflow
    HOW: Instantiate each agent class

    Note: COBOL agents are pure Python parsers (no LLM calls)
    Unlike VB6 agents which use Claude API

    Returns:
        Tuple of (data_agent, logic_agent, io_agent)
    """
    data_agent = COBOLDataAgent()
    logic_agent = COBOLLogicAgent()
    io_agent = COBOLIOAgent()

    return data_agent, logic_agent, io_agent


# Global agents (initialized by workflow builder)
_data_agent = None
_logic_agent = None
_io_agent = None


def set_cobol_agents(data_agent, logic_agent, io_agent):
    """Set global agent instances (called by workflow builder)"""
    global _data_agent, _logic_agent, _io_agent
    _data_agent = data_agent
    _logic_agent = logic_agent
    _io_agent = io_agent


# ============================================================
# NODE FUNCTIONS (called by LangGraph)
# ============================================================

def data_agent_node(state: COBOLState) -> Dict[str, Any]:
    """
    WHAT: Data Agent Node - Extract data structures from COBOL
    WHY: Specialized extraction of WORKING-STORAGE, FILE SECTION, etc.
    HOW: Call existing COBOLDataAgent, return state updates

    LangGraph automatically:
    - Passes current state
    - Merges returned dict into state
    - Handles parallel execution with other agents

    Args:
        state: Current workflow state

    Returns:
        Dict with data_ir and timing updates
    """
    start_time = time.time()

    try:
        print("  💾 Data Agent: Extracting data structures...")

        # Call COBOL Data Agent (pure Python, no LLM)
        data_ir = _data_agent.analyze(
            state['source_code'],
            state['source_file']
        )

        elapsed = time.time() - start_time

        # Extract metrics for display
        entities_count = len(data_ir.get('entities', []))
        files_count = len(data_ir.get('cobol_files', []))
        confidence = data_ir.get('confidence', 0.0)

        print(f"  ✓ Data Agent: Found {entities_count} entities")
        print(f"  ✓ Data Agent: Found {files_count} file(s)")
        print(f"  ✓ Data Agent: Confidence {confidence:.1%}")
        print(f"  ⏱  Data Agent: {elapsed:.2f}s")

        # Return updates to merge into state
        return {
            "data_ir": data_ir,
            "timing": {"data_agent": elapsed}
        }

    except Exception as e:
        print(f"  ✗ Data Agent: Failed - {e}")
        # Return error in errors list (will be accumulated)
        return {
            "errors": [f"Data Agent error: {str(e)}"],
            "timing": {"data_agent": time.time() - start_time}
        }


def logic_agent_node(state: COBOLState) -> Dict[str, Any]:
    """
    WHAT: Logic Agent Node - Extract business logic from PROCEDURE DIVISION
    WHY: Specialized extraction of procedures, control flow, calculations
    HOW: Call existing COBOLLogicAgent, return state updates

    Args:
        state: Current workflow state

    Returns:
        Dict with logic_ir and timing updates
    """
    start_time = time.time()

    try:
        print("  ⚙️  Logic Agent: Extracting business logic...")

        # Call COBOL Logic Agent (pure Python, no LLM)
        logic_ir = _logic_agent.analyze(
            state['source_code'],
            state['source_file']
        )

        elapsed = time.time() - start_time

        # Extract metrics for display
        procedures_count = len(logic_ir.get('procedures', []))
        workflows_count = len(logic_ir.get('workflows', []))
        calculations_count = len(logic_ir.get('calculations', []))
        confidence = logic_ir.get('confidence', 0.0)

        print(f"  ✓ Logic Agent: Found {procedures_count} procedures")
        print(f"  ✓ Logic Agent: Found {workflows_count} workflow(s)")
        print(f"  ✓ Logic Agent: Found {calculations_count} calculation(s)")
        print(f"  ✓ Logic Agent: Confidence {confidence:.1%}")
        print(f"  ⏱  Logic Agent: {elapsed:.2f}s")

        return {
            "logic_ir": logic_ir,
            "timing": {"logic_agent": elapsed}
        }

    except Exception as e:
        print(f"  ✗ Logic Agent: Failed - {e}")
        return {
            "errors": [f"Logic Agent error: {str(e)}"],
            "timing": {"logic_agent": time.time() - start_time}
        }


def io_agent_node(state: COBOLState) -> Dict[str, Any]:
    """
    WHAT: I/O Agent Node - Extract file I/O operations and patterns
    WHY: Specialized extraction of CRUD operations, access patterns
    HOW: Call existing COBOLIOAgent, return state updates

    Args:
        state: Current workflow state

    Returns:
        Dict with io_ir and timing updates
    """
    start_time = time.time()

    try:
        print("  📁 I/O Agent: Extracting file operations...")

        # Call COBOL I/O Agent (pure Python, no LLM)
        io_ir = _io_agent.analyze(
            state['source_code'],
            state['source_file']
        )

        elapsed = time.time() - start_time

        # Extract metrics for display
        operations_count = len(io_ir.get('operations', []))
        patterns_count = len(io_ir.get('io_patterns', []))
        repos_count = len(io_ir.get('repository_patterns', []))
        confidence = io_ir.get('confidence', 0.0)

        print(f"  ✓ I/O Agent: Found {operations_count} I/O operations")
        print(f"  ✓ I/O Agent: Detected {patterns_count} pattern(s)")
        print(f"  ✓ I/O Agent: Recommended {repos_count} repository/ies")
        print(f"  ✓ I/O Agent: Confidence {confidence:.1%}")
        print(f"  ⏱  I/O Agent: {elapsed:.2f}s")

        return {
            "io_ir": io_ir,
            "timing": {"io_agent": elapsed}
        }

    except Exception as e:
        print(f"  ✗ I/O Agent: Failed - {e}")
        return {
            "errors": [f"I/O Agent error: {str(e)}"],
            "timing": {"io_agent": time.time() - start_time}
        }


def merge_node(state: COBOLState) -> Dict[str, Any]:
    """
    WHAT: Merge Node - Combine partial IRs into complete unified IR
    WHY: Each agent outputs one section, need unified IR for Spring Boot generation
    HOW: Call merge_cobol_ir() from core.cobol_ir module

    This is the COBOL equivalent of VB6's merge_node

    Args:
        state: Current workflow state with data_ir, logic_ir, io_ir

    Returns:
        Dict with complete_ir, confidence, complexity
    """
    start_time = time.time()

    print("\n🔗 Merging partial IRs...")

    try:
        # Extract partial IRs from state
        data_ir = state.get('data_ir')
        logic_ir = state.get('logic_ir')
        io_ir = state.get('io_ir')

        # Get source info
        source_file = state.get('source_file', 'unknown.cbl')
        source_lines = len(state.get('source_code', '').split('\n'))

        # Call unified merger (handles None values gracefully)
        complete_ir = merge_cobol_ir(
            data_ir=data_ir,
            logic_ir=logic_ir,
            io_ir=io_ir,
            source_file=source_file,
            source_lines=source_lines
        )

        elapsed = time.time() - start_time

        # Extract metadata for display
        metadata = complete_ir.get('metadata', {})
        overall_confidence = metadata.get('confidence', 0.0)
        complexity = metadata.get('complexity', 'unknown')

        print(f"✓ Merge complete in {elapsed:.2f}s")
        print(f"📈 Overall confidence: {overall_confidence:.1%}")
        print(f"🎯 Complexity: {complexity}")

        # Show confidence breakdown
        print(f"   - Data Agent: {metadata.get('data_agent_confidence', 0.0):.1%}")
        print(f"   - Logic Agent: {metadata.get('logic_agent_confidence', 0.0):.1%}")
        print(f"   - I/O Agent: {metadata.get('io_agent_confidence', 0.0):.1%}")

        return {
            "complete_ir": complete_ir,
            "confidence": overall_confidence,
            "complexity": complexity,
            "timing": {"merge": elapsed}
        }

    except Exception as e:
        print(f"✗ Merge error: {e}")
        import traceback
        traceback.print_exc()
        return {
            "errors": [f"Merge error: {str(e)}"],
            "timing": {"merge": time.time() - start_time}
        }


def validate_node(state: COBOLState) -> Dict[str, Any]:
    """
    WHAT: Validate Node - Check schema compliance
    WHY: Ensure IR has all required sections before writing to disk
    HOW: Call validate_cobol_ir() from core.cobol_ir module

    This is the COBOL equivalent of VB6's validate_node

    Args:
        state: Current workflow state with complete_ir

    Returns:
        Dict with errors if validation fails, empty dict if valid
    """
    print("✓ Validating schema compliance...")

    complete_ir = state.get('complete_ir', {})

    # Validate using core.cobol_ir validator
    validation_errors = validate_cobol_ir(complete_ir)

    if validation_errors:
        print(f"✗ Validation failed: {len(validation_errors)} error(s)")
        for error in validation_errors:
            print(f"  - {error}")

        return {
            "errors": validation_errors
        }

    print("✓ Schema validation passed")
    print("  - All 8 required sections present")
    print("  - Metadata fields complete")

    return {}  # No updates needed if valid

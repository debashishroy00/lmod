#!/usr/bin/env python3
"""
LangGraph Workflow Definition for COBOL Parsing

WHAT: Production-grade COBOL parsing workflow using LangGraph
WHY: Orchestrate three COBOL agents in parallel with robust error handling
HOW: StateGraph with parallel agent execution + sequential merge/validate

Design Pattern:
- Mirrors VB6 workflow structure (src/orchestrator/langgraph_workflow.py)
- Three agents run in PARALLEL → synchronized → merged → validated
- No LLM calls (COBOL agents are pure Python parsers)
- Robust error handling
- Timing metrics

Architecture:
    START → [Data Agent, Logic Agent, I/O Agent] → Merge → Validate → END
             ↑ These 3 run in parallel ↑
"""

from langgraph.graph import StateGraph, START, END

# Import state and nodes
from orchestrator.cobol_langgraph_state import COBOLState
from orchestrator.cobol_langgraph_nodes import (
    initialize_cobol_agents,
    set_cobol_agents,
    data_agent_node,
    logic_agent_node,
    io_agent_node,
    merge_node,
    validate_node
)


def build_cobol_workflow():
    """
    WHAT: Build and compile the COBOL parsing workflow
    WHY: LangGraph provides production features (parallel execution, state management, error handling)
    HOW: Define nodes, edges for parallel execution, compile

    Architecture:
        START → [Data Agent, Logic Agent, I/O Agent] → Merge → Validate → END
                 ↑ These 3 run in parallel ↑

    Key Differences from VB6:
    - No API key needed (pure Python parsers, no LLM)
    - No model selection (no LLM calls)
    - Faster execution (no API latency)

    Returns:
        Compiled LangGraph application ready to invoke
    """
    print("🔧 Building COBOL LangGraph workflow...")

    # Initialize COBOL agents (pure Python parsers)
    data_agent, logic_agent, io_agent = initialize_cobol_agents()
    set_cobol_agents(data_agent, logic_agent, io_agent)

    # Create StateGraph with COBOLState schema
    workflow = StateGraph(COBOLState)

    # ========================================
    # Add nodes (functions that process state)
    # ========================================

    # Agent nodes (run in parallel)
    workflow.add_node("data_agent", data_agent_node)
    workflow.add_node("logic_agent", logic_agent_node)
    workflow.add_node("io_agent", io_agent_node)

    # Processing nodes (run sequentially)
    workflow.add_node("merge", merge_node)
    workflow.add_node("validate", validate_node)

    # ========================================
    # Define edges (execution flow)
    # ========================================

    # PARALLEL EXECUTION: All 3 agents start simultaneously from START
    # LangGraph will run these concurrently
    workflow.add_edge(START, "data_agent")
    workflow.add_edge(START, "logic_agent")
    workflow.add_edge(START, "io_agent")

    # SYNCHRONIZATION: All agents must complete before merge
    # LangGraph automatically waits for all incoming edges
    workflow.add_edge("data_agent", "merge")
    workflow.add_edge("logic_agent", "merge")
    workflow.add_edge("io_agent", "merge")

    # SEQUENTIAL: merge → validate → end
    workflow.add_edge("merge", "validate")
    workflow.add_edge("validate", END)

    # ========================================
    # Compile workflow into executable app
    # ========================================

    # Compile creates optimized execution plan
    # Benefits:
    # - Automatic parallelization (3 agents run concurrently)
    # - State management (automatic merging)
    # - Error recovery (errors accumulated, not fatal)
    # - Deterministic execution order
    app = workflow.compile()

    print("✅ COBOL LangGraph workflow compiled successfully")
    print("   - 3 parallel agent nodes (Data, Logic, I/O)")
    print("   - 2 sequential processing nodes (Merge, Validate)")
    print("   - Auto state management")
    print("   - Pure Python (no LLM calls)")
    print()

    return app


# ============================================================
# ORCHESTRATOR CLASS (API compatible with COBOL agents)
# ============================================================

class LangGraphCOBOLOrchestrator:
    """
    WHAT: LangGraph-based COBOL orchestrator
    WHY: Production-ready orchestration with parallel execution and error handling
    HOW: Wrap LangGraph workflow with simple API

    Similar to LangGraphVB6Orchestrator but:
    - No LLM calls (pure Python parsing)
    - No API key needed
    - Synchronous execution (no async/await)
    - Faster (no API latency)

    Usage:
        orchestrator = LangGraphCOBOLOrchestrator()
        ir = orchestrator.parse(cobol_source, "program.cbl")
    """

    def __init__(self):
        """
        WHAT: Initialize LangGraph COBOL orchestrator
        WHY: Build workflow once, reuse for multiple files
        HOW: Compile LangGraph workflow with agents

        No Args needed - COBOL agents are pure Python parsers
        """
        print("=" * 60)
        print("🔍 COBOL Parser v1.0.0 (LangGraph Architecture)")
        print("=" * 60)
        print()

        self.app = build_cobol_workflow()

    def parse(self, cobol_content: str, source_file: str = "unknown.cbl") -> dict:
        """
        WHAT: Parse COBOL program using LangGraph workflow
        WHY: Production-grade orchestration with automatic parallelization
        HOW: Invoke workflow with input state, return complete IR

        Key Benefits:
        - 3 agents run in parallel (faster than sequential)
        - Robust error handling (partial results if one agent fails)
        - Complete timing breakdown
        - Unified IR with 8 sections

        Args:
            cobol_content: COBOL source code
            source_file: Filename (e.g., "seq.cbl", "CBL0001.cbl")

        Returns:
            Complete unified IR dictionary with 8 sections:
            - metadata
            - data_structures
            - business_logic
            - io_operations
            - patterns
            - external_references
            - security_issues
            - generation_metadata
        """
        print(f"📄 Parsing: {source_file}")
        print(f"📊 Size: {len(cobol_content)} chars, {len(cobol_content.split(chr(10)))} lines")
        print()
        print("⚡ Launching agents in parallel via LangGraph...")
        print()

        # Execute workflow
        # LangGraph automatically:
        # - Runs 3 agents in parallel (concurrent execution)
        # - Waits for all to complete (synchronization)
        # - Merges state updates (automatic state management)
        # - Handles errors gracefully (accumulated in errors list)

        # Note: invoke() is synchronous (not async) for COBOL
        # VB6 uses ainvoke() because it calls async LLM APIs
        result = self.app.invoke({
            "source_code": cobol_content,
            "source_file": source_file,
            "data_ir": None,
            "logic_ir": None,
            "io_ir": None,
            "complete_ir": None,
            "confidence": None,
            "complexity": None,
            "errors": [],    # Initialize empty (Annotated[list, operator.add])
            "timing": {}     # Initialize empty timing dict
        })

        # Check for errors accumulated during workflow
        if result.get('errors'):
            print()
            print("⚠️  Errors encountered during parsing:")
            for error in result['errors']:
                print(f"  - {error}")
            print()

        # Display timing breakdown
        timing = result.get('timing', {})
        if timing:
            print()
            print("⏱  Timing breakdown:")
            for node, duration in timing.items():
                print(f"  - {node}: {duration:.2f}s")

            total_time = sum(timing.values())
            print(f"  - Total: {total_time:.2f}s")
            print()

        # Validate we got complete IR
        complete_ir = result.get('complete_ir')
        if not complete_ir:
            raise ValueError("Workflow did not produce complete IR")

        print("🎉 Parsing complete!")
        print(f"   Confidence: {result.get('confidence', 0.0):.1%}")
        print(f"   Complexity: {result.get('complexity', 'unknown')}")
        print()

        return complete_ir

#!/usr/bin/env python3
"""
LangGraph Workflow Definition for VB6 Parsing

WHAT: Production-grade VB6 parsing workflow using LangGraph
WHY: Replace custom async orchestration with proven framework
HOW: StateGraph with parallel agent execution + sequential merge/validate
"""

import os
from langgraph.graph import StateGraph, START, END

# Import state and nodes
from orchestrator.langgraph_state import VB6State
from orchestrator.langgraph_nodes import (
    initialize_agents,
    set_agents,
    ui_agent_node,
    logic_agent_node,
    data_agent_node,
    merge_node,
    validate_node
)


def build_vb6_workflow(api_key: str = None, model: str = None):
    """
    WHAT: Build and compile the VB6 parsing workflow
    WHY: LangGraph provides production features (retry, streaming, checkpoints)
    HOW: Define nodes, edges for parallel execution, compile

    Architecture:
        START → [UI Agent, Logic Agent, Data Agent] → Merge → Validate → END
                 ↑ These 3 run in parallel ↑

    Args:
        api_key: Anthropic API key (optional, loads from .env)
        model: Model to use (optional, defaults to Haiku in dev, Sonnet in prod)

    Returns:
        Compiled LangGraph application ready to invoke
    """
    print("🔧 Building LangGraph workflow...")

    # Initialize agents with model selection
    ui_agent, logic_agent, data_agent = initialize_agents(api_key, model)
    set_agents(ui_agent, logic_agent, data_agent)

    # Create StateGraph with VB6State schema
    workflow = StateGraph(VB6State)

    # ========================================
    # Add nodes (functions that process state)
    # ========================================

    # Agent nodes (run in parallel)
    workflow.add_node("ui_agent", ui_agent_node)
    workflow.add_node("logic_agent", logic_agent_node)
    workflow.add_node("data_agent", data_agent_node)

    # Processing nodes (run sequentially)
    workflow.add_node("merge", merge_node)
    workflow.add_node("validate", validate_node)

    # ========================================
    # Define edges (execution flow)
    # ========================================

    # PARALLEL EXECUTION: All 3 agents start simultaneously from START
    workflow.add_edge(START, "ui_agent")
    workflow.add_edge(START, "logic_agent")
    workflow.add_edge(START, "data_agent")

    # SYNCHRONIZATION: All agents must complete before merge
    # LangGraph automatically waits for all incoming edges
    workflow.add_edge("ui_agent", "merge")
    workflow.add_edge("logic_agent", "merge")
    workflow.add_edge("data_agent", "merge")

    # SEQUENTIAL: merge → validate → end
    workflow.add_edge("merge", "validate")
    workflow.add_edge("validate", END)

    # ========================================
    # Compile workflow into executable app
    # ========================================

    # Compile creates optimized execution plan
    # Benefits:
    # - Automatic parallelization
    # - State management
    # - Error recovery
    # - Streaming support
    app = workflow.compile()

    print("✅ LangGraph workflow compiled successfully")
    print("   - 3 parallel agent nodes")
    print("   - 2 sequential processing nodes")
    print("   - Auto state management")
    print()

    return app


# ============================================================
# ORCHESTRATOR CLASS (API compatible with existing code)
# ============================================================

class LangGraphVB6Orchestrator:
    """
    WHAT: LangGraph-based VB6 orchestrator
    WHY: Production-ready replacement for custom async orchestrator
    HOW: Wrap LangGraph workflow with same API as original

    Drop-in replacement for vb6_orchestrator.VB6Orchestrator:
    - Same parse() method signature
    - Same output format
    - Enhanced: retry, streaming, error handling
    """

    def __init__(self, api_key: str = None, model: str = None):
        """
        WHAT: Initialize LangGraph orchestrator
        WHY: Build workflow once, reuse for multiple files
        HOW: Compile LangGraph workflow with agents

        Args:
            api_key: Anthropic API key (optional, loads from .env)
            model: Model to use (optional, defaults to Haiku in dev)
        """
        print("=" * 60)
        print("🔍 VB6 Parser v2.1.0 (LangGraph Architecture)")
        print("=" * 60)
        print()

        self.app = build_vb6_workflow(api_key, model)

    async def parse(self, frm_content: str, source_file: str = "unknown.frm") -> dict:
        """
        WHAT: Parse VB6 form using LangGraph workflow
        WHY: Production-grade orchestration with automatic parallelization
        HOW: Invoke workflow with input state, return complete IR

        Args:
            frm_content: VB6 form source code
            source_file: Filename (e.g., "StartForm.frm")

        Returns:
            Complete IR dictionary with all 8 sections
        """
        print(f"📄 Parsing: {source_file}")
        print(f"📊 Size: {len(frm_content)} chars, {len(frm_content.split(chr(10)))} lines")
        print()
        print("⚡ Launching agents in parallel via LangGraph...")
        print()

        # Execute workflow
        # LangGraph automatically:
        # - Runs 3 agents in parallel
        # - Waits for all to complete
        # - Merges state updates
        # - Handles errors gracefully
        # - Provides streaming support (future)

        result = await self.app.ainvoke({
            "source_code": frm_content,
            "source_file": source_file,
            "ui_ir": None,
            "logic_ir": None,
            "data_ir": None,
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
                print(f"  - {node}: {duration:.1f}s")

            total_time = sum(timing.values())
            print(f"  - Total: {total_time:.1f}s")
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

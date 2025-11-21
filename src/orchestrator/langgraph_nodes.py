#!/usr/bin/env python3
"""
LangGraph Node Functions for VB6 Parsing Workflow

WHAT: Node functions for LangGraph workflow
WHY: Each node receives state, calls agent, returns state updates
HOW: Wrap existing agents in LangGraph-compatible node functions
"""

import time
import os
import asyncio
from typing import Dict, Any
from datetime import datetime
from anthropic import AsyncAnthropic
from dotenv import load_dotenv

# Import state definition
from orchestrator.langgraph_state import VB6State

# Import existing agent classes (reuse them!)
from agents.vb6_ui_agent import VB6UIAgent
from agents.vb6_logic_agent import VB6LogicAgent
from agents.vb6_data_agent import VB6DataAgent

# Load .env file from src directory
load_dotenv(os.path.join(os.path.dirname(__file__), '..', '.env'))


# ============================================================
# RETRY LOGIC WITH EXPONENTIAL BACKOFF
# ============================================================

async def retry_with_backoff(func, max_retries=3, initial_delay=1.0):
    """
    WHAT: Retry function with exponential backoff
    WHY: Handle transient API errors (rate limits, overload)
    HOW: Exponential backoff: 1s, 2s, 4s, 8s...

    Args:
        func: Async function to retry
        max_retries: Maximum number of retry attempts
        initial_delay: Initial delay in seconds

    Returns:
        Function result on success

    Raises:
        Last exception if all retries fail
    """
    delay = initial_delay
    last_exception = None

    for attempt in range(max_retries + 1):
        try:
            return await func()
        except Exception as e:
            last_exception = e
            error_str = str(e)

            # Check if retryable error
            is_overload = "overloaded" in error_str.lower() or "529" in error_str or "500" in error_str
            is_rate_limit = "rate_limit" in error_str.lower() or "429" in error_str
            is_timeout = "timeout" in error_str.lower()
            is_server_error = "api_error" in error_str.lower() or "internal server" in error_str.lower()

            is_retryable = is_overload or is_rate_limit or is_timeout or is_server_error

            if attempt < max_retries and is_retryable:
                print(f"  ⚠️  Retryable error: {error_str[:100]}")
                print(f"  🔄 Retry {attempt + 1}/{max_retries} in {delay:.1f}s...")
                await asyncio.sleep(delay)
                delay *= 2  # Exponential backoff
            else:
                # Non-retryable error or max retries reached
                raise

    # Should never reach here, but for safety
    raise last_exception


# ============================================================
# AGENT INITIALIZATION (shared Anthropic client)
# ============================================================

def initialize_agents(api_key: str = None, model: str = None):
    """
    WHAT: Initialize all 3 agents with shared client
    WHY: Reuse connection, avoid redundant clients
    HOW: Create AsyncAnthropic once, pass to agents

    Args:
        api_key: Anthropic API key (optional, loads from .env)
        model: Model to use (optional, defaults based on env)

    Returns:
        Tuple of (ui_agent, logic_agent, data_agent)
    """
    # Get API key from .env or environment
    api_key = api_key or os.getenv('ANTHROPIC_API_KEY')
    if not api_key:
        raise ValueError(
            "ANTHROPIC_API_KEY not found in .env or environment.\n"
            "Add to src/.env: ANTHROPIC_API_KEY=\"your-key-here\""
        )

    # Determine model based on environment
    # DEV: Use Haiku (fast, cheap: $0.25/1M input tokens)
    # PROD: Use Sonnet 4 (accurate: $3/1M input tokens)
    if model is None:
        env = os.getenv('ENVIRONMENT', 'dev').lower()
        if env == 'prod' or env == 'production':
            model = "claude-sonnet-4-20250514"
            print("🚀 Using Claude Sonnet 4 (production mode)")
        else:
            model = "claude-3-5-haiku-20241022"
            print("⚡ Using Claude Haiku (development mode - faster & cheaper)")

    # Create shared client
    client = AsyncAnthropic(api_key=api_key)

    # Initialize agents with model override
    ui_agent = VB6UIAgent(client)
    logic_agent = VB6LogicAgent(client)
    data_agent = VB6DataAgent(client)

    # Override model for dev
    ui_agent.model = model
    logic_agent.model = model
    data_agent.model = model

    return ui_agent, logic_agent, data_agent


# Global agents (initialized by workflow builder)
_ui_agent = None
_logic_agent = None
_data_agent = None


def set_agents(ui_agent, logic_agent, data_agent):
    """Set global agent instances (called by workflow builder)"""
    global _ui_agent, _logic_agent, _data_agent
    _ui_agent = ui_agent
    _logic_agent = logic_agent
    _data_agent = data_agent


# ============================================================
# NODE FUNCTIONS (called by LangGraph)
# ============================================================

async def ui_agent_node(state: VB6State) -> Dict[str, Any]:
    """
    WHAT: UI Agent Node - Extract form and controls
    WHY: Specialized extraction = high accuracy
    HOW: Call existing VB6UIAgent with retry logic, return state updates

    LangGraph automatically:
    - Passes current state
    - Merges returned dict into state
    - Handles parallel execution

    Args:
        state: Current workflow state

    Returns:
        Dict with ui_ir and timing updates
    """
    start_time = time.time()

    try:
        print("  🎨 UI Agent: Extracting form and controls...")

        # Call agent with retry logic (handles 529 overload, rate limits)
        async def call_agent():
            return await _ui_agent.extract(state['source_code'])

        ui_ir = await retry_with_backoff(call_agent, max_retries=3, initial_delay=2.0)

        elapsed = time.time() - start_time

        # Extract metrics for display
        controls_count = len(ui_ir.get('ui', {}).get('controls', []))
        confidence = ui_ir.get('ui', {}).get('confidence', 0.0)

        print(f"  ✓ UI Agent: Found {controls_count} controls")
        print(f"  ✓ UI Agent: Confidence {confidence:.1%}")
        print(f"  ⏱  UI Agent: {elapsed:.1f}s")

        # Return updates to merge into state
        return {
            "ui_ir": ui_ir,
            "timing": {"ui_agent": elapsed}
        }

    except Exception as e:
        print(f"  ✗ UI Agent: Failed after retries - {e}")
        # Return error in errors list (will be accumulated)
        return {
            "errors": [f"UI Agent error: {str(e)}"],
            "timing": {"ui_agent": time.time() - start_time}
        }


async def logic_agent_node(state: VB6State) -> Dict[str, Any]:
    """
    WHAT: Logic Agent Node - Extract event handlers and validations
    WHY: Specialized extraction = high accuracy
    HOW: Call existing VB6LogicAgent with retry logic, return state updates

    Args:
        state: Current workflow state

    Returns:
        Dict with logic_ir and timing updates
    """
    start_time = time.time()

    try:
        print("  ⚙️  Logic Agent: Extracting event handlers and validations...")

        # Call agent with retry logic
        async def call_agent():
            return await _logic_agent.extract(state['source_code'])

        logic_ir = await retry_with_backoff(call_agent, max_retries=3, initial_delay=2.0)

        elapsed = time.time() - start_time

        # Extract metrics for display
        handlers_count = len(logic_ir.get('logic', {}).get('event_handlers', []))
        validations_count = len(logic_ir.get('logic', {}).get('validations', []))
        confidence = logic_ir.get('logic', {}).get('confidence', 0.0)

        print(f"  ✓ Logic Agent: Found {handlers_count} event handlers")
        print(f"  ✓ Logic Agent: Found {validations_count} validations")
        print(f"  ✓ Logic Agent: Confidence {confidence:.1%}")
        print(f"  ⏱  Logic Agent: {elapsed:.1f}s")

        return {
            "logic_ir": logic_ir,
            "timing": {"logic_agent": elapsed}
        }

    except Exception as e:
        print(f"  ✗ Logic Agent: Failed after retries - {e}")
        return {
            "errors": [f"Logic Agent error: {str(e)}"],
            "timing": {"logic_agent": time.time() - start_time}
        }


async def data_agent_node(state: VB6State) -> Dict[str, Any]:
    """
    WHAT: Data Agent Node - Extract business entities and operations
    WHY: Specialized extraction = high accuracy
    HOW: Call existing VB6DataAgent with retry logic, return state updates

    Args:
        state: Current workflow state

    Returns:
        Dict with data_ir and timing updates
    """
    start_time = time.time()

    try:
        print("  💾 Data Agent: Extracting entities and operations...")

        # Call agent with retry logic
        async def call_agent():
            return await _data_agent.extract(state['source_code'])

        data_ir = await retry_with_backoff(call_agent, max_retries=3, initial_delay=2.0)

        elapsed = time.time() - start_time

        # Extract metrics for display
        entities_count = len(data_ir.get('data', {}).get('entities', []))
        operations_count = len(data_ir.get('data', {}).get('operations', []))
        confidence = data_ir.get('data', {}).get('confidence', 0.0)

        print(f"  ✓ Data Agent: Found {entities_count} entities")
        print(f"  ✓ Data Agent: Found {operations_count} operations")
        print(f"  ✓ Data Agent: Confidence {confidence:.1%}")
        print(f"  ⏱  Data Agent: {elapsed:.1f}s")

        return {
            "data_ir": data_ir,
            "timing": {"data_agent": elapsed}
        }

    except Exception as e:
        print(f"  ✗ Data Agent: Failed after retries - {e}")
        return {
            "errors": [f"Data Agent error: {str(e)}"],
            "timing": {"data_agent": time.time() - start_time}
        }


def merge_node(state: VB6State) -> Dict[str, Any]:
    """
    WHAT: Merge Node - Combine partial IRs into complete IR
    WHY: Each agent outputs one section, need unified IR
    HOW: Merge algorithm from original orchestrator

    This replicates the logic from vb6_orchestrator.py:_merge_irs()

    Args:
        state: Current workflow state with ui_ir, logic_ir, data_ir

    Returns:
        Dict with complete_ir, confidence, complexity
    """
    start_time = time.time()

    print("\n🔗 Merging partial IRs...")

    try:
        # Extract partial IRs from state
        ui_ir = state.get('ui_ir', {})
        logic_ir = state.get('logic_ir', {})
        data_ir = state.get('data_ir', {})

        # Extract core sections
        ui_section = ui_ir.get('ui', {})
        logic_section = logic_ir.get('logic', {})
        data_section = data_ir.get('data', {})

        # Calculate overall confidence (weighted average)
        ui_conf = ui_section.get('confidence', 0.0)
        logic_conf = logic_section.get('confidence', 0.0)
        data_conf = data_section.get('confidence', 0.0)

        # UI: 30%, Logic: 40%, Data: 20%, Base: 10%
        overall_confidence = ui_conf * 0.3 + logic_conf * 0.4 + data_conf * 0.2 + 0.95 * 0.1

        # Assess complexity
        complexity = _assess_complexity(ui_section, logic_section, data_section)

        # Build metadata (10 fields to match golden fixture)
        metadata = {
            'source_language': 'VB6',
            'source_file': state['source_file'],
            'source_lines_of_code': len(state['source_code'].split('\n')),
            'target_framework': 'Angular',
            'analysis_timestamp': datetime.now().isoformat(),
            'analyzer_version': '2.1.0-langgraph',
            'parser_type': 'langgraph-subagent',
            'confidence': round(overall_confidence, 3),
            'complexity': complexity,
            'subagents_used': ['vb6-ui-agent', 'vb6-logic-agent', 'vb6-data-agent']
        }

        # Detect cross-section patterns
        patterns = _detect_patterns(ui_section, logic_section, data_section)

        # Extract external references (merge from logic error_handling and data)
        external_refs = _extract_external_refs(logic_section, data_section)

        # Extract security issues from logic error_handling
        security_issues = _extract_security_issues(logic_section)

        # Calculate generation metadata
        generation_metadata = {
            'estimated_automation_rate': _calculate_automation_rate(patterns),
            'estimated_manual_effort_hours': _estimate_manual_effort(
                len(ui_section.get('controls', [])),
                len(logic_section.get('event_handlers', []))
            ),
            'complexity_score': _calculate_complexity_score(ui_section, logic_section),
            'recommended_template': _recommend_template(patterns),
            'generation_notes': _generate_notes(external_refs, security_issues)
        }

        # Assemble complete IR (8 sections)
        complete_ir = {
            'metadata': metadata,
            'ui': ui_section,
            'logic': logic_section,
            'data': data_section,
            'patterns': patterns,
            'external_references': external_refs,
            'security_issues': security_issues,
            'generation_metadata': generation_metadata
        }

        elapsed = time.time() - start_time

        print(f"✓ Merge complete in {elapsed:.1f}s")
        print(f"📈 Overall confidence: {overall_confidence:.1%}")
        print(f"🎯 Complexity: {complexity}")

        return {
            "complete_ir": complete_ir,
            "confidence": overall_confidence,
            "complexity": complexity,
            "timing": {"merge": elapsed}
        }

    except Exception as e:
        print(f"✗ Merge error: {e}")
        return {
            "errors": [f"Merge error: {str(e)}"],
            "timing": {"merge": time.time() - start_time}
        }


def validate_node(state: VB6State) -> Dict[str, Any]:
    """
    WHAT: Validate Node - Check schema compliance
    WHY: Ensure IR has all required sections before returning
    HOW: Validate presence of 8 required sections

    Args:
        state: Current workflow state with complete_ir

    Returns:
        Dict with errors if validation fails, empty dict if valid
    """
    print("✓ Validating schema compliance...")

    complete_ir = state.get('complete_ir', {})

    # Check all 8 required sections present
    required_sections = [
        'metadata', 'ui', 'logic', 'data',
        'patterns', 'external_references',
        'security_issues', 'generation_metadata'
    ]

    missing = [s for s in required_sections if s not in complete_ir]

    if missing:
        error_msg = f"Missing IR sections: {missing}"
        print(f"✗ Validation failed: {error_msg}")
        return {
            "errors": [error_msg]
        }

    print("✓ Schema validation passed")
    return {}  # No updates needed if valid


# ============================================================
# HELPER FUNCTIONS (from original orchestrator)
# ============================================================

def _assess_complexity(ui: Dict, logic: Dict, data: Dict) -> str:
    """Assess overall complexity level"""
    control_count = len(ui.get('controls', []))
    handler_count = len(logic.get('event_handlers', []))

    score = control_count + (handler_count * 2)

    if score < 15:
        return 'simple'
    elif score < 40:
        return 'medium'
    else:
        return 'complex'


def _detect_patterns(ui: Dict, logic: Dict, data: Dict) -> list:
    """Detect design patterns across sections"""
    patterns = []

    # Get patterns from logic section (agents may detect some)
    logic_patterns = logic.get('patterns', [])
    patterns.extend(logic_patterns)

    # Add UI-based pattern detection
    if ui.get('form', {}).get('border_style') == 'FixedDialog':
        patterns.append({
            'pattern_type': 'MODAL_DIALOG',
            'pattern_name': 'Modal Dialog',
            'confidence': 0.95
        })

    return patterns


def _extract_external_refs(logic: Dict, data: Dict) -> Dict:
    """Extract external references from logic and data sections"""
    external_refs = {
        'classes': [],
        'modules': [],
        'copybooks': [],
        'controls': []
    }

    # Extract classes from event handlers (external classes used)
    classes_set = set()
    for handler in logic.get('event_handlers', []):
        for step in handler.get('logic_steps', []):
            # Look for object_creation steps with external classes
            if step.get('step_type') == 'object_creation':
                code = step.get('code_snippet', '')
                # Parse "Dim x As New ClassName" or "Dim x As ClassName"
                if ' As New ' in code:
                    class_name = code.split(' As New ')[-1].strip()
                    classes_set.add(class_name)
                elif ' As ' in code and ' As New ' not in code:
                    class_name = code.split(' As ')[-1].strip()
                    if class_name and class_name[0].isupper():  # Class names start with uppercase
                        classes_set.add(class_name)

    # Extract functions from event handlers (external modules)
    modules_set = set()
    for handler in logic.get('event_handlers', []):
        for step in handler.get('logic_steps', []):
            if step.get('step_type') == 'method_call':
                code = step.get('code_snippet', '')
                # Look for function calls like GetClient()
                if '(' in code and '.' not in code.split('(')[0]:
                    func_name = code.split('(')[0].split()[-1]
                    if func_name and func_name[0].isupper():
                        modules_set.add(func_name)

    # Build classes list
    for class_name in classes_set:
        external_refs['classes'].append({
            'name': class_name,
            'source': 'external',
            '_source_note': f'Referenced but not defined in this file',
            'methods_called': []
        })

    # Build modules list with functions
    if modules_set:
        external_refs['modules'].append({
            'name': 'unknown_module',
            '_what': 'Module containing external functions',
            'type': 'standard_module',
            '_type_note': 'Likely .bas file',
            'functions_called': list(modules_set)
        })

    return external_refs


def _extract_security_issues(logic: Dict) -> list:
    """Convert error_handling entries into security_issues format"""
    security_issues = []

    for error_handler in logic.get('error_handling', []):
        issue = {
            '_source_lines': error_handler.get('_source_lines', ''),
            '_what': error_handler.get('_what', ''),
            'issue_type': 'INSECURE_ERROR_HANDLING' if 'error' in error_handler.get('type', '').lower() else 'OTHER',
            'severity': 'medium',
            'location': error_handler.get('scope', ''),
            'description': error_handler.get('_security_issue', ''),
            'recommendation': error_handler.get('_recommendation', ''),
            '_angular_code': error_handler.get('_angular_equivalent', '')
        }
        security_issues.append(issue)

    return security_issues


def _calculate_automation_rate(patterns: list) -> float:
    """Calculate estimated automation rate"""
    rate = 0.85

    # Adjust based on patterns
    if any(p.get('pattern_type') == 'CRUD_FORM' for p in patterns):
        rate += 0.05

    return min(rate, 0.95)


def _estimate_manual_effort(control_count: int, handler_count: int) -> float:
    """Estimate manual effort in hours"""
    base = 0.2
    effort = base + (control_count * 0.05) + (handler_count * 0.1)
    return round(effort * 10) / 10


def _calculate_complexity_score(ui: Dict, logic: Dict) -> int:
    """Calculate complexity score (0-10)"""
    control_count = len(ui.get('controls', []))
    handler_count = len(logic.get('event_handlers', []))

    control_score = min(control_count / 10, 3)
    handler_score = min(handler_count / 5, 3)
    logic_steps = sum(len(h.get('logic_steps', [])) for h in logic.get('event_handlers', []))
    logic_score = min(logic_steps / 20, 4)

    return round(control_score + handler_score + logic_score)


def _recommend_template(patterns: list) -> str:
    """Recommend Angular template based on patterns"""
    pattern_types = [p.get('pattern_type') for p in patterns]

    if 'MODAL_DIALOG' in pattern_types:
        return 'angular-dialog-form'
    if 'CRUD_FORM' in pattern_types:
        return 'angular-crud-form'
    if 'SEARCH_FORM' in pattern_types:
        return 'angular-search-form'

    return 'angular-basic-component'


def _generate_notes(external_refs: Dict, security_issues: list) -> list:
    """Generate implementation notes"""
    notes = []

    if external_refs.get('classes'):
        class_names = [c['name'] for c in external_refs['classes']]
        notes.append(f"Create interfaces/services for: {', '.join(class_names)}")

    if security_issues:
        notes.append(f"Fix {len(security_issues)} security issue(s)")

    notes.append('Use Angular Material Dialog for modal behavior')
    notes.append('Use Reactive Forms with validation')

    return notes

#!/usr/bin/env python3
"""
VB6 Orchestrator - Subagent System Controller

WHAT: Main controller for parallel subagent processing
WHY: Manages 3 specialized agents, merges results for enterprise scale
HOW: Async API calls, parallel execution, IR merging
"""

import asyncio
import os
import sys
from typing import Dict, Any, List
from anthropic import AsyncAnthropic


class VB6Orchestrator:
    """
    WHAT: Main controller for subagent system
    WHY: Manages parallel agents, merges results at scale
    HOW: Async API calls, IR merging, validation
    """

    def __init__(self, api_key: str = None):
        """
        WHAT: Initialize orchestrator with API client
        WHY: Setup for parallel agent execution
        HOW: Create async Anthropic client, prepare agents
        """
        # Get API key from environment if not provided
        self.api_key = api_key or os.environ.get('ANTHROPIC_API_KEY')

        if not self.api_key:
            raise ValueError(
                "ANTHROPIC_API_KEY not found. Set environment variable or pass api_key parameter.\n"
                "Example: export ANTHROPIC_API_KEY='your-key-here'"
            )

        # Create async client
        self.client = AsyncAnthropic(api_key=self.api_key)

        # Agent registry (will be populated with actual agents)
        self.agents = {}

    def register_agent(self, name: str, agent):
        """
        WHAT: Register a specialized agent
        WHY: Allow dynamic agent composition
        HOW: Store agent in registry by name
        """
        self.agents[name] = agent

    async def parse(self, frm_content: str, source_file: str = "unknown.frm") -> Dict[str, Any]:
        """
        WHAT: Parse VB6 form using 3 parallel subagents
        WHY: Parallel processing = faster, specialized = accurate
        HOW: Launch agents async, merge partial IRs

        Args:
            frm_content: VB6 form source code
            source_file: Name of source file (for metadata)

        Returns:
            Complete IR JSON with all sections
        """
        print(f"🔍 VB6 Orchestrator - Parsing {source_file}")
        print(f"📊 File size: {len(frm_content)} characters, {len(frm_content.split(chr(10)))} lines")

        # Verify agents are registered
        required_agents = ['ui', 'logic', 'data']
        for agent_name in required_agents:
            if agent_name not in self.agents:
                raise ValueError(f"Agent '{agent_name}' not registered. Call register_agent() first.")

        print(f"\n⚡ Launching 3 agents in parallel...")
        start_time = asyncio.get_event_loop().time()

        # Launch all agents in parallel (THIS IS THE KEY!)
        ui_task = self.agents['ui'].extract(frm_content)
        logic_task = self.agents['logic'].extract(frm_content)
        data_task = self.agents['data'].extract(frm_content)

        # Wait for all to complete (~2 min total, not 6 min sequential)
        try:
            ui_ir, logic_ir, data_ir = await asyncio.gather(
                ui_task,
                logic_task,
                data_task
            )
        except Exception as e:
            print(f"\n❌ Error during agent execution: {e}")
            raise

        elapsed = asyncio.get_event_loop().time() - start_time
        print(f"\n✅ All agents completed in {elapsed:.1f} seconds")

        # Merge partial IRs into complete IR
        print(f"🔗 Merging partial IRs...")
        complete_ir = self._merge_irs({
            'ui': ui_ir,
            'logic': logic_ir,
            'data': data_ir
        }, source_file)

        # Validate completeness
        print(f"✓ Validating schema compliance...")
        self._validate_ir(complete_ir)

        print(f"\n🎉 Parsing complete!")
        print(f"   Confidence: {complete_ir['metadata']['confidence']:.1%}")
        print(f"   Complexity: {complete_ir['metadata']['complexity']}")

        return complete_ir

    def _merge_irs(self, partial_irs: Dict[str, Dict], source_file: str) -> Dict[str, Any]:
        """
        WHAT: Combine partial IRs into complete IR
        WHY: Each agent outputs one section, need unified IR
        HOW: Merge algorithm with metadata generation

        Args:
            partial_irs: Dictionary with 'ui', 'logic', 'data' keys
            source_file: Source file name for metadata

        Returns:
            Complete IR with all sections
        """
        ui_data = partial_irs.get('ui', {})
        logic_data = partial_irs.get('logic', {})
        data_data = partial_irs.get('data', {})

        # Extract core sections
        ui_section = ui_data.get('ui', {})
        logic_section = logic_data.get('logic', {})
        data_section = data_data.get('data', {})

        # Build metadata
        metadata = {
            'source_language': 'VB6',
            'source_file': source_file,
            'source_lines_of_code': len(partial_irs.get('ui', {}).get('_source_content', '').split('\n')),
            'target_framework': 'Angular',
            'analysis_timestamp': self._get_timestamp(),
            'analyzer_version': '2.0.0-subagent',
            'confidence': self._calculate_overall_confidence(
                ui_section.get('confidence', 0.0),
                logic_section.get('confidence', 0.0),
                data_section.get('confidence', 0.0)
            ),
            'complexity': self._assess_complexity(ui_section, logic_section, data_section),
            'subagents_used': ['vb6-ui-agent', 'vb6-logic-agent', 'vb6-data-agent']
        }

        # Detect cross-section patterns
        patterns = self._detect_patterns(ui_section, logic_section, data_section)

        # Extract external references (from all sections)
        external_refs = self._extract_external_refs(logic_section, data_section)

        # Detect security issues (from logic section primarily)
        security_issues = logic_section.get('security_issues', [])

        # Calculate generation metadata
        generation_metadata = {
            'estimated_automation_rate': self._calculate_automation_rate(patterns),
            'estimated_manual_effort_hours': self._estimate_manual_effort(
                len(ui_section.get('controls', [])),
                len(logic_section.get('event_handlers', []))
            ),
            'complexity_score': self._calculate_complexity_score(ui_section, logic_section),
            'recommended_template': self._recommend_template(patterns),
            'generation_notes': self._generate_notes(external_refs, security_issues)
        }

        # Assemble complete IR
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

        return complete_ir

    def _calculate_overall_confidence(self, ui_conf: float, logic_conf: float, data_conf: float) -> float:
        """Calculate weighted overall confidence"""
        # UI weight: 30%, Logic weight: 40%, Data weight: 20%, Base: 10%
        return ui_conf * 0.3 + logic_conf * 0.4 + data_conf * 0.2 + 0.95 * 0.1

    def _assess_complexity(self, ui: Dict, logic: Dict, data: Dict) -> str:
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

    def _detect_patterns(self, ui: Dict, logic: Dict, data: Dict) -> List[Dict]:
        """Detect design patterns across sections"""
        patterns = []

        # Pattern detection logic (simplified for now)
        # In production, this would analyze all sections together

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

    def _extract_external_refs(self, logic: Dict, data: Dict) -> Dict:
        """Extract external references from logic and data sections"""
        # Combine external references from both sections
        external_refs = {
            'classes': [],
            'modules': []
        }

        # Get from data section
        if 'external_references' in data:
            external_refs = data['external_references']

        # Could also get from logic section if provided

        return external_refs

    def _calculate_automation_rate(self, patterns: List[Dict]) -> float:
        """Calculate estimated automation rate"""
        # Base rate
        rate = 0.85

        # Adjust based on patterns
        if any(p['pattern_type'] == 'CRUD_FORM' for p in patterns):
            rate += 0.05

        return min(rate, 0.95)

    def _estimate_manual_effort(self, control_count: int, handler_count: int) -> float:
        """Estimate manual effort in hours"""
        base = 0.2
        effort = base + (control_count * 0.05) + (handler_count * 0.1)
        return round(effort * 10) / 10

    def _calculate_complexity_score(self, ui: Dict, logic: Dict) -> int:
        """Calculate complexity score (0-10)"""
        control_count = len(ui.get('controls', []))
        handler_count = len(logic.get('event_handlers', []))

        control_score = min(control_count / 10, 3)
        handler_score = min(handler_count / 5, 3)
        logic_steps = sum(len(h.get('logic_steps', [])) for h in logic.get('event_handlers', []))
        logic_score = min(logic_steps / 20, 4)

        return round(control_score + handler_score + logic_score)

    def _recommend_template(self, patterns: List[Dict]) -> str:
        """Recommend Angular template based on patterns"""
        pattern_types = [p['pattern_type'] for p in patterns]

        if 'MODAL_DIALOG' in pattern_types:
            return 'angular-dialog-form'
        if 'CRUD_FORM' in pattern_types:
            return 'angular-crud-form'
        if 'SEARCH_FORM' in pattern_types:
            return 'angular-search-form'

        return 'angular-basic-component'

    def _generate_notes(self, external_refs: Dict, security_issues: List) -> List[str]:
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

    def _get_timestamp(self) -> str:
        """Get current timestamp in ISO format"""
        from datetime import datetime
        return datetime.now().isoformat()

    def _validate_ir(self, ir: Dict[str, Any]):
        """
        WHAT: Validate IR has all required sections
        WHY: Ensure completeness before returning
        HOW: Check for required keys
        """
        required_sections = [
            'metadata', 'ui', 'logic', 'data',
            'patterns', 'external_references',
            'security_issues', 'generation_metadata'
        ]

        for section in required_sections:
            if section not in ir:
                raise ValueError(f"Missing required IR section: {section}")

        # Validate metadata has required fields
        required_metadata = ['source_language', 'source_file', 'confidence', 'complexity']
        for field in required_metadata:
            if field not in ir['metadata']:
                raise ValueError(f"Missing required metadata field: {field}")

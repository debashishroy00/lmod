#!/usr/bin/env python3
"""
VB6 Logic Agent - Specialized Logic Extraction

WHAT: Extract ONLY business logic (events, validations, workflows) from VB6 code
WHY: Specialization = higher accuracy for complex logic
HOW: Claude API with focused prompt, output partial IR
"""

import json
from typing import Dict, Any
from anthropic import AsyncAnthropic
from ir_canonicalizer import canonicalize_ir


class VB6LogicAgent:
    """
    WHAT: Specialized agent for logic extraction
    WHY: Focus on event handlers and business logic = 95%+ accuracy
    HOW: Targeted prompt + Claude Sonnet 4 API
    """

    def __init__(self, client: AsyncAnthropic):
        """
        WHAT: Initialize Logic agent with API client
        WHY: Reuse orchestrator's client for efficiency
        HOW: Store client reference
        """
        self.client = client
        self.model = "claude-sonnet-4-20250514"

    async def extract(self, frm_content: str) -> Dict[str, Any]:
        """
        WHAT: Extract business logic from VB6 form
        WHY: Generate partial IR with ONLY logic section
        HOW: Send specialized prompt to Claude API

        Args:
            frm_content: VB6 form source code

        Returns:
            Partial IR with logic section and confidence score
        """
        print("  ⚙️  Logic Agent: Extracting event handlers and validations...")

        # Build specialized prompt
        prompt = self._build_logic_prompt(frm_content)

        # Call Claude API
        try:
            # Haiku max: 8192, Sonnet max: 16000
            max_tokens = 8000 if "haiku" in self.model.lower() else 16000

            response = await self.client.messages.create(
                model=self.model,
                max_tokens=max_tokens,
                temperature=0.0,  # Deterministic for parsing
                messages=[{
                    "role": "user",
                    "content": prompt
                }]
            )

            # Extract JSON from response
            response_text = response.content[0].text
            partial_ir = self._parse_response(response_text)

            print(f"  ✓ Logic Agent: Found {len(partial_ir.get('logic', {}).get('event_handlers', []))} event handlers")
            print(f"  ✓ Logic Agent: Found {len(partial_ir.get('logic', {}).get('validations', []))} validations")
            print(f"  ✓ Logic Agent: Confidence {partial_ir.get('logic', {}).get('confidence', 0.0):.1%}")

            return partial_ir

        except Exception as e:
            print(f"  ✗ Logic Agent: Error - {e}")
            # Return minimal partial IR on error
            return {
                'logic': {
                    'event_handlers': [],
                    'validations': [],
                    'workflows': [],
                    'error_handling': [],
                    'calculations': [],
                    'confidence': 0.0
                }
            }

    def _build_logic_prompt(self, frm_content: str) -> str:
        """
        WHAT: Build specialized logic extraction prompt
        WHY: Focused prompt = better accuracy
        HOW: Template with instructions + VB6 code
        """
        prompt = f"""You are a VB6 business logic extraction specialist. Your ONLY job is to extract event handlers, validations, and workflows.

**CRITICAL**: Output ONLY the JSON. No explanations, no markdown, no code blocks.

**INPUT**: VB6 Form Source Code

**YOUR TASK**: Extract ONLY:
1. Event handlers (Click, Load, Change, etc.)
2. Validations (Len(), Trim(), IsNumeric, etc.)
3. Workflows (multi-step processes)
4. Error handling patterns
5. Calculations (if any)

**DO NOT EXTRACT**:
- Form properties or control definitions (UI Agent's job)
- Data entities or database operations (Data Agent's job)

**OUTPUT FORMAT** (JSON only, no markdown):
{{
  "logic": {{
    "_source_lines": "Lines X-Y",
    "_what": "Business logic, event handlers, validations",
    "_why": "Defines component behavior and validation rules",
    "confidence": 0.88,
    "_confidence_why": "Explanation of confidence score",

    "event_handlers": [
      {{
        "_handler_index": 1,
        "_source_lines": "Lines 71-78",
        "_what": "Brief description of what handler does",
        "control_id": "cmdNew",
        "event_type": "Click",
        "handler_name": "cmdNew_Click",
        "_handler_signature": "Private Sub cmdNew_Click()",
        "logic_steps": [
          {{
            "step_type": "object_creation",
            "description": "Create new Client object",
            "code_snippet": "Dim objClient As New Client",
            "_line": 72,
            "_angular_equivalent": "const objClient = new Client(); or this.clientService.createNew()"
          }},
          {{
            "step_type": "method_call",
            "description": "Pass client to form",
            "code_snippet": "Set frm.Client = objClient",
            "_line": 75,
            "_angular_equivalent": "Pass data via dialog config or @Input"
          }},
          {{
            "step_type": "navigation",
            "description": "Show form as modal dialog",
            "code_snippet": "frm.Show vbModal",
            "_line": 76,
            "_angular_equivalent": "this.dialog.open(ClientEditComponent, {{ data: objClient }})",
            "_modal_note": "vbModal = blocks parent until closed"
          }}
        ],
        "confidence": 0.85,
        "_confidence_why": "Logic clear, but Client and ClientEdit are external dependencies"
      }}
    ],

    "validations": [
      {{
        "_validation_index": 1,
        "_source_lines": "Lines 87-88",
        "_what": "Validate client ID exists in database",
        "_implicit": true,
        "field": "txtID",
        "_field_why": "Input comes from txtID.Text",
        "rule_type": "custom",
        "_rule_subtype": "database_lookup",
        "rule_value": "Client must exist in database",
        "error_message": "Client ID not found",
        "_error_message_source": "Line 88: MsgBox \\"Client ID not found\\"",
        "original_code": "If objClient Is Nothing Then\\n  MsgBox \\"Client ID not found\\"",
        "confidence": 0.90,
        "_confidence_why": "Validation logic is clear"
      }},
      {{
        "_validation_index": 2,
        "_what": "Implicit type validation - ID must be numeric",
        "_source_lines": "Line 85",
        "_implicit": true,
        "field": "txtID",
        "rule_type": "numeric",
        "_rule_reasoning": "CLng() function requires numeric string",
        "rule_value": "Must convert to Long integer",
        "error_message": "Type mismatch",
        "_error_message_note": "VB6 runtime error if CLng() fails",
        "original_code": "CLng(txtID.Text)",
        "confidence": 0.75,
        "_confidence_why": "Implicit validation, error handling may suppress"
      }}
    ],

    "calculations": [],
    "_calculations_note": "No calculations in this form",

    "workflows": [
      {{
        "_workflow_index": 1,
        "_what": "New client creation workflow",
        "name": "create_new_client",
        "condition": "User clicks 'New' button",
        "actions": [
          {{
            "action_type": "show_form",
            "description": "Open ClientEdit dialog with new client",
            "parameters": {{
              "form": "ClientEdit",
              "mode": "create",
              "modal": true
            }}
          }}
        ],
        "confidence": 0.85
      }}
    ],

    "error_handling": [
      {{
        "_source_lines": "Line 84",
        "_what": "Error suppression for GetClient operation",
        "type": "on_error_resume_next",
        "scope": "cmdOpen_Click",
        "handler": "Resume next statement (suppress errors)",
        "_security_issue": "Hides errors instead of handling them properly",
        "_angular_equivalent": "Should use try-catch with proper error handling",
        "_recommendation": "Replace with explicit error handling and user feedback"
      }}
    ]
  }}
}}

**IMPORTANT RULES**:
1. Include ALL underscore-prefixed fields (_what, _source_lines, _confidence_why, etc.) for documentation
2. Use step_type values: "object_creation", "method_call", "navigation", "validation", "message", "other"
3. For validations, use rule_type: "required", "numeric", "length", "range", "custom", "pattern"
4. For workflows, use action_type: "show_form", "data_operation", "navigation", "validation", "other"
5. Always include confidence scores (0.0-1.0) and _confidence_why explanations
6. Set _implicit: true for validations that aren't explicit validation functions
7. Include Angular equivalents in _angular_equivalent fields to help code generation

**VB6 FORM SOURCE CODE**:
```vb6
{frm_content}
```

Output the JSON now (no markdown, no code blocks):"""

        return prompt

    def _parse_response(self, response_text: str) -> Dict[str, Any]:
        """
        WHAT: Parse Claude's response into partial IR
        WHY: Handle various response formats
        HOW: Extract JSON, handle markdown wrapping
        """
        # Remove markdown code blocks if present
        text = response_text.strip()

        if text.startswith("```json"):
            text = text[7:]
        elif text.startswith("```"):
            text = text[3:]

        if text.endswith("```"):
            text = text[:-3]

        text = text.strip()

        # Parse JSON
        try:
            partial_ir = json.loads(text)

            # Validate structure
            if 'logic' not in partial_ir:
                raise ValueError("Response missing 'logic' section")

            # Ensure required subsections exist
            logic = partial_ir['logic']
            if 'event_handlers' not in logic:
                logic['event_handlers'] = []
            if 'validations' not in logic:
                logic['validations'] = []
            if 'workflows' not in logic:
                logic['workflows'] = []
            if 'error_handling' not in logic:
                logic['error_handling'] = []
            if 'calculations' not in logic:
                logic['calculations'] = []

            # Canonicalize IR for consistent comparison
            partial_ir = canonicalize_ir(partial_ir, ir_type="logic")

            return partial_ir

        except json.JSONDecodeError as e:
            print(f"  ✗ Logic Agent: JSON parse error - {e}")
            # Return minimal structure
            return {
                'logic': {
                    'event_handlers': [],
                    'validations': [],
                    'workflows': [],
                    'error_handling': [],
                    'calculations': [],
                    'confidence': 0.0
                }
            }

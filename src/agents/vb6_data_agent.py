#!/usr/bin/env python3
"""
VB6 Data Agent - Specialized Data Extraction

WHAT: Extract ONLY data entities and operations from VB6 code
WHY: Specialization = accurate data modeling
HOW: Claude API with focused prompt, output partial IR
"""

import json
from typing import Dict, Any
from anthropic import AsyncAnthropic
from ir_canonicalizer import canonicalize_ir


class VB6DataAgent:
    """
    WHAT: Specialized agent for data extraction
    WHY: Focus on entities and operations = accurate data models
    HOW: Targeted prompt + Claude Sonnet 4 API
    """

    def __init__(self, client: AsyncAnthropic):
        """
        WHAT: Initialize Data agent with API client
        WHY: Reuse orchestrator's client for efficiency
        HOW: Store client reference
        """
        self.client = client
        self.model = "claude-sonnet-4-20250514"

    async def extract(self, frm_content: str) -> Dict[str, Any]:
        """
        WHAT: Extract data entities and operations from VB6 form
        WHY: Generate partial IR with ONLY data section
        HOW: Send specialized prompt to Claude API

        Args:
            frm_content: VB6 form source code

        Returns:
            Partial IR with data section and confidence score
        """
        print("  💾 Data Agent: Extracting entities and operations...")

        # Build specialized prompt
        prompt = self._build_data_prompt(frm_content)

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

            print(f"  ✓ Data Agent: Found {len(partial_ir.get('data', {}).get('entities', []))} entities")
            print(f"  ✓ Data Agent: Found {len(partial_ir.get('data', {}).get('operations', []))} operations")
            print(f"  ✓ Data Agent: Confidence {partial_ir.get('data', {}).get('confidence', 0.0):.1%}")

            return partial_ir

        except Exception as e:
            print(f"  ✗ Data Agent: Error - {e}")
            # Return minimal partial IR on error
            return {
                'data': {
                    'data_source': {},
                    'entities': [],
                    'operations': [],
                    'cobol_files': [],
                    'data_transformations': [],
                    'confidence': 0.0
                }
            }

    def _build_data_prompt(self, frm_content: str) -> str:
        """
        WHAT: Build specialized data extraction prompt
        WHY: Focused prompt = better accuracy
        HOW: Template with instructions + VB6 code
        """
        prompt = f"""You are a VB6 data modeling specialist. Your ONLY job is to extract business entities and data operations.

**CRITICAL**: Output ONLY the JSON. No explanations, no markdown, no code blocks.

**INPUT**: VB6 Form Source Code

**YOUR TASK**: Extract ONLY:
1. Business entities (Customer, Supplier, Product, etc.)
2. Data operations (SELECT, INSERT, UPDATE, DELETE)
3. Data source information
4. Data transformations (type conversions, data mapping)

**DO NOT EXTRACT**:
- UI forms or controls (UI Agent's job)
- Event handlers or logic (Logic Agent's job)

**IMPORTANT**: Many VB6 forms have NO data entities or database operations. If there is no database code, entities, or SQL, return EMPTY arrays.

**OUTPUT FORMAT** (JSON only, no markdown):
{{
  "data": {{
    "_source_lines": "Lines X-Y where data operations occur",
    "_what": "Data access patterns and external dependencies",
    "confidence": 0.70,
    "_confidence_why": "Explanation of confidence score",

    "data_source": {{
      "type": "other",
      "_type_note": "Data access delegated to external Client class and GetClient function",
      "connection_string": null,
      "_connection_note": "No connection string in this file",
      "connection_variable": null,
      "is_external": true,
      "_external_note": "All data operations via external dependencies"
    }},

    "entities": [
      {{
        "_entity_index": 1,
        "_what": "Client entity (inferred from usage)",
        "name": "Client",
        "type": "object",
        "_type_note": "VB6 class, not database table directly",
        "fields": [],
        "_fields_note": "Field structure unknown - Client class not in this file",
        "source": "inferred",
        "_source_reasoning": "Referenced in lines 72, 81, 85 but not defined"
      }}
    ],

    "operations": [
      {{
        "_operation_index": 1,
        "_source_lines": "Line 85",
        "_what": "Retrieve client by ID",
        "type": "SELECT",
        "_type_reasoning": "GetClient() performs database lookup",
        "entity": "Client",
        "fields": ["*"],
        "_fields_note": "Unknown which fields are retrieved",
        "where_clause": "ID = <value>",
        "_where_reasoning": "GetClient(CLng(txtID.Text)) passes ID as parameter",
        "query": null,
        "_query_note": "Actual SQL unknown - inside GetClient() function",
        "method": "cmdOpen_Click",
        "_method_line": 85,
        "security_flags": [],
        "_security_note": "GetClient() likely parameterized (function call), not direct SQL concatenation",
        "confidence": 0.75,
        "_confidence_why": "Operation inferred from function call, actual implementation unknown"
      }}
    ],

    "cobol_files": [],
    "_cobol_note": "Not applicable for VB6",

    "data_transformations": [
      {{
        "_transformation_index": 1,
        "_source_lines": "Line 85",
        "_what": "Convert text input to Long integer",
        "from_field": "txtID.Text",
        "to_field": "CLng() result",
        "transformation": "String to Long integer conversion",
        "original_code": "CLng(txtID.Text)",
        "_angular_equivalent": "Number() or parseInt() or unary + operator"
      }}
    ]
  }}
}}

**IMPORTANT RULES**:
1. Include ALL underscore-prefixed fields (_what, _source_lines, _confidence_why, etc.) for documentation
2. For entities, use type: "object" (for VB6 classes), "table" (for database tables), or "inferred"
3. For operations, use type: "SELECT", "INSERT", "UPDATE", "DELETE", "PROCEDURE"
4. Set source: "inferred" for entities referenced but not defined in this file
5. If no fields are known, use empty array [] with _fields_note explanation
6. Always include confidence scores (0.0-1.0) and _confidence_why
7. If NO data operations exist, return empty entities and operations arrays
8. Include Angular equivalents in _angular_equivalent fields

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
            if 'data' not in partial_ir:
                raise ValueError("Response missing 'data' section")

            # Ensure required subsections exist
            data = partial_ir['data']
            if 'data_source' not in data:
                data['data_source'] = {}
            if 'entities' not in data:
                data['entities'] = []
            if 'operations' not in data:
                data['operations'] = []
            if 'cobol_files' not in data:
                data['cobol_files'] = []
            if 'data_transformations' not in data:
                data['data_transformations'] = []

            # Canonicalize IR for consistent comparison
            partial_ir = canonicalize_ir(partial_ir, ir_type="data")

            return partial_ir

        except json.JSONDecodeError as e:
            print(f"  ✗ Data Agent: JSON parse error - {e}")
            # Return minimal structure
            return {
                'data': {
                    'data_source': {},
                    'entities': [],
                    'operations': [],
                    'cobol_files': [],
                    'data_transformations': [],
                    'confidence': 0.0
                }
            }

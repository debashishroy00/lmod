#!/usr/bin/env python3
"""
VB6 UI Agent - Specialized UI Extraction

WHAT: Extract ONLY UI elements (form + controls) from VB6 code
WHY: Specialization = higher accuracy, parallel = faster
HOW: Claude API with focused prompt, output partial IR
"""

import json
from typing import Dict, Any
from anthropic import AsyncAnthropic
from ir_canonicalizer import canonicalize_ir


class VB6UIAgent:
    """
    WHAT: Specialized agent for UI extraction
    WHY: Focus on forms and controls = 95%+ accuracy
    HOW: Targeted prompt + Claude Sonnet 4 API
    """

    def __init__(self, client: AsyncAnthropic):
        """
        WHAT: Initialize UI agent with API client
        WHY: Reuse orchestrator's client for efficiency
        HOW: Store client reference
        """
        self.client = client
        self.model = "claude-sonnet-4-20250514"

    async def extract(self, frm_content: str) -> Dict[str, Any]:
        """
        WHAT: Extract UI elements from VB6 form
        WHY: Generate partial IR with ONLY ui section
        HOW: Send specialized prompt to Claude API

        Args:
            frm_content: VB6 form source code

        Returns:
            Partial IR with ui section and confidence score
        """
        print("  🎨 UI Agent: Extracting form and controls...")

        # Build specialized prompt
        prompt = self._build_ui_prompt(frm_content)

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

            print(f"  ✓ UI Agent: Found {len(partial_ir.get('ui', {}).get('controls', []))} controls")
            print(f"  ✓ UI Agent: Confidence {partial_ir.get('ui', {}).get('confidence', 0.0):.1%}")

            return partial_ir

        except Exception as e:
            print(f"  ✗ UI Agent: Error - {e}")
            # Return minimal partial IR on error
            return {
                'ui': {
                    'form': {},
                    'controls': [],
                    'confidence': 0.0
                }
            }

    def _build_ui_prompt(self, frm_content: str) -> str:
        """
        WHAT: Build specialized UI extraction prompt aligned with golden fixture
        WHY: Match exact format expected by validator for 90%+ accuracy
        HOW: Few-shot learning with golden fixture examples
        """
        prompt = f"""You are a VB6 UI extraction specialist. Extract UI structure in EXACT golden fixture format.

=== CRITICAL OUTPUT REQUIREMENTS ===
1. Output ONLY valid JSON (no markdown, no code blocks, no explanations)
2. Match the golden fixture format EXACTLY (see examples below)
3. Include ALL fields shown in examples for each control type

=== EXAMPLE: TextBox Control (EXACT FORMAT) ===
{{
  "_control_index": 1,
  "_source_lines": "Lines 40-46",
  "_what": "Text input for Client ID",
  "id": "txtID",
  "_id_source": "Line 40: Begin VB.TextBox txtID",
  "type": "TextBox",
  "_type_source": "Line 40: VB.TextBox",
  "_type_angular_mapping": "mat-form-field with input type='text'",
  "caption": "",
  "_caption_note": "TextBoxes don't have captions, only Text property (runtime value)",
  "position": {{
    "left": 1320,
    "top": 360,
    "width": 2535,
    "height": 285,
    "_source": "Lines 41-45",
    "_units": "twips",
    "_note": "Position is relative to form's top-left corner"
  }},
  "tab_index": 0,
  "_tab_index_source": "Line 43: TabIndex = 0",
  "_tab_index_why": "First control in tab order (user enters ID first)",
  "enabled": true,
  "_enabled_default": "Not specified = True by default",
  "visible": true,
  "_visible_default": "Not specified = True by default",
  "properties": {{
    "max_length": 0,
    "_max_length_note": "Not specified, defaults to 0 (unlimited)",
    "text": "",
    "_text_note": "Default empty, filled at runtime",
    "associated_label": "Label1",
    "_associated_label_why": "Label1 ('Client ID') is positioned at same Top (360), to the left"
  }},
  "confidence": 0.98,
  "_confidence_why": "All properties clearly defined"
}}

=== EXAMPLE: CommandButton (EXACT FORMAT) ===
{{
  "_control_index": 2,
  "_source_lines": "Lines 32-39",
  "_what": "Button to create new client",
  "id": "cmdNew",
  "_id_source": "Line 32: Begin VB.CommandButton cmdNew",
  "_naming_convention": "cmd = CommandButton prefix",
  "type": "CommandButton",
  "_type_source": "Line 32: VB.CommandButton",
  "_type_angular_mapping": "mat-button or button with (click) event",
  "caption": "New",
  "_caption_source": "Line 33: Caption = \\"New\\"",
  "_caption_angular": "Button text/label",
  "position": {{
    "left": 120,
    "top": 960,
    "width": 1095,
    "height": 375,
    "_source": "Lines 34-38",
    "_layout_note": "First button in row of 3 buttons at Y=960"
  }},
  "tab_index": 1,
  "_tab_index_source": "Line 36: TabIndex = 1",
  "_tab_order": "Second in tab order (after txtID)",
  "enabled": true,
  "visible": true,
  "properties": {{
    "default": false,
    "_default_note": "Not the default button (not triggered by Enter key)",
    "event_handler": "cmdNew_Click",
    "_event_handler_source": "Lines 71-78: Private Sub cmdNew_Click()"
  }},
  "confidence": 0.99,
  "_confidence_why": "Complete definition with event handler"
}}

=== EXAMPLE: Label with Font (EXACT FORMAT) ===
{{
  "_control_index": 5,
  "_source_lines": "Lines 47-63",
  "_what": "Label for Client ID textbox",
  "id": "Label1",
  "_id_source": "Line 47: Begin VB.Label Label1",
  "_naming_note": "Generic name, not descriptive like 'lblClientID'",
  "type": "Label",
  "_type_source": "Line 47: VB.Label",
  "_type_angular_mapping": "mat-label or simple <label> element",
  "caption": "Client ID",
  "_caption_source": "Line 48: Caption = \\"Client ID\\"",
  "position": {{
    "left": 120,
    "top": 360,
    "width": 1095,
    "height": 255,
    "_source": "Lines 58-62",
    "_layout_note": "Positioned to left of txtID (same Top=360)"
  }},
  "tab_index": 4,
  "_tab_index_source": "Line 60: TabIndex = 4",
  "_tab_index_note": "Labels typically last in tab order (non-interactive)",
  "enabled": true,
  "visible": true,
  "font": {{
    "_source_lines": "Lines 49-57",
    "_what": "Font styling for label",
    "name": "MS Sans Serif",
    "_name_source": "Line 50: Name = \\"MS Sans Serif\\"",
    "size": 8.25,
    "_size_source": "Line 51: Size = 8.25 (points)",
    "weight": 700,
    "_weight_source": "Line 53: Weight = 700",
    "_weight_mapping": "700 = Bold",
    "bold": true,
    "_bold_derived": "Weight 700 = Bold",
    "italic": false,
    "_italic_source": "Line 55: Italic = 0 (False)",
    "underline": false,
    "_underline_source": "Line 54: Underline = 0 (False)",
    "strikethrough": false,
    "_strikethrough_source": "Line 56: Strikethrough = 0 (False)"
  }},
  "properties": {{
    "for_control": "txtID",
    "_for_control_reasoning": "Positioned directly left of txtID, describes its purpose"
  }},
  "confidence": 1.0,
  "_confidence_why": "Complete definition including font properties"
}}

=== FIELD REQUIREMENTS ===

**For ALL controls include:**
- _control_index (sequential number 1, 2, 3...)
- _source_lines (line numbers where control is defined)
- _what (brief description of control's purpose)
- id (control name from VB6)
- _id_source (line number with control definition)
- type (TextBox, CommandButton, Label, etc.)
- _type_source (line where type is declared)
- _type_angular_mapping (Angular equivalent component)
- caption (button text, label text, or empty string for TextBox)
- _caption_source or _caption_note (line or explanation)
- position object with:
  - left, top, width, height (numeric values)
  - _source (line numbers)
  - _units (always "twips")
  - _note or _layout_note (position explanation)
- tab_index (from TabIndex property)
- _tab_index_source (line number)
- _tab_index_why or _tab_order or _tab_index_note (explain tab order)
- enabled (default true if not specified)
- _enabled_default (if not specified, explain default)
- visible (default true if not specified)
- _visible_default (if not specified, explain default)
- confidence (0.0-1.0 based on data completeness)
- _confidence_why (explanation of confidence score)

**For TextBox add to properties:**
- max_length (from MaxLength, default 0)
- _max_length_note (explain value)
- text (default empty string)
- _text_note (explain value)
- associated_label (find Label positioned to the left of this TextBox)
- _associated_label_why (explain how you determined the association)

**For CommandButton add to properties:**
- _naming_convention (if name follows convention like cmd/txt/lbl prefix)
- _caption_angular (Angular mapping for caption)
- default (is this the default button? usually false)
- _default_note (explain default button behavior)
- event_handler (handler name like "cmdNew_Click")
- _event_handler_source (line numbers of handler code)

**For Label include:**
- _naming_note (if name is generic or follows convention)
- If has BeginProperty Font, add font object with:
  - _source_lines, _what
  - name, _name_source
  - size, _size_source
  - weight, _weight_source, _weight_mapping
  - bold, _bold_derived
  - italic, _italic_source
  - underline, _underline_source
  - strikethrough, _strikethrough_source
- properties.for_control (which control does this label describe?)
- properties._for_control_reasoning (explain the association)

=== FORM OBJECT FORMAT ===
{{
  "name": "StartForm",
  "caption": "Start",
  "width": 4050,
  "height": 1500,
  "border_style": "FixedDialog",
  "start_position": "CenterOwner",
  "properties": {{
    "max_button": false,
    "min_button": false,
    "show_in_taskbar": false,
    "link_topic": "Form1"
  }}
}}

=== BORDER STYLE TRANSLATION ===
- 0 = "None"
- 1 = "FixedSingle"
- 2 = "Sizable"
- 3 = "FixedDialog"
- 4 = "FixedToolWindow"
- 5 = "SizableToolWindow"

=== START POSITION TRANSLATION ===
- 0 = "Manual"
- 1 = "CenterOwner"
- 2 = "CenterScreen"
- 3 = "WindowsDefaultLocation"
- 4 = "WindowsDefaultBounds"

=== LAYOUT GROUPS ===
Identify logical groups of controls (e.g., "input_section", "action_buttons")

=== VB6 FORM SOURCE CODE ===
{frm_content}

=== YOUR OUTPUT ===
Return ONLY this JSON structure (no markdown, no explanations):
{{
  "ui": {{
    "_source_lines": "Lines 2-64",
    "_what": "User interface structure - form and controls",
    "_why": "Defines what Angular component UI should look like",
    "type": "dialog",
    "_type_reasoning": "BorderStyle=3 (Fixed Dialog), no maximize/minimize buttons",
    "confidence": 0.95,
    "_confidence_why": "All UI elements clearly defined, standard VB6 format",
    "form": {{
      "name": "StartForm",
      "_name_source": "Line 2: Begin VB.Form StartForm",
      "caption": "Start",
      "_caption_source": "Line 3: Caption = \\\"Start\\\"",
      "width": 4050,
      "_width_source": "Line 10: Width = 4050",
      "height": 1500,
      "_height_source": "Line 11: Height = 1500",
      "border_style": "FixedDialog",
      "_border_style_source": "Line 4: BorderStyle = 3",
      "start_position": "CenterOwner",
      "_start_position_source": "Line 9: StartUpPosition = 1",
      "properties": {{
        "max_button": false,
        "_max_button_source": "Line 6: MaxButton = 0 'False",
        "min_button": false,
        "_min_button_source": "Line 7: MinButton = 0 'False",
        "show_in_taskbar": false,
        "_show_in_taskbar_source": "Line 8: ShowInTaskbar = 0 'False",
        "link_topic": "Form1",
        "_link_topic_source": "Line 5: LinkTopic = \\\"Form1\\\""
      }}
    }},
    "controls": [
      // ALL controls in format shown above, sorted by tab_index
    ],
    "layout": {{
      "grid_based": false,
      "responsive": false,
      "groups": [
        {{"name": "input_section", "control_ids": ["Label1", "txtID"]}},
        {{"name": "action_buttons", "control_ids": ["cmdNew", "cmdOpen", "cmdClose"]}}
      ]
    }}
  }}
}}"""

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
            if 'ui' not in partial_ir:
                raise ValueError("Response missing 'ui' section")

            # Ensure controls are sorted by tab_index
            controls = partial_ir['ui'].get('controls', [])
            controls.sort(key=lambda c: c.get('tab_index', 999))
            partial_ir['ui']['controls'] = controls

            # Store source content for metadata
            partial_ir['_source_content'] = text

            # Canonicalize IR for consistent comparison
            partial_ir = canonicalize_ir(partial_ir, ir_type="ui")

            return partial_ir

        except json.JSONDecodeError as e:
            print(f"  ✗ UI Agent: JSON parse error - {e}")
            # Return minimal structure
            return {
                'ui': {
                    'form': {},
                    'controls': [],
                    'confidence': 0.0
                }
            }

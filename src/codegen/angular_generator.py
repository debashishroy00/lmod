"""
Main Angular code generator - orchestrates the entire code generation process

WHAT: Generates Angular components from IR using Claude API
WHY: Automates VB6 → Angular transformation
HOW: LLM-powered code generation with validation and retry logic
"""

import os
import json
import re
from typing import Dict, Any, Optional
from anthropic import Anthropic
from pathlib import Path

from .prompt_builder import build_angular_generation_prompt
from .file_writer import write_angular_files
from .validators import validate_generated_code, format_validation_report


def _load_env_file():
    """Load .env file from src/ directory"""
    env_path = Path(__file__).parent.parent / '.env'
    if env_path.exists():
        with open(env_path, 'r') as f:
            for line in f:
                line = line.strip()
                if line and not line.startswith('#') and '=' in line:
                    key, value = line.split('=', 1)
                    # Remove quotes if present
                    value = value.strip('"').strip("'")
                    os.environ[key] = value


class AngularGenerator:
    """
    Generates Angular components from IR using Claude API
    """

    def __init__(self, api_key: Optional[str] = None):
        """
        Initialize generator

        Args:
            api_key: Anthropic API key (or uses ANTHROPIC_API_KEY env var)
        """
        # Try to load .env file
        _load_env_file()

        self.api_key = api_key or os.getenv('ANTHROPIC_API_KEY')
        if not self.api_key:
            raise ValueError("ANTHROPIC_API_KEY not found in environment")

        self.client = Anthropic(api_key=self.api_key)


    def generate(self, ir: Dict[str, Any], output_dir: str, validate: bool = True) -> Dict[str, str]:
        """
        Generate Angular component from Universal IR

        Args:
            ir: Universal IR JSON (Phase 3 - language-agnostic schema)
            output_dir: Directory to write generated files
            validate: Whether to validate generated code (default: True)

        Returns:
            Dictionary of {filename: content}
        """
        print("\n🚀 Starting Angular code generation...")

        # Extract form name from Universal IR structure
        forms = ir.get('ui', {}).get('forms', [])
        if not forms:
            raise ValueError("No forms found in Universal IR")
        form_name = forms[0]['name']
        print(f"📝 Form: {form_name}")

        # Step 1: Build prompt
        print("📝 Building LLM prompt...")
        prompt = build_angular_generation_prompt(ir)
        print(f"   Prompt size: {len(prompt)} characters")

        # Step 2: Call Claude API
        print("🤖 Calling Claude Haiku...")
        try:
            generated_code = self._call_claude_api(prompt)
            print(f"   Response size: {len(generated_code)} characters")
        except Exception as e:
            print(f"❌ API call failed: {e}")
            raise

        # Step 3: Parse response into files
        print("📦 Parsing generated files...")
        files = self._parse_generated_files(generated_code)
        print(f"   Parsed {len(files)} files")

        if not files:
            raise ValueError("No files were generated - parsing failed")

        # Step 4: Validate generated code
        if validate:
            print("✅ Validating generated code...")
            validation_result = validate_generated_code(files)
            print(format_validation_report(validation_result))

            if not validation_result['valid']:
                print("⚠️  Validation failed, retrying with error feedback...")
                try:
                    files = self._retry_with_feedback(ir, prompt, validation_result)
                    # Validate again
                    validation_result = validate_generated_code(files)
                    if not validation_result['valid']:
                        print("⚠️  Still has errors after retry, but proceeding...")
                except Exception as e:
                    print(f"⚠️  Retry failed: {e}, proceeding with original generation")

        # Step 5: Write files to disk
        print(f"💾 Writing files to {output_dir}...")
        write_angular_files(files, output_dir, ir)

        print("✨ Code generation complete!")
        return files


    def _call_claude_api(self, prompt: str) -> str:
        """
        Call Claude API to generate code

        Args:
            prompt: Complete generation prompt

        Returns:
            Raw response text
        """
        response = self.client.messages.create(
            model="claude-3-5-haiku-20241022",
            max_tokens=8192,  # Haiku's maximum output token limit
            temperature=0.0,  # Deterministic for code generation
            messages=[{
                "role": "user",
                "content": prompt
            }]
        )

        return response.content[0].text


    def _parse_generated_files(self, response: str) -> Dict[str, str]:
        """
        Parse LLM response into dictionary of files

        Expected format:
        === FILE: component.ts ===
        [code]
        === FILE: component.html ===
        [code]

        Args:
            response: Raw LLM response

        Returns:
            Dictionary of {filename: content}
        """
        files = {}

        # Split by file markers
        file_pattern = r'===\s*FILE:\s*([^\s]+)\s*==='
        parts = re.split(file_pattern, response)

        # parts[0] is text before first marker (usually empty or preamble)
        # parts[1], parts[2], parts[3], parts[4], ... are filename, content, filename, content, ...
        for i in range(1, len(parts), 2):
            if i + 1 < len(parts):
                filename = parts[i].strip()
                content = parts[i + 1].strip()

                # Clean up content - remove markdown code fences if present
                content = self._clean_code_content(content)

                files[filename] = content

        return files


    def _clean_code_content(self, content: str) -> str:
        """
        Clean up code content - remove markdown fences, extra whitespace

        Args:
            content: Raw content from LLM

        Returns:
            Cleaned content
        """
        # Remove markdown code fences (```typescript, ```html, etc.)
        content = re.sub(r'^```[\w]*\n', '', content, flags=re.MULTILINE)
        content = re.sub(r'\n```$', '', content, flags=re.MULTILINE)
        content = re.sub(r'^```$', '', content, flags=re.MULTILINE)

        # Strip leading/trailing whitespace
        content = content.strip()

        return content


    def _retry_with_feedback(
        self,
        ir: Dict[str, Any],
        original_prompt: str,
        validation_result: Dict[str, Any]
    ) -> Dict[str, str]:
        """
        Retry code generation with validation error feedback

        Args:
            ir: Original IR
            original_prompt: Original prompt
            validation_result: Validation errors from first attempt

        Returns:
            Corrected files
        """
        errors = validation_result.get('errors', [])
        error_summary = '\n'.join(f"  - {error}" for error in errors)

        retry_prompt = f"""{original_prompt}

---

**VALIDATION ERRORS FROM PREVIOUS ATTEMPT**:

The following errors were found in your previous generation:

{error_summary}

Please fix these errors and regenerate the files. Make sure to:
1. Use the exact file separator format: === FILE: filename ===
2. Do NOT use markdown code fences
3. Ensure all braces, brackets, and parentheses are balanced
4. Include all required imports
5. Generate complete code without placeholders

Now regenerate the Angular component files with corrections.
"""

        print("🔄 Retrying with error feedback...")
        generated_code = self._call_claude_api(retry_prompt)
        files = self._parse_generated_files(generated_code)

        if not files:
            raise ValueError("Retry parsing failed - no files generated")

        return files


    def generate_from_file(self, ir_file_path: str, output_dir: str) -> Dict[str, str]:
        """
        Generate Angular component from IR file

        Args:
            ir_file_path: Path to IR JSON file
            output_dir: Output directory

        Returns:
            Dictionary of generated files
        """
        # Load IR
        with open(ir_file_path, 'r', encoding='utf-8') as f:
            ir = json.load(f)

        # Generate
        return self.generate(ir, output_dir)

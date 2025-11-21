"""
Validate generated Angular code

WHAT: Basic validation of generated TypeScript/HTML/SCSS
WHY: Catch syntax errors before writing files
HOW: String-based checks (full compilation requires Angular CLI)
"""

from typing import Dict, List, Any


def validate_generated_code(files: Dict[str, str]) -> Dict[str, Any]:
    """
    Validate generated TypeScript/HTML/CSS

    Args:
        files: Dictionary of {filename: content}

    Returns:
        {
            'valid': bool,
            'errors': List[str],
            'warnings': List[str]
        }
    """
    errors = []
    warnings = []

    # Check 1: TypeScript syntax (basic check without full compilation)
    for filename, content in files.items():
        if filename.endswith('.ts'):
            ts_errors = _validate_typescript_syntax(content, filename)
            errors.extend(ts_errors)

    # Check 2: HTML structure
    for filename, content in files.items():
        if filename.endswith('.html'):
            html_errors = _validate_html_structure(content, filename)
            errors.extend(html_errors)

    # Check 3: SCSS syntax
    for filename, content in files.items():
        if filename.endswith('.scss') or filename.endswith('.css'):
            scss_errors = _validate_scss_syntax(content, filename)
            errors.extend(scss_errors)

    # Check 4: Common mistakes
    common_errors, common_warnings = _check_common_mistakes(files)
    errors.extend(common_errors)
    warnings.extend(common_warnings)

    return {
        'valid': len(errors) == 0,
        'errors': errors,
        'warnings': warnings
    }


def _validate_typescript_syntax(content: str, filename: str) -> List[str]:
    """Basic TypeScript syntax validation"""
    errors = []

    # Check for balanced braces
    if content.count('{') != content.count('}'):
        errors.append(f"{filename}: Unbalanced curly braces")

    # Check for balanced parentheses
    if content.count('(') != content.count(')'):
        errors.append(f"{filename}: Unbalanced parentheses")

    # Check for balanced brackets
    if content.count('[') != content.count(']'):
        errors.append(f"{filename}: Unbalanced square brackets")

    # Check for required imports in component files
    if '@Component' in content:
        if 'import' not in content:
            errors.append(f"{filename}: Missing imports")

        if 'export class' not in content:
            errors.append(f"{filename}: Missing component class export")

        # Check for Component decorator
        if '@Component({' not in content and '@Component({{' not in content:
            errors.append(f"{filename}: Malformed @Component decorator")

    # Check for test file requirements
    if filename.endswith('.spec.ts'):
        if 'describe(' not in content:
            errors.append(f"{filename}: Missing describe block")
        if 'it(' not in content:
            errors.append(f"{filename}: Missing test cases (it blocks)")

    return errors


def _validate_html_structure(content: str, filename: str) -> List[str]:
    """Basic HTML structure validation"""
    errors = []

    # Check for balanced angle brackets
    open_count = content.count('<')
    close_count = content.count('>')
    if open_count != close_count:
        errors.append(f"{filename}: Unbalanced HTML tags (< and > mismatch)")

    # Check for common Angular syntax errors
    if '{{{{' in content or '}}}}' in content:
        errors.append(f"{filename}: Potential double-escaped interpolation ({{{{...}}}})")

    # Check for unclosed interpolation
    open_interp = content.count('{{')
    close_interp = content.count('}}')
    if open_interp != close_interp:
        errors.append(f"{filename}: Unbalanced Angular interpolation {{ }} ")

    # Check for common Material UI mistakes
    if 'mat-form-field' in content and 'matInput' not in content:
        # Only warning, not error (might be a select or textarea)
        pass

    return errors


def _validate_scss_syntax(content: str, filename: str) -> List[str]:
    """Basic SCSS syntax validation"""
    errors = []

    # Check for balanced braces
    if content.count('{') != content.count('}'):
        errors.append(f"{filename}: Unbalanced SCSS braces")

    # Check for balanced parentheses in functions
    if content.count('(') != content.count(')'):
        errors.append(f"{filename}: Unbalanced parentheses")

    return errors


def _check_common_mistakes(files: Dict[str, str]) -> tuple[List[str], List[str]]:
    """Check for common code generation mistakes"""
    errors = []
    warnings = []

    # Check for TODO markers (LLM might add these)
    for filename, content in files.items():
        if 'TODO' in content.upper():
            warnings.append(f"{filename}: Contains TODO markers - manual review needed")

    # Check for 'any' type overuse
    ts_files = [f for f in files if f.endswith('.ts') and not f.endswith('.spec.ts')]
    for filename in ts_files:
        content = files[filename]
        any_count = content.count(': any')
        if any_count > 5:
            warnings.append(f"{filename}: Heavy use of 'any' type ({any_count} occurrences)")

    # Check that we have all required files
    required_extensions = ['.component.ts', '.component.html', '.component.scss', '.component.spec.ts']
    for ext in required_extensions:
        has_file = any(f.endswith(ext) for f in files.keys())
        if not has_file:
            errors.append(f"Missing required file type: *{ext}")

    # Check for placeholder code
    for filename, content in files.items():
        if '...' in content and filename.endswith('.ts'):
            warnings.append(f"{filename}: Contains ellipsis (...) which may indicate incomplete code")

        if '// rest of' in content.lower():
            errors.append(f"{filename}: Contains placeholder comment 'rest of...' - incomplete code")

    # Check component class naming
    for filename, content in files.items():
        if filename.endswith('.component.ts'):
            if 'export class' in content:
                # Extract class name
                import re
                match = re.search(r'export class (\w+Component)', content)
                if match:
                    class_name = match.group(1)
                    expected_prefix = filename.replace('.component.ts', '').replace('-', '')
                    # This is just a warning, not an error
                else:
                    errors.append(f"{filename}: Cannot find exported component class")

    return errors, warnings


def format_validation_report(result: Dict[str, Any]) -> str:
    """
    Format validation result as human-readable report

    Args:
        result: Validation result dictionary

    Returns:
        Formatted string report
    """
    lines = []

    if result['valid']:
        lines.append("✅ Validation passed!")
    else:
        lines.append("❌ Validation failed")

    if result['errors']:
        lines.append(f"\n🚨 Errors ({len(result['errors'])}):")
        for error in result['errors']:
            lines.append(f"  - {error}")

    if result['warnings']:
        lines.append(f"\n⚠️  Warnings ({len(result['warnings'])}):")
        for warning in result['warnings']:
            lines.append(f"  - {warning}")

    return "\n".join(lines)

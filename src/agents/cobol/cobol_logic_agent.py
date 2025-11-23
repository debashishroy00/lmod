"""
COBOL Logic Agent - Extracts business logic from COBOL PROCEDURE DIVISION

This agent analyzes COBOL programs and extracts:
- PROCEDURE DIVISION paragraphs and sections
- Control flow logic (IF/ELSE, PERFORM, EVALUATE)
- PERFORM statements → method calls
- Calculations and data transformations
- Conditional logic and loops

Part of Phase 1, Week 2 of the Multi-Language LMOD Platform
"""

import re
from typing import Dict, List, Optional, Tuple
from datetime import datetime


class COBOLLogicAgent:
    """
    Specialized agent for extracting business logic from COBOL PROCEDURE DIVISION.

    Analyzes:
    - Paragraphs and sections
    - PERFORM statements (loops, method calls)
    - IF/ELSE conditional logic
    - EVALUATE (switch/case)
    - COMPUTE and data manipulation
    - Control flow patterns
    """

    def __init__(self):
        self.version = "1.0.0"

    def analyze(self, cobol_source: str, source_file: str = "") -> Dict:
        """
        Main entry point for COBOL logic analysis.

        Args:
            cobol_source: COBOL source code as string
            source_file: Path to source file (for metadata)

        Returns:
            Dictionary containing extracted logic in IR format
        """
        lines = cobol_source.split('\n')

        result = {
            "_agent": "cobol-logic-agent",
            "_version": self.version,
            "_timestamp": datetime.now().isoformat(),
            "_what": "Business logic extracted from COBOL PROCEDURE DIVISION",
            "confidence": 0.0,  # Will be calculated

            # Paragraphs and sections (like methods/functions)
            "procedures": self._extract_procedures(lines),

            # Workflows (high-level business processes)
            "workflows": self._extract_workflows(lines),

            # Calculations
            "calculations": self._extract_calculations(lines),

            # Error handling
            "error_handling": self._extract_error_handling(lines)
        }

        # Calculate overall confidence
        result["confidence"] = self._calculate_confidence(result)

        return result

    def _extract_procedures(self, lines: List[str]) -> List[Dict]:
        """
        Extract procedures (paragraphs and sections) from PROCEDURE DIVISION.

        Example COBOL:
            p000-Begin.
                OPEN OUTPUT DIAG-FILE.
                ...

            p300-ReadItem.
                READ DIAG-FILE
                   AT END MOVE 1 TO READ-EOF.
        """
        procedures = []
        in_procedure_division = False
        current_procedure = None
        current_procedure_lines = []
        procedure_start_line = 0

        for i, line in enumerate(lines):
            line_stripped = line.strip()

            # Detect PROCEDURE DIVISION
            if 'PROCEDURE DIVISION' in line_stripped:
                in_procedure_division = True
                continue

            if not in_procedure_division:
                continue

            # Paragraph/Section name (ends with period, starts at column 8 or earlier)
            # Example: "p000-Begin." or "READ-RECORD."
            if re.match(r'^[A-Z0-9\-]+\.$', line_stripped, re.IGNORECASE):
                # Save previous procedure
                if current_procedure:
                    procedures.append(self._analyze_procedure(
                        current_procedure,
                        current_procedure_lines,
                        procedure_start_line
                    ))

                # Start new procedure
                current_procedure = line_stripped.rstrip('.')
                current_procedure_lines = []
                procedure_start_line = i + 1
                continue

            # Accumulate lines for current procedure
            if current_procedure and line_stripped:
                current_procedure_lines.append((i + 1, line_stripped))

        # Save last procedure
        if current_procedure:
            procedures.append(self._analyze_procedure(
                current_procedure,
                current_procedure_lines,
                procedure_start_line
            ))

        return procedures

    def _analyze_procedure(
        self,
        procedure_name: str,
        lines: List[Tuple[int, str]],
        start_line: int
    ) -> Dict:
        """
        Analyze a single procedure and extract logic steps.
        """
        logic_steps = []

        for line_num, line_text in lines:
            # Categorize statement type and extract step
            step = self._categorize_statement(line_text, line_num)
            if step:
                logic_steps.append(step)

        # Detect procedure purpose
        what, purpose_type = self._detect_procedure_purpose(procedure_name, logic_steps)

        return {
            "name": procedure_name,
            "type": purpose_type,
            "_what": what,
            "logic_steps": logic_steps,
            "start_line": start_line,
            "end_line": lines[-1][0] if lines else start_line,
            "confidence": 0.9
        }

    def _categorize_statement(self, statement: str, line_num: int) -> Optional[Dict]:
        """
        Categorize a COBOL statement and extract relevant information.

        Returns a logic step dictionary or None if not categorizable.
        """
        stmt_upper = statement.upper()

        # MOVE statement (data transformation)
        if stmt_upper.startswith('MOVE '):
            return self._parse_move_statement(statement, line_num)

        # OPEN statement (file I/O)
        if stmt_upper.startswith('OPEN '):
            return self._parse_open_statement(statement, line_num)

        # CLOSE statement (file I/O)
        if stmt_upper.startswith('CLOSE '):
            return self._parse_close_statement(statement, line_num)

        # READ statement (file I/O)
        if stmt_upper.startswith('READ '):
            return self._parse_read_statement(statement, line_num)

        # WRITE statement (file I/O)
        if stmt_upper.startswith('WRITE '):
            return self._parse_write_statement(statement, line_num)

        # PERFORM statement (method call / loop)
        if stmt_upper.startswith('PERFORM '):
            return self._parse_perform_statement(statement, line_num)

        # IF statement (conditional)
        if stmt_upper.startswith('IF '):
            return self._parse_if_statement(statement, line_num)

        # EVALUATE statement (switch/case)
        if stmt_upper.startswith('EVALUATE '):
            return {
                "step_type": "conditional",
                "description": "EVALUATE (switch/case) statement",
                "code_snippet": statement,
                "_line": line_num
            }

        # COMPUTE statement (calculation)
        if stmt_upper.startswith('COMPUTE '):
            return self._parse_compute_statement(statement, line_num)

        # DISPLAY statement (output/message)
        if stmt_upper.startswith('DISPLAY '):
            return self._parse_display_statement(statement, line_num)

        # STOP RUN / GOBACK (program termination)
        if stmt_upper.startswith('STOP RUN') or stmt_upper.startswith('GOBACK'):
            return {
                "step_type": "navigation",
                "description": "Exit program",
                "code_snippet": statement,
                "_line": line_num,
                "_spring_boot_equivalent": "return / System.exit()"
            }

        # CALL statement (external program call)
        if stmt_upper.startswith('CALL '):
            return {
                "step_type": "method_call",
                "description": f"Call external program: {statement}",
                "code_snippet": statement,
                "_line": line_num,
                "_spring_boot_equivalent": "External service call"
            }

        # Other statements - generic
        return {
            "step_type": "other",
            "description": statement,
            "code_snippet": statement,
            "_line": line_num
        }

    def _parse_move_statement(self, statement: str, line_num: int) -> Dict:
        """
        Parse MOVE statement.

        Example: MOVE "J01" TO DiagCode.
                 MOVE ACCT-NO TO ACCT-NO-O.
        """
        # Pattern: MOVE <source> TO <target>
        match = re.search(r'MOVE\s+(.+?)\s+TO\s+(.+)', statement, re.IGNORECASE)

        if match:
            source = match.group(1).strip().rstrip('.')
            target = match.group(2).strip().rstrip('.')

            return {
                "step_type": "data_transformation",
                "description": f"Move {source} to {target}",
                "from_field": source,
                "to_field": target,
                "code_snippet": statement,
                "_line": line_num,
                "_spring_boot_equivalent": f"{target} = {source}"
            }

        return {
            "step_type": "data_transformation",
            "description": statement,
            "code_snippet": statement,
            "_line": line_num
        }

    def _parse_open_statement(self, statement: str, line_num: int) -> Dict:
        """
        Parse OPEN statement.

        Example: OPEN OUTPUT DIAG-FILE.
                 OPEN INPUT ACCT-REC.
        """
        mode_match = re.search(r'OPEN\s+(INPUT|OUTPUT|I-O|EXTEND)\s+(.+)',
                               statement, re.IGNORECASE)

        if mode_match:
            mode = mode_match.group(1).upper()
            file_name = mode_match.group(2).strip().rstrip('.')

            return {
                "step_type": "data_operation",
                "description": f"Open file {file_name} for {mode}",
                "operation": "OPEN",
                "file": file_name,
                "mode": mode,
                "code_snippet": statement,
                "_line": line_num,
                "_spring_boot_equivalent": "Open database connection / file stream"
            }

        return {
            "step_type": "data_operation",
            "description": statement,
            "code_snippet": statement,
            "_line": line_num
        }

    def _parse_close_statement(self, statement: str, line_num: int) -> Dict:
        """
        Parse CLOSE statement.

        Example: CLOSE DIAG-FILE.
        """
        file_match = re.search(r'CLOSE\s+(.+)', statement, re.IGNORECASE)

        if file_match:
            file_name = file_match.group(1).strip().rstrip('.')

            return {
                "step_type": "data_operation",
                "description": f"Close file {file_name}",
                "operation": "CLOSE",
                "file": file_name,
                "code_snippet": statement,
                "_line": line_num,
                "_spring_boot_equivalent": "Close database connection / file stream"
            }

        return {
            "step_type": "data_operation",
            "description": statement,
            "code_snippet": statement,
            "_line": line_num
        }

    def _parse_read_statement(self, statement: str, line_num: int) -> Dict:
        """
        Parse READ statement.

        Example: READ DIAG-FILE AT END MOVE 1 TO READ-EOF.
                 READ ACCT-REC.
        """
        file_match = re.search(r'READ\s+([A-Z0-9\-]+)', statement, re.IGNORECASE)

        if file_match:
            file_name = file_match.group(1)

            # Check for AT END clause
            at_end = 'AT END' in statement.upper()

            return {
                "step_type": "data_operation",
                "description": f"Read record from {file_name}",
                "operation": "READ",
                "file": file_name,
                "has_at_end": at_end,
                "code_snippet": statement,
                "_line": line_num,
                "_spring_boot_equivalent": "Read from database / file"
            }

        return {
            "step_type": "data_operation",
            "description": statement,
            "code_snippet": statement,
            "_line": line_num
        }

    def _parse_write_statement(self, statement: str, line_num: int) -> Dict:
        """
        Parse WRITE statement.

        Example: WRITE DiagDetails.
                 WRITE PRINT-REC.
        """
        record_match = re.search(r'WRITE\s+([A-Z0-9\-]+)', statement, re.IGNORECASE)

        if record_match:
            record_name = record_match.group(1)

            return {
                "step_type": "data_operation",
                "description": f"Write record {record_name}",
                "operation": "WRITE",
                "record": record_name,
                "code_snippet": statement,
                "_line": line_num,
                "_spring_boot_equivalent": "Insert into database / write to file"
            }

        return {
            "step_type": "data_operation",
            "description": statement,
            "code_snippet": statement,
            "_line": line_num
        }

    def _parse_perform_statement(self, statement: str, line_num: int) -> Dict:
        """
        Parse PERFORM statement (method call or loop).

        Examples:
            PERFORM READ-RECORD
            PERFORM p300-ReadItem UNTIL IS-EOF
            PERFORM UNTIL LASTREC = 'Y'
        """
        # PERFORM <paragraph> UNTIL <condition>
        until_match = re.search(
            r'PERFORM\s+([A-Z0-9\-]+)\s+UNTIL\s+(.+)',
            statement,
            re.IGNORECASE
        )

        if until_match:
            paragraph = until_match.group(1)
            condition = until_match.group(2).strip().rstrip('.')

            return {
                "step_type": "loop",
                "description": f"Loop: PERFORM {paragraph} UNTIL {condition}",
                "loop_type": "UNTIL",
                "procedure": paragraph,
                "condition": condition,
                "code_snippet": statement,
                "_line": line_num,
                "_spring_boot_equivalent": f"while (!({condition})) {{ {paragraph}() }}"
            }

        # PERFORM UNTIL <condition> (inline)
        until_inline_match = re.search(
            r'PERFORM\s+UNTIL\s+(.+)',
            statement,
            re.IGNORECASE
        )

        if until_inline_match:
            condition = until_inline_match.group(1).strip().rstrip('.')

            return {
                "step_type": "loop",
                "description": f"Loop UNTIL {condition}",
                "loop_type": "UNTIL",
                "condition": condition,
                "code_snippet": statement,
                "_line": line_num,
                "_spring_boot_equivalent": f"while (!({condition})) {{ ... }}"
            }

        # PERFORM <paragraph> (simple method call)
        paragraph_match = re.search(r'PERFORM\s+([A-Z0-9\-]+)', statement, re.IGNORECASE)

        if paragraph_match:
            paragraph = paragraph_match.group(1)

            return {
                "step_type": "method_call",
                "description": f"Call procedure {paragraph}",
                "procedure": paragraph,
                "code_snippet": statement,
                "_line": line_num,
                "_spring_boot_equivalent": f"{paragraph}()"
            }

        return {
            "step_type": "method_call",
            "description": statement,
            "code_snippet": statement,
            "_line": line_num
        }

    def _parse_if_statement(self, statement: str, line_num: int) -> Dict:
        """
        Parse IF statement.

        Example: IF objClient Is Nothing Then
                 IF NOT IS-EOF
        """
        # Extract condition
        condition_match = re.search(r'IF\s+(.+?)(?:\s+THEN)?$', statement, re.IGNORECASE)

        if condition_match:
            condition = condition_match.group(1).strip()

            return {
                "step_type": "conditional",
                "description": f"IF {condition}",
                "condition": condition,
                "code_snippet": statement,
                "_line": line_num,
                "_spring_boot_equivalent": f"if ({condition}) {{ ... }}"
            }

        return {
            "step_type": "conditional",
            "description": statement,
            "code_snippet": statement,
            "_line": line_num
        }

    def _parse_compute_statement(self, statement: str, line_num: int) -> Dict:
        """
        Parse COMPUTE statement (calculation).

        Example: COMPUTE TOTAL = PRICE * QUANTITY.
        """
        # COMPUTE <target> = <expression>
        compute_match = re.search(r'COMPUTE\s+([A-Z0-9\-]+)\s*=\s*(.+)',
                                  statement, re.IGNORECASE)

        if compute_match:
            target = compute_match.group(1)
            expression = compute_match.group(2).strip().rstrip('.')

            return {
                "step_type": "calculation",
                "description": f"Calculate {target} = {expression}",
                "target": target,
                "formula": expression,
                "code_snippet": statement,
                "_line": line_num,
                "_spring_boot_equivalent": f"{target} = {expression}"
            }

        return {
            "step_type": "calculation",
            "description": statement,
            "code_snippet": statement,
            "_line": line_num
        }

    def _parse_display_statement(self, statement: str, line_num: int) -> Dict:
        """
        Parse DISPLAY statement (console output).

        Example: DISPLAY "Client ID not found".
                 DISPLAY DiagCode " " DiagName.
        """
        # Extract what's being displayed
        display_match = re.search(r'DISPLAY\s+(.+)', statement, re.IGNORECASE)

        if display_match:
            output = display_match.group(1).strip().rstrip('.')

            return {
                "step_type": "message",
                "description": f"Display: {output}",
                "output": output,
                "code_snippet": statement,
                "_line": line_num,
                "_spring_boot_equivalent": f"System.out.println({output})"
            }

        return {
            "step_type": "message",
            "description": statement,
            "code_snippet": statement,
            "_line": line_num
        }

    def _detect_procedure_purpose(
        self,
        procedure_name: str,
        logic_steps: List[Dict]
    ) -> Tuple[str, str]:
        """
        Detect the purpose of a procedure based on its name and logic steps.

        Returns: (what_description, procedure_type)
        """
        name_upper = procedure_name.upper()

        # Analyze name patterns
        if 'READ' in name_upper:
            return (f"Read data from file/database", "data_read")

        if 'WRITE' in name_upper:
            return (f"Write data to file/database", "data_write")

        if 'OPEN' in name_upper:
            return (f"Open files/connections", "initialization")

        if 'CLOSE' in name_upper:
            return (f"Close files/connections and exit", "cleanup")

        if 'INIT' in name_upper or 'BEGIN' in name_upper or 'START' in name_upper:
            return (f"Initialize program/process", "initialization")

        if 'CALC' in name_upper or 'COMPUTE' in name_upper:
            return (f"Perform calculations", "calculation")

        if 'VALID' in name_upper or 'CHECK' in name_upper:
            return (f"Validate data", "validation")

        # Analyze logic steps
        if logic_steps:
            first_steps = logic_steps[:3]
            step_types = [step.get("step_type") for step in first_steps]

            if "data_operation" in step_types:
                operations = [
                    step.get("operation")
                    for step in first_steps
                    if step.get("step_type") == "data_operation"
                ]

                if "READ" in operations:
                    return (f"Read and process data", "data_read")
                if "WRITE" in operations:
                    return (f"Write data to output", "data_write")
                if "OPEN" in operations:
                    return (f"Initialize files and connections", "initialization")
                if "CLOSE" in operations:
                    return (f"Cleanup and exit", "cleanup")

        # Default
        return (f"Business logic procedure", "business_logic")

    def _extract_workflows(self, lines: List[str]) -> List[Dict]:
        """
        Extract high-level workflows (business processes) from PROCEDURE DIVISION.

        Identifies main entry points and their flow.
        """
        workflows = []

        # The first paragraph after PROCEDURE DIVISION is usually the main entry point
        procedures = self._extract_procedures(lines)

        if procedures:
            main_procedure = procedures[0]

            # Check if it calls other procedures
            perform_steps = [
                step for step in main_procedure.get("logic_steps", [])
                if step.get("step_type") in ["method_call", "loop"]
            ]

            if perform_steps:
                workflows.append({
                    "name": "main_program_flow",
                    "entry_point": main_procedure["name"],
                    "_what": "Main program workflow",
                    "procedures_called": [
                        step.get("procedure")
                        for step in perform_steps
                        if step.get("procedure")
                    ],
                    "confidence": 0.85
                })

        return workflows

    def _extract_calculations(self, lines: List[str]) -> List[Dict]:
        """
        Extract calculations from COMPUTE statements and arithmetic expressions.
        """
        calculations = []
        in_procedure_division = False

        for i, line in enumerate(lines):
            line_stripped = line.strip()

            if 'PROCEDURE DIVISION' in line_stripped:
                in_procedure_division = True
                continue

            if not in_procedure_division:
                continue

            # COMPUTE statement
            if line_stripped.upper().startswith('COMPUTE '):
                compute_match = re.search(
                    r'COMPUTE\s+([A-Z0-9\-]+)\s*=\s*(.+)',
                    line_stripped,
                    re.IGNORECASE
                )

                if compute_match:
                    target = compute_match.group(1)
                    formula = compute_match.group(2).strip().rstrip('.')

                    calculations.append({
                        "name": target,
                        "formula": formula,
                        "original_code": line_stripped,
                        "_line": i + 1,
                        "_what": f"Calculate {target}",
                        "confidence": 0.9
                    })

            # ADD, SUBTRACT, MULTIPLY, DIVIDE statements
            arithmetic_match = re.match(
                r'(ADD|SUBTRACT|MULTIPLY|DIVIDE)\s+(.+)',
                line_stripped,
                re.IGNORECASE
            )

            if arithmetic_match:
                operation = arithmetic_match.group(1).upper()
                expression = arithmetic_match.group(2).strip().rstrip('.')

                calculations.append({
                    "name": f"{operation}_calculation",
                    "formula": f"{operation} {expression}",
                    "original_code": line_stripped,
                    "_line": i + 1,
                    "_what": f"{operation} operation",
                    "confidence": 0.85
                })

        return calculations

    def _extract_error_handling(self, lines: List[str]) -> List[Dict]:
        """
        Extract error handling patterns from COBOL.

        COBOL error handling includes:
        - AT END clause (file EOF)
        - INVALID KEY clause (indexed file errors)
        - ON SIZE ERROR (arithmetic overflow)
        - ON OVERFLOW (string operations)
        """
        error_handling = []
        in_procedure_division = False

        for i, line in enumerate(lines):
            line_stripped = line.strip()
            line_upper = line_stripped.upper()

            if 'PROCEDURE DIVISION' in line_stripped:
                in_procedure_division = True
                continue

            if not in_procedure_division:
                continue

            # AT END clause (file EOF handling)
            if 'AT END' in line_upper:
                error_handling.append({
                    "type": "at_end_clause",
                    "scope": "File I/O",
                    "handler": line_stripped,
                    "_line": i + 1,
                    "_what": "End-of-file handling",
                    "_spring_boot_equivalent": "Exception handling or null check"
                })

            # INVALID KEY clause
            if 'INVALID KEY' in line_upper:
                error_handling.append({
                    "type": "invalid_key",
                    "scope": "Indexed file I/O",
                    "handler": line_stripped,
                    "_line": i + 1,
                    "_what": "Invalid key error handling"
                })

            # ON SIZE ERROR clause
            if 'ON SIZE ERROR' in line_upper:
                error_handling.append({
                    "type": "size_error",
                    "scope": "Arithmetic operation",
                    "handler": line_stripped,
                    "_line": i + 1,
                    "_what": "Arithmetic overflow handling"
                })

        return error_handling

    def _calculate_confidence(self, result: Dict) -> float:
        """
        Calculate overall confidence score for logic extraction.
        """
        scores = []

        # Procedures extracted?
        if result.get("procedures"):
            num_procedures = len(result["procedures"])
            scores.append(min(1.0, 0.7 + (num_procedures * 0.05)))

        # Workflows identified?
        if result.get("workflows"):
            scores.append(0.85)
        else:
            scores.append(0.7)  # OK if no workflows

        # Calculations found?
        if result.get("calculations"):
            scores.append(0.9)
        else:
            scores.append(0.8)  # OK if no calculations

        # Error handling found?
        if result.get("error_handling"):
            scores.append(0.9)
        else:
            scores.append(0.75)  # OK if no error handling

        return sum(scores) / len(scores) if scores else 0.5


def main():
    """Test the COBOL Logic Agent with seq.cbl sample."""
    import sys

    if len(sys.argv) < 2:
        print("Usage: python cobol_logic_agent.py <cobol_file>")
        sys.exit(1)

    cobol_file = sys.argv[1]

    with open(cobol_file, 'r') as f:
        cobol_source = f.read()

    agent = COBOLLogicAgent()
    result = agent.analyze(cobol_source, cobol_file)

    # Print results
    import json
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()

"""
COBOL I/O Agent - Extracts file I/O operations and maps to database patterns

This agent analyzes COBOL programs and extracts:
- File I/O operations (OPEN, CLOSE, READ, WRITE, REWRITE)
- File access patterns (sequential, indexed, random)
- Record-level operations and their context
- File status handling and error conditions
- Mapping to Spring Boot/JPA patterns

Part of Phase 1, Week 2 of the Multi-Language LMOD Platform
"""

import re
from typing import Dict, List, Optional, Tuple
from datetime import datetime


class COBOLIOAgent:
    """
    Specialized agent for extracting I/O operations and patterns from COBOL.

    Analyzes:
    - File I/O operations (OPEN, CLOSE, READ, WRITE, REWRITE, DELETE)
    - Access patterns (sequential read, random access, indexed lookup)
    - File status handling
    - AT END, INVALID KEY, and other I/O error conditions
    - Mapping to Spring Boot repository patterns
    """

    def __init__(self):
        self.version = "1.0.0"

    def analyze(self, cobol_source: str, source_file: str = "") -> Dict:
        """
        Main entry point for COBOL I/O analysis.

        Args:
            cobol_source: COBOL source code as string
            source_file: Path to source file (for metadata)

        Returns:
            Dictionary containing extracted I/O operations in IR format
        """
        lines = cobol_source.split('\n')

        result = {
            "_agent": "cobol-io-agent",
            "_version": self.version,
            "_timestamp": datetime.now().isoformat(),
            "_what": "File I/O operations and patterns extracted from COBOL",
            "confidence": 0.0,  # Will be calculated

            # All I/O operations found in the code
            "operations": self._extract_io_operations(lines),

            # I/O patterns detected (sequential read, indexed lookup, etc.)
            "io_patterns": self._detect_io_patterns(lines),

            # File status handling
            "file_status_handling": self._extract_file_status_handling(lines),

            # Spring Boot repository pattern recommendations
            "repository_patterns": self._recommend_repository_patterns(lines)
        }

        # Calculate overall confidence
        result["confidence"] = self._calculate_confidence(result)

        return result

    def _extract_io_operations(self, lines: List[str]) -> List[Dict]:
        """
        Extract all file I/O operations from PROCEDURE DIVISION.

        Returns list of operations with context.
        """
        operations = []
        in_procedure_division = False
        current_procedure = None

        for i, line in enumerate(lines):
            line_stripped = line.strip()
            line_upper = line_stripped.upper()

            # Track PROCEDURE DIVISION
            if 'PROCEDURE DIVISION' in line_stripped:
                in_procedure_division = True
                continue

            if not in_procedure_division:
                continue

            # Track current procedure
            if re.match(r'^[A-Z0-9\-]+\.$', line_stripped, re.IGNORECASE):
                current_procedure = line_stripped.rstrip('.')
                continue

            # OPEN statement
            if line_upper.startswith('OPEN '):
                op = self._parse_open_operation(line_stripped, i + 1, current_procedure)
                if op:
                    operations.append(op)

            # CLOSE statement
            if line_upper.startswith('CLOSE '):
                op = self._parse_close_operation(line_stripped, i + 1, current_procedure)
                if op:
                    operations.append(op)

            # READ statement
            if line_upper.startswith('READ '):
                op = self._parse_read_operation(line_stripped, i + 1, current_procedure, lines, i)
                if op:
                    operations.append(op)

            # WRITE statement
            if line_upper.startswith('WRITE '):
                op = self._parse_write_operation(line_stripped, i + 1, current_procedure)
                if op:
                    operations.append(op)

            # REWRITE statement
            if line_upper.startswith('REWRITE '):
                op = self._parse_rewrite_operation(line_stripped, i + 1, current_procedure)
                if op:
                    operations.append(op)

            # DELETE statement
            if line_upper.startswith('DELETE '):
                op = self._parse_delete_operation(line_stripped, i + 1, current_procedure)
                if op:
                    operations.append(op)

            # START statement (indexed file positioning)
            if line_upper.startswith('START '):
                op = self._parse_start_operation(line_stripped, i + 1, current_procedure)
                if op:
                    operations.append(op)

        return operations

    def _parse_open_operation(
        self,
        statement: str,
        line_num: int,
        procedure: Optional[str]
    ) -> Optional[Dict]:
        """
        Parse OPEN statement.

        Examples:
            OPEN INPUT ACCT-FILE.
            OPEN OUTPUT REPORT-FILE.
            OPEN I-O MASTER-FILE.
            OPEN EXTEND LOG-FILE.
        """
        match = re.search(
            r'OPEN\s+(INPUT|OUTPUT|I-O|EXTEND)\s+([A-Z0-9\-]+)',
            statement,
            re.IGNORECASE
        )

        if not match:
            return None

        mode = match.group(1).upper()
        file_name = match.group(2)

        # Map to Spring Boot operation
        spring_boot_op = {
            "INPUT": "Repository read operations (SELECT)",
            "OUTPUT": "Repository write operations (INSERT)",
            "I-O": "Repository read/write operations (SELECT/UPDATE)",
            "EXTEND": "Repository append operations (INSERT)"
        }.get(mode, "Unknown")

        return {
            "type": "OPEN",
            "entity": file_name,
            "mode": mode,
            "procedure": procedure,
            "_line": line_num,
            "_what": f"Open {file_name} for {mode}",
            "_spring_boot": spring_boot_op,
            "confidence": 0.95
        }

    def _parse_close_operation(
        self,
        statement: str,
        line_num: int,
        procedure: Optional[str]
    ) -> Optional[Dict]:
        """
        Parse CLOSE statement.

        Examples:
            CLOSE ACCT-FILE.
            CLOSE REPORT-FILE MASTER-FILE.
        """
        # Extract all file names after CLOSE
        files_part = re.sub(r'CLOSE\s+', '', statement, flags=re.IGNORECASE)
        file_names = [f.strip().rstrip('.') for f in files_part.split()]

        # Create operation for each file
        # For now, return the first one (we'll handle multiple in the loop)
        if file_names:
            return {
                "type": "CLOSE",
                "entity": file_names[0],
                "procedure": procedure,
                "_line": line_num,
                "_what": f"Close {file_names[0]}",
                "_spring_boot": "Transaction commit / resource cleanup",
                "confidence": 0.95
            }

        return None

    def _parse_read_operation(
        self,
        statement: str,
        line_num: int,
        procedure: Optional[str],
        lines: List[str],
        current_index: int
    ) -> Optional[Dict]:
        """
        Parse READ statement.

        Examples:
            READ ACCT-FILE.
            READ ACCT-FILE INTO WS-RECORD.
            READ ACCT-FILE NEXT RECORD.
            READ ACCT-FILE KEY IS ACCT-KEY.
            READ ACCT-FILE AT END MOVE 'Y' TO EOF-FLAG.
            READ ACCT-FILE INVALID KEY DISPLAY 'NOT FOUND'.
        """
        # Extract file name
        file_match = re.search(r'READ\s+([A-Z0-9\-]+)', statement, re.IGNORECASE)
        if not file_match:
            return None

        file_name = file_match.group(1)

        # Check for INTO clause
        into_match = re.search(r'INTO\s+([A-Z0-9\-]+)', statement, re.IGNORECASE)
        into_var = into_match.group(1) if into_match else None

        # Check for NEXT RECORD (sequential)
        is_sequential = 'NEXT' in statement.upper()

        # Check for KEY clause (indexed/random access)
        key_match = re.search(r'KEY\s+IS\s+([A-Z0-9\-]+)', statement, re.IGNORECASE)
        key_field = key_match.group(1) if key_match else None

        # Check for AT END clause
        has_at_end = 'AT END' in statement.upper()

        # Check for INVALID KEY clause
        has_invalid_key = 'INVALID KEY' in statement.upper()

        # Look ahead for multi-line AT END or INVALID KEY
        error_handling = []
        if has_at_end:
            error_handling.append({
                "type": "AT_END",
                "description": "End of file condition"
            })

        if has_invalid_key:
            error_handling.append({
                "type": "INVALID_KEY",
                "description": "Record not found"
            })

        # Determine access pattern
        if key_field:
            access_pattern = "RANDOM"
            spring_boot = f"findById({key_field}) or findBy{key_field}()"
        elif is_sequential:
            access_pattern = "SEQUENTIAL"
            spring_boot = "findAll() with iterator"
        else:
            access_pattern = "SEQUENTIAL"
            spring_boot = "findAll() with iterator"

        return {
            "type": "READ",
            "entity": file_name,
            "into_variable": into_var,
            "access_pattern": access_pattern,
            "key_field": key_field,
            "error_handling": error_handling if error_handling else None,
            "procedure": procedure,
            "_line": line_num,
            "_what": f"Read record from {file_name}" + (f" using key {key_field}" if key_field else ""),
            "_spring_boot": spring_boot,
            "confidence": 0.9
        }

    def _parse_write_operation(
        self,
        statement: str,
        line_num: int,
        procedure: Optional[str]
    ) -> Optional[Dict]:
        """
        Parse WRITE statement.

        Examples:
            WRITE ACCT-RECORD.
            WRITE ACCT-RECORD FROM WS-DATA.
            WRITE REPORT-LINE AFTER ADVANCING 2 LINES.
        """
        # Extract record name
        record_match = re.search(r'WRITE\s+([A-Z0-9\-]+)', statement, re.IGNORECASE)
        if not record_match:
            return None

        record_name = record_match.group(1)

        # Check for FROM clause
        from_match = re.search(r'FROM\s+([A-Z0-9\-]+)', statement, re.IGNORECASE)
        from_var = from_match.group(1) if from_match else None

        # Check for ADVANCING clause (print files)
        has_advancing = 'ADVANCING' in statement.upper()

        return {
            "type": "WRITE",
            "entity": record_name,
            "from_variable": from_var,
            "is_print": has_advancing,
            "procedure": procedure,
            "_line": line_num,
            "_what": f"Write record {record_name}",
            "_spring_boot": "repository.save(entity) or INSERT",
            "confidence": 0.9
        }

    def _parse_rewrite_operation(
        self,
        statement: str,
        line_num: int,
        procedure: Optional[str]
    ) -> Optional[Dict]:
        """
        Parse REWRITE statement (update existing record).

        Examples:
            REWRITE ACCT-RECORD.
            REWRITE ACCT-RECORD FROM WS-DATA.
        """
        # Extract record name
        record_match = re.search(r'REWRITE\s+([A-Z0-9\-]+)', statement, re.IGNORECASE)
        if not record_match:
            return None

        record_name = record_match.group(1)

        # Check for FROM clause
        from_match = re.search(r'FROM\s+([A-Z0-9\-]+)', statement, re.IGNORECASE)
        from_var = from_match.group(1) if from_match else None

        return {
            "type": "REWRITE",
            "entity": record_name,
            "from_variable": from_var,
            "procedure": procedure,
            "_line": line_num,
            "_what": f"Update record {record_name}",
            "_spring_boot": "repository.save(entity) or UPDATE",
            "confidence": 0.9
        }

    def _parse_delete_operation(
        self,
        statement: str,
        line_num: int,
        procedure: Optional[str]
    ) -> Optional[Dict]:
        """
        Parse DELETE statement.

        Examples:
            DELETE ACCT-FILE RECORD.
        """
        # Extract file name
        file_match = re.search(r'DELETE\s+([A-Z0-9\-]+)', statement, re.IGNORECASE)
        if not file_match:
            return None

        file_name = file_match.group(1)

        return {
            "type": "DELETE",
            "entity": file_name,
            "procedure": procedure,
            "_line": line_num,
            "_what": f"Delete current record from {file_name}",
            "_spring_boot": "repository.delete(entity) or DELETE",
            "confidence": 0.9
        }

    def _parse_start_operation(
        self,
        statement: str,
        line_num: int,
        procedure: Optional[str]
    ) -> Optional[Dict]:
        """
        Parse START statement (position indexed file).

        Examples:
            START ACCT-FILE KEY IS >= ACCT-KEY.
            START MASTER-FILE KEY IS EQUAL TO SEARCH-KEY.
        """
        # Extract file name
        file_match = re.search(r'START\s+([A-Z0-9\-]+)', statement, re.IGNORECASE)
        if not file_match:
            return None

        file_name = file_match.group(1)

        # Extract KEY clause
        key_match = re.search(
            r'KEY\s+IS\s+(>=|>|<=|<|=|EQUAL TO|GREATER THAN|LESS THAN)?\s*([A-Z0-9\-]+)',
            statement,
            re.IGNORECASE
        )

        key_operator = None
        key_field = None

        if key_match:
            key_operator = key_match.group(1) or "="
            key_field = key_match.group(2)

        return {
            "type": "START",
            "entity": file_name,
            "key_field": key_field,
            "key_operator": key_operator,
            "procedure": procedure,
            "_line": line_num,
            "_what": f"Position {file_name} at key {key_field}",
            "_spring_boot": f"findBy{key_field}GreaterThanEqual() or similar",
            "confidence": 0.85
        }

    def _detect_io_patterns(self, lines: List[str]) -> List[Dict]:
        """
        Detect common I/O patterns in COBOL code.

        Patterns:
        - Sequential read (OPEN INPUT, READ...AT END, CLOSE)
        - Sequential write (OPEN OUTPUT, WRITE, CLOSE)
        - Random access (OPEN I-O, READ...KEY, REWRITE, CLOSE)
        - Master file update (read master, update, rewrite)
        - Report generation (read data, write report)
        """
        patterns = []

        # Extract operations first
        operations = self._extract_io_operations(lines)

        # Pattern 1: Sequential Read Loop
        if self._has_sequential_read_loop(operations):
            patterns.append({
                "pattern_type": "SEQUENTIAL_READ_LOOP",
                "pattern_name": "Sequential File Read with AT END",
                "_what": "Read all records from file sequentially",
                "_spring_boot": "repository.findAll() with forEach or Stream",
                "confidence": 0.9
            })

        # Pattern 2: Sequential Write
        if self._has_sequential_write(operations):
            patterns.append({
                "pattern_type": "SEQUENTIAL_WRITE",
                "pattern_name": "Sequential File Write",
                "_what": "Write records to output file",
                "_spring_boot": "repository.saveAll() or batch insert",
                "confidence": 0.9
            })

        # Pattern 3: Random Access / Indexed Lookup
        if self._has_random_access(operations):
            patterns.append({
                "pattern_type": "RANDOM_ACCESS",
                "pattern_name": "Indexed File Lookup",
                "_what": "Direct record access by key",
                "_spring_boot": "repository.findById() or findByKey()",
                "confidence": 0.85
            })

        # Pattern 4: Master File Update
        if self._has_master_update(operations):
            patterns.append({
                "pattern_type": "MASTER_UPDATE",
                "pattern_name": "Master File Update Pattern",
                "_what": "Read record, modify, rewrite",
                "_spring_boot": "Optimistic locking with @Version",
                "confidence": 0.85
            })

        # Pattern 5: Report Generation
        if self._has_report_generation(operations):
            patterns.append({
                "pattern_type": "REPORT_GENERATION",
                "pattern_name": "Report File Generation",
                "_what": "Read data file, write formatted report",
                "_spring_boot": "Read entities, generate PDF/CSV with template",
                "confidence": 0.8
            })

        return patterns

    def _has_sequential_read_loop(self, operations: List[Dict]) -> bool:
        """Check if operations contain sequential read loop pattern."""
        has_input_open = any(
            op.get("type") == "OPEN" and op.get("mode") == "INPUT"
            for op in operations
        )

        has_sequential_read = any(
            op.get("type") == "READ" and
            op.get("access_pattern") == "SEQUENTIAL" and
            op.get("error_handling") is not None
            for op in operations
        )

        return has_input_open and has_sequential_read

    def _has_sequential_write(self, operations: List[Dict]) -> bool:
        """Check if operations contain sequential write pattern."""
        has_output_open = any(
            op.get("type") == "OPEN" and op.get("mode") == "OUTPUT"
            for op in operations
        )

        has_write = any(op.get("type") == "WRITE" for op in operations)

        return has_output_open and has_write

    def _has_random_access(self, operations: List[Dict]) -> bool:
        """Check if operations contain random access pattern."""
        return any(
            op.get("type") == "READ" and op.get("access_pattern") == "RANDOM"
            for op in operations
        )

    def _has_master_update(self, operations: List[Dict]) -> bool:
        """Check if operations contain master update pattern."""
        has_io_open = any(
            op.get("type") == "OPEN" and op.get("mode") == "I-O"
            for op in operations
        )

        has_rewrite = any(op.get("type") == "REWRITE" for op in operations)

        return has_io_open and has_rewrite

    def _has_report_generation(self, operations: List[Dict]) -> bool:
        """Check if operations contain report generation pattern."""
        has_input_read = any(
            op.get("type") == "READ" and op.get("access_pattern") == "SEQUENTIAL"
            for op in operations
        )

        has_print_write = any(
            op.get("type") == "WRITE" and op.get("is_print") == True
            for op in operations
        )

        return has_input_read and has_print_write

    def _extract_file_status_handling(self, lines: List[str]) -> List[Dict]:
        """
        Extract file status variable handling.

        COBOL uses FILE STATUS variables to check I/O operation results.
        Example: FILE STATUS IS WS-FILE-STATUS.
        """
        file_status = []
        in_file_control = False

        for i, line in enumerate(lines):
            line_stripped = line.strip()

            if 'FILE-CONTROL' in line_stripped:
                in_file_control = True
                continue

            if in_file_control and line_stripped.startswith('DATA DIVISION'):
                break

            if in_file_control:
                # FILE STATUS clause
                status_match = re.search(
                    r'FILE\s+STATUS\s+IS\s+([A-Z0-9\-]+)',
                    line_stripped,
                    re.IGNORECASE
                )

                if status_match:
                    status_var = status_match.group(1)
                    file_status.append({
                        "status_variable": status_var,
                        "_line": i + 1,
                        "_what": f"File status stored in {status_var}",
                        "_spring_boot": "Exception handling with try/catch"
                    })

        return file_status

    def _recommend_repository_patterns(self, lines: List[str]) -> List[Dict]:
        """
        Recommend Spring Boot repository patterns based on I/O operations.
        """
        recommendations = []
        operations = self._extract_io_operations(lines)

        # Group operations by file/entity
        files = {}
        for op in operations:
            entity = op.get("entity")
            if entity:
                if entity not in files:
                    files[entity] = []
                files[entity].append(op)

        # Analyze each file's operations and recommend repository pattern
        for file_name, file_ops in files.items():
            op_types = [op.get("type") for op in file_ops]

            # Determine repository interface needed
            needs_crud = any(t in ["WRITE", "REWRITE", "DELETE"] for t in op_types)
            needs_findby = any(
                op.get("type") == "READ" and op.get("key_field")
                for op in file_ops
            )
            needs_findall = any(
                op.get("type") == "READ" and op.get("access_pattern") == "SEQUENTIAL"
                for op in file_ops
            )

            repository_type = "JpaRepository"
            if needs_crud and needs_findby:
                repository_type = "JpaRepository with custom queries"
            elif needs_findall and not needs_crud:
                repository_type = "ReadOnlyRepository or @Query methods"

            recommendations.append({
                "entity": file_name,
                "repository_type": repository_type,
                "operations_needed": list(set(op_types)),
                "_what": f"Repository for {file_name}",
                "_spring_boot_interface": f"public interface {file_name}Repository extends {repository_type}<{file_name}, Long>",
                "confidence": 0.85
            })

        return recommendations

    def _calculate_confidence(self, result: Dict) -> float:
        """
        Calculate overall confidence score for I/O extraction.
        """
        scores = []

        # Operations extracted?
        if result.get("operations"):
            num_ops = len(result["operations"])
            scores.append(min(1.0, 0.75 + (num_ops * 0.03)))

        # Patterns detected?
        if result.get("io_patterns"):
            scores.append(0.9)
        else:
            scores.append(0.7)  # OK if no patterns

        # Repository recommendations?
        if result.get("repository_patterns"):
            scores.append(0.85)

        return sum(scores) / len(scores) if scores else 0.5


def main():
    """Test the COBOL I/O Agent with seq.cbl sample."""
    import sys

    if len(sys.argv) < 2:
        print("Usage: python cobol_io_agent.py <cobol_file>")
        sys.exit(1)

    cobol_file = sys.argv[1]

    with open(cobol_file, 'r') as f:
        cobol_source = f.read()

    agent = COBOLIOAgent()
    result = agent.analyze(cobol_source, cobol_file)

    # Print results
    import json
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()

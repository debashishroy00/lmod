"""
COBOL Data Agent - Extracts data structures from COBOL source code

This agent analyzes COBOL programs and extracts:
- WORKING-STORAGE SECTION data structures
- FILE-CONTROL section definitions
- Data record layouts (FD definitions)
- PIC clause mappings to Java types
- COPY book references

Part of Phase 1, Week 1 of the Multi-Language LMOD Platform
"""

import re
from typing import Dict, List, Optional, Tuple
from datetime import datetime


class COBOLDataAgent:
    """
    Specialized agent for extracting data structures from COBOL programs.

    Analyzes:
    - WORKING-STORAGE SECTION: Variables, constants, level numbers
    - FILE SECTION: FD definitions, record layouts
    - FILE-CONTROL: File assignments and organization
    - PIC clauses: Data types, lengths, decimal places
    """

    def __init__(self):
        self.version = "1.0.0"

    def analyze(self, cobol_source: str, source_file: str = "") -> Dict:
        """
        Main entry point for COBOL data analysis.

        Args:
            cobol_source: COBOL source code as string
            source_file: Path to source file (for metadata)

        Returns:
            Dictionary containing extracted data structures in IR format
        """
        lines = cobol_source.split('\n')

        result = {
            "_agent": "cobol-data-agent",
            "_version": self.version,
            "_timestamp": datetime.now().isoformat(),
            "_what": "Data structures extracted from COBOL",
            "confidence": 0.0,  # Will be calculated

            # File definitions (from FILE-CONTROL and FD sections)
            "cobol_files": self._extract_file_definitions(lines),

            # Data structures (from WORKING-STORAGE and FILE SECTION)
            "entities": self._extract_data_structures(lines),

            # COPY book references
            "copybooks": self._extract_copybooks(lines),

            # Data source metadata
            "data_source": self._determine_data_source(lines)
        }

        # Calculate overall confidence
        result["confidence"] = self._calculate_confidence(result)

        return result

    def _extract_file_definitions(self, lines: List[str]) -> List[Dict]:
        """
        Extract file definitions from FILE-CONTROL and FD sections.

        Example COBOL:
            SELECT DIAG-FILE ASSIGN TO DISK
                ORGANIZATION IS SEQUENTIAL.

            FD DIAG-FILE
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS 'DIAG.DAT'
               DATA RECORD IS DiagDetails.
        """
        files = []

        # Extract from FILE-CONTROL section
        file_control_map = self._parse_file_control_section(lines)

        # Extract from FD definitions
        fd_map = self._parse_fd_section(lines)

        # Merge FILE-CONTROL and FD information
        all_file_names = set(file_control_map.keys()) | set(fd_map.keys())

        for file_name in all_file_names:
            file_def = {
                "select_name": file_name,
                "_what": f"File definition for {file_name}"
            }

            # Add FILE-CONTROL information
            if file_name in file_control_map:
                fc = file_control_map[file_name]
                file_def.update({
                    "assign_to": fc.get("assign_to"),
                    "organization": fc.get("organization", "SEQUENTIAL"),
                    "access_mode": fc.get("access_mode", "SEQUENTIAL"),
                    "_select_line": fc.get("_line")
                })

            # Add FD information
            if file_name in fd_map:
                fd = fd_map[file_name]
                file_def.update({
                    "record_layout": fd.get("record_layout"),
                    "file_status": fd.get("file_status"),
                    "label_record": fd.get("label_record"),
                    "value_of_file_id": fd.get("value_of_file_id"),
                    "_fd_line": fd.get("_line")
                })

            files.append(file_def)

        return files

    def _parse_file_control_section(self, lines: List[str]) -> Dict[str, Dict]:
        """
        Parse FILE-CONTROL section.

        Returns: {file_name: {assign_to, organization, access_mode}}
        """
        result = {}
        in_file_control = False
        current_file = None
        current_file_data = {}

        for i, line in enumerate(lines):
            line_stripped = line.strip()

            # Detect FILE-CONTROL section
            if 'FILE-CONTROL' in line_stripped:
                in_file_control = True
                continue

            # Exit when we hit next section
            if in_file_control and (line_stripped.startswith('DATA DIVISION') or
                                    line_stripped.startswith('I-O-CONTROL')):
                if current_file:
                    result[current_file] = current_file_data
                break

            if not in_file_control:
                continue

            # SELECT statement
            select_match = re.search(r'SELECT\s+([A-Z0-9\-]+)\s+ASSIGN\s+TO\s+(.+)',
                                     line_stripped, re.IGNORECASE)
            if select_match:
                # Save previous file
                if current_file:
                    result[current_file] = current_file_data

                # Start new file
                current_file = select_match.group(1)
                assign_to = select_match.group(2).strip().rstrip('.')
                current_file_data = {
                    "assign_to": assign_to,
                    "_line": i + 1
                }
                continue

            # ORGANIZATION clause
            org_match = re.search(r'ORGANIZATION\s+IS\s+([A-Z\s]+)',
                                  line_stripped, re.IGNORECASE)
            if org_match and current_file:
                org = org_match.group(1).strip().rstrip('.')
                current_file_data["organization"] = org
                continue

            # ACCESS MODE clause
            access_match = re.search(r'ACCESS\s+MODE\s+IS\s+([A-Z]+)',
                                     line_stripped, re.IGNORECASE)
            if access_match and current_file:
                current_file_data["access_mode"] = access_match.group(1).strip().rstrip('.')

        # Save last file
        if current_file:
            result[current_file] = current_file_data

        return result

    def _parse_fd_section(self, lines: List[str]) -> Dict[str, Dict]:
        """
        Parse FD (File Description) definitions from FILE SECTION.

        Returns: {file_name: {record_layout, label_record, value_of_file_id}}
        """
        result = {}
        in_file_section = False
        current_file = None
        current_fd = {}

        for i, line in enumerate(lines):
            line_stripped = line.strip()

            # Detect FILE SECTION
            if 'FILE SECTION' in line_stripped:
                in_file_section = True
                continue

            # Exit when we hit next section
            if in_file_section and (line_stripped.startswith('WORKING-STORAGE') or
                                    line_stripped.startswith('LINKAGE') or
                                    line_stripped.startswith('PROCEDURE')):
                if current_file:
                    result[current_file] = current_fd
                break

            if not in_file_section:
                continue

            # FD statement
            fd_match = re.search(r'FD\s+([A-Z0-9\-]+)', line_stripped, re.IGNORECASE)
            if fd_match:
                # Save previous FD
                if current_file:
                    result[current_file] = current_fd

                # Start new FD
                current_file = fd_match.group(1)
                current_fd = {"_line": i + 1}
                continue

            # LABEL RECORD clause
            label_match = re.search(r'LABEL\s+RECORD[S]?\s+(?:IS|ARE)\s+([A-Z]+)',
                                    line_stripped, re.IGNORECASE)
            if label_match and current_file:
                current_fd["label_record"] = label_match.group(1)

            # VALUE OF FILE-ID clause
            file_id_match = re.search(r"VALUE\s+OF\s+FILE-ID\s+IS\s+['\"]([^'\"]+)['\"]",
                                      line_stripped, re.IGNORECASE)
            if file_id_match and current_file:
                current_fd["value_of_file_id"] = file_id_match.group(1)

            # DATA RECORD clause
            data_rec_match = re.search(r'DATA\s+RECORD[S]?\s+(?:IS|ARE)\s+([A-Z0-9\-]+)',
                                       line_stripped, re.IGNORECASE)
            if data_rec_match and current_file:
                current_fd["record_layout"] = data_rec_match.group(1)

        # Save last FD
        if current_file:
            result[current_file] = current_fd

        return result

    def _extract_data_structures(self, lines: List[str]) -> List[Dict]:
        """
        Extract data structures from WORKING-STORAGE and FILE SECTION.

        Parses hierarchical data structures using level numbers (01-49).
        """
        entities = []

        # Extract from FILE SECTION (record layouts)
        file_section_entities = self._parse_record_layouts(lines)
        entities.extend(file_section_entities)

        # Extract from WORKING-STORAGE SECTION
        working_storage_entities = self._parse_working_storage(lines)
        entities.extend(working_storage_entities)

        return entities

    def _parse_record_layouts(self, lines: List[str]) -> List[Dict]:
        """
        Parse record layouts from FILE SECTION (01-level items after FD).
        """
        entities = []
        in_file_section = False
        current_record = None
        current_fields = []

        for i, line in enumerate(lines):
            line_stripped = line.strip()

            # Detect FILE SECTION
            if 'FILE SECTION' in line_stripped:
                in_file_section = True
                continue

            # Exit when we hit next section
            if in_file_section and (line_stripped.startswith('WORKING-STORAGE') or
                                    line_stripped.startswith('LINKAGE') or
                                    line_stripped.startswith('PROCEDURE')):
                # Save last record
                if current_record:
                    entities.append({
                        "name": current_record,
                        "type": "record",
                        "fields": current_fields,
                        "source": "FILE SECTION",
                        "_what": f"Record layout for {current_record}"
                    })
                break

            if not in_file_section:
                continue

            # Skip FD lines
            if line_stripped.startswith('FD '):
                continue

            # 01-level item (record start)
            level_01_match = re.match(r'01\s+([A-Z0-9\-]+)', line_stripped, re.IGNORECASE)
            if level_01_match:
                # Save previous record
                if current_record:
                    entities.append({
                        "name": current_record,
                        "type": "record",
                        "fields": current_fields,
                        "source": "FILE SECTION",
                        "_what": f"Record layout for {current_record}"
                    })

                # Start new record
                current_record = level_01_match.group(1)
                current_fields = []
                continue

            # 02-49 level items (fields)
            if current_record:
                field = self._parse_field_definition(line_stripped, i + 1)
                if field:
                    current_fields.append(field)

        # Save last record
        if current_record:
            entities.append({
                "name": current_record,
                "type": "record",
                "fields": current_fields,
                "source": "FILE SECTION",
                "_what": f"Record layout for {current_record}"
            })

        return entities

    def _parse_working_storage(self, lines: List[str]) -> List[Dict]:
        """
        Parse data structures from WORKING-STORAGE SECTION.
        """
        entities = []
        in_working_storage = False
        current_structure = None
        current_fields = []

        for i, line in enumerate(lines):
            line_stripped = line.strip()

            # Detect WORKING-STORAGE SECTION
            if 'WORKING-STORAGE SECTION' in line_stripped:
                in_working_storage = True
                continue

            # Exit when we hit next section
            if in_working_storage and (line_stripped.startswith('LINKAGE') or
                                       line_stripped.startswith('PROCEDURE')):
                # Save last structure
                if current_structure:
                    entities.append({
                        "name": current_structure,
                        "type": "variable",
                        "fields": current_fields if current_fields else None,
                        "source": "WORKING-STORAGE",
                        "_what": f"Variable {current_structure}"
                    })
                break

            if not in_working_storage:
                continue

            # 01-level item (structure start)
            level_01_match = re.match(r'01\s+([A-Z0-9\-]+)', line_stripped, re.IGNORECASE)
            if level_01_match:
                # Save previous structure
                if current_structure:
                    entities.append({
                        "name": current_structure,
                        "type": "variable",
                        "fields": current_fields if current_fields else None,
                        "source": "WORKING-STORAGE",
                        "_what": f"Variable {current_structure}"
                    })

                # Start new structure
                current_structure = level_01_match.group(1)
                current_fields = []

                # Check if it has a PIC clause (elementary item)
                if 'PIC' in line_stripped:
                    field = self._parse_field_definition(line_stripped, i + 1)
                    if field:
                        # This is an elementary item, not a group
                        entities.append({
                            "name": current_structure,
                            "type": "variable",
                            "fields": [field],
                            "source": "WORKING-STORAGE",
                            "_what": f"Variable {current_structure}"
                        })
                        current_structure = None
                        current_fields = []
                continue

            # 02-88 level items (fields or conditions)
            if current_structure:
                # 88-level (condition name)
                if re.match(r'88\s+', line_stripped, re.IGNORECASE):
                    condition = self._parse_condition_name(line_stripped, i + 1)
                    if condition:
                        current_fields.append(condition)
                else:
                    # Regular field
                    field = self._parse_field_definition(line_stripped, i + 1)
                    if field:
                        current_fields.append(field)

        # Save last structure
        if current_structure:
            entities.append({
                "name": current_structure,
                "type": "variable",
                "fields": current_fields if current_fields else None,
                "source": "WORKING-STORAGE",
                "_what": f"Variable {current_structure}"
            })

        return entities

    def _parse_field_definition(self, line: str, line_num: int) -> Optional[Dict]:
        """
        Parse a field definition with PIC clause.

        Example: 02 DiagCode PIC X(5).
        """
        # Match level number, field name, and PIC clause
        match = re.match(
            r'(\d{2})\s+([A-Z0-9\-]+)\s+PIC(?:TURE)?\s+([^\s\.]+)',
            line,
            re.IGNORECASE
        )

        if not match:
            return None

        level = int(match.group(1))
        field_name = match.group(2)
        pic_clause = match.group(3)

        # Parse PIC clause to determine type and length
        java_type, length, decimals = self._parse_pic_clause(pic_clause)

        # Extract VALUE clause if present
        value_match = re.search(r'VALUE\s+(?:IS\s+)?(.+?)(?:\.|$)', line, re.IGNORECASE)
        default_value = value_match.group(1).strip() if value_match else None

        return {
            "name": field_name,
            "level": level,
            "cobol_picture": pic_clause,
            "data_type": java_type,
            "length": length,
            "decimals": decimals,
            "default_value": default_value,
            "_line": line_num,
            "_what": f"Field {field_name} ({java_type})"
        }

    def _parse_condition_name(self, line: str, line_num: int) -> Optional[Dict]:
        """
        Parse 88-level condition name.

        Example: 88 IS-EOF VALUE IS 1.
        """
        match = re.match(
            r'88\s+([A-Z0-9\-]+)\s+VALUE[S]?\s+(?:IS|ARE)\s+(.+)',
            line,
            re.IGNORECASE
        )

        if not match:
            return None

        condition_name = match.group(1)
        value = match.group(2).strip().rstrip('.')

        return {
            "name": condition_name,
            "level": 88,
            "type": "condition",
            "value": value,
            "_line": line_num,
            "_what": f"Condition {condition_name} = {value}"
        }

    def _parse_pic_clause(self, pic_clause: str) -> Tuple[str, Optional[int], Optional[int]]:
        """
        Parse PIC clause and map to Java type.

        Examples:
            X(5)     -> String, length 5
            9(3)     -> Integer, length 3
            9(5)V99  -> BigDecimal, length 5, decimals 2
            S9(7)V99 -> BigDecimal (signed), length 7, decimals 2

        Returns: (java_type, length, decimals)
        """
        pic_upper = pic_clause.upper()

        # Alphanumeric (X)
        if 'X' in pic_upper:
            length_match = re.search(r'X\((\d+)\)', pic_upper)
            length = int(length_match.group(1)) if length_match else 1
            return ("String", length, None)

        # Numeric with decimals (V)
        if 'V' in pic_upper:
            # Example: 9(5)V99 or S9(7)V99
            int_match = re.search(r'9\((\d+)\)', pic_upper)
            dec_match = re.search(r'V9+', pic_upper)

            int_length = int(int_match.group(1)) if int_match else 1
            dec_length = len(dec_match.group(0)) - 1 if dec_match else 0  # -1 for 'V'

            return ("BigDecimal", int_length + dec_length, dec_length)

        # Integer (9)
        if '9' in pic_upper:
            length_match = re.search(r'9\((\d+)\)', pic_upper)
            length = int(length_match.group(1)) if length_match else 1

            # Choose appropriate Java type based on length
            if length <= 4:
                return ("Integer", length, None)
            elif length <= 9:
                return ("Long", length, None)
            else:
                return ("BigInteger", length, None)

        # Alphabetic (A)
        if 'A' in pic_upper:
            length_match = re.search(r'A\((\d+)\)', pic_upper)
            length = int(length_match.group(1)) if length_match else 1
            return ("String", length, None)

        # Unknown
        return ("Object", None, None)

    def _extract_copybooks(self, lines: List[str]) -> List[Dict]:
        """
        Extract COPY statements.

        Example: COPY CUSTCOPY.
        """
        copybooks = []

        for i, line in enumerate(lines):
            line_stripped = line.strip()

            # COPY statement
            match = re.search(r'COPY\s+([A-Z0-9\-]+)', line_stripped, re.IGNORECASE)
            if match:
                copybook_name = match.group(1).rstrip('.')

                # Try to determine type (heuristic)
                copybook_type = "DATA"  # Default
                if 'PROC' in copybook_name.upper():
                    copybook_type = "PROCEDURE"

                copybooks.append({
                    "name": copybook_name,
                    "type": copybook_type,
                    "_line": i + 1,
                    "_what": f"COPY book {copybook_name}"
                })

        return copybooks

    def _determine_data_source(self, lines: List[str]) -> Dict:
        """
        Determine data source type from COBOL code.
        """
        source_code = '\n'.join(lines).upper()

        # Check for different data source types
        if 'EXEC SQL' in source_code:
            if 'DB2' in source_code:
                return {"type": "DB2", "_what": "DB2 database via embedded SQL"}
            else:
                return {"type": "other", "_what": "SQL database (embedded SQL)"}

        if 'EXEC CICS' in source_code:
            if 'VSAM' in source_code:
                return {"type": "VSAM", "_what": "VSAM files via CICS"}
            else:
                return {"type": "other", "_what": "CICS transaction"}

        if 'IMS' in source_code or 'DL/I' in source_code:
            return {"type": "IMS", "_what": "IMS database"}

        # Check file organization
        if 'ORGANIZATION IS SEQUENTIAL' in source_code:
            return {"type": "sequential_file", "_what": "Sequential flat files"}

        if 'ORGANIZATION IS INDEXED' in source_code:
            return {"type": "indexed_file", "_what": "Indexed sequential files (ISAM)"}

        if 'VSAM' in source_code:
            return {"type": "VSAM", "_what": "VSAM files"}

        # Default
        return {"type": "sequential_file", "_what": "File-based (assumed sequential)"}

    def _calculate_confidence(self, result: Dict) -> float:
        """
        Calculate overall confidence score for data extraction.
        """
        scores = []

        # Files extracted?
        if result.get("cobol_files"):
            scores.append(0.9)

        # Entities extracted?
        if result.get("entities"):
            num_entities = len(result["entities"])
            scores.append(min(1.0, 0.7 + (num_entities * 0.05)))

        # Data source identified?
        if result.get("data_source", {}).get("type") != "other":
            scores.append(0.85)

        # Copybooks found?
        if result.get("copybooks"):
            scores.append(0.8)
        else:
            scores.append(0.9)  # Not having copybooks is fine

        return sum(scores) / len(scores) if scores else 0.5


def main():
    """Test the COBOL Data Agent with seq.cbl sample."""
    import sys

    if len(sys.argv) < 2:
        print("Usage: python cobol_data_agent.py <cobol_file>")
        sys.exit(1)

    cobol_file = sys.argv[1]

    with open(cobol_file, 'r') as f:
        cobol_source = f.read()

    agent = COBOLDataAgent()
    result = agent.analyze(cobol_source, cobol_file)

    # Print results
    import json
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()

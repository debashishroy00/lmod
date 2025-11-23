#!/usr/bin/env python3
"""
Spring Boot Code Generator - COBOL IR → Spring Boot

WHAT: Generates Spring Boot application from COBOL IR
WHY: Automate COBOL → Spring Boot migration
HOW: Read COBOL IR JSON → Generate @Entity, @Repository, @Service, config files

Architecture:
- Mirrors Angular generator structure (src/codegen/angular_generator.py)
- Template-based generation (not LLM-based for determinism)
- Generates complete Spring Boot project structure

Input: COBOL IR (8 sections)
Output: Spring Boot project with:
  - src/main/java/com/legacy/cobol/entity/*.java (@Entity classes)
  - src/main/java/com/legacy/cobol/repository/*.java (@Repository interfaces)
  - src/main/java/com/legacy/cobol/service/*.java (@Service classes)
  - src/main/resources/application.properties
  - pom.xml

Design Decisions:
- Java 17 (LTS)
- Spring Boot 3.2.0
- Spring Data JPA for data access
- H2 in-memory database for testing
- Maven build system
"""

import json
from pathlib import Path
from typing import Dict, List, Any, Optional
from datetime import datetime


class SpringBootGenerator:
    """
    WHAT: Generates Spring Boot code from COBOL IR
    WHY: Provide automated COBOL → Spring Boot migration
    HOW: Template-based code generation (deterministic, no LLM)
    """

    def __init__(self, base_package: str = "com.legacy.cobol"):
        """
        Initialize Spring Boot generator.

        Args:
            base_package: Base Java package name (default: com.legacy.cobol)
        """
        self.base_package = base_package
        self.package_path = base_package.replace('.', '/')

    def generate(self, ir_json_path: Path, output_dir: Path) -> Dict[str, Any]:
        """
        WHAT: Main entry point - generate complete Spring Boot project
        WHY: Convert COBOL IR to Spring Boot application
        HOW: Load IR → Generate entities → Repositories → Services → Config

        Args:
            ir_json_path: Path to COBOL IR JSON file
            output_dir: Output directory for generated Spring Boot project

        Returns:
            Dict with generation statistics and file paths
        """
        # Load COBOL IR
        with open(ir_json_path, 'r', encoding='utf-8') as f:
            ir = json.load(f)

        # Extract IR sections
        metadata = ir.get('metadata', {})
        data_structures = ir.get('data_structures', {})
        business_logic = ir.get('business_logic', {})
        io_operations = ir.get('io_operations', {})
        generation_metadata = ir.get('generation_metadata', {})

        # Create output directory structure
        self._create_project_structure(output_dir)

        # Generate code components
        stats = {
            'source_file': metadata.get('source_file', 'unknown'),
            'timestamp': datetime.now().isoformat(),
            'files_generated': [],
            'entities_count': 0,
            'repositories_count': 0,
            'services_count': 0,
        }

        # 1. Generate @Entity classes
        entities = self._generate_entities(data_structures, output_dir)
        stats['entities_count'] = len(entities)
        stats['files_generated'].extend(entities)

        # 2. Generate @Repository interfaces
        repositories = self._generate_repositories(io_operations, data_structures, output_dir)
        stats['repositories_count'] = len(repositories)
        stats['files_generated'].extend(repositories)

        # 3. Generate @Service classes
        services = self._generate_services(business_logic, data_structures, io_operations, output_dir)
        stats['services_count'] = len(services)
        stats['files_generated'].extend(services)

        # 4. Generate Application class (main entry point)
        app_file = self._generate_application_class(metadata, output_dir)
        stats['files_generated'].append(app_file)

        # 5. Generate application.properties
        props_file = self._generate_application_properties(metadata, output_dir)
        stats['files_generated'].append(props_file)

        # 6. Generate pom.xml
        pom_file = self._generate_pom_xml(metadata, output_dir)
        stats['files_generated'].append(pom_file)

        # 7. Generate README
        readme_file = self._generate_readme(metadata, stats, output_dir)
        stats['files_generated'].append(readme_file)

        return stats

    def _create_project_structure(self, output_dir: Path):
        """Create Spring Boot project directory structure"""
        dirs = [
            output_dir / 'src' / 'main' / 'java' / self.package_path / 'entity',
            output_dir / 'src' / 'main' / 'java' / self.package_path / 'repository',
            output_dir / 'src' / 'main' / 'java' / self.package_path / 'service',
            output_dir / 'src' / 'main' / 'resources',
            output_dir / 'src' / 'test' / 'java' / self.package_path,
        ]
        for dir_path in dirs:
            dir_path.mkdir(parents=True, exist_ok=True)

    def _generate_entities(self, data_structures: Dict, output_dir: Path) -> List[str]:
        """
        Generate JPA @Entity classes from COBOL data structures.

        WHAT: Convert COBOL records → Java @Entity classes
        WHY: Map COBOL file structures to database tables
        HOW: For each entity in IR, generate Java class with JPA annotations
        """
        entities_generated = []
        entities = data_structures.get('entities', [])

        for entity in entities:
            entity_name = entity['name']
            fields = entity.get('fields', [])

            # Generate entity class
            java_code = self._generate_entity_class(entity_name, fields, entity)

            # Write to file
            entity_file = output_dir / 'src' / 'main' / 'java' / self.package_path / 'entity' / f'{entity_name}.java'
            entity_file.write_text(java_code, encoding='utf-8')

            entities_generated.append(str(entity_file))

        return entities_generated

    def _generate_entity_class(self, entity_name: str, fields: List[Dict], entity_info: Dict) -> str:
        """Generate single @Entity class"""

        # Import statements
        imports = [
            "import jakarta.persistence.*;",
            "import lombok.Data;",
            "import lombok.NoArgsConstructor;",
            "import lombok.AllArgsConstructor;",
        ]

        # Check if BigDecimal is needed
        needs_bigdecimal = any(f.get('data_type') == 'BigDecimal' for f in fields)
        if needs_bigdecimal:
            imports.append("import java.math.BigDecimal;")

        imports_section = '\n'.join(imports)

        # Generate field declarations
        field_declarations = []
        for field in fields:
            field_name = self._to_camel_case(field['name'])
            data_type = field.get('data_type', 'String')
            cobol_pic = field.get('cobol_picture', '')
            length = field.get('length')

            # Add JPA column annotation
            if data_type == 'String' and length:
                field_declarations.append(f'    @Column(length = {length})')
            elif data_type == 'BigDecimal':
                decimals = field.get('decimals', 2)
                precision = length if length else 10
                field_declarations.append(f'    @Column(precision = {precision}, scale = {decimals})')
            else:
                field_declarations.append(f'    @Column')

            # Map COBOL type to Java type
            java_type = self._map_cobol_to_java_type(data_type)
            field_declarations.append(f'    private {java_type} {field_name};')
            field_declarations.append('')

        fields_section = '\n'.join(field_declarations)

        # Generate class
        class_code = f'''package {self.base_package}.entity;

{imports_section}

/**
 * Entity class generated from COBOL structure: {entity_name}
 * Source: {entity_info.get('source', 'COBOL')}
 * Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
 */
@Entity
@Table(name = "{entity_name.upper()}")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class {entity_name} {{

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

{fields_section}}}
'''
        return class_code

    def _generate_repositories(self, io_operations: Dict, data_structures: Dict, output_dir: Path) -> List[str]:
        """
        Generate JPA Repository interfaces.

        WHAT: Create Spring Data JPA repositories
        WHY: Provide data access layer for entities
        HOW: Use repository_recommendations from IR to generate interfaces
        """
        repositories_generated = []
        repo_recommendations = io_operations.get('repository_recommendations', [])

        for repo in repo_recommendations:
            entity_name = repo['entity']
            repo_type = repo.get('repository_type', 'JpaRepository')
            operations = repo.get('operations_needed', [])

            # Generate repository interface
            java_code = self._generate_repository_interface(entity_name, repo_type, operations)

            # Write to file
            repo_file = output_dir / 'src' / 'main' / 'java' / self.package_path / 'repository' / f'{entity_name}Repository.java'
            repo_file.write_text(java_code, encoding='utf-8')

            repositories_generated.append(str(repo_file))

        return repositories_generated

    def _generate_repository_interface(self, entity_name: str, repo_type: str, operations: List[str]) -> str:
        """Generate single Repository interface"""

        # Determine repository base class
        if 'ReadOnly' in repo_type or 'READ' in operations and 'WRITE' not in operations:
            extends = f'Repository<{entity_name}, Long>'
            custom_methods = self._generate_read_only_methods(entity_name)
        else:
            extends = f'JpaRepository<{entity_name}, Long>'
            custom_methods = ''

        interface_code = f'''package {self.base_package}.repository;

import {self.base_package}.entity.{entity_name};
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.Repository;

import java.util.List;
import java.util.Optional;

/**
 * Repository interface for {entity_name}
 * Operations needed: {', '.join(operations)}
 * Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
 */
public interface {entity_name}Repository extends {extends} {{
{custom_methods}}}
'''
        return interface_code

    def _generate_read_only_methods(self, entity_name: str) -> str:
        """Generate custom methods for read-only repositories"""
        return f'''
    List<{entity_name}> findAll();

    Optional<{entity_name}> findById(Long id);
'''

    def _generate_services(self, business_logic: Dict, data_structures: Dict, io_operations: Dict, output_dir: Path) -> List[str]:
        """
        Generate @Service classes from business logic procedures.

        WHAT: Convert COBOL procedures → Spring @Service methods
        WHY: Implement business logic layer
        HOW: Map procedures to service methods, inject repositories
        """
        services_generated = []

        # For now, generate a single service class that orchestrates all operations
        # In future iterations, could split by domain/entity

        procedures = business_logic.get('procedures', [])
        workflows = business_logic.get('workflows', [])
        repo_recommendations = io_operations.get('repository_recommendations', [])

        if procedures:
            service_code = self._generate_main_service_class(
                procedures, workflows, repo_recommendations, data_structures
            )

            service_file = output_dir / 'src' / 'main' / 'java' / self.package_path / 'service' / 'CobolMigrationService.java'
            service_file.write_text(service_code, encoding='utf-8')

            services_generated.append(str(service_file))

        return services_generated

    def _generate_main_service_class(self, procedures: List[Dict], workflows: List[Dict],
                                     repos: List[Dict], data_structures: Dict) -> str:
        """Generate main service class with business logic"""

        # Generate repository field injections
        repo_fields = []
        for repo in repos:
            entity_name = repo['entity']
            field_name = self._to_camel_case(entity_name) + 'Repository'
            repo_fields.append(f'    private final {entity_name}Repository {field_name};')

        repo_fields_section = '\n'.join(repo_fields) if repo_fields else '    // No repositories needed'

        # Generate constructor
        constructor_params = []
        for repo in repos:
            entity_name = repo['entity']
            field_name = self._to_camel_case(entity_name) + 'Repository'
            constructor_params.append(f'{entity_name}Repository {field_name}')

        constructor_section = ', '.join(constructor_params) if constructor_params else ''

        # Generate service methods from procedures
        service_methods = []
        for proc in procedures:
            method_code = self._generate_service_method(proc, repos, data_structures)
            service_methods.append(method_code)

        methods_section = '\n\n'.join(service_methods)

        service_code = f'''package {self.base_package}.service;

import {self.base_package}.entity.*;
import {self.base_package}.repository.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * Main service class - migrated from COBOL business logic
 * Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
 */
@Service
@Slf4j
@RequiredArgsConstructor
public class CobolMigrationService {{

{repo_fields_section}

{methods_section}
}}
'''
        return service_code

    def _generate_service_method(self, procedure: Dict, repos: List[Dict], data_structures: Dict) -> str:
        """Generate single service method from COBOL procedure"""

        proc_name = procedure['name']
        proc_type = procedure.get('type', 'business_logic')
        logic_steps = procedure.get('logic_steps', [])

        # Convert procedure name to Java method name
        method_name = self._to_camel_case(proc_name)

        # Determine if method needs @Transactional
        has_write_ops = any(
            step.get('step_type') == 'data_operation' and step.get('operation') in ['WRITE', 'OPEN']
            for step in logic_steps
        )
        transactional = '    @Transactional\n' if has_write_ops else ''

        # Generate method body from logic steps
        method_body = self._generate_method_body(logic_steps, repos, data_structures)

        # Generate method comment
        what = procedure.get('_what', 'Migrated COBOL procedure')

        method_code = f'''    /**
     * {what}
     * Migrated from COBOL procedure: {proc_name}
     * Type: {proc_type}
     */
{transactional}    public void {method_name}() {{
        log.info("Executing {proc_name}");

{method_body}

        log.info("Completed {proc_name}");
    }}'''

        return method_code

    def _generate_method_body(self, logic_steps: List[Dict], repos: List[Dict], data_structures: Dict) -> str:
        """Generate Java code from COBOL logic steps"""

        java_lines = []
        indent = '        '

        for step in logic_steps:
            step_type = step.get('step_type')

            if step_type == 'data_operation':
                operation = step.get('operation')

                if operation == 'WRITE':
                    record = step.get('record')
                    entity = step.get('entity', record)
                    # TODO: Generate save operation
                    java_lines.append(f'{indent}// WRITE {record} -> repository.save()')
                    java_lines.append(f'{indent}// {step.get("description", "")}')

                elif operation == 'READ':
                    file = step.get('file')
                    java_lines.append(f'{indent}// READ {file} -> repository.findAll()')
                    java_lines.append(f'{indent}// {step.get("description", "")}')

                elif operation == 'OPEN':
                    java_lines.append(f'{indent}// OPEN operation (Spring Data handles connections)')

                elif operation == 'CLOSE':
                    java_lines.append(f'{indent}// CLOSE operation (Spring Data handles cleanup)')

            elif step_type == 'data_transformation':
                from_field = step.get('from_field', '')
                to_field = step.get('to_field', '')
                spring_equiv = step.get('_spring_boot_equivalent', '')
                java_lines.append(f'{indent}// {spring_equiv}')

            elif step_type == 'message':
                output = step.get('output', '')
                java_lines.append(f'{indent}log.info({output});')

            elif step_type == 'loop':
                condition = step.get('condition', '')
                java_lines.append(f'{indent}// TODO: Implement loop (UNTIL {condition})')

            elif step_type == 'conditional':
                condition = step.get('condition', '')
                java_lines.append(f'{indent}// TODO: Implement conditional (IF {condition})')

            else:
                # Generic comment for unhandled steps
                desc = step.get('description', step.get('code_snippet', ''))
                if desc and not desc.startswith('*'):
                    java_lines.append(f'{indent}// {desc}')

        return '\n'.join(java_lines) if java_lines else f'{indent}// TODO: Implement procedure logic'

    def _generate_application_class(self, metadata: Dict, output_dir: Path) -> str:
        """Generate Spring Boot Application main class"""

        app_code = f'''package {self.base_package};

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * Spring Boot Application - Migrated from COBOL
 * Source: {metadata.get('source_file', 'unknown')}
 * Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
 */
@SpringBootApplication
public class CobolMigrationApplication {{

    public static void main(String[] args) {{
        SpringApplication.run(CobolMigrationApplication.class, args);
    }}
}}
'''

        app_file = output_dir / 'src' / 'main' / 'java' / self.package_path / 'CobolMigrationApplication.java'
        app_file.write_text(app_code, encoding='utf-8')

        return str(app_file)

    def _generate_application_properties(self, metadata: Dict, output_dir: Path) -> str:
        """Generate application.properties configuration"""

        props = f'''# Spring Boot Application Properties
# Generated from COBOL: {metadata.get('source_file', 'unknown')}
# Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}

# Application
spring.application.name=cobol-migration-service

# H2 Database (in-memory for testing)
spring.datasource.url=jdbc:h2:mem:coboldb
spring.datasource.driverClassName=org.h2.Driver
spring.datasource.username=sa
spring.datasource.password=

# JPA / Hibernate
spring.jpa.database-platform=org.hibernate.dialect.H2Dialect
spring.jpa.hibernate.ddl-auto=create-drop
spring.jpa.show-sql=true
spring.jpa.properties.hibernate.format_sql=true

# H2 Console (for development)
spring.h2.console.enabled=true
spring.h2.console.path=/h2-console

# Logging
logging.level.{self.base_package}=DEBUG
logging.level.org.springframework.data.jpa=DEBUG
'''

        props_file = output_dir / 'src' / 'main' / 'resources' / 'application.properties'
        props_file.write_text(props, encoding='utf-8')

        return str(props_file)

    def _generate_pom_xml(self, metadata: Dict, output_dir: Path) -> str:
        """Generate Maven pom.xml"""

        pom = f'''<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
         https://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <parent>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-parent</artifactId>
        <version>3.2.0</version>
        <relativePath/>
    </parent>

    <groupId>com.legacy</groupId>
    <artifactId>cobol-migration</artifactId>
    <version>1.0.0-SNAPSHOT</version>
    <name>COBOL Migration Service</name>
    <description>Spring Boot application migrated from COBOL: {metadata.get('source_file', 'unknown')}</description>

    <properties>
        <java.version>17</java.version>
        <maven.compiler.source>17</maven.compiler.source>
        <maven.compiler.target>17</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>

    <dependencies>
        <!-- Spring Boot Starters -->
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-data-jpa</artifactId>
        </dependency>

        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-web</artifactId>
        </dependency>

        <!-- H2 Database (for testing) -->
        <dependency>
            <groupId>com.h2database</groupId>
            <artifactId>h2</artifactId>
            <scope>runtime</scope>
        </dependency>

        <!-- Lombok (reduces boilerplate) -->
        <dependency>
            <groupId>org.projectlombok</groupId>
            <artifactId>lombok</artifactId>
            <optional>true</optional>
        </dependency>

        <!-- Testing -->
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-test</artifactId>
            <scope>test</scope>
        </dependency>
    </dependencies>

    <build>
        <plugins>
            <plugin>
                <groupId>org.springframework.boot</groupId>
                <artifactId>spring-boot-maven-plugin</artifactId>
                <configuration>
                    <excludes>
                        <exclude>
                            <groupId>org.projectlombok</groupId>
                            <artifactId>lombok</artifactId>
                        </exclude>
                    </excludes>
                </configuration>
            </plugin>
        </plugins>
    </build>
</project>
'''

        pom_file = output_dir / 'pom.xml'
        pom_file.write_text(pom, encoding='utf-8')

        return str(pom_file)

    def _generate_readme(self, metadata: Dict, stats: Dict, output_dir: Path) -> str:
        """Generate README with build/run instructions"""

        readme = f'''# COBOL Migration - Spring Boot Application

**Generated from**: `{metadata.get('source_file', 'unknown')}`
**Generated on**: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
**Generator**: LMOD COBOL → Spring Boot Migration Tool

---

## 📊 Generation Statistics

- **Entities**: {stats['entities_count']} JPA entities
- **Repositories**: {stats['repositories_count']} Spring Data repositories
- **Services**: {stats['services_count']} service classes
- **Total Files**: {len(stats['files_generated'])} files generated

---

## 🚀 Quick Start

### Prerequisites

- **Java 17** or higher
- **Maven 3.8+**

### Build

```bash
mvn clean package
```

### Run

```bash
mvn spring-boot:run
```

The application will start on `http://localhost:8080`

### Access H2 Console

Open browser: `http://localhost:8080/h2-console`

- **JDBC URL**: `jdbc:h2:mem:coboldb`
- **Username**: `sa`
- **Password**: _(leave empty)_

---

## 📁 Project Structure

```
.
├── pom.xml                              # Maven dependencies
├── src/
│   ├── main/
│   │   ├── java/com/legacy/cobol/
│   │   │   ├── CobolMigrationApplication.java   # Main entry point
│   │   │   ├── entity/                          # JPA @Entity classes
│   │   │   ├── repository/                      # Spring Data repositories
│   │   │   └── service/                         # Business logic services
│   │   └── resources/
│   │       └── application.properties           # Spring Boot config
│   └── test/
│       └── java/                                # Unit tests (TODO)
└── README.md                            # This file
```

---

## 🔧 Configuration

Edit `src/main/resources/application.properties` to:

- Change database (H2 → PostgreSQL/MySQL)
- Adjust logging levels
- Configure server port

---

## 🧪 Testing

```bash
# Run all tests
mvn test

# Run with coverage
mvn test jacoco:report
```

---

## 📦 Deployment

### Build JAR

```bash
mvn clean package
java -jar target/cobol-migration-1.0.0-SNAPSHOT.jar
```

### Docker (TODO)

```bash
docker build -t cobol-migration .
docker run -p 8080:8080 cobol-migration
```

---

## 🛠️ Next Steps

1. **Review generated code**: Check entity mappings and service logic
2. **Add validation**: Implement Jakarta Validation constraints
3. **Add tests**: Write unit and integration tests
4. **Add REST API**: Create @RestController endpoints
5. **Configure production DB**: Replace H2 with PostgreSQL/MySQL
6. **Add security**: Implement Spring Security if needed

---

## 📝 Notes

- This is a **generated starting point**, not production-ready code
- Review all `// TODO` comments for incomplete logic
- Test thoroughly before deploying to production
- Consider adding error handling and validation

---

**Generated by LMOD v1.0** - Legacy Modernization Platform
'''

        readme_file = output_dir / 'README.md'
        readme_file.write_text(readme, encoding='utf-8')

        return str(readme_file)

    # ========================================
    # Helper Methods
    # ========================================

    def _to_camel_case(self, name: str) -> str:
        """Convert COBOL-STYLE-NAME to camelCaseName"""
        parts = name.replace('_', '-').split('-')
        if not parts:
            return name
        # First part lowercase, rest title case
        return parts[0].lower() + ''.join(p.capitalize() for p in parts[1:])

    def _map_cobol_to_java_type(self, cobol_type: str) -> str:
        """Map COBOL data type to Java type"""
        type_map = {
            'String': 'String',
            'Integer': 'Integer',
            'BigDecimal': 'BigDecimal',
            'Long': 'Long',
            'Double': 'Double',
            'Boolean': 'Boolean',
        }
        return type_map.get(cobol_type, 'String')

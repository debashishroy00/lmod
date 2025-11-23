package com.legacy.cobol.entity;

import jakarta.persistence.*;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;

/**
 * Entity class generated from COBOL structure: DiagDetails
 * Source: FILE SECTION
 * Generated: 2025-11-22 19:40:16
 */
@Entity
@Table(name = "DIAGDETAILS")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class DiagDetails {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(length = 5)
    private String diagcode;

    @Column(length = 70)
    private String diagname;
}

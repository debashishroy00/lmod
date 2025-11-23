package com.legacy.cobol.entity;

import jakarta.persistence.*;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;

/**
 * Entity class generated from COBOL structure: READ-EOF
 * Source: WORKING-STORAGE
 * Generated: 2025-11-22 19:40:16
 */
@Entity
@Table(name = "READ-EOF")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class READ-EOF {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column
    private Integer readEof;
}

package com.legacy.cobol.repository;

import com.legacy.cobol.entity.DiagDetails;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.Repository;

import java.util.List;
import java.util.Optional;

/**
 * Repository interface for DiagDetails
 * Operations needed: WRITE
 * Generated: 2025-11-22 19:40:16
 */
public interface DiagDetailsRepository extends JpaRepository<DiagDetails, Long> {
}

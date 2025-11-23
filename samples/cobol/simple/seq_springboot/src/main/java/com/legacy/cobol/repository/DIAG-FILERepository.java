package com.legacy.cobol.repository;

import com.legacy.cobol.entity.DIAG-FILE;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.Repository;

import java.util.List;
import java.util.Optional;

/**
 * Repository interface for DIAG-FILE
 * Operations needed: CLOSE, OPEN, READ
 * Generated: 2025-11-22 19:40:16
 */
public interface DIAG-FILERepository extends Repository<DIAG-FILE, Long> {

    List<DIAG-FILE> findAll();

    Optional<DIAG-FILE> findById(Long id);
}

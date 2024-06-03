package com.service.productcatalogue.repository;

import com.service.productcatalogue.entity.ProductAssociation;
import jakarta.transaction.Transactional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface ProductAssociationRepository extends JpaRepository<ProductAssociation,Long> {

    Optional<ProductAssociation> findBySku(String sku);
    @Transactional
    @Modifying
    void deleteBySku(String sku);

}

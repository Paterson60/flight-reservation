package com.service.productcatalogue.repository;

import com.service.productcatalogue.entity.Product;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface ProductRepository extends JpaRepository<Product,Long> {

//    Optional<Product> findBySku(Long sku);
    Optional<Product> findBySku(String sku);
}

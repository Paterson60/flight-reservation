package com.service.productcatalogue.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import lombok.*;

@Entity
@Getter@Setter@ToString@AllArgsConstructor@NoArgsConstructor
public class ProductAssociation {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long associationId;
    private String sku;
    private String relatedProducts;
    private String bundleDeals;
    private String productVariations;
}

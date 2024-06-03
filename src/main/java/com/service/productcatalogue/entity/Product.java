package com.service.productcatalogue.entity;

import jakarta.persistence.*;
import lombok.*;

import java.util.List;

@Entity
@Getter@Setter@ToString@AllArgsConstructor@NoArgsConstructor
public class Product {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long productId;
    private String name;
    private String category;
    private String description;
    private String image;
    private String specification;
    private String sku;

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "price_id", nullable = false)
    private Price price;
//    @ManyToOne(cascade = CascadeType.ALL)
//    @JoinColumn(name = "association_id", nullable = false)
//    private ProductAssociation productAssociation;
}

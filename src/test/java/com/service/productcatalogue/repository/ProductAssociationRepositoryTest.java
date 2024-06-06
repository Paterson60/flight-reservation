package com.service.productcatalogue.repository;

import com.service.productcatalogue.entity.ProductAssociation; import org.junit.jupiter.api.Test; import org.springframework.beans.factory.annotation.Autowired; import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

@DataJpaTest class ProductAssociationRepositoryTest {

    @Autowired
    private ProductAssociationRepository productAssociationRepository;

    @Test
    void testSaveAndFindById() {
        ProductAssociation productAssociation = new ProductAssociation();
        productAssociation.setSku("AZVP1!");
        productAssociation.setRelatedProducts("Updated Related Products");
        productAssociation.setBundleDeals("Updated Bundle Deals");
        productAssociation.setProductVariations("Updated Product Variations");
        ProductAssociation savedProductAssociation = productAssociationRepository.save(productAssociation);

        Optional<ProductAssociation> retrievedProductAssociation = productAssociationRepository.findById(savedProductAssociation.getAssociationId());
        assertThat(retrievedProductAssociation).isPresent();
        assertThat(retrievedProductAssociation.get().getRelatedProducts()).isEqualTo("Updated Related Products");
    }

    @Test
    void testDeleteById() {
        ProductAssociation productAssociation = new ProductAssociation();
        productAssociation.setSku("AZVP1!");
        productAssociation.setRelatedProducts("Updated Related Products");
        productAssociation.setBundleDeals("Updated Bundle Deals");
        productAssociation.setProductVariations("Updated Product Variations");
        ProductAssociation savedProductAssociation = productAssociationRepository.save(productAssociation);

        productAssociationRepository.deleteById(savedProductAssociation.getAssociationId());
        Optional<ProductAssociation> retrievedProductAssociation = productAssociationRepository.findById(savedProductAssociation.getAssociationId());
        assertThat(retrievedProductAssociation).isNotPresent();
    }
}
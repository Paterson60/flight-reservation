package com.service.productcatalogue.repository;

import com.service.productcatalogue.entity.Price; import com.service.productcatalogue.entity.Product; import com.service.productcatalogue.entity.ProductAssociation; import org.junit.jupiter.api.Test; import org.springframework.beans.factory.annotation.Autowired; import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

@DataJpaTest class ProductRepositoryTest {

    @Autowired
    private ProductRepository productRepository;

    @Autowired
    private PriceRepository priceRepository;

    @Autowired
    private ProductAssociationRepository productAssociationRepository;

    @Test
    void testSaveAndFindById() {
        Price price = new Price();
        price.setAmount(1200L);
        price.setDiscount(200L);
        Price savedPrice = priceRepository.save(price);

        ProductAssociation productAssociation = new ProductAssociation();
        productAssociation.setSku("AZVP1!");
        productAssociation.setRelatedProducts("Updated Related Products");
        productAssociation.setBundleDeals("Updated Bundle Deals");
        productAssociation.setProductVariations("Updated Product Variations");
        ProductAssociation savedProductAssociation = productAssociationRepository.save(productAssociation);

        Product product = new Product();
        product.setName("Updated Airpod");
        product.setCategory("Headphone");
        product.setDescription("Updated Wireless Bluetooth Device");
        product.setImage("updated_image_url");
        product.setSpecification("Updated Airpod Gen 2.0");
        product.setSku("AZVP1!");
        product.setPrice(savedPrice);
        product.setProductAssociation(savedProductAssociation);
        Product savedProduct = productRepository.save(product);

        Optional<Product> retrievedProduct = productRepository.findById(savedProduct.getProductId());
        assertThat(retrievedProduct).isPresent();
        assertThat(retrievedProduct.get().getName()).isEqualTo("Updated Airpod");
    }

    @Test
    void testDeleteById() {
        Price price = new Price();
        price.setAmount(1200L);
        price.setDiscount(200L);
        Price savedPrice = priceRepository.save(price);

        ProductAssociation productAssociation = new ProductAssociation();
        productAssociation.setSku("AZVP1!");
        productAssociation.setRelatedProducts("Updated Related Products");
        productAssociation.setBundleDeals("Updated Bundle Deals");
        productAssociation.setProductVariations("Updated Product Variations");
        ProductAssociation savedProductAssociation = productAssociationRepository.save(productAssociation);

        Product product = new Product();
        product.setName("Updated Airpod");
        product.setCategory("Headphone");
        product.setDescription("Updated Wireless Bluetooth Device");
        product.setImage("updated_image_url");
        product.setSpecification("Updated Airpod Gen 2.0");
        product.setSku("AZVP1!");
        product.setPrice(savedPrice);
        product.setProductAssociation(savedProductAssociation);
        Product savedProduct = productRepository.save(product);

        productRepository.deleteById(savedProduct.getProductId());
        Optional<Product> retrievedProduct = productRepository.findById(savedProduct.getProductId());
        assertThat(retrievedProduct).isNotPresent();
    }
}
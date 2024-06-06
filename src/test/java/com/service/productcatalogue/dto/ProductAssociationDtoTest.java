package com.service.productcatalogue.dto;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ProductAssociationDtoTest {

    private ProductAssociationDto productAssociationDtoUnderTest;

    @BeforeEach
    void setUp() {
        productAssociationDtoUnderTest = new ProductAssociationDto();
    }

    @Test
    void testSkuGetterAndSetter() {
        final String sku = "sku";
        productAssociationDtoUnderTest.setSku(sku);
        assertThat(productAssociationDtoUnderTest.getSku()).isEqualTo(sku);
    }

    @Test
    void testRelatedProductsGetterAndSetter() {
        final String relatedProducts = "relatedProducts";
        productAssociationDtoUnderTest.setRelatedProducts(relatedProducts);
        assertThat(productAssociationDtoUnderTest.getRelatedProducts()).isEqualTo(relatedProducts);
    }

    @Test
    void testBundleDealsGetterAndSetter() {
        final String bundleDeals = "bundleDeals";
        productAssociationDtoUnderTest.setBundleDeals(bundleDeals);
        assertThat(productAssociationDtoUnderTest.getBundleDeals()).isEqualTo(bundleDeals);
    }

    @Test
    void testProductVariationsGetterAndSetter() {
        final String productVariations = "productVariations";
        productAssociationDtoUnderTest.setProductVariations(productVariations);
        assertThat(productAssociationDtoUnderTest.getProductVariations()).isEqualTo(productVariations);
    }

    @Test
    void testEquals() {
        assertThat(productAssociationDtoUnderTest.equals("o")).isFalse();
    }

    @Test
    void testCanEqual() {
        assertThat(productAssociationDtoUnderTest.canEqual("other")).isFalse();
    }

}

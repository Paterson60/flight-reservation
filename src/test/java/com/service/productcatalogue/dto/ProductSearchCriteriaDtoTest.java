package com.service.productcatalogue.dto;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ProductSearchCriteriaDtoTest {

    private ProductSearchCriteriaDto productSearchCriteriaDtoUnderTest;

    @BeforeEach
    void setUp() {
        productSearchCriteriaDtoUnderTest = new ProductSearchCriteriaDto();
    }

    @Test
    void testNameGetterAndSetter() {
        final String name = "name";
        productSearchCriteriaDtoUnderTest.setName(name);
        assertThat(productSearchCriteriaDtoUnderTest.getName()).isEqualTo(name);
    }

    @Test
    void testCategoryGetterAndSetter() {
        final String category = "category";
        productSearchCriteriaDtoUnderTest.setCategory(category);
        assertThat(productSearchCriteriaDtoUnderTest.getCategory()).isEqualTo(category);
    }

    @Test
    void testDescriptionGetterAndSetter() {
        final String description = "description";
        productSearchCriteriaDtoUnderTest.setDescription(description);
        assertThat(productSearchCriteriaDtoUnderTest.getDescription()).isEqualTo(description);
    }

    @Test
    void testImageGetterAndSetter() {
        final String image = "image";
        productSearchCriteriaDtoUnderTest.setImage(image);
        assertThat(productSearchCriteriaDtoUnderTest.getImage()).isEqualTo(image);
    }

    @Test
    void testSpecificationGetterAndSetter() {
        final String specification = "specification";
        productSearchCriteriaDtoUnderTest.setSpecification(specification);
        assertThat(productSearchCriteriaDtoUnderTest.getSpecification()).isEqualTo(specification);
    }

    @Test
    void testSkuGetterAndSetter() {
        final String sku = "sku";
        productSearchCriteriaDtoUnderTest.setSku(sku);
        assertThat(productSearchCriteriaDtoUnderTest.getSku()).isEqualTo(sku);
    }

    @Test
    void testBrandGetterAndSetter() {
        final String brand = "brand";
        productSearchCriteriaDtoUnderTest.setBrand(brand);
        assertThat(productSearchCriteriaDtoUnderTest.getBrand()).isEqualTo(brand);
    }

    @Test
    void testAmountGetterAndSetter() {
        final Long amount = 0L;
        productSearchCriteriaDtoUnderTest.setAmount(amount);
        assertThat(productSearchCriteriaDtoUnderTest.getAmount()).isEqualTo(amount);
    }

    @Test
    void testEquals() {
        assertThat(productSearchCriteriaDtoUnderTest.equals("o")).isFalse();
    }

    @Test
    void testCanEqual() {
        assertThat(productSearchCriteriaDtoUnderTest.canEqual("other")).isFalse();
    }

}

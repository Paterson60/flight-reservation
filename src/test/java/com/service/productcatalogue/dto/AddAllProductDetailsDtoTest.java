package com.service.productcatalogue.dto;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class AddAllProductDetailsDtoTest {

    private AddAllProductDetailsDto addAllProductDetailsDtoUnderTest;

    @BeforeEach
    void setUp() {
        addAllProductDetailsDtoUnderTest = new AddAllProductDetailsDto();
    }

    @Test
    void testNameGetterAndSetter() {
        final String name = "name";
        addAllProductDetailsDtoUnderTest.setName(name);
        assertThat(addAllProductDetailsDtoUnderTest.getName()).isEqualTo(name);
    }

    @Test
    void testCategoryGetterAndSetter() {
        final String category = "category";
        addAllProductDetailsDtoUnderTest.setCategory(category);
        assertThat(addAllProductDetailsDtoUnderTest.getCategory()).isEqualTo(category);
    }

    @Test
    void testDescriptionGetterAndSetter() {
        final String description = "description";
        addAllProductDetailsDtoUnderTest.setDescription(description);
        assertThat(addAllProductDetailsDtoUnderTest.getDescription()).isEqualTo(description);
    }

    @Test
    void testImageGetterAndSetter() {
        final String image = "image";
        addAllProductDetailsDtoUnderTest.setImage(image);
        assertThat(addAllProductDetailsDtoUnderTest.getImage()).isEqualTo(image);
    }

    @Test
    void testSpecificationGetterAndSetter() {
        final String specification = "specification";
        addAllProductDetailsDtoUnderTest.setSpecification(specification);
        assertThat(addAllProductDetailsDtoUnderTest.getSpecification()).isEqualTo(specification);
    }

    @Test
    void testSkuGetterAndSetter() {
        final String sku = "sku";
        addAllProductDetailsDtoUnderTest.setSku(sku);
        assertThat(addAllProductDetailsDtoUnderTest.getSku()).isEqualTo(sku);
    }

    @Test
    void testBrandGetterAndSetter() {
        final String brand = "brand";
        addAllProductDetailsDtoUnderTest.setBrand(brand);
        assertThat(addAllProductDetailsDtoUnderTest.getBrand()).isEqualTo(brand);
    }

    @Test
    void testRelatedProductsGetterAndSetter() {
        final String relatedProducts = "relatedProducts";
        addAllProductDetailsDtoUnderTest.setRelatedProducts(relatedProducts);
        assertThat(addAllProductDetailsDtoUnderTest.getRelatedProducts()).isEqualTo(relatedProducts);
    }

    @Test
    void testBundleDealsGetterAndSetter() {
        final String bundleDeals = "bundleDeals";
        addAllProductDetailsDtoUnderTest.setBundleDeals(bundleDeals);
        assertThat(addAllProductDetailsDtoUnderTest.getBundleDeals()).isEqualTo(bundleDeals);
    }

    @Test
    void testProductVariationsGetterAndSetter() {
        final String productVariations = "productVariations";
        addAllProductDetailsDtoUnderTest.setProductVariations(productVariations);
        assertThat(addAllProductDetailsDtoUnderTest.getProductVariations()).isEqualTo(productVariations);
    }

    @Test
    void testAmountGetterAndSetter() {
        final Long amount = 0L;
        addAllProductDetailsDtoUnderTest.setAmount(amount);
        assertThat(addAllProductDetailsDtoUnderTest.getAmount()).isEqualTo(amount);
    }

    @Test
    void testDiscountGetterAndSetter() {
        final Long discount = 0L;
        addAllProductDetailsDtoUnderTest.setDiscount(discount);
        assertThat(addAllProductDetailsDtoUnderTest.getDiscount()).isEqualTo(discount);
    }

    @Test
    void testEquals() {
        assertThat(addAllProductDetailsDtoUnderTest.equals("o")).isFalse();
    }

    @Test
    void testCanEqual() {
        assertThat(addAllProductDetailsDtoUnderTest.canEqual("other")).isFalse();
    }


}

package com.service.productcatalogue.dto;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ProductDtoTest {

    private ProductDto productDtoUnderTest;

    @BeforeEach
    void setUp() {
        productDtoUnderTest = new ProductDto();
    }

    @Test
    void testNameGetterAndSetter() {
        final String name = "name";
        productDtoUnderTest.setName(name);
        assertThat(productDtoUnderTest.getName()).isEqualTo(name);
    }

    @Test
    void testCategoryGetterAndSetter() {
        final String category = "category";
        productDtoUnderTest.setCategory(category);
        assertThat(productDtoUnderTest.getCategory()).isEqualTo(category);
    }

    @Test
    void testDescriptionGetterAndSetter() {
        final String description = "description";
        productDtoUnderTest.setDescription(description);
        assertThat(productDtoUnderTest.getDescription()).isEqualTo(description);
    }

    @Test
    void testImageGetterAndSetter() {
        final String image = "image";
        productDtoUnderTest.setImage(image);
        assertThat(productDtoUnderTest.getImage()).isEqualTo(image);
    }

    @Test
    void testSpecificationGetterAndSetter() {
        final String specification = "specification";
        productDtoUnderTest.setSpecification(specification);
        assertThat(productDtoUnderTest.getSpecification()).isEqualTo(specification);
    }

    @Test
    void testSkuGetterAndSetter() {
        final String sku = "sku";
        productDtoUnderTest.setSku(sku);
        assertThat(productDtoUnderTest.getSku()).isEqualTo(sku);
    }

    @Test
    void testBrandGetterAndSetter() {
        final String brand = "brand";
        productDtoUnderTest.setBrand(brand);
        assertThat(productDtoUnderTest.getBrand()).isEqualTo(brand);
    }

    @Test
    void testProductAssociationDtoGetterAndSetter() {
        final ProductAssociationDto productAssociationDto = new ProductAssociationDto();
        productDtoUnderTest.setProductAssociationDto(productAssociationDto);
        assertThat(productDtoUnderTest.getProductAssociationDto()).isEqualTo(productAssociationDto);
    }

    @Test
    void testPriceDtoGetterAndSetter() {
        final PriceDto priceDto = new PriceDto();
        productDtoUnderTest.setPriceDto(priceDto);
        assertThat(productDtoUnderTest.getPriceDto()).isEqualTo(priceDto);
    }

    @Test
    void testEquals() {
        assertThat(productDtoUnderTest.equals("o")).isFalse();
    }

    @Test
    void testCanEqual() {
        assertThat(productDtoUnderTest.canEqual("other")).isFalse();
    }

}

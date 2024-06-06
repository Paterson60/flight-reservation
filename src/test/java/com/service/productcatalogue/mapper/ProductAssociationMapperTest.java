package com.service.productcatalogue.mapper;

import com.service.productcatalogue.dto.ProductAssociationDto;
import com.service.productcatalogue.entity.ProductAssociation;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ProductAssociationMapperTest {

    @Test
    void testMapToProductAssociationDto() {

        final ProductAssociation productAssociation = new ProductAssociation(0L, "sku", "relatedProducts",
                "bundleDeals", "productVariations");
        final ProductAssociationDto productAssociationDto = new ProductAssociationDto();
        productAssociationDto.setSku("sku");
        productAssociationDto.setRelatedProducts("relatedProducts");
        productAssociationDto.setBundleDeals("bundleDeals");
        productAssociationDto.setProductVariations("productVariations");

        final ProductAssociationDto expectedResult = new ProductAssociationDto();
        expectedResult.setSku("sku");
        expectedResult.setRelatedProducts("relatedProducts");
        expectedResult.setBundleDeals("bundleDeals");
        expectedResult.setProductVariations("productVariations");

        final ProductAssociationDto result = ProductAssociationMapper.mapToProductAssociationDto(productAssociation,
                productAssociationDto);

        assertThat(result).isEqualTo(expectedResult);
    }

    @Test
    void testMapToProductAssociation() {

        final ProductAssociationDto productAssociationDto = new ProductAssociationDto();
        productAssociationDto.setSku("sku");
        productAssociationDto.setRelatedProducts("relatedProducts");
        productAssociationDto.setBundleDeals("bundleDeals");
        productAssociationDto.setProductVariations("productVariations");

        final ProductAssociation productAssociation = new ProductAssociation(0L, "sku", "relatedProducts",
                "bundleDeals", "productVariations");

        final ProductAssociation result = ProductAssociationMapper.mapToProductAssociation(productAssociationDto,
                productAssociation);
    }
}

package com.service.productcatalogue.mapper;

import com.service.productcatalogue.dto.AddAllProductDetailsDto;
import com.service.productcatalogue.entity.Product;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class AllProductMapperProductMapperTest {

    @Test
    void testMapToAllProductDto() {

        final Product product = new Product();
        product.setName("name");
        product.setCategory("category");
        product.setDescription("description");
        product.setImage("image");
        product.setSpecification("specification");
        product.setSku("sku");
        product.setBrand("brand");

        final AddAllProductDetailsDto addAllProductDetailsDto = new AddAllProductDetailsDto();
        addAllProductDetailsDto.setName("name");
        addAllProductDetailsDto.setCategory("category");
        addAllProductDetailsDto.setDescription("description");
        addAllProductDetailsDto.setImage("image");
        addAllProductDetailsDto.setSpecification("specification");
        addAllProductDetailsDto.setSku("sku");
        addAllProductDetailsDto.setBrand("brand");

        final AddAllProductDetailsDto expectedResult = new AddAllProductDetailsDto();
        expectedResult.setName("name");
        expectedResult.setCategory("category");
        expectedResult.setDescription("description");
        expectedResult.setImage("image");
        expectedResult.setSpecification("specification");
        expectedResult.setSku("sku");
        expectedResult.setBrand("brand");

        final AddAllProductDetailsDto result = AllProductMapperProductMapper.mapToAllProductDto(product,
                addAllProductDetailsDto);

        assertThat(result).isEqualTo(expectedResult);
    }

    @Test
    void testMapToProduct() {

        final AddAllProductDetailsDto addAllProductDetailsDto = new AddAllProductDetailsDto();
        addAllProductDetailsDto.setName("name");
        addAllProductDetailsDto.setCategory("category");
        addAllProductDetailsDto.setDescription("description");
        addAllProductDetailsDto.setImage("image");
        addAllProductDetailsDto.setSpecification("specification");
        addAllProductDetailsDto.setSku("sku");
        addAllProductDetailsDto.setBrand("brand");

        final Product product = new Product();
        product.setName("name");
        product.setCategory("category");
        product.setDescription("description");
        product.setImage("image");
        product.setSpecification("specification");
        product.setSku("sku");
        product.setBrand("brand");

        final Product result = AllProductMapperProductMapper.mapToProduct(addAllProductDetailsDto, product);
    }
}

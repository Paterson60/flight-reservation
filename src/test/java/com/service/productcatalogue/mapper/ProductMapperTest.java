package com.service.productcatalogue.mapper;

import com.service.productcatalogue.dto.PriceDto;
import com.service.productcatalogue.dto.ProductDto;
import com.service.productcatalogue.entity.Price;
import com.service.productcatalogue.entity.Product;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ProductMapperTest {

    @Test
    void testMapToProductDto() {

        final Product product = new Product();
        product.setName("name");
        product.setCategory("category");
        product.setDescription("description");
        product.setImage("image");
        product.setSpecification("specification");
        product.setSku("sku");
        product.setBrand("brand");

        final ProductDto productDto = new ProductDto();
        productDto.setName("name");
        productDto.setCategory("category");
        productDto.setDescription("description");
        productDto.setImage("image");
        productDto.setSpecification("specification");
        productDto.setSku("sku");
        productDto.setBrand("brand");

        final ProductDto expectedResult = new ProductDto();
        expectedResult.setName("name");
        expectedResult.setCategory("category");
        expectedResult.setDescription("description");
        expectedResult.setImage("image");
        expectedResult.setSpecification("specification");
        expectedResult.setSku("sku");
        expectedResult.setBrand("brand");

        final ProductDto result = ProductMapper.mapToProductDto(product, productDto);

        assertThat(result).isEqualTo(expectedResult);
    }

    @Test
    void testMapToProduct() {

        final ProductDto productDto = new ProductDto();
        productDto.setName("name");
        productDto.setCategory("category");
        productDto.setDescription("description");
        productDto.setImage("image");
        productDto.setSpecification("specification");
        productDto.setSku("sku");
        productDto.setBrand("brand");

        final Product product = new Product();
        product.setName("name");
        product.setCategory("category");
        product.setDescription("description");
        product.setImage("image");
        product.setSpecification("specification");
        product.setSku("sku");
        product.setBrand("brand");

        final Product result = ProductMapper.mapToProduct(productDto, product);
    }

    @Test
    void testMapToPriceDto() {

        final Price price = new Price(0L, 0L, 0L);
        final PriceDto expectedResult = new PriceDto();
        expectedResult.setAmount(0L);
        expectedResult.setDiscount(0L);

        final PriceDto result = ProductMapper.mapToPriceDto(price);

        assertThat(result).isEqualTo(expectedResult);
    }

    @Test
    void testMapToPrice() {

        final PriceDto priceDto = new PriceDto();
        priceDto.setAmount(0L);
        priceDto.setDiscount(0L);

        final Price result = ProductMapper.mapToPrice(priceDto);
    }
}

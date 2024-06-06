package com.service.productcatalogue.mapper;

import com.service.productcatalogue.dto.PriceDto;
import com.service.productcatalogue.entity.Price;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class PriceMapperTest {

    @Test
    void testMapToPriceDto() {

        final Price price = new Price(0L, 0L, 0L);
        final PriceDto priceDto = new PriceDto();
        priceDto.setAmount(0L);
        priceDto.setDiscount(0L);

        final PriceDto expectedResult = new PriceDto();
        expectedResult.setAmount(0L);
        expectedResult.setDiscount(0L);

        final PriceDto result = PriceMapper.mapToPriceDto(price, priceDto);

        assertThat(result).isEqualTo(expectedResult);
    }

    @Test
    void testMapToPrice() {

        final PriceDto priceDto = new PriceDto();
        priceDto.setAmount(0L);
        priceDto.setDiscount(0L);

        final Price price = new Price(0L, 0L, 0L);

        final Price result = PriceMapper.mapToPrice(priceDto, price);
    }
}

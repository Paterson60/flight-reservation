package com.service.productcatalogue.mapper;

import com.service.productcatalogue.dto.PriceDto;
import com.service.productcatalogue.entity.Price;

public class PriceMapper {

    public static PriceDto mapToPriceDto(Price price, PriceDto priceDto){
        priceDto.setAmount(price.getAmount());
        return priceDto;
    }

    public static Price mapToPrice(PriceDto priceDto, Price price){
        price.setAmount(priceDto.getAmount());
        return price;
    }
}

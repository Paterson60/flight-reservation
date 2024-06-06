package com.service.productcatalogue.mapper;

import com.service.productcatalogue.dto.PriceDto;
import com.service.productcatalogue.dto.ProductDto;
import com.service.productcatalogue.entity.Price;
import com.service.productcatalogue.entity.Product;

public class ProductMapper {

    public static ProductDto mapToProductDto(Product product, ProductDto productDto){
        productDto.setName(product.getName());
        productDto.setCategory(product.getCategory());
        productDto.setDescription(product.getDescription());
        productDto.setImage(product.getImage());
        productDto.setSpecification(product.getSpecification());
        productDto.setSku(product.getSku());
        productDto.setBrand(product.getBrand());
        return productDto;
    }

    public static Product mapToProduct(ProductDto productDto, Product product){
        product.setName(productDto.getName());
        product.setCategory(productDto.getCategory());
        product.setDescription(productDto.getDescription());
        product.setImage(productDto.getImage());
        product.setSpecification(productDto.getSpecification());
        product.setSku(productDto.getSku());
        product.setBrand(productDto.getBrand());
        return product;
    }

    public static PriceDto mapToPriceDto(Price price){
        PriceDto priceDto = new PriceDto();
        priceDto.setAmount(price.getAmount());
        priceDto.setDiscount(price.getDiscount());
        return priceDto;
    }

    public static Price mapToPrice(PriceDto priceDto){
        Price price = new Price();
        price.setAmount(priceDto.getAmount());
        price.setDiscount(priceDto.getDiscount());
        return price;
    }
}

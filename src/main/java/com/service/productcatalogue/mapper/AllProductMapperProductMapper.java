package com.service.productcatalogue.mapper;

import com.service.productcatalogue.dto.AddAllProductDetailsDto;
import com.service.productcatalogue.entity.Product;

public class AllProductMapperProductMapper {

    public static AddAllProductDetailsDto mapToAllProductDto(Product product, AddAllProductDetailsDto addAllProductDetailsDto){
        addAllProductDetailsDto.setName(product.getName());
        addAllProductDetailsDto.setCategory(product.getCategory());
        addAllProductDetailsDto.setDescription(product.getDescription());
        addAllProductDetailsDto.setImage(product.getImage());
        addAllProductDetailsDto.setSpecification(product.getSpecification());
        addAllProductDetailsDto.setSku(product.getSku());
        addAllProductDetailsDto.setBrand(product.getBrand());
        return addAllProductDetailsDto;
    }

    public static Product mapToProduct(AddAllProductDetailsDto addAllProductDetailsDto, Product product){
        product.setName(addAllProductDetailsDto.getName());
        product.setCategory(addAllProductDetailsDto.getCategory());
        product.setDescription(addAllProductDetailsDto.getDescription());
        product.setImage(addAllProductDetailsDto.getImage());
        product.setSpecification(addAllProductDetailsDto.getSpecification());
        product.setSku(addAllProductDetailsDto.getSku());
        product.setBrand(addAllProductDetailsDto.getBrand());
        return product;
    }
}

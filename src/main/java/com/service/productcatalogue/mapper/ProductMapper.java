package com.service.productcatalogue.mapper;

import com.service.productcatalogue.dto.ProductDto;
import com.service.productcatalogue.entity.Product;

public class ProductMapper {

    public static ProductDto mapToProductDto(Product product, ProductDto productDto){
        productDto.setName(product.getName());
        productDto.setCategory(product.getCategory());
        productDto.setDescription(product.getDescription());
        productDto.setImage(product.getImage());
        productDto.setSpecification(product.getSpecification());
        productDto.setSku(product.getSku());
        productDto.setPrice(product.getPrice());
        //productDto.setProductAssociationDto(product.getProductAssociation());
        return productDto;
    }

    public static Product mapToProduct(ProductDto productDto, Product product){
        product.setName(productDto.getName());
        product.setCategory(productDto.getCategory());
        product.setDescription(productDto.getDescription());
        product.setImage(productDto.getImage());
        product.setSpecification(productDto.getSpecification());
        product.setSku(productDto.getSku());
        product.setPrice(productDto.getPrice());
        //product.setProductAssociation(productDto.getProductAssociationDto());
        return product;
    }
}

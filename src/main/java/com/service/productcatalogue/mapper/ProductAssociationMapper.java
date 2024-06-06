package com.service.productcatalogue.mapper;

import com.service.productcatalogue.dto.ProductAssociationDto;
import com.service.productcatalogue.entity.ProductAssociation;

public class ProductAssociationMapper {

    public static ProductAssociationDto mapToProductAssociationDto(ProductAssociation productAssociation, ProductAssociationDto productAssociationDto){
        productAssociationDto.setSku(productAssociation.getSku());
        productAssociationDto.setRelatedProducts(productAssociation.getRelatedProducts());
        productAssociationDto.setBundleDeals(productAssociation.getBundleDeals());
        productAssociationDto.setProductVariations(productAssociation.getProductVariations());
        return productAssociationDto;
    }

    public static ProductAssociation mapToProductAssociation(ProductAssociationDto productAssociationDto, ProductAssociation productAssociation){

        productAssociation.setSku(productAssociationDto.getSku());
       productAssociation.setRelatedProducts(productAssociationDto.getRelatedProducts());
       productAssociation.setBundleDeals(productAssociationDto.getBundleDeals());
       productAssociation.setProductVariations(productAssociationDto.getProductVariations());
       return productAssociation;
    }
}

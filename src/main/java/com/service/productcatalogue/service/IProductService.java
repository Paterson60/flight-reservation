package com.service.productcatalogue.service;

import com.service.productcatalogue.dto.ProductDto;
import com.service.productcatalogue.entity.Product;

public interface IProductService {

    /**
     *
     * @param productDto - ProductDto Object
     */
    void addProduct(ProductDto productDto);


    /**
     *
     * @param sku - Input sku
     */
    ProductDto fetchProduct(String sku);

    boolean updateProduct(ProductDto productDto);

    boolean deleteProduct(String sku);

}

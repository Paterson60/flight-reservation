package com.service.productcatalogue.service;

import com.service.productcatalogue.dto.*;
import com.service.productcatalogue.entity.Product;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IProductService {

    /**
     *
     * @param addAllProductDetailsDto - AddAllProductDetailsDto Object
     */
    void addProduct(AddAllProductDetailsDto addAllProductDetailsDto);


    /**
     *
     * @param sku - Input sku
     */
    ProductDto fetchProduct(String sku);

    public List<Product> fetchAllProducts();

    /**
     *
     * @param productDto - Input productDto
     */
    boolean updateProduct(ProductDto productDto);

    /**
     *
     * @param sku - Input sku
     */
    boolean deleteProduct(String sku);

    /**
     *
     * @param searchDto - Input searchDto
     */
    List<ProductDto> searchProducts(ProductSearchCriteriaDto searchDto);

    /**
     *
     * @param pageNumber,pageSize,sortProperty - Input pageNumber,pageSize,sortProperty
     */
    Page<Product> getProductPagination(Integer pageNumber, Integer pageSize, String sortProperty);

    /**
     * Fetches the price details by price ID.
     *
     * @param  - productId of the product entity
     * @return the PriceDto object containing price details
     */
    void updatePrice(Long productId, PriceDto priceDto);

}

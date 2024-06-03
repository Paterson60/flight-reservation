package com.service.productcatalogue.service.impl;

import com.service.productcatalogue.dto.ProductAssociationDto;
import com.service.productcatalogue.dto.ProductDto;
import com.service.productcatalogue.entity.Product;
import com.service.productcatalogue.entity.ProductAssociation;
import com.service.productcatalogue.exception.ProductExistsException;
import com.service.productcatalogue.exception.ResourceNotFoundException;
import com.service.productcatalogue.mapper.ProductAssociationMapper;
import com.service.productcatalogue.mapper.ProductMapper;
import com.service.productcatalogue.repository.PriceRepository;
import com.service.productcatalogue.repository.ProductAssociationRepository;
import com.service.productcatalogue.repository.ProductRepository;
import com.service.productcatalogue.service.IProductService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
@AllArgsConstructor
public class ProductServiceImpl implements IProductService {

    private ProductRepository productRepository;

    private PriceRepository priceRepository;

    private ProductAssociationRepository productAssociationRepository;

    @Override
    public void addProduct(ProductDto productDto) {
        Product product = ProductMapper.mapToProduct(productDto, new Product());
        Optional<Product> optionalSku = productRepository.findBySku(productDto.getSku());
        if(optionalSku.isPresent()){
            throw new ProductExistsException("Product Already Exists" + productDto.getSku());
        }
        Product savedProduct = productRepository.save(product);
        productAssociationRepository.save(productAssociation(savedProduct));

    }

    private ProductAssociation productAssociation(Product product){
        ProductAssociation newProductAssociation = new ProductAssociation();
        newProductAssociation.setSku(product.getSku());
        newProductAssociation.setRelatedProducts(product.getCategory());
        newProductAssociation.setBundleDeals(product.getSpecification());
        newProductAssociation.setProductVariations(product.getDescription());
        return newProductAssociation;
    }


    /**
     * @param sku - Input sku
     */
    @Override
    public ProductDto fetchProduct(String sku) {
       Product product = productRepository.findBySku(sku).orElseThrow(
                ()-> new ResourceNotFoundException("Product","Sku", sku)
        );
        ProductAssociation productAssociation = productAssociationRepository.findBySku(product.getSku()).orElseThrow(
                ()-> new ResourceNotFoundException("relatedProduct","Sku", sku)
        );
        ProductDto productDto = ProductMapper.mapToProductDto(product, new ProductDto());
        productDto.setProductAssociationDto(ProductAssociationMapper.mapToProductAssociationDto(productAssociation, new ProductAssociationDto()));
        return productDto;
    }

    @Override
    public boolean updateProduct(ProductDto productDto) {
        boolean isUpdated =  false;
        ProductAssociationDto productAssociationDto = productDto.getProductAssociationDto();
        if(productAssociationDto != null){
            ProductAssociation productAssociation = productAssociationRepository.findBySku(productAssociationDto.getSku()).orElseThrow(
                    ()-> new ResourceNotFoundException("relatedProduct","Sku",productAssociationDto.getSku())
            );

            ProductAssociationMapper.mapToProductAssociation(productAssociationDto, productAssociation);
            productAssociation = productAssociationRepository.save(productAssociation);

            Long productId= productAssociation.getAssociationId();
            Product product = productRepository.findById(productId).orElseThrow(
                    () -> new ResourceNotFoundException("Product","ProductId",productId.toString())
            );
            ProductMapper.mapToProduct(productDto,product);
            productRepository.save(product);
            isUpdated = true;
        }
        return isUpdated;
    }

    @Override
    public boolean deleteProduct(String sku) {
        Product product = productRepository.findBySku(sku).orElseThrow(
                () -> new ResourceNotFoundException("Product","sku",sku)
        );
        productAssociationRepository.deleteBySku(product.getSku());
        productRepository.deleteById(product.getProductId());
        return true;
    }


}

package com.service.productcatalogue.service.impl;

import com.service.productcatalogue.dto.*;
import com.service.productcatalogue.entity.Price;
import com.service.productcatalogue.entity.Product;
import com.service.productcatalogue.entity.ProductAssociation;
import com.service.productcatalogue.exception.ProductExistsException;
import com.service.productcatalogue.exception.ResourceNotFoundException;
import com.service.productcatalogue.mapper.AllProductMapperProductMapper;
import com.service.productcatalogue.mapper.ProductAssociationMapper;
import com.service.productcatalogue.mapper.ProductMapper;
import com.service.productcatalogue.repository.PriceRepository;
import com.service.productcatalogue.repository.ProductAssociationRepository;
import com.service.productcatalogue.repository.ProductRepository;
import com.service.productcatalogue.service.IProductService;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;


import org.springframework.data.domain.Sort;

import java.util.ArrayList;
import java.util.List;

import java.util.Optional;

@Service
@AllArgsConstructor
public class ProductServiceImpl implements IProductService {

    private ProductRepository productRepository;

    private PriceRepository priceRepository;

    private ProductAssociationRepository productAssociationRepository;

    @Override
    public void addProduct(AddAllProductDetailsDto addAllProductDetailsDto) {
        Product product = AllProductMapperProductMapper.mapToProduct(addAllProductDetailsDto, new Product());

        ProductAssociation newProductAssociation = new ProductAssociation();
        newProductAssociation.setSku(addAllProductDetailsDto.getSku());
        newProductAssociation.setRelatedProducts(addAllProductDetailsDto.getRelatedProducts());
        newProductAssociation.setBundleDeals(addAllProductDetailsDto.getBundleDeals());
        newProductAssociation.setProductVariations(addAllProductDetailsDto.getProductVariations());
        productAssociationRepository.save(newProductAssociation);

        Price price = new Price();
        price.setAmount(addAllProductDetailsDto.getAmount());
        price.setDiscount(addAllProductDetailsDto.getDiscount());
        priceRepository.save(price);

        String sku = addAllProductDetailsDto.getSku();
        Optional<Product> optionalSku = productRepository.findBySku(sku);
        if(optionalSku.isPresent()){
            throw new ProductExistsException("Product Already Exists " + addAllProductDetailsDto.getSku());
        }
        product.setProductAssociation(newProductAssociation);
        product.setPrice(price);
        productRepository.save(product);
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
        Price price = priceRepository.findByPriceId(product.getProductId());
        ProductDto productDto = ProductMapper.mapToProductDto(product, new ProductDto());
        productDto.setProductAssociationDto(ProductAssociationMapper.mapToProductAssociationDto(productAssociation, new ProductAssociationDto()));
        productDto.setPriceDto(ProductMapper.mapToPriceDto(price));
        return productDto;
    }

    @Override
    public List<Product> fetchAllProducts() {
        return productRepository.findAll();
    }


    @Override
    public boolean updateProduct(ProductDto productDto) {
        boolean isUpdated =  false;

        String productDtoSku;
        productDtoSku = productDto.getSku();
        if(productDtoSku !=null){
             productRepository.findBySku(productDtoSku).orElseThrow(
                    () -> new ResourceNotFoundException("Product","Sku", productDto.getSku())
            );
        }

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
        productAssociationRepository.deleteById(product.getProductAssociation().getAssociationId());
        productRepository.deleteById(product.getProductId());
        return true;
    }

    @Override
    public List<ProductDto> searchProducts(ProductSearchCriteriaDto searchCriteria) {
        List<Product> products = productRepository.findAll();
        List<ProductDto> matchingProducts = new ArrayList<>();

        for (Product product : products) {
            if (matchesCriteria(product, searchCriteria)) {
                matchingProducts.add(ProductMapper.mapToProductDto(product, new ProductDto()));
            }
        }

        return matchingProducts;
    }

    private boolean matchesCriteria(Product product, ProductSearchCriteriaDto criteria) {
        if (criteria.getName() != null && !product.getName().equalsIgnoreCase(criteria.getName())) {
            return false;
        }
        if (criteria.getCategory() != null && !product.getCategory().equalsIgnoreCase(criteria.getCategory())) {
            return false;
        }
        if (criteria.getDescription() != null && !product.getDescription().equalsIgnoreCase(criteria.getDescription())) {
            return false;
        }
        if (criteria.getImage() != null && !product.getImage().equalsIgnoreCase(criteria.getImage())) {
            return false;
        }
        if (criteria.getSpecification() != null && !product.getSpecification().equalsIgnoreCase(criteria.getSpecification())) {
            return false;
        }
        if (criteria.getSku() != null && !product.getSku().equalsIgnoreCase(criteria.getSku())) {
            return false;
        }
        if (criteria.getBrand() != null && !product.getBrand().equalsIgnoreCase(criteria.getBrand())) {
            return false;
        }
        if (criteria.getAmount() != null && !(product.getPrice().getAmount() < (criteria.getAmount()))) {
            return false;
        }
        return true;
    }

    @Override
    public Page<Product> getProductPagination(Integer pageNumber, Integer pageSize, String sortProperty) {
        Pageable pageable = null;
        if(null != sortProperty){
            pageable = PageRequest.of(pageNumber, pageSize, Sort.Direction.ASC, sortProperty);
        }else {
            pageable = PageRequest.of(pageNumber, pageSize, Sort.Direction.ASC, "name");
        }
        return productRepository.findAll(pageable);
    }

    /**
     * Fetches the price details by price ID.
     *
     * @param productId
     * @param priceDto
     * @return the PriceDto object containing price details
     */
    @Override
    public void updatePrice(Long productId, PriceDto priceDto) {
        Product product = productRepository.findById(productId).orElseThrow(
                () -> new ResourceNotFoundException("Product","productId",productId.toString())
        );
        Price price = product.getPrice();
        price.setAmount(priceDto.getAmount());
        price.setDiscount(priceDto.getDiscount());
        priceRepository.save(price);
    }
}

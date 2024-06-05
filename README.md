# flight-reservation

dto
package com.service.productcatalogue.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@Schema(name = "SearchCriteria", description = "Schema to hold search criteria for products")
public class SearchCriteriaDto {
    @Schema(description = "Name of the Product", example = "Airpod")
    private String name;

    @Schema(description = "Category of the Product", example = "Headphone")
    private String category;

    @Schema(description = "Minimum Price of the Product", example = "500")
    private Double minPrice;

    @Schema(description = "Maximum Price of the Product", example = "1500")
    private Double maxPrice;

    @Schema(description = "Sort by field", example = "name")
    private String sortBy;

    @Schema(description = "Sort direction", example = "asc")
    private String sortDirection;
}


Repository 

package com.service.productcatalogue.repository;

import com.service.productcatalogue.entity.Product;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

@Repository
public interface ProductRepository extends JpaRepository<Product, Long>, JpaSpecificationExecutor<Product> {
    Optional<Product> findBySku(String sku);
}

service layer

package com.service.productcatalogue.service.impl;

import com.service.productcatalogue.dto.ProductDto;
import com.service.productcatalogue.dto.SearchCriteriaDto;
import com.service.productcatalogue.entity.Product;
import com.service.productcatalogue.mapper.ProductMapper;
import com.service.productcatalogue.repository.ProductRepository;
import com.service.productcatalogue.service.IProductService;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
@AllArgsConstructor
public class ProductServiceImpl implements IProductService {

    private final ProductRepository productRepository;

    @Override
    public void addProduct(ProductDto productDto) {
        Product product = ProductMapper.mapToProduct(productDto, new Product());
        Optional<Product> optionalSku = productRepository.findBySku(productDto.getSku());
        if(optionalSku.isPresent()){
            throw new ProductExistsException("Product Already Exists" + productDto.getSku());
        }
        productRepository.save(product);
    }

    @Override
    public ProductDto fetchProduct(String sku) {
        Product product = productRepository.findBySku(sku).orElseThrow(
                () -> new ResourceNotFoundException("Product", "Sku", sku)
        );
        return ProductMapper.mapToProductDto(product, new ProductDto());
    }

    @Override
    public boolean updateProduct(ProductDto productDto) {
        Product product = productRepository.findBySku(productDto.getSku()).orElseThrow(
                () -> new ResourceNotFoundException("Product", "Sku", productDto.getSku())
        );
        ProductMapper.mapToProduct(productDto, product);
        productRepository.save(product);
        return true;
    }

    @Override
    public boolean deleteProduct(String sku) {
        Product product = productRepository.findBySku(sku).orElseThrow(
                () -> new ResourceNotFoundException("Product", "Sku", sku)
        );
        productRepository.delete(product);
        return true;
    }

    @Override
    public List<ProductDto> searchProducts(SearchCriteriaDto searchCriteriaDto) {
        Specification<Product> spec = Specification.where(null);

        if (searchCriteriaDto.getName() != null && !searchCriteriaDto.getName().isEmpty()) {
            spec = spec.and((root, query, criteriaBuilder) ->
                    criteriaBuilder.like(root.get("name"), "%" + searchCriteriaDto.getName() + "%"));
        }

        if (searchCriteriaDto.getCategory() != null && !searchCriteriaDto.getCategory().isEmpty()) {
            spec = spec.and((root, query, criteriaBuilder) ->
                    criteriaBuilder.equal(root.get("category"), searchCriteriaDto.getCategory()));
        }

        if (searchCriteriaDto.getMinPrice() != null) {
            spec = spec.and((root, query, criteriaBuilder) ->
                    criteriaBuilder.greaterThanOrEqualTo(root.get("price").get("amount"), searchCriteriaDto.getMinPrice()));
        }

        if (searchCriteriaDto.getMaxPrice() != null) {
            spec = spec.and((root, query, criteriaBuilder) ->
                    criteriaBuilder.lessThanOrEqualTo(root.get("price").get("amount"), searchCriteriaDto.getMaxPrice()));
        }

        Sort sort = Sort.unsorted();
        if (searchCriteriaDto.getSortBy() != null && searchCriteriaDto.getSortDirection() != null) {
            sort = Sort.by(Sort.Direction.fromString(searchCriteriaDto.getSortDirection()), searchCriteriaDto.getSortBy());
        }

        List<Product> products = productRepository.findAll(spec, sort);
        return products.stream()
                .map(product -> ProductMapper.mapToProductDto(product, new ProductDto()))
                .collect(Collectors.toList());
    }
}



controller layer
package com.service.productcatalogue.controller;

import com.service.productcatalogue.dto.ProductDto;
import com.service.productcatalogue.dto.ResponseDto;
import com.service.productcatalogue.dto.SearchCriteriaDto;
import com.service.productcatalogue.service.IProductService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotEmpty;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(
        name = "CRUD Rest APIs For Product In Product Catalogue Service",
        description = "CRUD Rest APIs in Product"
)
@RestController
@RequestMapping(path = "/api", produces = {MediaType.APPLICATION_JSON_VALUE})
@AllArgsConstructor
@Validated
public class ProductController {

    private final IProductService iProductService;

    @Operation(
            summary = "Add Product Rest API",
            description = "Rest API to add a new Product in Product Catalogue Service"
    )
    @ApiResponse(
            responseCode = "201",
            description = "HTTP Status Product Added"
    )
    @PostMapping("/addProduct")
    public ResponseEntity<ResponseDto> addProduct(@Valid @RequestBody ProductDto productDto) {
        iProductService.addProduct(productDto);
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(new ResponseDto("201", "Product Created Successfully"));
    }

    @Operation(
            summary = "Fetch Product Details Rest API",
            description = "Rest API to fetch Product Details"
    )
    @ApiResponse(
            responseCode = "200",
            description = "HTTP Status Ok"
    )
    @GetMapping("/fetchProduct")
    public ResponseEntity<ProductDto> fetchProductDetails(@RequestParam @NotEmpty(message = "SKU cannot be empty") String sku) {
        ProductDto productDto = iProductService.fetchProduct(sku);
        return ResponseEntity.status(HttpStatus.OK).body(productDto);
    }

    @Operation(
            summary = "Update Product Details Rest API",
            description = "Rest API to update Product Details"
    )
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "HTTP Status OK"),
            @ApiResponse(responseCode = "417", description = "Expectation Failed"),
            @ApiResponse(responseCode = "500", description = "HTTP Status Internal Server Error")
    })
    @PutMapping("/updateProduct")
    public ResponseEntity<ResponseDto> updateProduct(@Valid @RequestBody ProductDto productDto) {
        boolean isUpdated = iProductService.updateProduct(productDto);
        if (isUpdated) {
            return ResponseEntity
                    .status(HttpStatus.OK)
                    .body(new ResponseDto("200", "Product Updated Successfully"));
        } else {
            return ResponseEntity
                    .status(HttpStatus.EXPECTATION_FAILED)
                    .body(new ResponseDto("417", "Product Update Failed"));
        }
    }

    @Operation(
            summary = "Delete Product Rest API",
            description = "Rest API to Delete a Product"
    )
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "HTTP Status OK"),
            @ApiResponse(responseCode = "417", description = "Expectation Failed"),
            @ApiResponse(responseCode = "500", description = "HTTP Status Internal Server Error")
    })
    @DeleteMapping("/deleteProduct")
    public ResponseEntity<ResponseDto> deleteProduct(@RequestParam @NotEmpty(message = "SKU cannot be empty") String sku) {
        boolean isDeleted = iProductService.deleteProduct(sku);
        if (isDeleted) {
            return ResponseEntity
                    .status(HttpStatus.OK)
                    .body(new ResponseDto("200", "Product Deleted Successfully"));
        } else {
            return ResponseEntity
                    .status(HttpStatus.EXPECTATION_FAILED)
                    .body(new ResponseDto("417", "Product Deletion Failed"));
        }
    }

    @Operation(
            summary = "Search Products Rest API",
            description = "Rest API to search products with filters and sorting"
    )
    @ApiResponse(
            responseCode = "200",
            description = "HTTP Status OK"
    )
    @PostMapping("/searchProducts")
    public ResponseEntity<List<ProductDto>> searchProducts(@Valid @RequestBody SearchCriteriaDto searchCriteriaDto) {
        List<ProductDto> products = iProductService.searchProducts(searchCriteriaDto);
        return ResponseEntity.status(HttpStatus.OK).body(products);
    }
}


Json
{
  "name": "Airpod",
  "category": "Headphone",
  "minPrice": 500,
  "maxPrice": 1500,
  "sortBy": "name",
  "sortDirection": "asc"
}


Entity Price

package com.service.productcatalogue.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import lombok.*;

@Entity
@Getter@Setter@ToString@AllArgsConstructor@NoArgsConstructor
public class Price {
@Id
@GeneratedValue(strategy = GenerationType.IDENTITY)
private Long priceId;
private Long amount;

repository

package com.service.productcatalogue.repository;

import com.service.productcatalogue.entity.Price;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface PriceRepository extends JpaRepository<Price,Long> {
}

iproductservice

package com.service.productcatalogue.service;

import com.service.productcatalogue.dto.*;
import com.service.productcatalogue.entity.Product;

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

    boolean updateProduct(ProductDto productDto);

    boolean deleteProduct(String sku);

    List<ProductDto> searchProducts(ProductSearchCriteriaDto searchDto);

    //List<ProductDto> searchProducts(String name,String category,Double minPrice,Double maxPrice);

}

ServiceImpl
package com.service.productcatalogue.service.impl;

import com.service.productcatalogue.dto.AddAllProductDetailsDto;
import com.service.productcatalogue.dto.ProductAssociationDto;
import com.service.productcatalogue.dto.ProductDto;
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
import org.springframework.stereotype.Service;


import com.service.productcatalogue.dto.ProductSearchCriteriaDto;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

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
        priceRepository.save(price);

        String sku = addAllProductDetailsDto.getSku();
        Optional<Product> optionalSku = productRepository.findBySku(sku);
        if(optionalSku.isPresent()){
            throw new ProductExistsException("Product Already Exists " + addAllProductDetailsDto.getSku());
        }
        product.setProductAssociation(newProductAssociation);
        product.setPrice(price);
        productRepository.save(product);
//        productAssociationRepository.save(productAssociation(savedProduct));

    }

//    private ProductAssociation productAssociation(Product product){
//        ProductAssociation newProductAssociation = new ProductAssociation();
//        newProductAssociation.setSku(product.getSku());
//        newProductAssociation.setRelatedProducts(product.getCategory());
//        newProductAssociation.setBundleDeals(product.getSpecification());
//        newProductAssociation.setProductVariations(product.getDescription());
//        return newProductAssociation;
//    }

//    private Price price(Product product){
//        Price newPrice = new Price();
//        newPrice.setAmount(product.getA);
//    }


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
    public List<Product> fetchAllProducts() {
        return productRepository.findAll();
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
        productAssociationRepository.deleteById(product.getProductAssociation().getAssociationId());
        productRepository.deleteById(product.getProductId());
        return true;
    }

    @Override
    public List<ProductDto> searchProducts(ProductSearchCriteriaDto productSearchCriteriaDto) {
        Specification<Product> spec = Specification.where(null);

        if (productSearchCriteriaDto.getName() != null && !productSearchCriteriaDto.getName().isEmpty()) {
            spec = spec.and((root, query, criteriaBuilder) ->
                    criteriaBuilder.like(root.get("name"), "%" + productSearchCriteriaDto.getName() + "%"));
        }

        if (productSearchCriteriaDto.getCategory() != null && !productSearchCriteriaDto.getCategory().isEmpty()) {
            spec = spec.and((root, query, criteriaBuilder) ->
                    criteriaBuilder.equal(root.get("category"), productSearchCriteriaDto.getCategory()));
        }

        if (productSearchCriteriaDto.getMinPrice() != null) {
            spec = spec.and((root, query, criteriaBuilder) ->
                    criteriaBuilder.greaterThanOrEqualTo(root.get("price").get("amount"), productSearchCriteriaDto.getMinPrice()));
        }

        if (productSearchCriteriaDto.getMaxPrice() != null) {
            spec = spec.and((root, query, criteriaBuilder) ->
                    criteriaBuilder.lessThanOrEqualTo(root.get("price").get("amount"), productSearchCriteriaDto.getMaxPrice()));
        }

        Sort sort = Sort.unsorted();
        if (productSearchCriteriaDto.getSortBy() != null && productSearchCriteriaDto.getSortDirection() != null) {
            sort = Sort.by(Sort.Direction.fromString(productSearchCriteriaDto.getSortDirection()), productSearchCriteriaDto.getSortBy());
        }

        List<Product> products = productRepository.findAll(spec, sort);
        return products.stream()
                .map(product -> ProductMapper.mapToProductDto(product, new ProductDto()))
                .collect(Collectors.toList());
    }

//    @Override
//    public List<ProductDto> searchProducts(String name,String category,Double minPrice,Double maxPrice) {
//        List<Product> products = productRepository.findAll();
//        List<ProductDto> matchingProducts = new ArrayList<>();
//
//        for (Product product : products) {
//            if (matchesCriteria(product, new ProductSearchCriteriaDto())) {
//                matchingProducts.add(ProductMapper.mapToProductDto(product, new ProductDto()));
//            }
//        }
//
//        return matchingProducts;
//    }
//
//    private boolean matchesCriteria(Product product, ProductSearchCriteriaDto searchCriteriaDto) {
//        if (searchCriteriaDto.getName() != null && !product.getName().equalsIgnoreCase(searchCriteriaDto.getName())) {
//            return false;
//        }
//
//        if (searchCriteriaDto.getCategory() != null && !product.getCategory().equalsIgnoreCase(searchCriteriaDto.getCategory())) {
//            return false;
//        }
//
//        if (searchCriteriaDto.getMinPrice() != null && product.getPrice().getAmount() < searchCriteriaDto.getMinPrice()) {
//            return false;
//        }
//
//        if (searchCriteriaDto.getMaxPrice() != null && product.getPrice().getAmount() > searchCriteriaDto.getMaxPrice()) {
//            return false;
//        }
//
//        return true;
//    }


}

DTO

package com.service.productcatalogue.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;

@Data
@Schema(
        name = "Product Pricing",
        description = "Schema to holds the details of Associated Product Prices"
)
public class PriceDto {

    @NotEmpty(message = "Amount cannot be empty or null")
    @Schema(
            description = "Holds the details of Associated each Product price"
    )
    private Long amount;
}

Controller

package com.service.productcatalogue.controller;

import com.service.productcatalogue.constants.ProductConstants;
import com.service.productcatalogue.dto.*;
import com.service.productcatalogue.entity.Product;
import com.service.productcatalogue.service.IProductService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotEmpty;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

@Tag(
        name = "CRUD Rest APIs For Product In Product Catalogue Service",
        description = "CRUD Rest APIs in Product"
)
@RestController
@RequestMapping(path="/api", produces = {MediaType.APPLICATION_JSON_VALUE})
@AllArgsConstructor
@Validated
public class ProductController {

    private IProductService iProductService;

    @Operation(
            summary = "Add Product Rest API",
            description = "Rest API to add a new Product in Product Catalogue Service"
    )
    @ApiResponse(
            responseCode = "201",
            description = "HTTP Status Product Added"
    )
    @PostMapping("/addProduct")
    public ResponseEntity<ResponseDto> addProduct(@Valid @RequestBody AddAllProductDetailsDto addAllProductDetailsDto){
        iProductService.addProduct(addAllProductDetailsDto);
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(new ResponseDto(ProductConstants.STATUS_201,ProductConstants.MESSAGE_201));
    }

    @Operation(
            summary = "Fetch Product Details Rest API",
            description = "Rest API to fetch  Product Details"
    )
    @ApiResponse(
            responseCode = "200",
            description = "HTTP Status Ok"
    )
    @GetMapping("/fetchProduct")
    public ResponseEntity<ProductDto> fetchProductDetails(@RequestParam
                                                              @NotEmpty(message = "SKU cannot be empty")
                                                              String sku){
        ProductDto productDto = iProductService.fetchProduct(sku);
        return ResponseEntity.status(HttpStatus.OK).body(productDto);
    }

    @Operation(
            summary = "Fetch All Product Details Rest API",
            description = "Rest API to fetch All Product Details"
    )
    @ApiResponse(
            responseCode = "200",
            description = "HTTP Status Ok"
    )
    @GetMapping("/fetchAllProducts")
    public List<Product> fetchAllProducts(){
        return iProductService.fetchAllProducts();
    }

    @Operation(
            summary = "Update Product Details Rest API",
            description = "Rest API to update Product Details"
    )
    @ApiResponses({
            @ApiResponse(
                    responseCode = "200",
                    description = "HTTP Status OK"
            ),
            @ApiResponse(
                    responseCode = "417",
                    description = "Expectation Failed"
            ),
            @ApiResponse(
                    responseCode = "500",
                    description = "HTTP Status Internal Server Error",
                    content = @Content(
                            schema = @Schema(
                                    implementation = ErrorResponseDto.class
                            )
                    )
            )
    })
    @PutMapping("/updateProduct")
    public ResponseEntity<ResponseDto> updateProduct(@Valid @RequestBody ProductDto productDto){
        boolean isUpdated = iProductService.updateProduct(productDto);
        if(isUpdated){
            return ResponseEntity
                    .status(HttpStatus.OK)
                    .body(new ResponseDto(ProductConstants.STATUS_200, ProductConstants.MESSAGE_200));
        }else{
            return ResponseEntity
                    .status(HttpStatus.EXPECTATION_FAILED)
                    .body(new ResponseDto(ProductConstants.STATUS_417,ProductConstants.MESSAGE_417_UPDATE));
        }
    }

    @Operation(
            summary = "Delete Product Rest API",
            description = "Rest API to Delete a Product"
    )
    @ApiResponses({
            @ApiResponse(
                    responseCode = "200",
                    description = "HTTP Status OK"
            ),
            @ApiResponse(
                    responseCode = "417",
                    description = "Expectation Failed"
            ),
            @ApiResponse(
                    responseCode = "500",
                    description = "HTTP Status Internal Server Error"
            )
    })
    @DeleteMapping("/deleteProduct")
    public ResponseEntity<ResponseDto> deleteProduct(@RequestParam
                                                         @NotEmpty(message = "SKU cannot be empty")
                                                         String sku){
        boolean isDeleted = iProductService.deleteProduct(sku);
        if (isDeleted){
            return ResponseEntity
                    .status(HttpStatus.OK)
                    .body(new ResponseDto(ProductConstants.STATUS_200, ProductConstants.MESSAGE_200));
        }else{
            return ResponseEntity
                    .status(HttpStatus.EXPECTATION_FAILED)
                    .body(new ResponseDto(ProductConstants.STATUS_417, ProductConstants.MESSAGE_417_DELETE));
        }
    }

    @PostMapping("/searchProducts")
    public ResponseEntity<List<ProductDto>> searchProducts(@Valid @RequestBody ProductSearchCriteriaDto productSearchCriteriaDto) {
        List<ProductDto> products = iProductService.searchProducts(productSearchCriteriaDto);
        return ResponseEntity.status(HttpStatus.OK).body(products);
    }


//    @GetMapping("/searchProducts")
//    public ResponseEntity<List<ProductDto>> searchProducts(
//            @RequestParam(required = false) String name,
//            @RequestParam(required = false) String category,
//            @RequestParam(required = false) Double minPrice,
//            @RequestParam(required = false) Double maxPrice) {
//
//        List<ProductDto> products = iProductService.searchProducts(name, category, minPrice, maxPrice);
//        return ResponseEntity.status(HttpStatus.OK).body(products);
//    }


}

Search

 @PostMapping("/searchProducts")
    public ResponseEntity<List<ProductDto>> searchProducts(@Valid @RequestBody ProductSearchCriteriaDto searchCriteria) {
        List<ProductDto> matchingProducts = iProductService.searchProducts(searchCriteria);
        if(matchingProducts.isEmpty()){
            ResponseEntity<List<ProductDto>> response = ResponseEntity
                    .status(HttpStatus.EXPECTATION_FAILED).body((List<ProductDto>) new ResponseDto(ProductConstants.STATUS_417, ProductConstants.MESSAGE_417_DELETE));
            return response;

        }else{
            return ResponseEntity.status(HttpStatus.OK).body(matchingProducts);
        }

    }

    resposne dto

    package com.service.productcatalogue.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;

@Data@AllArgsConstructor
@Schema(
        name = "Response",
        description = "Schema to hold successful response information"
)
public class ResponseDto {

    @Schema(
            description = "Status code in the response"
    )
    private String statusCode;

    @Schema(
            description = "Status code in the response"
    )
    private String statusMsg;
}

error message 
"class com.service.productcatalogue.dto.ResponseDto cannot be cast to class java.util.List (com.service.productcatalogue.dto.ResponseDto is in unnamed module of loader 'app'; java.util.List is in module java.base of loader 'bootstrap')",

error message1
Resolved [org.springframework.http.converter.HttpMessageNotReadableException: JSON parse error: Cannot construct instance of `com.service.productcatalogue.dto.PriceDto` (although at least one Creator exists): no String-argument constructor/factory method to deserialize from String value ('amount')]


junitproductrepo

package com.service.productcatalogue.repository;

import com.service.productcatalogue.entity.Price;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

@DataJpaTest
class PriceRepositoryTest {

    @Autowired
    private PriceRepository priceRepository;

    @Test
    void testSaveAndFindById() {
        Price price = new Price();
        price.setAmount(1200.00);
        price.setDiscount(200.00);
        Price savedPrice = priceRepository.save(price);

        Optional<Price> retrievedPrice = priceRepository.findById(savedPrice.getPriceId());
        assertThat(retrievedPrice).isPresent();
        assertThat(retrievedPrice.get().getAmount()).isEqualTo(1200.00);
        assertThat(retrievedPrice.get().getDiscount()).isEqualTo(200.00);
    }

    @Test
    void testDeleteById() {
        Price price = new Price();
        price.setAmount(1200.00);
        price.setDiscount(200.00);
        Price savedPrice = priceRepository.save(price);

        priceRepository.deleteById(savedPrice.getPriceId());
        Optional<Price> retrievedPrice = priceRepository.findById(savedPrice.getPriceId());
        assertThat(retrievedPrice).isNotPresent();
    }
}

junitproductassociaterepo

package com.service.productcatalogue.repository;

import com.service.productcatalogue.entity.ProductAssociation;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

@DataJpaTest
class ProductAssociationRepositoryTest {

    @Autowired
    private ProductAssociationRepository productAssociationRepository;

    @Test
    void testSaveAndFindById() {
        ProductAssociation productAssociation = new ProductAssociation();
        productAssociation.setSku("AZVP1!");
        productAssociation.setRelatedProducts("Updated Related Products");
        productAssociation.setBundleDeals("Updated Bundle Deals");
        productAssociation.setProductVariations("Updated Product Variations");
        ProductAssociation savedProductAssociation = productAssociationRepository.save(productAssociation);

        Optional<ProductAssociation> retrievedProductAssociation = productAssociationRepository.findById(savedProductAssociation.getSku());
        assertThat(retrievedProductAssociation).isPresent();
        assertThat(retrievedProductAssociation.get().getRelatedProducts()).isEqualTo("Updated Related Products");
    }

    @Test
    void testDeleteById() {
        ProductAssociation productAssociation = new ProductAssociation();
        productAssociation.setSku("AZVP1!");
        productAssociation.setRelatedProducts("Updated Related Products");
        productAssociation.setBundleDeals("Updated Bundle Deals");
        productAssociation.setProductVariations("Updated Product Variations");
        ProductAssociation savedProductAssociation = productAssociationRepository.save(productAssociation);

        productAssociationRepository.deleteById(savedProductAssociation.getSku());
        Optional<ProductAssociation> retrievedProductAssociation = productAssociationRepository.findById(savedProductAssociation.getSku());
        assertThat(retrievedProductAssociation).isNotPresent();
    }
}

junitproductreopo

package com.service.productcatalogue.repository;

import com.service.productcatalogue.entity.Price;
import com.service.productcatalogue.entity.Product;
import com.service.productcatalogue.entity.ProductAssociation;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

@DataJpaTest
class ProductRepositoryTest {

    @Autowired
    private ProductRepository productRepository;

    @Autowired
    private PriceRepository priceRepository;

    @Autowired
    private ProductAssociationRepository productAssociationRepository;

    @Test
    void testSaveAndFindById() {
        Price price = new Price();
        price.setAmount(1200.00);
        price.setDiscount(200.00);
        Price savedPrice = priceRepository.save(price);

        ProductAssociation productAssociation = new ProductAssociation();
        productAssociation.setSku("AZVP1!");
        productAssociation.setRelatedProducts("Updated Related Products");
        productAssociation.setBundleDeals("Updated Bundle Deals");
        productAssociation.setProductVariations("Updated Product Variations");
        ProductAssociation savedProductAssociation = productAssociationRepository.save(productAssociation);

        Product product = new Product();
        product.setName("Updated Airpod");
        product.setCategory("Headphone");
        product.setDescription("Updated Wireless Bluetooth Device");
        product.setImage("updated_image_url");
        product.setSpecification("Updated Airpod Gen 2.0");
        product.setSku("AZVP1!");
        product.setPrice(savedPrice);
        product.setProductAssociation(savedProductAssociation);
        Product savedProduct = productRepository.save(product);

        Optional<Product> retrievedProduct = productRepository.findById(savedProduct.getProductId());
        assertThat(retrievedProduct).isPresent();
        assertThat(retrievedProduct.get().getName()).isEqualTo("Updated Airpod");
    }

    @Test
    void testDeleteById() {
        Price price = new Price();
        price.setAmount(1200.00);
        price.setDiscount(200.00);
        Price savedPrice = priceRepository.save(price);

        ProductAssociation productAssociation = new ProductAssociation();
        productAssociation.setSku("AZVP1!");
        productAssociation.setRelatedProducts("Updated Related Products");
        productAssociation.setBundleDeals("Updated Bundle Deals");
        productAssociation.setProductVariations("Updated Product Variations");
        ProductAssociation savedProductAssociation = productAssociationRepository.save(productAssociation);

        Product product = new Product();
        product.setName("Updated Airpod");
        product.setCategory("Headphone");
        product.setDescription("Updated Wireless Bluetooth Device");
        product.setImage("updated_image_url");
        product.setSpecification("Updated Airpod Gen 2.0");
        product.setSku("AZVP1!");
        product.setPrice(savedPrice);
        product.setProductAssociation(savedProductAssociation);
        Product savedProduct = productRepository.save(product);

        productRepository.deleteById(savedProduct.getProductId());
        Optional<Product> retrievedProduct = productRepository.findById(savedProduct.getProductId());
        assertThat(retrievedProduct).isNotPresent();
    }
}


Iproductservicejunit

package com.service.productcatalogue.service;

import com.service.productcatalogue.dto.*;
import com.service.productcatalogue.entity.Product;
import com.service.productcatalogue.repository.ProductRepository;
import com.service.productcatalogue.repository.PriceRepository;
import com.service.productcatalogue.repository.ProductAssociationRepository;
import com.service.productcatalogue.mapper.ProductMapper;
import com.service.productcatalogue.mapper.ProductAssociationMapper;
import com.service.productcatalogue.exception.ProductExistsException;
import com.service.productcatalogue.exception.ResourceNotFoundException;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(SpringExtension.class)
public class IProductServiceTest {

    @Mock
    private ProductRepository productRepository;

    @Mock
    private PriceRepository priceRepository;

    @Mock
    private ProductAssociationRepository productAssociationRepository;

    @InjectMocks
    private ProductServiceImpl productService;

    private Product product;
    private ProductDto productDto;
    private PriceDto priceDto;
    private ProductAssociationDto productAssociationDto;
    private AddAllProductDetailsDto addAllProductDetailsDto;

    @BeforeEach
    public void setUp() {
        product = new Product();
        product.setProductId(1L);
        product.setSku("SKU123");

        productDto = new ProductDto();
        productDto.setSku("SKU123");

        priceDto = new PriceDto();
        priceDto.setAmount(1200L);

        productAssociationDto = new ProductAssociationDto();
        productAssociationDto.setSku("SKU123");

        addAllProductDetailsDto = new AddAllProductDetailsDto();
        addAllProductDetailsDto.setProductDto(productDto);
        addAllProductDetailsDto.setPriceDto(priceDto);
        addAllProductDetailsDto.setProductAssociationDto(productAssociationDto);
    }

    @Test
    public void testAddProduct() {
        when(productRepository.findBySku(productDto.getSku())).thenReturn(Optional.empty());
        when(productRepository.save(any(Product.class))).thenReturn(product);

        assertDoesNotThrow(() -> productService.addProduct(addAllProductDetailsDto));

        verify(productRepository, times(1)).save(any(Product.class));
        verify(productAssociationRepository, times(1)).save(any());
    }

    @Test
    public void testFetchProduct() {
        when(productRepository.findBySku("SKU123")).thenReturn(Optional.of(product));

        ProductDto fetchedProduct = productService.fetchProduct("SKU123");

        assertNotNull(fetchedProduct);
        assertEquals("SKU123", fetchedProduct.getSku());
    }

    @Test
    public void testFetchAllProducts() {
        List<Product> products = new ArrayList<>();
        products.add(product);

        when(productRepository.findAll()).thenReturn(products);

        List<Product> fetchedProducts = productService.fetchAllProducts();

        assertEquals(1, fetchedProducts.size());
        assertEquals("SKU123", fetchedProducts.get(0).getSku());
    }

    @Test
    public void testUpdateProduct() {
        when(productRepository.findBySku(productDto.getSku())).thenReturn(Optional.of(product));
        when(productRepository.save(any(Product.class))).thenReturn(product);

        boolean isUpdated = productService.updateProduct(productDto);

        assertTrue(isUpdated);
    }

    @Test
    public void testDeleteProduct() {
        when(productRepository.findBySku("SKU123")).thenReturn(Optional.of(product));

        boolean isDeleted = productService.deleteProduct("SKU123");

        assertTrue(isDeleted);
        verify(productRepository, times(1)).deleteById(1L);
    }

    @Test
    public void testSearchProducts() {
        List<ProductDto> productDtos = new ArrayList<>();
        productDtos.add(productDto);

        when(productRepository.findAll()).thenReturn(List.of(product));

        List<ProductDto> foundProducts = productService.searchProducts(new ProductSearchCriteriaDto());

        assertEquals(1, foundProducts.size());
        assertEquals("SKU123", foundProducts.get(0).getSku());
    }

    @Test
    public void testGetProductPagination() {
        List<Product> products = new ArrayList<>();
        products.add(product);

        Page<Product> productPage = new PageImpl<>(products);
        Pageable pageable = PageRequest.of(0, 10, Sort.by("name"));

        when(productRepository.findAll(pageable)).thenReturn(productPage);

        Page<Product> paginatedProducts = productService.getProductPagination(0, 10, "name");

        assertEquals(1, paginatedProducts.getTotalElements());
    }

    @Test
    public void testUpdatePrice() {
        when(productRepository.findById(1L)).thenReturn(Optional.of(product));
        when(priceRepository.findById(product.getPrice().getPriceId())).thenReturn(Optional.of(product.getPrice()));
        when(priceRepository.save(any())).thenReturn(product.getPrice());

        assertDoesNotThrow(() -> productService.updatePrice(1L, priceDto));

        verify(priceRepository, times(1)).save(any());
    }
}

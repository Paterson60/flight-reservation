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

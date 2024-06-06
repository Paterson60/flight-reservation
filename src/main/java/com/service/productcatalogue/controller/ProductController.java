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
import org.springframework.data.domain.Page;
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

    @Operation(
            summary = "Search Products",
            description = "Search for products based on various criteria"
    )
    @ApiResponse(
            responseCode = "200",
            description = "Successfully retrieved matching products"
    )
    @PostMapping("/searchProducts")
    public ResponseEntity<?> searchProducts(@Valid @RequestBody ProductSearchCriteriaDto searchCriteria) {
        List<ProductDto> matchingProducts = iProductService.searchProducts(searchCriteria);
        if(matchingProducts.isEmpty()){
            ResponseDto responseDto = new ResponseDto(ProductConstants.STATUS_500,ProductConstants.MESSAGE_500);
             return ResponseEntity
                    .status(HttpStatus.EXPECTATION_FAILED).body(responseDto);
        }else{
            return ResponseEntity.status(HttpStatus.OK).body(matchingProducts);
        }

    }

    @Operation(
            summary = "Search Products by Name",
            description = "Search for products based on name"
    )
    @ApiResponse(
            responseCode = "200",
            description = "Successfully retrieved matching products"
    )
    @RequestMapping(value = "/paginAndSortingProducts/{pageNumber}/{pageSize}/{sortProperty}", method = RequestMethod.GET)
    public Page<Product> productPagination(@PathVariable Integer pageNumber,
                                           @PathVariable Integer pageSize,
                                           @PathVariable String sortProperty){
        return iProductService.getProductPagination(pageNumber, pageSize, sortProperty);
    }

    @Operation(
            summary = "Update Price Rest API",
            description = "Rest API to update Product price"
    )
    @ApiResponse(
            responseCode = "200",
            description = "Http Status price updated"
    )
    @PutMapping("/updatePrice")
    public ResponseEntity<ResponseDto> updatePrice(@RequestParam Long productId,
                                                   @RequestBody PriceDto priceDto){
        iProductService.updatePrice(productId,priceDto);
        return ResponseEntity.status(HttpStatus.OK).body(new ResponseDto(ProductConstants.STATUS_200,ProductConstants.MESSAGE_200));
    }
}

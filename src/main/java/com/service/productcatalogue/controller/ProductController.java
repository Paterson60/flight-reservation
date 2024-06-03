package com.service.productcatalogue.controller;

import com.service.productcatalogue.constants.ProductConstants;
import com.service.productcatalogue.dto.ErrorResponseDto;
import com.service.productcatalogue.dto.ProductDto;
import com.service.productcatalogue.dto.ResponseDto;
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
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
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
    public ResponseEntity<ResponseDto> addProduct(@Valid @RequestBody ProductDto productDto){
        iProductService.addProduct(productDto);
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
}

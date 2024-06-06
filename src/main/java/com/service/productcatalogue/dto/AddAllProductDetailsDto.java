package com.service.productcatalogue.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Size;
import lombok.Data;


@Data
@Schema(
        name = "All Product Details",
        description = "Schema to hold Product details"
)
public class AddAllProductDetailsDto {

    @Schema(
            description = "Name of the Product", example = "Airpod"
    )
    @NotEmpty(message = "Name cannot be null")
    @Size(min=3,max = 100,message = "The length of the Product name should be between 3 to 100 ")
    private String name;

    @Schema(
            description = "Category of the Product", example = "Headphone"
    )
    private String category;

    @Schema(
            description = "Description of the Product", example = "Wireless Bluetooth Device"
    )
    @Size(min=3,max = 300,message = "The length of the Description should be between 3 to 300 ")
    private String description;

    @Schema(
            description = "Image of the Product"
    )
    private String image;

    @Schema(
            description = "Specification of the Product", example = "Airpod Gen 2.0"
    )
    private String specification;

    @Schema(
            description = "Unique representation of the Product", example = "AZVP1!"
    )
    @NotEmpty(message = "SKU cannot be empty")
    @Size(min=1,max = 6,message = "The length of the SKU should be between 1 to 5 ")
    private String sku;

    @Schema(
            description = "Unique representation of the Brand Name", example = "Apple"
    )
    @NotEmpty(message = "Brand cannot be empty")
    @Size(min=3,max = 100,message = "The length of the SKU should be between 3 to 100 ")
    private String brand;

    @Schema(
            description = "Product details having related products"
    )
    private String relatedProducts;

    @Schema(
            description = "Products which are bundled together"
    )
    private String bundleDeals;

    @Schema(
            description = "Shows multiple variations of the Product"
    )
    private String productVariations;


    @Schema(
            description = "Price of the Product", example = "1000"
    )
    private Long amount;

    @Schema(
            description = "discount price of the Product", example = "200"
    )
    private Long discount;

}

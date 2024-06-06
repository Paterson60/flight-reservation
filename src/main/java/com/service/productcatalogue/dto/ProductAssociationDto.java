package com.service.productcatalogue.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Size;
import lombok.Data;

@Data
@Schema(
        name = "Product Association",
        description = "Schema to holds the details of Associated Product"
)
public class ProductAssociationDto {

    @NotEmpty(message = "SKU cannot be empty")
    @Size(min=1,max = 5,message = "The length of the SKU should be between 1 to 6 ")
    @Schema(
            description = "Product Unique Number"
    )
    private String sku;

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
}

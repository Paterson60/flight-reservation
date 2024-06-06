package com.service.productcatalogue.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@Schema(name = "SearchCriteria", description = "Schema to hold search criteria for products")
public class ProductSearchCriteriaDto {

    @Schema(description = "Name of the Product", example = "Airpod")
    private String name;

    @Schema(description = "Category of the Product", example = "Headphone")
    private String category;

    @Schema(description = "Description of the Product", example = "Headphone")
    private String description;

    @Schema(description = "Image of the Product")
    private String image;

    @Schema(description = "Specification of the Product", example = "Wireless")
    private String specification;

    @Schema(description = "Unique representation of the Product", example = "AZV12!")
    private String sku;

    @Schema(description = "Brand name of the Product", example = "Apple")
    private String brand;

    @Schema(description = "Product price", example = "1000")
    private Long amount;
}


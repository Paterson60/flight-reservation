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

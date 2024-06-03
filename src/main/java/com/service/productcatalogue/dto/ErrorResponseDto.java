package com.service.productcatalogue.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import org.springframework.http.HttpStatus;

import java.time.LocalDateTime;

@Data@AllArgsConstructor
@Schema(
        name = "Error Response",
        description = "Schema to hold error response information"
)
public class ErrorResponseDto {

    @Schema(
            description = "API path invoked by Customer"
    )
    private String apiPath;
    @Schema(
            description = "Represents the ErrorCode which error has occurred"
    )
    private HttpStatus errorCode;
    @Schema(
            description = "Represents the message of the error"
    )
    private String errorMessage;

    @Schema(
            description = "Represents the time when an error occurred"
    )
    private LocalDateTime errorTime;
}

package com.service.productcatalogue.dto;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;

import java.time.LocalDateTime;

import static org.assertj.core.api.Assertions.assertThat;

class ErrorResponseDtoTest {

    private ErrorResponseDto errorResponseDtoUnderTest;

    @BeforeEach
    void setUp() {
        errorResponseDtoUnderTest = new ErrorResponseDto("apiPath", HttpStatus.OK, "errorMessage",
                LocalDateTime.of(2020, 1, 1, 0, 0, 0));
    }

    @Test
    void testApiPathGetterAndSetter() {
        final String apiPath = "apiPath";
        errorResponseDtoUnderTest.setApiPath(apiPath);
        assertThat(errorResponseDtoUnderTest.getApiPath()).isEqualTo(apiPath);
    }

    @Test
    void testErrorCodeGetterAndSetter() {
        final HttpStatus errorCode = HttpStatus.OK;
        errorResponseDtoUnderTest.setErrorCode(errorCode);
        assertThat(errorResponseDtoUnderTest.getErrorCode()).isEqualTo(errorCode);
    }

    @Test
    void testErrorMessageGetterAndSetter() {
        final String errorMessage = "errorMessage";
        errorResponseDtoUnderTest.setErrorMessage(errorMessage);
        assertThat(errorResponseDtoUnderTest.getErrorMessage()).isEqualTo(errorMessage);
    }

    @Test
    void testErrorTimeGetterAndSetter() {
        final LocalDateTime errorTime = LocalDateTime.of(2020, 1, 1, 0, 0, 0);
        errorResponseDtoUnderTest.setErrorTime(errorTime);
        assertThat(errorResponseDtoUnderTest.getErrorTime()).isEqualTo(errorTime);
    }

    @Test
    void testEquals() {
        assertThat(errorResponseDtoUnderTest.equals("o")).isFalse();
    }

    @Test
    void testCanEqual() {
        assertThat(errorResponseDtoUnderTest.canEqual("other")).isFalse();
    }

}

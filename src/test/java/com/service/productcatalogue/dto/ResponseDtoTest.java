package com.service.productcatalogue.dto;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ResponseDtoTest {

    private ResponseDto responseDtoUnderTest;

    @BeforeEach
    void setUp() {
        responseDtoUnderTest = new ResponseDto("statusCode", "statusMsg");
    }

    @Test
    void testStatusCodeGetterAndSetter() {
        final String statusCode = "statusCode";
        responseDtoUnderTest.setStatusCode(statusCode);
        assertThat(responseDtoUnderTest.getStatusCode()).isEqualTo(statusCode);
    }

    @Test
    void testStatusMsgGetterAndSetter() {
        final String statusMsg = "statusMsg";
        responseDtoUnderTest.setStatusMsg(statusMsg);
        assertThat(responseDtoUnderTest.getStatusMsg()).isEqualTo(statusMsg);
    }

    @Test
    void testEquals() {
        assertThat(responseDtoUnderTest.equals("o")).isFalse();
    }

    @Test
    void testCanEqual() {
        assertThat(responseDtoUnderTest.canEqual("other")).isFalse();
    }

}

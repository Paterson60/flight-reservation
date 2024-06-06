package com.service.productcatalogue.dto;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class PriceDtoTest {

    private PriceDto priceDtoUnderTest;

    @BeforeEach
    void setUp() {
        priceDtoUnderTest = new PriceDto();
    }

    @Test
    void testAmountGetterAndSetter() {
        final Long amount = 0L;
        priceDtoUnderTest.setAmount(amount);
        assertThat(priceDtoUnderTest.getAmount()).isEqualTo(amount);
    }

    @Test
    void testDiscountGetterAndSetter() {
        final Long discount = 0L;
        priceDtoUnderTest.setDiscount(discount);
        assertThat(priceDtoUnderTest.getDiscount()).isEqualTo(discount);
    }

    @Test
    void testEquals() {
        assertThat(priceDtoUnderTest.equals("o")).isFalse();
    }

    @Test
    void testCanEqual() {
        assertThat(priceDtoUnderTest.canEqual("other")).isFalse();
    }

}

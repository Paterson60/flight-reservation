package com.service.productcatalogue.entity;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class PriceTest {

    @Test
    void testConstructor() {
        Price actualPrice = new Price();
        actualPrice.setAmount(10L);
        actualPrice.setDiscount(3L);
        actualPrice.setPriceId(1L);
        String actualToStringResult = actualPrice.toString();
        Long actualAmount = actualPrice.getAmount();
        Long actualDiscount = actualPrice.getDiscount();
        Long actualPriceId = actualPrice.getPriceId();
        assertEquals("Price(priceId=1, amount=10, discount=3)", actualToStringResult);
        assertEquals(10L, actualAmount.longValue());
        assertEquals(1L, actualPriceId.longValue());
        assertEquals(3L, actualDiscount.longValue());
    }


    @Test
    void testConstructor2() {
        Price actualPrice = new Price(1L, 10L, 3L);
        actualPrice.setAmount(10L);
        actualPrice.setDiscount(3L);
        actualPrice.setPriceId(1L);
        String actualToStringResult = actualPrice.toString();
        Long actualAmount = actualPrice.getAmount();
        Long actualDiscount = actualPrice.getDiscount();
        Long actualPriceId = actualPrice.getPriceId();
        assertEquals("Price(priceId=1, amount=10, discount=3)", actualToStringResult);
        assertEquals(10L, actualAmount.longValue());
        assertEquals(1L, actualPriceId.longValue());
        assertEquals(3L, actualDiscount.longValue());
    }
}

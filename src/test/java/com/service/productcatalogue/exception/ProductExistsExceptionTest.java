package com.service.productcatalogue.exception;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class ProductExistsExceptionTest {

    @Test
    public void test_exception_thrown_with_correct_message() {

        String expectedMessage = "Product already exists";

        ProductExistsException exception = new ProductExistsException(expectedMessage);

        assertEquals(expectedMessage, exception.getMessage());
    }

    @Test
    public void test_exception_thrown_with_empty_message() {

        String expectedMessage = "";

        ProductExistsException exception = new ProductExistsException(expectedMessage);

        assertEquals(expectedMessage, exception.getMessage());
    }
}
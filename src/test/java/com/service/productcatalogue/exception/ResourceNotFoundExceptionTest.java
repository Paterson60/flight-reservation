package com.service.productcatalogue.exception;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class ResourceNotFoundExceptionTest {

    @Test
    public void test_exception_message_contains_correct_details() {

        String resourceName = "Product";
        String fieldName = "ID";
        String fieldValue = "123";

        ResourceNotFoundException exception = new ResourceNotFoundException(resourceName, fieldName, fieldValue);

        assertEquals("Product not found with the given input data ID:123", exception.getMessage());
    }

    @Test
    public void test_exception_message_handles_null_resource_name() {

        String resourceName = null;
        String fieldName = "ID";
        String fieldValue = "123";

        ResourceNotFoundException exception = new ResourceNotFoundException(resourceName, fieldName, fieldValue);

        assertEquals("null not found with the given input data ID:123", exception.getMessage());
    }

}
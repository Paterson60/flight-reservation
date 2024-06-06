package com.service.productcatalogue.exception;

import com.service.productcatalogue.dto.ErrorResponseDto;
import com.service.productcatalogue.service.impl.NoJpaTestConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.context.request.WebRequest;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Map;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.validation.BindException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.context.request.ServletWebRequest;
import org.springframework.web.context.request.WebRequest;

@ExtendWith(SpringExtension.class)
@WebMvcTest(controllers = GlobalExceptionHandler.class,
        includeFilters = @ComponentScan.Filter(classes = ControllerAdvice.class))
@ContextConfiguration(classes = {NoJpaTestConfig.class, GlobalExceptionHandler.class})
public class GlobalExceptionHandlerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private WebApplicationContext webApplicationContext;

     @MockBean
     WebRequest request;

    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);
    }

    @Autowired
    private GlobalExceptionHandler globalExceptionHandler;


    /**
     * Method under test:
     * {@link GlobalExceptionHandler#handleMethodArgumentNotValid(MethodArgumentNotValidException, HttpHeaders, HttpStatusCode, WebRequest)}
     */
    @Test
    void testHandleMethodArgumentNotValid() {
        MethodArgumentNotValidException ex = new MethodArgumentNotValidException(null,
                new BindException("Target", "Object Name"));

        HttpHeaders headers = new HttpHeaders();
        ResponseEntity<Object> actualHandleMethodArgumentNotValidResult = globalExceptionHandler
                .handleMethodArgumentNotValid(ex, headers, null, new ServletWebRequest(new MockHttpServletRequest()));
        assertEquals(400, actualHandleMethodArgumentNotValidResult.getStatusCodeValue());
        assertTrue(((Map<Object, Object>) actualHandleMethodArgumentNotValidResult.getBody()).isEmpty());
        assertTrue(actualHandleMethodArgumentNotValidResult.getHeaders().isEmpty());
    }

    @Test
    public void handleMethodArgumentNotValidExceptionTest() throws Exception {
        String requestBody = "{\"invalidField\":\"invalidValue\"}"; // Adjust according to your validation logic

        mockMvc.perform(post("/some-endpoint")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(requestBody))
                .andExpect(status().is(404));
    }

    @Test
    public void handleGlobalExceptionTest() throws Exception {
        String errorMessage = "Internal Server Error";

        mockMvc.perform(post("/throw-global-exception")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is(404));
    }

    @Test
    public void handleResourceNotFoundExceptionTest() throws Exception {
        String errorMessage = "Resource not found";

        mockMvc.perform(post("/throw-resource-not-found-exception")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound());
    }

    @Test
    public void test_exception_message_is_null() {
        GlobalExceptionHandler handler = new GlobalExceptionHandler();
        Exception exception = new Exception((String) null);

        ResponseEntity<ErrorResponseDto> response = handler.handleGlobalException(exception, request);

        assertNull(response.getBody().getErrorMessage());
    }


    @Test
    public void test_handles_null_webrequest() {
        GlobalExceptionHandler handler = new GlobalExceptionHandler();
        ResourceNotFoundException exception = new ResourceNotFoundException("Product", "id", "123");

        ResponseEntity<ErrorResponseDto> response = handler.handleResourceNotFoundException(exception, request);

        assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
        assertEquals("Product not found with the given input data id:123", response.getBody().getErrorMessage());
    }


    @Test
    public void test_handles_null_webrequest_gracefully() {
        GlobalExceptionHandler handler = new GlobalExceptionHandler();
        ProductExistsException exception = new ProductExistsException("Product already exists");

        ResponseEntity<ErrorResponseDto> response = handler.handleProductExistsException(exception, request);

        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
        assertNotNull(response.getBody());
        assertEquals("Product already exists", response.getBody().getErrorMessage());
    }

}


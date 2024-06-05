package com.service.productcatalogue.exception;

import com.service.productcatalogue.dto.ErrorResponseDto;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.web.context.WebApplicationContext;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@ExtendWith(SpringExtension.class)
@WebMvcTest(controllers = GlobalExceptionHandler.class, 
            includeFilters = @ComponentScan.Filter(classes = ControllerAdvice.class))
public class GlobalExceptionHandlerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private WebApplicationContext webApplicationContext;

    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void handleMethodArgumentNotValidExceptionTest() throws Exception {
        String requestBody = "{\"invalidField\":\"invalidValue\"}"; // Adjust according to your validation logic

        mockMvc.perform(post("/some-endpoint")
                .contentType(MediaType.APPLICATION_JSON)
                .content(requestBody))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.invalidField").exists());
    }

    @Test
    public void handleGlobalExceptionTest() throws Exception {
        String errorMessage = "Internal Server Error";

        mockMvc.perform(post("/throw-global-exception")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.message").value(errorMessage));
    }

    @Test
    public void handleResourceNotFoundExceptionTest() throws Exception {
        String errorMessage = "Resource not found";

        mockMvc.perform(post("/throw-resource-not-found-exception")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value(errorMessage));
    }

    @Test
    public void handleProductExistsExceptionTest() throws Exception {
        String errorMessage = "Product already exists";

        mockMvc.perform(post("/throw-product-exists-exception")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value(errorMessage));
    }
}


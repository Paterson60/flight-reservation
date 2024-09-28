import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import reactor.core.publisher.Mono;

public class CustomerControllerTest {

    private MockMvc mockMvc;

    @Mock
    private CustomerKycController customerKycController;

    @Mock
    private CustomerQualificationController customerQualificationController;

    @Mock
    private PreferencesController preferencesController;

    @Mock
    private InteractionServiceImplOffline interactionServiceImplOffline;

    @InjectMocks
    private CustomerController customerController; // The controller class where createLogInteractionOffline is located

    @Autowired
    private ObjectMapper objectMapper;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        mockMvc = MockMvcBuilders.standaloneSetup(customerController).build();
        objectMapper = new ObjectMapper();
    }

    @Test
    void testCreateLogInteractionOffline() throws Exception {
        // Set up the mock responses for dependencies
        ResponseStatus mockResponseStatus = new ResponseStatus();
        CustomerPreferencesResponseStatus mockPreferencesResponseStatus = new CustomerPreferencesResponseStatus();

        when(customerKycController.updateHighdiscoveryQues(any())).thenReturn(ResponseEntity.ok(new GenericResponse<>(mockResponseStatus)));
        when(customerQualificationController.updateQualifications(anyString(), any())).thenReturn(ResponseEntity.ok(new GenericResponse<>(mockResponseStatus)));
        when(preferencesController.updatePreferencesData(any())).thenReturn(ResponseEntity.ok(new GenericResponse<>(mockPreferencesResponseStatus)));

        doNothing().when(interactionServiceImplOffline).createLogInteractionOffline(anyString(), any(LogInteractionOfflineRequestWrapper.class));

        // Prepare test data for LogInteractionOfflineRequestWrapper
        LogInteractionOfflineRequestWrapper requestWrapper = new LogInteractionOfflineRequestWrapper();
        LogInteraction logInteraction = new LogInteraction();
        logInteraction.setAccountNumber("12345");
        requestWrapper.setLogInteraction(logInteraction);

        UpdateCustomerQuestions updateCustomerQuestions = new UpdateCustomerQuestions();
        updateCustomerQuestions.setHighDiscovery(new HighDiscovery());
        updateCustomerQuestions.setUpdateQualifications(new UpdateQualifications());
        updateCustomerQuestions.setCustomerPreferences(new CustomerPreferences());
        requestWrapper.setRequests(new Requests(updateCustomerQuestions));

        // Convert the requestWrapper to JSON string for the request body
        String requestBody = objectMapper.writeValueAsString(requestWrapper);

        // Perform the test request using MockMvc
        mockMvc.perform(post("/customers/test-customer-id/log-interactions-offline")
                .param("planId", "test-plan-id")
                .contentType(MediaType.APPLICATION_JSON)
                .content(requestBody))
                .andExpect(status().isOk());

        // Verify that each service method is called
        verify(customerKycController, times(1)).updateHighdiscoveryQues(any());
        verify(customerQualificationController, times(1)).updateQualifications(eq("12345"), any());
        verify(preferencesController, times(1)).updatePreferencesData(any());
        verify(interactionServiceImplOffline, times(1)).createLogInteractionOffline(eq("test-plan-id"), any(LogInteractionOfflineRequestWrapper.class));
    }

    @Test
    void testCreateLogInteractionOffline_NullRequestWrapper() throws Exception {
        // Send a null request wrapper
        mockMvc.perform(post("/customers/test-customer-id/log-interactions-offline")
                .param("planId", "test-plan-id")
                .contentType(MediaType.APPLICATION_JSON)
                .content("{}")) // Sending an empty JSON
                .andExpect(status().isOk());

        // Verify no calls to dependent services
        verify(customerKycController, never()).updateHighdiscoveryQues(any());
        verify(customerQualificationController, never()).updateQualifications(anyString(), any());
        verify(preferencesController, never()).updatePreferencesData(any());
        verify(interactionServiceImplOffline, times(1)).createLogInteractionOffline(eq("test-plan-id"), any(LogInteractionOfflineRequestWrapper.class));
    }
}

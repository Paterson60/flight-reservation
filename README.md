huimport static org.mockito.ArgumentMatchers.any;
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


// Replace GenericResponse with a similar mock class or use a simpler structure that matches the return type.
ResponseEntity<Object> mockHighDiscoveryResponse = ResponseEntity.ok(mockResponseStatus);
ResponseEntity<Object> mockQualificationsResponse = ResponseEntity.ok(mockResponseStatus);
ResponseEntity<Object> mockCustomerPreferencesResponse = ResponseEntity.ok(mockPreferencesResponseStatus);

// Use the mock responses instead of new GenericResponse<>()
when(customerKycController.updateHighdiscoveryQues(any())).thenReturn(mockHighDiscoveryResponse);
when(customerQualificationController.updateQualifications(anyString(), any())).thenReturn(mockQualificationsResponse);
when(preferencesController.updatePreferencesData(any())).thenReturn(mockCustomerPreferencesResponse);






import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class LogInteractionServiceImplTest {

    @InjectMocks
    private LogInteractionServiceImpl logInteractionServiceImpl;

    @Mock
    private CustomerRepository customerRepository;

    @Mock
    private InteractionRepository interactionRepository;

    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testCreateLogInteractionOffline_Success() {
        // Arrange
        String customerId = "12345";
        String planId = "plan123";
        LogInteractionOfflineRequestWrapper requestWrapper = new LogInteractionOfflineRequestWrapper();
        requestWrapper.setInteractionId("interaction1");
        
        // Mocking dependencies
        when(interactionRepository.save(any())).thenReturn(new Interaction());

        // Act
        logInteractionServiceImpl.createLogInteractionOffline(customerId, planId, requestWrapper);

        // Assert
        verify(interactionRepository, times(1)).save(any());
    }

    @Test
    public void testCreateLogInteractionOffline_NullInteraction() {
        // Arrange
        String customerId = "12345";
        String planId = "plan123";
        LogInteractionOfflineRequestWrapper requestWrapper = new LogInteractionOfflineRequestWrapper();

        // Act & Assert
        Exception exception = assertThrows(RuntimeException.class, () -> {
            logInteractionServiceImpl.createLogInteractionOffline(customerId, planId, requestWrapper);
        });

        assertEquals("An error occurred while completing offline functionality for log interactions:", exception.getMessage());
    }
}








import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.ResponseEntity;

public class LogInteractionControllerTest {

    @InjectMocks
    private LogInteractionController logInteractionController;  // Assuming this is the class where createLogInteractionOffline is defined.

    @Mock
    private CustomerKycController customerKycController;

    @Mock
    private CustomerQualificationController customerQualificationController;

    @Mock
    private PreferencesController preferencesController;

    @Mock
    private InteractionServiceImplOffline interactionServiceImplOffline;

    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testCreateLogInteractionOffline_WithValidInput() {
        // Arrange
        String customerId = "customer123";
        String planId = "planABC";
        
        LogInteractionOfflineRequestWrapper requestWrapper = new LogInteractionOfflineRequestWrapper();
        requestWrapper.setLogInteraction(new LogInteraction("accountNumber123"));

        UpdateCustomerQuestions updateCustomerQuestions = new UpdateCustomerQuestions();
        updateCustomerQuestions.setHighDiscovery("highDiscoveryValue");
        updateCustomerQuestions.setUpdateQualifications("qualificationData");
        updateCustomerQuestions.setCustomerPreferences("preferencesData");

        RequestWrapper requests = new RequestWrapper();
        requests.setUpdateCustomerQuestions(updateCustomerQuestions);
        requestWrapper.setRequests(requests);
        
        // Mock the responses of external service calls
        ResponseStatus responseStatus = new ResponseStatus();
        CustomerPreferencesResponseStatus customerPreferencesResponseStatus = new CustomerPreferencesResponseStatus();

        when(customerKycController.updateHighdiscoveryQues(anyString())).thenReturn(ResponseEntity.ok(new ResponseData<>(responseStatus)));
        when(customerQualificationController.updateQualifications(anyString(), anyString())).thenReturn(ResponseEntity.ok(new ResponseData<>(responseStatus)));
        when(preferencesController.updatePreferencesData(anyString())).thenReturn(ResponseEntity.ok(new ResponseData<>(customerPreferencesResponseStatus)));

        // Act
        logInteractionController.createLogInteractionOffline(customerId, planId, requestWrapper);

        // Assert
        verify(customerKycController, times(1)).updateHighdiscoveryQues(anyString());
        verify(customerQualificationController, times(1)).updateQualifications(anyString(), anyString());
        verify(preferencesController, times(1)).updatePreferencesData(anyString());
        verify(interactionServiceImplOffline, times(1)).createLogInteractionOffline(anyString(), eq(requestWrapper));
    }

    @Test
    public void testCreateLogInteractionOffline_WithoutLogInteraction() {
        // Arrange
        String customerId = "customer123";
        String planId = "planABC";
        
        LogInteractionOfflineRequestWrapper requestWrapper = new LogInteractionOfflineRequestWrapper();
        
        // Act
        logInteractionController.createLogInteractionOffline(customerId, planId, requestWrapper);

        // Assert - No other services should be invoked
        verify(interactionServiceImplOffline, times(1)).createLogInteractionOffline(anyString(), eq(requestWrapper));
    }

    @Test
    public void testCreateLogInteractionOffline_WithoutRequests() {
        // Arrange
        String customerId = "customer123";
        String planId = "planABC";
        
        LogInteractionOfflineRequestWrapper requestWrapper = new LogInteractionOfflineRequestWrapper();
        requestWrapper.setLogInteraction(new LogInteraction("accountNumber123"));

        // Act
        logInteractionController.createLogInteractionOffline(customerId, planId, requestWrapper);

        // Assert - Only interactionServiceImplOffline should be called
        verify(interactionServiceImplOffline, times(1)).createLogInteractionOffline(anyString(), eq(requestWrapper));
    }

    @Test
    public void testCreateLogInteractionOffline_ExceptionScenario() {
        // Arrange
        String customerId = "customer123";
        String planId = "planABC";
        
        LogInteractionOfflineRequestWrapper requestWrapper = new LogInteractionOfflineRequestWrapper();
        requestWrapper.setLogInteraction(new LogInteraction("accountNumber123"));

        // Mock exception during service call
        doThrow(new RuntimeException("Mocked Exception")).when(interactionServiceImplOffline).createLogInteractionOffline(anyString(), any());

        // Act & Assert
        Exception exception = assertThrows(RuntimeException.class, () -> {
            logInteractionController.createLogInteractionOffline(customerId, planId, requestWrapper);
        });

        assertTrue(exception.getMessage().contains("An error occurred while completing offline functionality"));
    }
}

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

public class InteractionServiceImplOfflineTest {

    @Mock
    private WebClient calendarClient;

    @InjectMocks
    private InteractionServiceImplOffline interactionService;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testCreateLogInteractionOffline() {
        // Define test data
        String planId = "test-plan-id";
        LogInteractionOfflineRequestWrapper requestWrapper = new LogInteractionOfflineRequestWrapper();
        requestWrapper.setLoginId("test-login-id");

        // Mocking WebClient behavior
        WebClient.RequestBodyUriSpec requestBodyUriSpec = mock(WebClient.RequestBodyUriSpec.class);
        WebClient.RequestHeadersSpec requestHeadersSpec = mock(WebClient.RequestHeadersSpec.class);
        WebClient.RequestHeadersUriSpec requestHeadersUriSpec = mock(WebClient.RequestHeadersUriSpec.class);
        WebClient.ResponseSpec responseSpec = mock(WebClient.ResponseSpec.class);

        when(calendarClient.post()).thenReturn(requestBodyUriSpec);
        when(requestBodyUriSpec.uri(anyString())).thenReturn(requestBodyUriSpec);
        when(requestBodyUriSpec.accept(any())).thenReturn(requestBodyUriSpec);
        when(requestBodyUriSpec.contentType(any())).thenReturn(requestBodyUriSpec);
        when(requestBodyUriSpec.body(any())).thenReturn(requestHeadersSpec);
        when(requestHeadersSpec.retrieve()).thenReturn(responseSpec);
        when(responseSpec.bodyToMono(any(Class.class))).thenReturn(Mono.just(new GenericResponse<>()));

        // Call the method
        interactionService.createLogInteractionOffline(planId, requestWrapper);

        // Verify the method call
        verify(calendarClient.post(), times(1)).uri(anyString());
        verify(requestBodyUriSpec, times(1)).accept(any());
        verify(requestBodyUriSpec, times(1)).contentType(any());
        verify(requestHeadersSpec, times(1)).retrieve();
    }
}

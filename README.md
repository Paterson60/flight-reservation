Controller class

@RestController
@RequestMapping("/v1")
@Slf4j
@Tag(name = "Customer", description = "Customer Operations")
public class CustomerController {
@PostMapping(value = "/customers/{customer-id}/log-interactions-offline", produces = {MediaType.APPLICATION_JSON_VALUE})
    public void createLogInteractionOffline(@PathVariable("customer-id") String customerId,
                                            @RequestParam(value = "planId",required = false) String planId,
                                            @RequestBody @NonNull LogInteractionOfflineRequestWrapper logInteractionOfflineRequestWrapper) {

        try {
            if (logInteractionOfflineRequestWrapper.getLogInteraction() != null) {
                createLogInteraction(customerId, logInteractionOfflineRequestWrapper.getLogInteraction());
            }
            if (logInteractionOfflineRequestWrapper.getRequests() != null && logInteractionOfflineRequestWrapper.getRequests().getUpdateCustomerQuestions() != null && logInteractionOfflineRequestWrapper.getRequests().getUpdateCustomerQuestions().getHighDiscovery() != null) {
                ResponseStatus updateHighDiscoveryResponse = customerKycController.updateHighdiscoveryQues(logInteractionOfflineRequestWrapper.getRequests().getUpdateCustomerQuestions().getHighDiscovery()).getBody().getData();
            }
            if (logInteractionOfflineRequestWrapper.getRequests().getUpdateCustomerQuestions() != null && logInteractionOfflineRequestWrapper.getRequests().getUpdateCustomerQuestions().getUpdateQualifications() != null) {
                ResponseStatus updateQualificationsResponse = customerQualificationController.updateQualifications(logInteractionOfflineRequestWrapper.getLogInteraction().getAccountNumber(), logInteractionOfflineRequestWrapper.getRequests().getUpdateCustomerQuestions().getUpdateQualifications()).getBody().getData();
            }
            if (logInteractionOfflineRequestWrapper.getRequests().getUpdateCustomerQuestions() != null && logInteractionOfflineRequestWrapper.getRequests().getUpdateCustomerQuestions().getCustomerPreferences() != null) {
                CustomerPreferencesResponseStatus customerPreferencesResponse = preferencesController.updatePreferencesData(logInteractionOfflineRequestWrapper.getRequests().getUpdateCustomerQuestions().getCustomerPreferences()).getBody().getData();
            }
            interactionServiceImplOffline.createLogInteractionOffline(planId,logInteractionOfflineRequestWrapper);
        } catch (Exception e) {
            throw new RuntimeException(StringUtils.join(" An error occurred while completing offline functionality for log interactions:",e.getMessage()),e);
        }
    }
}

@Service
@Slf4j
public class InteractionServiceImplOffline implements InteractionServiceOffline {

    @Autowired
    @Qualifier("calendar-client")
    private WebClient calendarClient;

    @Override
    public void createLogInteractionOffline(String planId, LogInteractionOfflineRequestWrapper logInteractionOfflineRequestWrapper) {

        if(!(Objects.isNull(planId) || planId.isBlank())) {
            GenericResponse<ResponseStatus> addInteractionToPlanResponse = calendarClient.post().uri(StringUtils.join("/users/", logInteractionOfflineRequestWrapper.getLogInteraction().getLoginId(), "/plans/", planId, "/add-interaction"))
                    .accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON)
                    .bodyValue(logInteractionOfflineRequestWrapper.getRequests().getAddInteractionToPlan())
                    .retrieve()
                    .bodyToMono(new ParameterizedTypeReference<GenericResponse<ResponseStatus>>() {
                    })
                    .block();
        }

        GenericResponse<CreateEventResponseStatus> createEventResponse = calendarClient.post().uri(StringUtils.join("/events"))
                .accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .bodyValue(logInteractionOfflineRequestWrapper.getRequests().getCreateEvent())
                .retrieve()
                .bodyToMono(new ParameterizedTypeReference<GenericResponse<CreateEventResponseStatus>>() {
                })
                .block();
    }
}

@Data
@AllArgsConstructor
@NoArgsConstructor
public class LogInteractionOfflineRequestWrapper {

        @JsonProperty("logInteraction")
        private CreateLogInteraction logInteraction;

        @JsonProperty("requests")
        private LogInteractionOfflineRequest requests;
}

public class LogInteractionOfflineRequest implements Serializable {
    private static final long serialVersionUID = 1L;
    @JsonProperty("updateCustomerQuestions")
    private LogInteractionOfflineRequestUpdateCustomerQuestions updateCustomerQuestions = null;
    @JsonProperty("createEvent")
    private CreateEvent createEvent = null;
    @JsonProperty("addInteractionToPlan")
    private PlanInfo addInteractionToPlan = null;
}
public class LogInteractionOfflineRequestUpdateCustomerQuestions implements Serializable {
    private static final long serialVersionUID = 1L;
    @JsonProperty("updateQualifications")
    private QualificationsUpdateRequest updateQualifications = null;
    @JsonProperty("customerPreferences")
    private CustomerPreferencesRequest customerPreferences = null;
    @JsonProperty("highDiscovery")
    private HighDiscoveryQuestionRequest highDiscovery = null;
}
public class QualificationsUpdateRequest implements Serializable {
    private static final long serialVersionUID = 1L;
    @JsonProperty("accountNumber")
    private String accountNumber = null;
    @JsonProperty("updateQualifications")
    private List<Qualification> updateQualifications = null;
}
public class CustomerPreferencesRequest implements Serializable {
    private static final long serialVersionUID = 1L;
    @JsonProperty("accountNumber")
    private String accountNumber = null;
    @JsonProperty("updatedBy")
    private String updatedBy = null;
    @JsonProperty("updateQuestions")
    private List<QuestionPreferences> updateQuestions = null;
}
public class HighDiscoveryQuestionRequest implements Serializable {
    private static final long serialVersionUID = 1L;
    @JsonProperty("accountNumber")
    private String accountNumber = null;
    @JsonProperty("userId")
    private String userId = null;
    @JsonProperty("highDiscoveryQuestion")
    private List<UpdateHighDiscoveryQuestionData> highDiscoveryQuestion = null;
}
public class UpdateHighDiscoveryQuestionData implements Serializable {
    private static final long serialVersionUID = 1L;
    @JsonProperty("questionId")
    private String questionId = null;
    @JsonProperty("answer")
    private String answer = null;
    @JsonProperty("answerId")
    private String answerId = null;
}

Above I have provide all supporting code to write the junit test case for createLogInteractionOffline() method present in CustomerController




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

public class CustomerControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private CustomerController customerController;

    @Mock
    private InteractionServiceImplOffline interactionServiceImplOffline;

    @Mock
    private CustomerKycController customerKycController;

    @Mock
    private CustomerQualificationController customerQualificationController;

    @Mock
    private PreferencesController preferencesController;

    private final ObjectMapper objectMapper = new ObjectMapper();

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        mockMvc = MockMvcBuilders.standaloneSetup(customerController).build();
    }

    @Test
    void testCreateLogInteractionOffline() throws Exception {
        // Prepare mock data
        String customerId = "12345";
        String planId = "98765";

        // Create a sample request object
        LogInteractionOfflineRequestWrapper logInteractionOfflineRequestWrapper = new LogInteractionOfflineRequestWrapper();
        logInteractionOfflineRequestWrapper.setLogInteraction(new CreateLogInteraction("sampleAccount"));
        LogInteractionOfflineRequest request = new LogInteractionOfflineRequest();
        request.setCreateEvent(new CreateEvent("eventTitle"));
        logInteractionOfflineRequestWrapper.setRequests(request);

        // Convert request object to JSON string
        String jsonRequest = objectMapper.writeValueAsString(logInteractionOfflineRequestWrapper);

        // Mock the interaction service and other dependencies if needed
        doNothing().when(interactionServiceImplOffline).createLogInteractionOffline(anyString(), any(LogInteractionOfflineRequestWrapper.class));

        // Perform the test request
        mockMvc.perform(post("/v1/customers/{customer-id}/log-interactions-offline", customerId)
                .contentType(MediaType.APPLICATION_JSON)
                .param("planId", planId)
                .content(jsonRequest))
                .andExpect(status().isOk());

        // Verify that the service method was called once with expected arguments
        verify(interactionServiceImplOffline, times(1)).createLogInteractionOffline(eq(planId), any(LogInteractionOfflineRequestWrapper.class));
    }
}

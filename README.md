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

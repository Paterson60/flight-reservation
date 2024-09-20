@RestController
@CrossOrigin
@RequestMapping("/v1")
@Slf4j
public class CustomerInteractionOfflineController {
    @Autowired
    CustomerKycService customerKycService;
    @Autowired
    PreferencesService preferenceService;
    @Autowired
    CustomerQualificationService customerQualificationService;
	
	@Autowired
    OfflineHelper offlineHelper;
	
	private static final Logger LOGGER = LoggerFactory.getLogger(CustomerInteractionOfflineController.class);
    private static final String REQUEST_ID = "requestId";
    private final InteractionService interactionService;
    private final CustomerService service;
    public CustomerInteractionOfflineController(InteractionService interactionService,
                                                CustomerService service) {
        this.interactionService = interactionService;
        this.service = service;
    }

    @PostMapping(value = "/customers/{customer-id}/log-interactions", produces = {MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity<GenericResponse<ResponseStatus>> createLogInteractionWithActions(@PathVariable("customer-id") String customerId,
                                                                                @RequestBody @NonNull CreateLogInteraction createLogInteraction) {
        if (Boolean.FALSE.equals(interactionService.validCreateInteractionResponse(customerId, createLogInteraction))) {
            return ResponseEntity.badRequest().body(GenericResponse.<ResponseStatus>builder()
                    .requestId(MDC.get(REQUEST_ID))
                    .message("Bad create log interaction request")
                    .build());
        } else {
            if (LOGGER.isDebugEnabled())
                LOGGER.debug("Request received to create log interaction for customerId : {}", customerId);
            GenericResponse<ResponseStatus> logInteractionResponse = GenericResponse.<ResponseStatus>builder()
                    .data(interactionService.createLogInteraction(customerId, createLogInteraction,
                            Boolean.TRUE.equals(createLogInteraction.getEmailForm().getSendEmail()) ?
                                    service.getCustomerInfo(createLogInteraction.getCustomerId(), "6000", createLogInteraction.getUserRole(), createLogInteraction.getLoginId()) :
                                    null))
                    .requestId(MDC.get(REQUEST_ID))
                    .message(HttpStatus.OK.getReasonPhrase())
                    .build();
            return ResponseEntity.ok(logInteractionResponse);

        }
    }

@Service
public class OfflineHelper {

    @Autowired
    @Qualifier("ords-client")
    private WebClient ordsClient;

    @Autowired
    WebClient webClient;

    public ResponseEntity<GenericResponse<CreateEventResponseStatus>> createEvents(@RequestBody CreateEvent createEvent) {
        String uri = "http://localhost:8080/v1/events";
        return webClient.post().uri(uri)
                .body(Mono.just(createEvent),CreateEvent.class)
                .retrieve()
                .bodyToMono(new ParameterizedTypeReference<ResponseEntity<GenericResponse<CreateEventResponseStatus>>>() {
                }).block();
    }

    public ResponseEntity<GenericResponse<ResponseStatus>> addInteractionToPlan(@PathVariable("user-id") String userId, @PathVariable("plan-id") String planId, @RequestBody PlanInfo planInfo
    ) {
        String uri = "http://localhost:8080/v1/events";
        return webClient.post().uri(uri)
                .body(Mono.just(planInfo),CreateEvent.class)
                .retrieve()
                .bodyToMono(new ParameterizedTypeReference<ResponseEntity<GenericResponse<ResponseStatus>>>() {
                }).block();
    }
}


@PostMapping(value = "/customers/{customer-id}/log-interactions", produces = {MediaType.APPLICATION_JSON_VALUE})
public ResponseEntity<GenericResponse<ResponseStatus>> createLogInteractionWithActions(
        @PathVariable("customer-id") String customerId,
        @RequestBody @NonNull CreateLogInteraction createLogInteraction) {

    if (Boolean.FALSE.equals(interactionService.validCreateInteractionResponse(customerId, createLogInteraction))) {
        return ResponseEntity.badRequest().body(GenericResponse.<ResponseStatus>builder()
                .requestId(MDC.get(REQUEST_ID))
                .message("Bad create log interaction request")
                .build());
    } else {
        if (LOGGER.isDebugEnabled())
            LOGGER.debug("Request received to create log interaction for customerId : {}", customerId);

        // Call OfflineHelper to create events (createEvents)
        CreateEvent createEvent = new CreateEvent();  // Populate the event details as needed
        ResponseEntity<GenericResponse<CreateEventResponseStatus>> createEventResponse = offlineHelper.createEvents(createEvent);

        // Log if the event creation is successful or not
        if (createEventResponse.getStatusCode().is2xxSuccessful()) {
            LOGGER.info("Event created successfully for customerId: {}", customerId);
        } else {
            LOGGER.error("Failed to create event for customerId: {}", customerId);
        }

        // Call OfflineHelper to add interaction to the plan (addInteractionToPlan)
        PlanInfo planInfo = new PlanInfo();  // Populate the plan info as needed
        ResponseEntity<GenericResponse<ResponseStatus>> addInteractionResponse = offlineHelper.addInteractionToPlan(
                createLogInteraction.getLoginId(), createLogInteraction.getPlanId(), planInfo);

        // Log if adding interaction to the plan is successful or not
        if (addInteractionResponse.getStatusCode().is2xxSuccessful()) {
            LOGGER.info("Interaction added to plan successfully for customerId: {}", customerId);
        } else {
            LOGGER.error("Failed to add interaction to plan for customerId: {}", customerId);
        }

        // Main log interaction response
        GenericResponse<ResponseStatus> logInteractionResponse = GenericResponse.<ResponseStatus>builder()
                .data(interactionService.createLogInteraction(customerId, createLogInteraction,
                        Boolean.TRUE.equals(createLogInteraction.getEmailForm().getSendEmail()) ?
                                service.getCustomerInfo(createLogInteraction.getCustomerId(), "6000", createLogInteraction.getUserRole(), createLogInteraction.getLoginId()) :
                                null))
                .requestId(MDC.get(REQUEST_ID))
                .message(HttpStatus.OK.getReasonPhrase())
                .build();
        return ResponseEntity.ok(logInteractionResponse);
    }
}

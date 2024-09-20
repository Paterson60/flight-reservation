package com.sherwinwilliams.service.rest;

import com.sherwinwilliams.service.domain.model.*;
import com.sherwinwilliams.service.domain.model.ResponseStatus;
import com.sherwinwilliams.service.domain.service.*;
import com.sherwinwilliams.service.model.CreateLogInteraction;
import com.sherwinwilliams.service.model.GenericResponse;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

import java.io.IOException;
import java.text.MessageFormat;

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
    @Qualifier("ords-client")
    private WebClient ordsClient;

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
            return ResponseEntity.ok(
                    GenericResponse.<ResponseStatus>builder()
                            .data(interactionService.createLogInteraction(customerId, createLogInteraction,
                                    Boolean.TRUE.equals(createLogInteraction.getEmailForm().getSendEmail()) ?
                                            service.getCustomerInfo(createLogInteraction.getCustomerId(), "6000", createLogInteraction.getUserRole(), createLogInteraction.getLoginId()) :
                                            null))
                            .requestId(MDC.get(REQUEST_ID))
                            .message(HttpStatus.OK.getReasonPhrase())
                            .build());
        }
    }

    @PostMapping(value = "/events/offline", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<GenericResponse<CreateEventResponseStatus>> createEvents(@RequestBody CreateEvent createEvent) {
        String uri = "http://localhost:8080/v1/events";
        return ordsClient.post().uri(uri)
                .body(Mono.just(createEvent),CreateEvent.class)
                .retrieve()
                .bodyToMono(new ParameterizedTypeReference<ResponseEntity<GenericResponse<CreateEventResponseStatus>>>() {
                }).block();
    }

    @PostMapping(value = "/users/{user-id}/plans/{plan-id}/add-interaction", produces = {MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity<GenericResponse<ResponseStatus>> addInteractionToPlan(@PathVariable("user-id") String userId, @PathVariable("plan-id") String planId, @RequestBody PlanInfo planInfo
    ) {

        String uri = "http://localhost:8080/v1/events";
        return ordsClient.post().uri(uri)
                .body(Mono.just(planInfo),CreateEvent.class)
                .retrieve()
                .bodyToMono(new ParameterizedTypeReference<ResponseEntity<GenericResponse<ResponseStatus>>>() {
                }).block();
    }

    @PostMapping(value = "/accounts/{account-number}/qualifications", produces = {MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity<GenericResponse<ResponseStatus>> updateQualifications(
            @PathVariable ("account-number") String accountNumber,
            @RequestBody QualificationsUpdateRequest qualificationsUpdateData) throws IOException {
        if (LOGGER.isDebugEnabled())
            LOGGER.debug("Request received to update the qualifications");
        return ResponseEntity.ok(
                GenericResponse.<ResponseStatus>builder()
                        .data(customerQualificationService.updateQualifications(accountNumber, qualificationsUpdateData).getBody())
                        .requestId(MDC.get(REQUEST_ID))
                        .message(HttpStatus.OK.getReasonPhrase())
                        .build());
    }

    @PostMapping(value = "/customers/update-preferences", produces = { MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity<GenericResponse<CustomerPreferencesResponseStatus>> updatePreferencesData(
            @RequestBody CustomerPreferencesRequest customerPreferencesRequest) {
        if (LOGGER.isDebugEnabled())
            LOGGER.debug("Request received to update the purchase behavior or nature of business");
        return ResponseEntity.ok(
                GenericResponse.<CustomerPreferencesResponseStatus>builder()
                        .data(preferenceService.updatePreferencesData(customerPreferencesRequest))
                        .requestId(MDC.get(REQUEST_ID))
                        .message(HttpStatus.OK.getReasonPhrase())
                        .build());
    }

    @PostMapping(value = "/accounts/update-high-discovery", produces = {
            MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity<GenericResponse<ResponseStatus>> updateHighdiscoveryQues(@RequestBody HighDiscoveryQuestionRequest request) {
        if (LOGGER.isDebugEnabled())
            LOGGER.debug(MessageFormat.format("Request received for update HighdiscoveryQues for Account Number {0}", request.getAccountNumber()));
        return ResponseEntity.ok(
                GenericResponse.<ResponseStatus>builder()
                        .data(customerKycService.updateHighdiscoveryQues(request))
                        .requestId(MDC.get(REQUEST_ID))
                        .message(HttpStatus.OK.getReasonPhrase())
                        .build());
    }
}

Can you write a java code logic where the methods createEvents, addInteractionToPlan, updateQualifications, updatePreferencesData, updateHighdiscoveryQues are called from inside createLogInteractionWithActions method 
           


@PostMapping(value = "/customers/{customer-id}/log-interactions", produces = {MediaType.APPLICATION_JSON_VALUE})
public ResponseEntity<GenericResponse<ResponseStatus>> createLogInteractionWithActions(
        @PathVariable("customer-id") String customerId,
        @RequestBody @NonNull CreateLogInteraction createLogInteraction) {

    // Validate the create interaction request
    if (Boolean.FALSE.equals(interactionService.validCreateInteractionResponse(customerId, createLogInteraction))) {
        return ResponseEntity.badRequest().body(GenericResponse.<ResponseStatus>builder()
                .requestId(MDC.get(REQUEST_ID))
                .message("Bad create log interaction request")
                .build());
    }

    if (LOGGER.isDebugEnabled()) {
        LOGGER.debug("Request received to create log interaction for customerId : {}", customerId);
    }

    // Create the log interaction
    GenericResponse<ResponseStatus> logInteractionResponse = GenericResponse.<ResponseStatus>builder()
            .data(interactionService.createLogInteraction(customerId, createLogInteraction,
                    Boolean.TRUE.equals(createLogInteraction.getEmailForm().getSendEmail()) ?
                            service.getCustomerInfo(createLogInteraction.getCustomerId(), "6000", createLogInteraction.getUserRole(), createLogInteraction.getLoginId()) :
                            null))
            .requestId(MDC.get(REQUEST_ID))
            .message(HttpStatus.OK.getReasonPhrase())
            .build();

    // Conditional logic to call additional methods

    // Example: Call createEvents method if a certain condition is met
    if (shouldCreateEvent(createLogInteraction)) {
        CreateEvent createEvent = buildCreateEvent(createLogInteraction);
        ResponseEntity<GenericResponse<CreateEventResponseStatus>> eventResponse = createEvents(createEvent);
        logResponses(eventResponse, "Create Event");
    }

    // Example: Call addInteractionToPlan method if needed
    if (shouldAddInteractionToPlan(createLogInteraction)) {
        PlanInfo planInfo = buildPlanInfo(createLogInteraction);
        ResponseEntity<GenericResponse<ResponseStatus>> planResponse = addInteractionToPlan(createLogInteraction.getUserId(), createLogInteraction.getPlanId(), planInfo);
        logResponses(planResponse, "Add Interaction to Plan");
    }

    // Example: Call updateQualifications method
    if (shouldUpdateQualifications(createLogInteraction)) {
        QualificationsUpdateRequest qualificationsUpdateData = buildQualificationsUpdateRequest(createLogInteraction);
        ResponseEntity<GenericResponse<ResponseStatus>> qualificationResponse = updateQualifications(createLogInteraction.getAccountNumber(), qualificationsUpdateData);
        logResponses(qualificationResponse, "Update Qualifications");
    }

    // Example: Call updatePreferencesData method
    if (shouldUpdatePreferences(createLogInteraction)) {
        CustomerPreferencesRequest preferencesRequest = buildCustomerPreferencesRequest(createLogInteraction);
        ResponseEntity<GenericResponse<CustomerPreferencesResponseStatus>> preferencesResponse = updatePreferencesData(preferencesRequest);
        logResponses(preferencesResponse, "Update Preferences");
    }

    // Example: Call updateHighdiscoveryQues method
    if (shouldUpdateHighDiscoveryQuestions(createLogInteraction)) {
        HighDiscoveryQuestionRequest highDiscoveryRequest = buildHighDiscoveryQuestionRequest(createLogInteraction);
        ResponseEntity<GenericResponse<ResponseStatus>> highDiscoveryResponse = updateHighdiscoveryQues(highDiscoveryRequest);
        logResponses(highDiscoveryResponse, "Update High Discovery Questions");
    }

    // Return the response for the log interaction creation
    return ResponseEntity.ok(logInteractionResponse);
}

// Utility method to log the response of each method call
private <T> void logResponses(ResponseEntity<GenericResponse<T>> response, String action) {
    if (response.getStatusCode().is2xxSuccessful()) {
        LOGGER.info("{} action was successful", action);
    } else {
        LOGGER.error("{} action failed with status: {}", action, response.getStatusCode());
    }
}

// Dummy condition-checking methods (Implement these based on your business logic)
private boolean shouldCreateEvent(CreateLogInteraction interaction) {
    return true; // Implement your logic here
}

private boolean shouldAddInteractionToPlan(CreateLogInteraction interaction) {
    return true; // Implement your logic here
}

private boolean shouldUpdateQualifications(CreateLogInteraction interaction) {
    return true; // Implement your logic here
}

private boolean shouldUpdatePreferences(CreateLogInteraction interaction) {
    return true; // Implement your logic here
}

private boolean shouldUpdateHighDiscoveryQuestions(CreateLogInteraction interaction) {
    return true; // Implement your logic here
}

// Dummy builder methods for constructing request objects (Implement these based on your data model)
private CreateEvent buildCreateEvent(CreateLogInteraction interaction) {
    // Create and return a CreateEvent object
    return new CreateEvent();
}

private PlanInfo buildPlanInfo(CreateLogInteraction interaction) {
    // Create and return a PlanInfo object
    return new PlanInfo();
}

private QualificationsUpdateRequest buildQualificationsUpdateRequest(CreateLogInteraction interaction) {
    // Create and return a QualificationsUpdateRequest object
    return new QualificationsUpdateRequest();
}

private CustomerPreferencesRequest buildCustomerPreferencesRequest(CreateLogInteraction interaction) {
    // Create and return a CustomerPreferencesRequest object
    return new CustomerPreferencesRequest();
}

private HighDiscoveryQuestionRequest buildHighDiscoveryQuestionRequest(CreateLogInteraction interaction) {
    // Create and return a HighDiscoveryQuestionRequest object
    return new HighDiscoveryQuestionRequest();
}

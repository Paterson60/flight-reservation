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
                                            service.getCustomerInfo(createLogInteraction.getCustomerId(), "6000", createLogInteraction.getUserRole(), 
											createLogInteraction.getLoginId()) :
                                            null))
                            .requestId(MDC.get(REQUEST_ID))
                            .message(HttpStatus.OK.getReasonPhrase())
                            .build());
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



I'd be glad to provide a Java code that integrates the updateQualifications method into the createLogInteractionWithActions method, considering the specific context and requirements you've mentioned:
@PostMapping(value = "/customers/{customer-id}/log-interactions", produces = {MediaType.APPLICATION_JSON_VALUE})
public ResponseEntity<GenericResponse> createLogInteractionWithActions(
        @PathVariable("customer-id") String customerId,
        @RequestBody @NonNull CreateLogInteraction createLogInteraction) {

    // ... (existing validation and log interaction creation logic)

    // Check if the account number exists in the createLogInteraction
    String accountNumber = createLogInteraction.getAccountNumber();
    if (accountNumber != null) {
        // Construct the URI for the updateQualifications endpoint
        String updateQualificationsUri = "http://localhost:8080/accounts/" + accountNumber + "/qualifications";

        // Call the updateQualifications endpoint asynchronously
        Mono<GenericResponse> updateQualificationsResponse = webClient.post()
                .uri(updateQualificationsUri)
                .body(Mono.just(qualificationsUpdateData), QualificationsUpdateData.class)
                .retrieve()
                .bodyToMono(GenericResponse.class);

        // Handle the response from updateQualifications (e.g., log success/failure)
        updateQualificationsResponse.subscribe(response -> {
            if (response.getStatusCode() == HttpStatus.OK) {
                LOGGER.info("Qualifications updated successfully for account: {}", accountNumber);
            } else {
                LOGGER.error("Failed to update qualifications for account: {}", accountNumber);
            }
        }, error -> {
            LOGGER.error("Error updating qualifications: {}", error.getMessage());
        });
    }

    // Return the response for creating the log interaction
    return ResponseEntity.status(HttpStatus.CREATED)
            .body(new GenericResponse<>(CreateLogInteractionResponseStatus.OK));
}

Explanation:
 * Check Account Number: Before calling updateQualifications, check if the accountNumber is present in the createLogInteraction object. If it's not, skip the update.
 * Construct URI: If the account number exists, construct the URI for the updateQualifications endpoint using the provided account number.
 * Asynchronous Call: Use WebClient to make an asynchronous POST request to the updateQualifications endpoint, sending the necessary update data.
 * Handle Response: Subscribe to the response to handle success or failure cases. You can log appropriate messages or take other actions based on the response status.
 * Return Original Response: Return the original response for creating the log interaction, indicating that the operation was successful, regardless of whether the qualifications update was successful.
Key Points:
 * The updateQualifications call is made asynchronously to avoid blocking the main thread.
 * You can customize the error handling and logging logic based on your specific requirements.
 * Ensure that the qualificationsUpdateData object contains the correct data for updating qualifications.
 * This code assumes you have a WebClient instance configured and accessible within your application.
By following these steps, you can effectively integrate the updateQualifications endpoint into the createLogInteractionWithActions method, ensuring that qualifications are updated whenever a new log interaction is created and the account number is provided.



gtp
@PostMapping(value = "/customers/{customer-id}/log-interactions", produces = {MediaType.APPLICATION_JSON_VALUE})
public ResponseEntity<GenericResponse> createLogInteractionWithActions(
        @PathVariable("customer-id") String customerId, 
        @RequestBody @NonNull CreateLogInteraction createLogInteraction) {

    // Validate the create interaction request
    if (Boolean.FALSE.equals(interactionService.validCreateInteractionResponse(customerId, createLogInteraction))) {
        return ResponseEntity.badRequest().body(GenericResponse.builder()
                .requestId(MDC.get(REQUEST_ID))
                .message("Bad create log interaction request")
                .build());
    } else {
        if (LOGGER.isDebugEnabled())
            LOGGER.debug("Request received to create log interaction for customerId : {}", customerId);

        // Create log interaction
        GenericResponse interactionResponse = GenericResponse.builder()
            .data(interactionService.createLogInteraction(
                customerId, 
                createLogInteraction, 
                Boolean.TRUE.equals(createLogInteraction.getEmailForm().getSendEmail()) 
                    ? service.getCustomerInfo(createLogInteraction.getCustomerId(), "6000", 
                        createLogInteraction.getUserRole(), createLogInteraction.getLoginId()) 
                    : null))
            .requestId(MDC.get(REQUEST_ID))
            .message(HttpStatus.OK.getReasonPhrase())
            .build();

        // Logic to check if qualifications should be updated
        if (shouldUpdateQualifications(createLogInteraction)) {
            if (LOGGER.isDebugEnabled()) 
                LOGGER.debug("Updating qualifications for account number: {}", createLogInteraction.getAccountNumber());
            
            QualificationsUpdateRequest qualificationsUpdateData = buildQualificationsUpdateRequest(createLogInteraction);

            // Call the updateQualifications method here
            ResponseEntity<GenericResponse> qualificationResponse = updateQualifications(
                createLogInteraction.getAccountNumber(), qualificationsUpdateData);

            if (!qualificationResponse.getStatusCode().is2xxSuccessful()) {
                return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(GenericResponse.builder()
                        .requestId(MDC.get(REQUEST_ID))
                        .message("Failed to update qualifications")
                        .build());
            }
        }

        // Return the response for creating the log interaction
        return ResponseEntity.ok(interactionResponse);
    }
}

// Example method to decide when to update qualifications
private boolean shouldUpdateQualifications(CreateLogInteraction createLogInteraction) {
    // Implement logic to determine whether qualifications need to be updated
    return createLogInteraction.isUpdateQualifications(); // Placeholder condition
}

// Example method to build the QualificationsUpdateRequest object
private QualificationsUpdateRequest buildQualificationsUpdateRequest(CreateLogInteraction createLogInteraction) {
    // Build and return the QualificationsUpdateRequest object based on the interaction details
    QualificationsUpdateRequest request = new QualificationsUpdateRequest();
    // Set the necessary fields from createLogInteraction
    return request;
}


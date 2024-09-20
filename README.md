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

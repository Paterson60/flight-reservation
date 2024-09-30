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

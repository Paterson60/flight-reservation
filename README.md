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


I have written junit testcase for the createLogInteractionOffline() present in customerController.
Can you read the each line of junit test below which I written and understand it


String customerId = "customerId";
        String planId = "planId";

        LogInteractionOfflineRequestWrapper requestWrapper = new LogInteractionOfflineRequestWrapper();
        CreateLogInteraction createLogInteraction = mock(CreateLogInteraction.class);
        requestWrapper.setLogInteraction(createLogInteraction);

        LogInteractionOfflineRequest logInteractionOfflineRequest = mock(LogInteractionOfflineRequest.class);
        requestWrapper.setRequests(logInteractionOfflineRequest);

        lenient().when(interactionService.validCreateInteractionResponse(customerId, createLogInteraction)).thenReturn(true);
        customerController.createLogInteractionOffline(customerId, planId, requestWrapper);

        ResponseStatus searchResponse = new ResponseStatus();
        searchResponse.setMessage("updated Successfully");
        searchResponse.setStatus(200);
        String userId = "xyz987";
        String account = "8322083";
        HighDiscoveryQuestionRequest highDiscoveryQuestionRequest = new HighDiscoveryQuestionRequest();
        List<UpdateHighDiscoveryQuestionData> updateHighDiscoveryQuestionDataList = new ArrayList<>();
        UpdateHighDiscoveryQuestionData updateHighDiscoveryQuestionData = new UpdateHighDiscoveryQuestionData();
        updateHighDiscoveryQuestionData.setAnswer("answer");
        updateHighDiscoveryQuestionData.setAnswerId("answerId");
        updateHighDiscoveryQuestionData.setQuestionId("questionId");
        updateHighDiscoveryQuestionDataList.add(updateHighDiscoveryQuestionData);

        highDiscoveryQuestionRequest.setHighDiscoveryQuestion(updateHighDiscoveryQuestionDataList);
        highDiscoveryQuestionRequest.setUserId(userId);
        highDiscoveryQuestionRequest.setAccountNumber(account);
        when(customerKycService.updateHighdiscoveryQues(highDiscoveryQuestionRequest))
                .thenReturn(searchResponse);
        ResponseEntity<GenericResponse<ResponseStatus>> responseEntity = customerKycController.updateHighdiscoveryQues(highDiscoveryQuestionRequest);
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertNotNull(responseEntity.getBody());
        assertEquals(200, responseEntity.getBody().getData().getStatus());
        assertEquals("updated Successfully", responseEntity.getBody().getData().getMessage());

        QualificationsUpdateRequest qualificationsUpdateRequest= Mockito.spy(QualificationsUpdateRequest.class);
        String accountNumber="111111111";
        List<Qualification> qualificationList=new ArrayList<>();
        Qualification qualification1=Mockito.spy(Qualification.class);
        qualification1.setDisplayOrder("1");
        qualification1.setAnswerKey("answerKey");
        qualification1.setQuestion("question");
        qualification1.setQuestionId("1");
        qualification1.dropDownApiId("2");
        qualification1.setAnswerValue("answerValue");
        qualificationList.add(qualification1);
        qualificationsUpdateRequest.setUpdateQualifications(qualificationList);
        qualificationsUpdateRequest.setAccountNumber("111111111");
        ResponseStatus responseStatus=Mockito.spy(ResponseStatus.class);
        responseStatus.setStatus(200);
        responseStatus.setMessage("Successful");
        ResponseEntity<ResponseStatus> qresponseEntity=new ResponseEntity<>(responseStatus,HttpStatus.OK);
        when(customerQualificationService.updateQualifications(accountNumber,qualificationsUpdateRequest)).thenReturn(qresponseEntity);
        ResponseEntity<GenericResponse<ResponseStatus>> qresponse=customerQualificationController.updateQualifications(accountNumber,qualificationsUpdateRequest);
        assertNotNull(qresponse);
        assertNotNull(qresponse.getBody());
        assertNotNull(qresponse.getBody().getData());
        assertEquals(HttpStatus.OK,qresponse.getStatusCode());
        assertThat(qresponse.getBody().getData()).usingRecursiveComparison().isEqualTo(responseStatus);


        CustomerPreferencesRequest customerPreferencesRequest = new CustomerPreferencesRequest();
        CustomerPreferencesResponseStatus customerPreferencesResponseStatus = new CustomerPreferencesResponseStatus();
        CustomerPreferencesUpdateQuestions customerPreferencesUpdateQuestions = new CustomerPreferencesUpdateQuestions();
        List<CustomerPreferencesUpdateQuestions> customerPreferencesUpdateQuestionsList = new ArrayList<>();

        customerPreferencesRequest.setAccountNumber("8327363");

        customerPreferencesUpdateQuestions.setQuestionId("1234");
        customerPreferencesUpdateQuestions.setMessage("Updated to DB Successfull");
        customerPreferencesUpdateQuestions.setStatus(200);
        customerPreferencesUpdateQuestionsList.add(customerPreferencesUpdateQuestions);
        customerPreferencesResponseStatus.setCustomerPreferencesUpdateQuestions(customerPreferencesUpdateQuestionsList);

        when(preferencesService.updatePreferencesData(customerPreferencesRequest))
                .thenReturn(customerPreferencesResponseStatus);
        ResponseEntity<GenericResponse<CustomerPreferencesResponseStatus>> prefresponse = preferencesController
                .updatePreferencesData(customerPreferencesRequest);

        assertEquals(HttpStatus.OK, prefresponse.getStatusCode());
        assertNotNull(prefresponse.getBody());
        assertNotNull(prefresponse.getBody().getData().getCustomerPreferencesUpdateQuestions());
        assertNotNull(prefresponse.getBody().getData().getCustomerPreferencesUpdateQuestions().get(0));
        assertEquals(200, prefresponse.getBody().getData().getCustomerPreferencesUpdateQuestions().get(0).getStatus());
        assertEquals("1234", prefresponse.getBody().getData().getCustomerPreferencesUpdateQuestions().get(0).getQuestionId());

        verify(interactionServiceImplOffline, times(1)).createLogInteractionOffline(planId, requestWrapper);

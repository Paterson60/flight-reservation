package com.service.message.dto;

/**
 *
 * @param quantity
 * @param sku
 * @param category
 * 
 */
public record InventoryMsgDto(String sku, int quantity, String category) {
}

Message funciton

package com.service.message.functions;

import com.service.message.dto.InventoryMsgDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.function.Function;

@Configuration
public class MessageFunctions {

    private static final Logger log = LoggerFactory.getLogger(MessageFunctions.class);

    @Bean
    public Function<InventoryMsgDto, InventoryMsgDto> message(){
            return inventoryMsgDto -> {
                log.info("Quantity of a product is less than 10: " + inventoryMsgDto.toString());
                return inventoryMsgDto;
            };
    }

    @Bean
    public Function<InventoryMsgDto, String> sms(){
        return inventoryMsgDto -> {
            log.info("Sending sms with the details: " + inventoryMsgDto);
            return inventoryMsgDto.sku();
        };
    }
}


StreamLambda Handler

package com.service.message;

import com.amazonaws.serverless.exceptions.ContainerInitializationException;
import com.amazonaws.serverless.proxy.model.AwsProxyRequest;
import com.amazonaws.serverless.proxy.model.AwsProxyResponse;
import com.amazonaws.serverless.proxy.spring.SpringBootLambdaContainerHandler;
import com.amazonaws.services.lambda.runtime.Context;
import com.amazonaws.services.lambda.runtime.RequestStreamHandler;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class StreamLambdaHandler implements RequestStreamHandler {
    private static SpringBootLambdaContainerHandler<AwsProxyRequest, AwsProxyResponse> handler;
    static {
        try {
            handler = SpringBootLambdaContainerHandler.getAwsProxyHandler(MessageApplication.class);
            // If you are using HTTP APIs with the version 2.0 of the proxy model, use the getHttpApiV2ProxyHandler
            // method: handler = SpringBootLambdaContainerHandler.getHttpApiV2ProxyHandler(Application.class);
        } catch (ContainerInitializationException e) {
            // if we fail here. We re-throw the exception to force another cold start
            e.printStackTrace();
            throw new RuntimeException("Could not initialize Spring Boot application", e);
        }
    }

    @Override
    public void handleRequest(InputStream inputStream, OutputStream outputStream, Context context)
            throws IOException {
        handler.proxyStream(inputStream, outputStream, context);
    }
}

application.yml
spring:
  application :
    name : "message"
  cloud:
    function:
      definition: message|sms

inventory catalogue

package com.service.inventorycatalogue.dto;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * @param quantity
 * @param sku
 * @param category
 * Dto layer for receiving the message for message artifact
 */

@Schema(
        name = "InventoryMsdDto",
        description = "Schema to hold Inventory message details"
)
public record InventoryMsgDto(String sku, int quantity, String category) {
}


entity

package com.service.inventorycatalogue.entity;

/**
 * Entity layer of Inventory Catalogue
 *
 */

import jakarta.persistence.*;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Positive;
import lombok.*;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import java.time.LocalDateTime;

@Entity
@Getter@Setter@ToString@AllArgsConstructor@NoArgsConstructor
@EntityListeners(AuditingEntityListener.class)
@Table(name = "inventory")
public class InventoryEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "inventory_Id")
    private Long id;

    @Column(unique=true)
    private String sku;

    @Min(1)
    @Max(100)
    @Positive
    private int quantity;

    private String category;

    private String status;

    private Boolean communication;

    @CreatedDate
    @Column(updatable = false)
    private LocalDateTime createdAt;

    @LastModifiedDate
    @Column(insertable = false)
    private LocalDateTime updatedAt;
}

Inventory function

package com.service.inventorycatalogue.functions;
/**
 * Consumes the message from message artifact of the queue message
 *
 */
import com.service.inventorycatalogue.service.IInventoryService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.function.Consumer;

@Configuration
public class InventoryFunctions {

    private static final Logger log = LoggerFactory.getLogger(InventoryFunctions.class);

    @Bean
    public Consumer<String> updateCommunication(IInventoryService iInventoryService){
        return sku -> {
            log.info("Updating Communication status: " + sku);
            iInventoryService.updateCommunication(sku);
        };
    }
}


package com.service.inventorycatalogue.service.impl;

/**
 * InventoryServiceImpl layer helps in performing CRUD operation and other business/
 * logic like sending communication
 *
 */

import com.service.inventorycatalogue.dto.InventoryDto;
import com.service.inventorycatalogue.dto.InventoryMsgDto;
import com.service.inventorycatalogue.dto.UpdateQuantityAFOrderDto;
import com.service.inventorycatalogue.entity.InventoryEntity;
import com.service.inventorycatalogue.exception.ResourceNotFoundException;
import com.service.inventorycatalogue.exception.SkuExistsException;
import com.service.inventorycatalogue.mapper.InventoryMapper;
import com.service.inventorycatalogue.mapper.UpdateQuantityAFOrderMapper;
import com.service.inventorycatalogue.repository.InventoryRepository;
import com.service.inventorycatalogue.service.IInventoryService;
import lombok.AllArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
@AllArgsConstructor
public class InventoryServiceImpl implements IInventoryService {

@Override
    public boolean reduceStock(UpdateQuantityAFOrderDto updateQuantityAFOrderDto, int quantity) {

        boolean isOrdered = false;
        if(updateQuantityAFOrderDto != null) {
            InventoryEntity inventoryEntity = inventoryRepository.findBySku(updateQuantityAFOrderDto.getSku()).orElseThrow(
                    () -> new ResourceNotFoundException("Inventory","Sku",updateQuantityAFOrderDto.getSku())
            );
            UpdateQuantityAFOrderMapper.mapToInventoryEntity(updateQuantityAFOrderDto, inventoryEntity);
            int newQuantity = inventoryEntity.getQuantity() - quantity;
            inventoryEntity.setQuantity(newQuantity);
            InventoryEntity savedInventoryQuantity = inventoryRepository.save(inventoryEntity);
            if (savedInventoryQuantity.getQuantity() < 10){
                sendCommunication(savedInventoryQuantity);
            }
            isOrdered = true;
        }
        return isOrdered;
    }

    /**
     * Helps in communication with Product Catalogue
     * sendCommunication method is integrated for communication with Product Catalogue
     * @param inventoryEntity
     * @return
     */
    private void sendCommunication(InventoryEntity inventoryEntity){
        var inventoryMsgDto = new InventoryMsgDto( inventoryEntity.getSku(),
                inventoryEntity.getQuantity(), inventoryEntity.getCategory());
        log.info("Sending Communication request for the details:{}", inventoryMsgDto);
        var result = streamBridge.send("sendCommunication-out-0", inventoryMsgDto);
        log.info("The Communication request successfully triggered?:{}", result);
    }

    /**
     * Sends the updated status and communication once the sendCommunication method is invoked in reduceStock method
     * @param sku
     * @return
     */
    @Override
    public boolean updateCommunication(String sku) {
        boolean isUpdated = false;
        if(sku != null){
            InventoryEntity inventoryEntity = inventoryRepository.findBySku(sku).orElseThrow(
                    () -> new ResourceNotFoundException("Inventory","Sku",sku)
            );
            inventoryEntity.setStatus("Product ending soon");
            inventoryEntity.setCommunication(true);
            inventoryRepository.save(inventoryEntity);
            isUpdated = true;
        }
        return isUpdated;
    }

package com.service.inventorycatalogue.controller;

/**
* Controller class
*
*/

import com.service.inventorycatalogue.constants.InventoryConstants;
import com.service.inventorycatalogue.dto.ErrorResponseDto;
import com.service.inventorycatalogue.dto.InventoryDto;
import com.service.inventorycatalogue.dto.ResponseDto;
import com.service.inventorycatalogue.dto.UpdateQuantityAFOrderDto;
import com.service.inventorycatalogue.service.IInventoryService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotEmpty;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@Tag(
        name = "CRUD Rest APIs of Inventory Management Service",
        description = "CRUD Rest APIs in Inventory"
)
@RestController
@RequestMapping(path="/api", produces = {MediaType.APPLICATION_JSON_VALUE})
@Validated
public class InventoryController {
    controller 

     @PutMapping("/order")
    public ResponseEntity<ResponseDto> placeOrder(@Valid @RequestBody UpdateQuantityAFOrderDto updateQuantityAFOrderDto, @RequestParam int quantity){
        boolean orderPlaced = iInventoryService.reduceStock(updateQuantityAFOrderDto, quantity);
        if(orderPlaced){
            return  ResponseEntity
                    .status(HttpStatus.OK)
                    .body(new ResponseDto(InventoryConstants.STATUS_200, InventoryConstants.MESSAGE_200));
        }else{
            return ResponseEntity
                    .status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(new ResponseDto(InventoryConstants.STATUS_500, InventoryConstants.MESSAGE_500));
        }
    }

As you remember I was building the logic for inventory microservice feature where prodcut reduces to certain threshold then it should send message in a queue that the quantity of the product is reduced below threshold I have kept the threshold number to 10 in inventory management microservice. Now this logic is present in inventory management microservice once the product is reduced to 10 it will invoke the message spring boot project to send message in queue.
Now the message springboot project and inventory management miscroservice are differnet project I need help here in how to create the lambda function in aws where the lambda function in aws should able to pick the functions from both message spring boot project and invenotry management microservice and it should get successful response 

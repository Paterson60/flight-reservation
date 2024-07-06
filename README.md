Below project name is message which I'm using it as event driven to send an message and sms which is built on springboot 3.
providing you the code which I have wrriten.

DTO package has the following code
package com.service.message.dto;

/**
 *
 * @param quantity
 * @param sku
 * @param category
 * Helps in sending the message to Inventory Catalogue
 */
public record InventoryMsgDto(String sku, int quantity, String category) {
}

Functions package has the following code
package com.service.message.dto;

/**
 *
 * @param quantity
 * @param sku
 * @param category
 * Helps in sending the message to Inventory Catalogue
 */
public record InventoryMsgDto(String sku, int quantity, String category) {
}

in parent package of message has the following code

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

package com.service.message;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;

@SpringBootApplication
@ComponentScan(basePackages= "com.service.message")
public class MessageApplication {

	public static void main(String[] args) {
		SpringApplication.run(MessageApplication.class, args);
	}

}

Now I'm providing you the code of microservice 2: inventory management serivce which has the logic to tirgger the event of message application

Entity Package has the following code:

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

    @CreatedDate
    @Column(updatable = false)
    private LocalDateTime createdAt;

    @LastModifiedDate
    @Column(insertable = false)
    private LocalDateTime updatedAt;
}

DTO package has the following code :

package com.service.inventorycatalogue.dto;

/**
 * Dto layer of InventoryEntity
 *
 */

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.*;
import lombok.Data;

@Data
@Schema(
        name = "Inventory",
        description = "Schema to hold Inventory details"
)
public class InventoryDto {

    @Schema(
            description = "Unique representation of the Inventory", example = "AZVP1!"
    )
    @NotEmpty(message = "Sku cannot be null")
    private String sku;

    @Schema(
            description = "Quantity of the Product", example = "20"
    )
    @Min(1)
    @Max(100)
    @Positive
    private int quantity;

    @Schema(
            description = "Category representation of the Inventory", example = "Mobile!"
    )
    @NotEmpty(message = "Category cannot be empty")
    private String category;

    @Schema(
            description = "Status representation of the Inventory"
    )
    @NotEmpty(message = "Status cannot be empty")
    private String status;
}


package com.service.message;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;

@SpringBootApplication
@ComponentScan(basePackages= "com.service.message")
public class MessageApplication {

	public static void main(String[] args) {
		SpringApplication.run(MessageApplication.class, args);
	}

}

package com.service.inventorycatalogue.dto;

/**
 * Dto layer to update quantity number
 *
 */

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import lombok.Data;

@Data
@Schema(
        name = "After Order Quantity update",
        description = "Schema to hold quantity details after order "
)
public class UpdateQuantityAFOrderDto {

    @Schema(
            description = "Unique representation of the Inventory", example = "AZVP1!"
    )
    @NotEmpty(message = "Sku cannot be null")
    private String sku;

    @Schema(
            description = "Quantity of the Product", example = "20"
    )
    @Min(1)
    @Max(100)
    @Positive
    private int quantity;
}

Mapper package has the following code :

package com.service.inventorycatalogue.mapper;

/**
 * Converts the Inventory Dto layer to Inventory Entity layer and vice versa
 *
 */

import com.service.inventorycatalogue.dto.InventoryDto;
import com.service.inventorycatalogue.entity.InventoryEntity;

public class InventoryMapper {

    public static InventoryDto mapToInventoryDto(InventoryEntity inventoryEntity, InventoryDto inventoryDto){
        inventoryDto.setSku(inventoryEntity.getSku());
        inventoryDto.setQuantity(inventoryEntity.getQuantity());
        inventoryDto.setCategory(inventoryEntity.getCategory());
        inventoryDto.setStatus(inventoryEntity.getStatus());
        return inventoryDto;
    }

    public static InventoryEntity mapToInventoryEntity(InventoryDto inventoryDto, InventoryEntity inventoryEntity){
        inventoryEntity.setSku(inventoryDto.getSku());
        inventoryEntity.setQuantity(inventoryDto.getQuantity());
        inventoryEntity.setCategory(inventoryDto.getCategory());
        inventoryEntity.setStatus(inventoryDto.getStatus());
        return inventoryEntity;
    }
}

package com.service.inventorycatalogue.mapper;

/**
 * Converts the UpdateQuantityAFOrderDto Dto layer to Inventory Entity layer and vice versa
 *
 */

import com.service.inventorycatalogue.dto.InventoryDto;
import com.service.inventorycatalogue.dto.UpdateQuantityAFOrderDto;
import com.service.inventorycatalogue.entity.InventoryEntity;

public class UpdateQuantityAFOrderMapper {

    public static UpdateQuantityAFOrderDto mapToUpdateQuantityAFOrderDto(InventoryEntity inventoryEntity, UpdateQuantityAFOrderDto updateQuantityAFOrderDto){
        updateQuantityAFOrderDto.setSku(inventoryEntity.getSku());
        updateQuantityAFOrderDto.setQuantity(inventoryEntity.getQuantity());
        return updateQuantityAFOrderDto;
    }

    public static InventoryEntity mapToInventoryEntity(UpdateQuantityAFOrderDto updateQuantityAFOrderDto, InventoryEntity inventoryEntity){
        inventoryEntity.setSku(updateQuantityAFOrderDto.getSku());
        inventoryEntity.setQuantity(updateQuantityAFOrderDto.getQuantity());
        return inventoryEntity;
    }
}

functions package has the following code :
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

service package has the following code:
package com.service.inventorycatalogue.service;

import com.service.inventorycatalogue.dto.InventoryDto;
import com.service.inventorycatalogue.dto.UpdateQuantityAFOrderDto;

public interface IInventoryService {
boolean reduceStock(UpdateQuantityAFOrderDto updateQuantityAFOrderDto, int quantity);

boolean updateCommunication(String sku);
}

service.impl package has the following code:
package com.service.inventorycatalogue.service.impl;
import com.service.inventorycatalogue.dto.InventoryDto;
import com.service.inventorycatalogue.dto.InventoryMsgDto;
import com.service.inventorycatalogue.dto.UpdateQuantityAFOrderDto;
import com.service.inventorycatalogue.mapper.UpdateQuantityAFOrderMapper;
import com.service.inventorycatalogue.repository.InventoryRepository;
import com.service.inventorycatalogue.service.IInventoryService;
import com.service.inventorycatalogue.entity.InventoryEntity;
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

    repository package has the following code :
    package com.service.inventorycatalogue.repository;

/**
 * Repository layer helps in performing CRUD operation
 *
 */

import com.service.inventorycatalogue.entity.InventoryEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface InventoryRepository extends JpaRepository<InventoryEntity,Long> {
    Optional<InventoryEntity> findBySku(String sku);
}

now analyse above both the application code written and help me in how can I create lambda function of message application,
and how can create a message event driven SQS in aws how can a message can be invoked once the product quantity is less then 10 how can I create lambda function of message, sms. And consumer in consumer method of inventory microservice

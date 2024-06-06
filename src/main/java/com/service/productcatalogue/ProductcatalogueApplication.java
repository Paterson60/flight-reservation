package com.service.productcatalogue;

import io.swagger.v3.oas.annotations.ExternalDocumentation;
import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.info.Contact;
import io.swagger.v3.oas.annotations.info.Info;
import io.swagger.v3.oas.annotations.info.License;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;

@SpringBootApplication
@EnableJpaAuditing
@OpenAPIDefinition(
		info = @Info(
				title = "Product Catalogue Service API Documentation",
				description = "This service is primarily responsible for maintaining and managing the entire product catalogue of an eCommerce site",
				version = "v1",
				contact = @Contact(
						name = "Paterson",
						email = "paterson4@deloitte.com",
						url = "https://people.deloitte/profile/paterson4"
				),
				license = @License(
						name = "Apache.2.0",
						url="https://people.deloitte/profile/paterson4"
				)
		),
		externalDocs = @ExternalDocumentation(
				description = "Product Catalogue Service API",
				url="http://localhost:8080/swagger-ui/index.html"
		)
)
public class ProductcatalogueApplication {

	public static void main(String[] args) {
		SpringApplication.run(ProductcatalogueApplication.class, args);
	}

}
